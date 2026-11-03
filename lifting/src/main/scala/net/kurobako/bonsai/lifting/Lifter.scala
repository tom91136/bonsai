package net.kurobako.bonsai.lifting

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.IntTag
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Symbols.requiredModule
import dotty.tools.dotc.util.Spans.Span
import net.kurobako.bonsai.collections.*
import scala.annotation.tailrec
import scala.collection.mutable

/** Reads chains of collection calls from typed trees, with the purity of their functions from `purity`, and which of
  * them are in classes whose qualified names `hot` accepts.
  */
class Lifter(purity: Purity, hot: String => Boolean = _ => false)(using Context) {
  private val kinds = Kinds()

  private case class Link(
      end: tpd.Tree,
      shown: tpd.Tree,
      receiver: tpd.Tree,
      method: String,
      args: List[Arg],
      spine: List[tpd.Tree],
      symbol: Option[Symbol],
      lowered: Boolean = false,
      component: Boolean = false,
      typed: Boolean = false,
      retyped: Boolean = false,
      desugared: Boolean = false,
      guard: Boolean = false,
      transparent: Boolean = false,
      opaque: Boolean = false,
      through: Option[tpd.Tree] = None
  )

  private enum Filtering {
    case Guard, Irrefutable, Refutable
  }

  private val SizeComparisons = Set("==", "!=", ">")
  private val Generated       = Set("map", "flatMap", "foreach", "withFilter")
  private val Yields          = Set("map", "flatMap", "foreach")
  private val Components      = Set("_1", "_2", "_3")
  private val Filters         = Set("filter", "withFilter")

  /** The maximal chains under `root`, innermost arguments included, that call a standard collection method; `owner` is
    * the method the code belongs to, and `descend` says whether the trees under a tree are read.
    */
  def chains(root: tpd.Tree, owner: Symbol, descend: tpd.Tree => Boolean = _ => true): Vector[Lifted] = {
    val traverser = Chains(owner, descend)
    traverser.traverse(root)
    traverser.found.toVector
  }

  private class Chains(owner: Symbol, descend: tpd.Tree => Boolean) extends tpd.TreeTraverser {
    val found: mutable.ArrayBuffer[Lifted] = mutable.ArrayBuffer.empty[Lifted]
    private val spine                      = mutable.Set.empty[tpd.Tree]
    // The cursor of a traversal that visits every tree of the build.
    // scalafix:off DisableSyntax.var
    private var ancestors: List[tpd.Tree] = Nil
    private var enclosing: Symbol         = owner
    private var surrounding: Symbol       = owner
    // scalafix:on DisableSyntax.var

    def traverse(tree: tpd.Tree)(using Context): Unit = if (descend(tree)) {
      tree match {
        case tpd.Apply(function, _)     => spine += function
        case tpd.TypeApply(function, _) => spine += function
        case _                          => ()
      }
      if (!spine(tree) && !flattened(tree, ancestors))
        build(tree, enclosing, surrounding, ancestors).filter(_._1.chain.steps.exists(_.receiver.library)).foreach {
          lifted =>
            found += lifted._1
            spine ++= lifted._2
        }
      val before = enclosing
      val outer  = surrounding
      tree match {
        case definition: tpd.DefDef if !definition.symbol.isAnonymousFunction => enclosing = definition.symbol
        case definition: tpd.TypeDef if definition.symbol.isClass             => surrounding = definition.symbol
        case _                                                                => ()
      }
      ancestors = tree :: ancestors
      traverseChildren(tree)
      ancestors = ancestors.tail
      enclosing = before
      surrounding = outer
    }
  }

  private def build(
      tree: tpd.Tree,
      owner: Symbol,
      surrounding: Symbol,
      ancestors: List[tpd.Tree]
  ): Option[(Lifted, List[tpd.Tree])] = {
    @annotation.tailrec
    def walk(current: tpd.Tree, links: List[Link]): (tpd.Tree, List[Link]) =
      binding(current, links.headOption)
        .orElse(link(current, owner, ancestors, links.headOption.map(_.method)))
        .orElse(guard(current, links.headOption, owner)) match {
        case Some(found) => walk(found.receiver, found :: links)
        case None        => (current, links)
      }
    val (source, walked) = walk(tree, Nil)
    val links            = walked.foldLeft(Vector.empty[Link]) { (kept, found) =>
      val repeated = found.guard && found.method == "withFilter" && kept.findLast(!_.transparent).exists(_.guard)
      kept :+ (if (repeated) found.copy(transparent = true, through = None) else found)
    }
    Option.when(
      links.exists(!_.transparent) && !links.exists(_.opaque) && links.forall(found => Texts.real(found.shown)) &&
        Texts.real(visible(source))
    ) {
      val (steps, _) = links.foldLeft((Vector.empty[Step], Option.empty[Kind])) { case ((built, carried), found) =>
        if (found.transparent) (built, found.through.flatMap(through => kinds.of(through.tpe)))
        else {
          val guarded  = built.lastOption.filter(step => step.desugared && Filters(step.method))
          val receiver =
            if (found.component) built.lastOption.map(_.result).getOrElse(kinds.scalar)
            else
              carried.orElse(guarded.map(_.result)).orElse(kinds.of(found.receiver.tpe)).getOrElse(kinds.scalar)
          val adjusted = receiver.copy(library = receiver.library && found.symbol.forall(kinds.library))
          val result   =
            if (found.guard) adjusted.copy(strict = false)
            else if (found.lowered) kinds.scalar
            else if (Set("unzip", "unzip3")(found.method)) kinds.pair
            else kinds.of(found.end.tpe).getOrElse(kinds.scalar)
          (
            built :+ Step(
              found.method,
              found.args.toVector,
              adjusted,
              result,
              found.typed,
              found.desugared,
              found.retyped
            ),
            None
          )
        }
      }
      val file    = visible(source).sourcePos.source
      val extents = links.collect { case found if !found.transparent => balanced(file.content(), found.shown.span) }
      val lifted  = Lifted(
        Chain(
          Argument(visible(source), purity, owner),
          steps,
          assertion(tree, ancestors),
          heated(surrounding),
          yielded(tree, ancestors)
        ),
        file,
        balanced(file.content(), visible(source).span),
        extents,
        standalone(tree, ancestors)
      )
      (lifted, links.flatMap(_.spine).toList :+ tree)
    }
  }

  private def heated(owner: Symbol): Boolean =
    owner.ownersIterator.exists(symbol =>
      symbol.isClass && !symbol.is(Flags.Package) && !symbol.isAnonymousClass && hot(Calls.qualified(symbol))
    )

  /** Whether `tree` is what the function of a generator that the compiler wrote returns, which is how the generators
    * after the first of a for-comprehension nest.
    */
  @tailrec
  private def yielded(tree: tpd.Tree, ancestors: List[tpd.Tree]): Boolean = ancestors match {
    case (alternative: tpd.CaseDef) :: (selection: tpd.Match) :: rest if alternative.body eq tree =>
      yielded(selection, rest)
    case (block: tpd.Block) :: rest if block.expr eq tree            => yielded(block, rest)
    case (inlined: tpd.Inlined) :: rest if inlined.expansion eq tree => yielded(inlined, rest)
    case (definition: tpd.DefDef) :: (block: tpd.Block) :: (tpd.Apply(function, arguments)) :: _
        if (definition.rhs eq tree) && definition.symbol.isAnonymousFunction && arguments.exists(_ eq block) =>
      Calls.selected(function).exists(select => select.name.toString == "flatMap" && desugared(select))
    case _ => false
  }

  /** Whether `tree` is the map of the elements of a row to themselves that the compiler writes for the innermost
    * generator of `for { row <- rows; element <- row } yield element`, which `flatMap` identity already is.
    */
  private def flattened(tree: tpd.Tree, ancestors: List[tpd.Tree]): Boolean = ancestors.headOption.exists {
    case definition: tpd.DefDef =>
      (definition.rhs eq tree) && Parameters.sole(definition).exists(Argument.flattening(tree, _))
    case _ => false
  }

  @tailrec
  private def visible(tree: tpd.Tree): tpd.Tree = tree match {
    case tpd.Apply(function, _) if function.tpe.widen.isImplicitMethod => visible(function)
    case other                                                         => other
  }

  /** `span` of `content`, ended later where it closes fewer brackets than it opens, which the span of an infix call
    * with a parenthesised argument does.
    */
  private def balanced(content: Array[Char], span: Span): Span = {
    @tailrec def opened(at: Int, depth: Int): Int =
      if (at < span.end) opened(
        at + 1,
        depth +
          (content(at) match {
            case '(' | '[' | '{' => 1
            case ')' | ']' | '}' => -1
            case _               => 0
          })
      )
      else depth
    @tailrec def closed(end: Int, depth: Int): Int =
      if (depth > 0 && end < content.length && (content(end) == ')' || content(end) == ']' || content(end) == '}'))
        closed(end + 1, depth - 1)
      else end
    Span(span.start, closed(span.end, opened(span.start, 0)))
  }

  private def assertion(tree: tpd.Tree, ancestors: List[tpd.Tree]): Boolean = ancestors.headOption.exists {
    case tpd.Apply(function, arguments) =>
      arguments.exists(_ eq tree) && function.symbol.name.toString.startsWith("assertEquals") &&
      function.symbol.owner.fullName.toString == "munit.Assertions"
    case _ => false
  }

  private def stepped(tree: tpd.Tree, consumer: Option[String], ancestors: List[tpd.Tree]): Boolean =
    consumer.orElse(ancestors.headOption.collect {
      case tpd.Select(qualifier, name) if qualifier eq tree => name.toString
    }).contains("by")

  private def standalone(tree: tpd.Tree, ancestors: List[tpd.Tree]): Boolean = ancestors.headOption.exists {
    case definition: tpd.ValDef  => definition.rhs eq tree
    case definition: tpd.DefDef  => !definition.symbol.isAnonymousFunction && (definition.rhs eq tree)
    case block: tpd.Block        => (block.expr eq tree) || block.stats.exists(_ eq tree)
    case tpd.Assign(_, rhs)      => rhs eq tree
    case tpd.Return(expr, _)     => expr eq tree
    case tpd.Apply(_, arguments) => arguments.exists(_ eq tree)
    case _                       => false
  }

  private def link(tree: tpd.Tree, owner: Symbol, ancestors: List[tpd.Tree], consumer: Option[String]): Option[Link] =
    lowered(tree, owner, ancestors, consumer).orElse(component(tree)).orElse(call(tree, owner, ancestors))

  private def argument(tree: tpd.Tree, owner: Symbol): Arg = Argument(tree, purity, owner)

  /** The tree a conversion or a supply of implicit arguments wraps, with the wrappers. */
  @annotation.tailrec
  private def unconverted(tree: tpd.Tree, wrappers: List[tpd.Tree] = Nil): (tpd.Tree, List[tpd.Tree]) = tree match {
    case tpd.Apply(function, List(wrapped)) if function.symbol.is(Flags.Implicit) =>
      unconverted(wrapped, tree :: wrappers)
    case tpd.Apply(inner: tpd.Apply, arguments)
        if inner.tpe.widen.isImplicitMethod && arguments.forall(_.span.isSynthetic) =>
      unconverted(inner, tree :: wrappers)
    case other => (other, wrappers)
  }

  private def stripped(tree: tpd.Tree): tpd.Tree = unconverted(tree)._1

  private def operand(found: Call): (tpd.Tree, List[tpd.Tree]) = unconverted(found.select.qualifier)

  /** Whether `qualifier` names a package, an object or the enclosing instance, which a selection from reads a member of
    * rather than calls on a receiver.
    */
  private def path(qualifier: tpd.Tree): Boolean = stripped(qualifier) match {
    case _: (tpd.This | tpd.Super)                  => true
    case reference @ (_: tpd.Ident | _: tpd.Select) =>
      reference.symbol.exists && reference.symbol.isOneOf(Flags.Module | Flags.Package)
    case _ => false
  }

  private def isInt(tree: tpd.Tree, value: Int): Boolean = Calls.peelInlined(tree) match {
    case tpd.Literal(constant) => constant.value == value && constant.tag == IntTag
    case _                     => false
  }

  private def sizeSelect(tree: tpd.Tree): Option[(tpd.Tree, Symbol, List[tpd.Tree])] =
    Calls.peelInlined(tree) match {
      case select @ tpd.Select(qualifier, name) if Set("size", "length")(name.toString) && !path(qualifier) =>
        Some((qualifier, select.symbol, List(select)))
      case call @ tpd.Apply(select @ tpd.Select(qualifier, name), Nil)
          if Set("size", "length")(name.toString) && !path(qualifier) =>
        Some((qualifier, select.symbol, List(call, select)))
      case _ => None
    }

  private def lowered(
      tree: tpd.Tree,
      owner: Symbol,
      ancestors: List[tpd.Tree],
      consumer: Option[String]
  ): Option[Link] =
    tree match {
      case Binary(binary) =>
        sizeComparison(binary).orElse(countComparison(binary, owner)).orElse(equality(
          binary,
          owner
        )).orElse(indices(binary, stepped(tree, consumer, ancestors)))
      case _ => None
    }

  private def sizeComparison(binary: Binary) = {
    import binary.*
    if (SizeComparisons(operator) && isInt(right, 0))
      sizeSelect(left).map { (qualifier, symbol, spine) =>
        val method = if (operator == "==") "sizeIsZero" else "sizeNonZero"
        Link(tree, tree, stripped(qualifier), method, Nil, tree :: spine, Some(symbol), lowered = true)
      }
    else None
  }

  private def countComparison(binary: Binary, owner: Symbol): Option[Link] = {
    import binary.*
    if (SizeComparisons(operator) && isInt(right, 0))
      peel(Calls.peelInlined(left)).filter(found =>
        found.select.name.toString == "count" && found.lists.map(_.size) == List(1) &&
          found.select.symbol.is(Flags.Method) && kinds.library(found.select.symbol)
      ).flatMap { found =>
        val (receiver, wrappers) = operand(found)
        Option.unless(path(receiver)) {
          val method = if (operator == "==") "countZero" else "countNonZero"
          val spine  = tree :: found.path ++ wrappers
          Link(
            tree,
            tree,
            receiver,
            method,
            List(argument(found.lists.head.head, owner)),
            spine,
            Some(found.select.symbol),
            lowered = true
          )
        }
      }
    else None
  }

  private def equality(binary: Binary, owner: Symbol) = {
    import binary.*
    if (operator == "==" || operator == "!=")
      List(left -> right, right -> left).collectFirst {
        case (option, none) if isNone(none) && optional(option)              => (option, "isNone", Nil)
        case (option, some) if someValue(some).isDefined && optional(option) =>
          (option, "equalsSome", List(argument(someValue(some).get, owner)))
      }.map { (option, method, arguments) =>
        Link(
          tree,
          tree,
          stripped(option),
          if (operator == "==") method else negated(method),
          arguments,
          List(tree),
          None,
          lowered = true
        )
      }
    else None
  }

  private def optional(tree: tpd.Tree): Boolean = kinds.of(tree.tpe).exists(_.single)

  private def isNone(tree: tpd.Tree): Boolean = Calls.peelInlined(tree).symbol == defn.NoneModule

  private def someValue(tree: tpd.Tree): Option[tpd.Tree] = Calls.peelInlined(tree) match {
    case tpd.Apply(function, List(value))
        if function.symbol.name.toString == "apply" &&
          function.symbol.owner == requiredModule("scala.Some").moduleClass =>
      Some(value)
    case _ => None
  }

  private def negated(method: String): String = method match {
    case "isNone"     => "isSome"
    case "equalsSome" => "notEqualsSome"
    case other        => other
  }

  private def indices(binary: Binary, stepped: Boolean): Option[Link] = {
    import binary.*
    if (stepped) None
    else if (isInt(stripped(left), 0) && operator == "until")
      sizeSelect(right).map((qualifier, symbol, spine) =>
        Link(tree, tree, stripped(qualifier), "untilSize", Nil, tree :: spine, Some(symbol), lowered = true)
      )
    else if (isInt(stripped(left), 0) && operator == "to")
      Calls.peelInlined(right) match {
        case tpd.Apply(tpd.Select(size, minus), List(one)) if minus.toString == "-" && isInt(one, 1) =>
          sizeSelect(size).map((qualifier, symbol, spine) =>
            Link(
              tree,
              tree,
              stripped(qualifier),
              "toLastIndex",
              Nil,
              tree :: Calls.peelInlined(right) :: spine,
              Some(symbol),
              lowered = true
            )
          )
        case _ => None
      }
    else None
  }

  /** A tuple component such as `_1` has no method that says it reads a library call, so it follows its receiver. */
  private def component(tree: tpd.Tree): Option[Link] = tree match {
    case select @ tpd.Select(qualifier, name) if Components(name.toString) && kinds.tuple(qualifier.tpe) =>
      Some(Link(tree, tree, qualifier, name.toString, Nil, List(select), None, lowered = true, component = true))
    case _ => None
  }

  private case class Call(
      select: tpd.Select,
      lists: List[List[tpd.Tree]],
      path: List[tpd.Tree],
      typed: Boolean
  )

  private def peel(tree: tpd.Tree): Option[Call] = Calls.spine(tree) match {
    case Calls.Spine(select: tpd.Select, nodes) =>
      val applied = nodes.zip(select :: nodes.dropRight(1))
      val lists   = applied.collect {
        case (tpd.Apply(_, arguments), function) if !function.tpe.widen.isImplicitMethod => arguments
      }
      val typed = applied.exists {
        case (_: tpd.TypeApply, function) => explicit(function)
        case _                            => false
      }
      Some(Call(select, lists, select :: nodes, typed))
    case _ => None
  }

  /** `xs.lift(i)`, which is `xs.lift.apply(i)`, as one call of `lift`. */
  private def lifting(tree: tpd.Tree, owner: Symbol, ancestors: List[tpd.Tree]): Option[Link] = tree match {
    case tpd.Apply(tpd.Select(inner @ tpd.Select(qualifier, name), apply), List(index))
        if name.toString == "lift" && apply.toString == "apply" && kinds.library(inner.symbol) =>
      val (receiver, wrappers) = unconverted(qualifier)
      if (path(receiver)) None
      else positioned(
        tree,
        visible(tree),
        receiver,
        "lift",
        index,
        tree :: inner :: wrappers,
        inner.symbol,
        false,
        owner,
        ancestors
      )
    case _ => None
  }

  /** Whether the type arguments of a call of `function` are written, which the text after the function shows. */
  private def explicit(function: tpd.Tree): Boolean = {
    val content = function.sourcePos.source.content()
    val next    = Iterator.range(function.span.end, content.length).map(content(_)).dropWhile(_.isWhitespace)
    next.nextOption().contains('[')
  }

  private def call(tree: tpd.Tree, owner: Symbol, ancestors: List[tpd.Tree]): Option[Link] =
    lifting(tree, owner, ancestors).orElse(plain(tree, owner, ancestors))

  private def plain(tree: tpd.Tree, owner: Symbol, ancestors: List[tpd.Tree]): Option[Link] =
    peel(tree).filter(found =>
      found.select.symbol.is(Flags.Method) && (kinds.library(found.select.symbol) || yields(found.select))
    ).flatMap {
      found =>
        val select               = found.select
        val (receiver, wrappers) = operand(found)
        if (path(receiver) || checked(tree, select) || bound(select, found.lists)) None
        else {
          val name  = select.name.toString
          val spine = found.path ++ wrappers
          val shown = visible(tree)
          (name, found.lists) match {
            case ("foldLeft", List(List(initial), List(function)))
                if foldPredicate(function).isDefined && isTrue(initial) =>
              Some(Link(
                tree,
                shown,
                receiver,
                "foldTrueAnd",
                foldPredicate(function).toList,
                spine,
                Some(select.symbol),
                lowered = true,
                typed = found.typed
              ))
            case ("apply" | "lift", List(List(index))) =>
              positioned(tree, shown, receiver, name, index, spine, select.symbol, found.typed, owner, ancestors)
            case _ =>
              Some(Link(
                tree,
                shown,
                receiver,
                name,
                found.lists.flatten.map(argument(_, owner)),
                spine,
                Option.unless(kinds.guarded(select.symbol))(select.symbol),
                typed = found.typed,
                retyped = found.typed && !kinds.sameElement(receiver.tpe, tree.tpe),
                desugared = desugared(select)
              ))
          }
        }
    }

  /** Whether the compiler wrote the call of `select` for a for-comprehension, which gives it the span of its receiver.
    */
  private def desugared(select: tpd.Select): Boolean = Generated(select.name.toString) && select.span.isSynthetic

  /** Whether `call` is a `withFilter` the compiler wrote for a guard or a pattern, which only `guard` reads, as a step
    * of the chain that ends in the yield after it.
    */
  private def continuation(call: tpd.Tree, select: tpd.Select): Boolean = desugared(select) && call.span.isSynthetic

  private def checked(call: tpd.Tree, select: tpd.Select): Boolean =
    select.name.toString == "withFilter" && continuation(call, select)

  /** Whether `lists` are the arguments of the map a for-comprehension makes to carry its bindings. */
  private def bound(select: tpd.Select, lists: List[List[tpd.Tree]]): Boolean =
    desugared(select) && select.name.toString == "map" &&
      (lists match {
        case List(List(function)) => tupled(function).isDefined
        case _                    => false
      })

  /** Whether `select` is the map, flatMap or foreach a for-comprehension calls on the guards of its first generator. */
  private def yields(select: tpd.Select): Boolean =
    desugared(select) && Yields(select.name.toString) && kinds.guarded(select.symbol)

  /** The guard `tree` of a for-comprehension as a filter, when what follows it in the chain is a yield, a later
    * generator or another guard. The compiler's check of a pattern generator is a filter too: not a step when the
    * pattern cannot fail, and otherwise a `withFilter` that no law reads.
    */
  private def guard(tree: tpd.Tree, following: Option[Link], owner: Symbol): Option[Link] =
    following.filter(next => next.guard || (next.desugared && Yields(next.method))).flatMap { next =>
      peel(tree).filter { found =>
        checked(tree, found.select) &&
        (kinds.library(found.select.symbol) || kinds.guarded(found.select.symbol))
      }.flatMap { found =>
        val (receiver, wrappers) = operand(found)
        found.lists match {
          case List(List(predicate)) if !path(receiver) =>
            filtering(predicate).map { filtering =>
              Link(
                tree,
                next.shown,
                receiver,
                if (filtering == Filtering.Guard) "filter" else "withFilter",
                if (filtering == Filtering.Irrefutable) Nil else List(argument(predicate, owner)),
                found.path ++ wrappers,
                Option.unless(kinds.guarded(found.select.symbol))(found.select.symbol),
                desugared = true,
                guard = true,
                transparent = filtering == Filtering.Irrefutable,
                through = Some(receiver)
              )
            }
          case _ => None
        }
      }
    }

  /** What the function of a `withFilter` the compiler wrote is: a guard, or the check of a generator's pattern. */
  private def filtering(predicate: tpd.Tree): Option[Filtering] =
    Calls.lambda(predicate).flatMap { definition =>
      Parameters.sole(definition).flatMap { parameter =>
        (Calls.peelInlined(definition.rhs), generated(parameter)) match {
          case (tpd.Match(_, cases), true) =>
            cases match {
              case List(
                    tpd.CaseDef(pattern, tpd.EmptyTree, accepted),
                    tpd.CaseDef(tpd.Ident(wildcard), tpd.EmptyTree, rejected)
                  ) if wildcard.toString == "_" && isTrue(accepted) && isFalse(rejected) =>
                Some(if (PatternShapes.irrefutable(pattern, parameter.info)) Filtering.Irrefutable
                else Filtering.Refutable)
              case List(tpd.CaseDef(_, tpd.EmptyTree, _)) => Some(Filtering.Guard)
              case _                                      => None
            }
          case (_: tpd.Match, false) => None
          case _                     => Some(Filtering.Guard)
        }
      }
    }

  /** Whether `parameter` is one the compiler named for a pattern, which a function written by hand never has. */
  private def generated(parameter: Symbol): Boolean = parameter.name.toString.startsWith("x$")

  /** The values a for-comprehension binds with `=` before a guard, which the compiler computes in a map to a tuple of
    * everything bound so far, and then reads from the tuple.
    */
  private def tupled(function: tpd.Tree): Option[List[tpd.Tree]] = Calls.lambda(function).flatMap { definition =>
    val body = Calls.peelInlined(definition.rhs) match {
      case tpd.Match(_, List(tpd.CaseDef(_, tpd.EmptyTree, only)))
          if definition.termParamss.flatten.forall(p => generated(p.symbol)) =>
        Calls.peelInlined(only)
      case other => other
    }
    body match {
      case tpd.Block(stats, tpd.Apply(constructor, arguments))
          if stats.nonEmpty && tupleApply(constructor.symbol) && stats.forall(_.isInstanceOf[tpd.ValDef]) &&
            arguments.forall(argument => Calls.peelInlined(argument).isInstanceOf[tpd.Ident]) =>
        Some(stats.collect { case value: tpd.ValDef => value.rhs })
      case _ => None
    }
  }

  private def tupleApply(symbol: Symbol): Boolean =
    symbol.exists && symbol.name.toString == "apply" && defn.isTupleClass(symbol.owner.linkedClass)

  /** The map to a tuple that carries the values bound with `=` to the guard that follows: read through when its values
    * are pure, and otherwise the end of the chain.
    */
  private def binding(tree: tpd.Tree, following: Option[Link]): Option[Link] =
    following.filter(_.guard).flatMap { next =>
      peel(tree).filter { found =>
        found.select.name.toString == "map" && desugared(found.select) &&
        (kinds.library(found.select.symbol) || kinds.guarded(found.select.symbol))
      }.flatMap { found =>
        val (receiver, wrappers) = operand(found)
        found.lists match {
          case List(List(function)) if !path(receiver) =>
            tupled(function).map { values =>
              val pure = values.forall(purity.value)
              Link(
                tree,
                next.shown,
                receiver,
                "map",
                Nil,
                found.path ++ wrappers,
                None,
                desugared = true,
                transparent = pure,
                opaque = !pure,
                through = Some(tree)
              )
            }
          case _ => None
        }
      }
    }

  private def isTrue(tree: tpd.Tree): Boolean = Calls.peelInlined(tree) match {
    case tpd.Literal(constant) => constant.value == true
    case _                     => false
  }

  private def isFalse(tree: tpd.Tree): Boolean = Calls.peelInlined(tree) match {
    case tpd.Literal(constant) => constant.value == false
    case _                     => false
  }

  /** The predicate of `(acc, x) => acc && p(x)` as `x => p(x)`, when `p` does not read the accumulator. */
  private def foldPredicate(function: tpd.Tree): Option[Arg] = Calls.lambda(function).flatMap { definition =>
    (definition.termParamss.flatten, Calls.peelInlined(definition.rhs)) match {
      case (List(first, second), tpd.Apply(tpd.Select(accumulator, operator), List(predicate)))
          if operator.toString == "&&" && Calls.peelInlined(accumulator).symbol == first.symbol &&
            !second.name.toString.contains('$') && !Texts.real(second.tpt) &&
            Argument.free(predicate, first.symbol) =>
        Some(TextArgument(s"${second.name} => ${Texts.of(predicate)}"))
      case _ => None
    }
  }

  /** A lookup by position: the first element, the last, or any other, which is an ordinary call. The first and the last
    * are not lifted when the definition reads the collection at another position, where the positions are the point.
    */
  private def positioned(
      tree: tpd.Tree,
      shown: tpd.Tree,
      receiver: tpd.Tree,
      method: String,
      index: tpd.Tree,
      spine: List[tpd.Tree],
      symbol: Symbol,
      typed: Boolean,
      owner: Symbol,
      ancestors: List[tpd.Tree]
  ): Option[Link] =
    if (lastOf(index, receiver))
      Option.when(method == "lift" || !parallel(receiver, index, ancestors))(
        Link(
          tree,
          shown,
          receiver,
          method + "Last",
          Nil,
          Calls.peelInlined(index) :: spine,
          Some(symbol),
          typed = typed
        )
      )
    else if (method == "apply" && isInt(index, 0) && parallel(receiver, index, ancestors)) None
    else Some(Link(tree, shown, receiver, method, List(argument(index, owner)), spine, Some(symbol), typed = typed))

  /** Whether `index` is `size - 1` or `length - 1` of `receiver`. */
  private def lastOf(index: tpd.Tree, receiver: tpd.Tree): Boolean = Calls.peelInlined(index) match {
    case tpd.Apply(tpd.Select(size, minus), List(one)) if minus.toString == "-" && isInt(one, 1) =>
      sizeSelect(size).exists((inner, symbol, _) =>
        kinds.library(symbol) && Texts.of(stripped(inner)) == Texts.of(receiver)
      )
    case _ => false
  }

  /** Whether `receiver`, a named collection, is indexed at another position: anywhere within the code for a local, or
    * within the method or definition enclosing the lookup for a member.
    */
  private def parallel(receiver: tpd.Tree, index: tpd.Tree, ancestors: List[tpd.Tree]): Boolean =
    receiver match {
      case reference @ (_: tpd.Ident | _: tpd.Select) if reference.symbol.exists =>
        val symbol = reference.symbol
        val scope  =
          if (symbol.owner.isTerm) ancestors.lastOption
          else
            ancestors.collectFirst { case scope: tpd.DefDef => scope }
              .orElse(ancestors.collectFirst { case scope: tpd.ValDef => scope })
        val wanted = Texts.of(index)
        scope.exists { scope =>
          Lookups(symbol, wanted)(false, scope)
        }
      case _ => false
    }

  private class Lookups(symbol: Symbol, wanted: String) extends tpd.TreeAccumulator[Boolean] {
    def apply(found: Boolean, tree: tpd.Tree)(using Context): Boolean = found ||
      (tree match {
        case tpd.Apply(tpd.Select(target, name), List(other))
            if name.toString == "apply" && stripped(target).symbol == symbol && Texts.of(other) != wanted =>
          true
        case _ => foldOver(found, tree)
      })
  }
}
