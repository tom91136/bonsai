package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import net.kurobako.bonsai.collections.*
import net.kurobako.bonsai.lifting.*
import scala.collection.mutable

/** Sizes of the collection-valued expressions of one method, and the trip counts of its loops. */
private class Sizes(
    library: Library,
    owner: Symbol,
    initialisers: collection.Map[Symbol, tpd.Tree],
    fixed: mutable.Map[Symbol, Option[Polynomial]],
    declared: Symbol => Option[Polynomial],
    seeks: Symbol => Boolean
)(using Context) {
  private val subtrees = Children()

  private val Preserving = Set(
    "map",
    "filter",
    "filterNot",
    "collect",
    "sortBy",
    "sortWith",
    "sorted",
    "reverse",
    "distinct",
    "toArray",
    "toBuffer",
    "view",
    "iterator",
    "indices",
    "zip",
    "zipWithIndex",
    "tail",
    "init",
    "take",
    "drop",
    "takeRight",
    "dropRight",
    "takeWhile",
    "dropWhile",
    "slice",
    "groupBy",
    "keys",
    "values",
    "keySet",
    "::",
    ":+",
    "+:",
    "appended",
    "prepended",
    "updated"
  )
  private val Joins     = Set("++", ":::", "++:", "concat", "appendedAll", "prependedAll", "union")
  private val Ranges    = Set("until", "to")
  private val Shrinking = Set("tail", "init", "drop", "dropRight", "dropWhile", "filter", "filterNot", "takeWhile")

  def peeled(tree: tpd.Tree): tpd.Tree = tree match {
    case tpd.Typed(expression, _)       => peeled(expression)
    case tpd.Inlined(_, Nil, expansion) => peeled(expansion)
    case tpd.Block(Nil, expression)     => peeled(expression)
    case _                              => tree
  }

  def literal(tree: tpd.Tree): Option[Int] = peeled(tree) match {
    case tpd.Literal(constant) if constant.tag == Constants.IntTag => Some(constant.intValue)
    case _                                                         => None
  }

  def exists(tree: tpd.Tree)(test: tpd.Tree => Boolean): Boolean =
    test(tree) || subtrees.of(tree).exists(exists(_)(test))

  /** Whether a suffix starts at the index of a sorted search, so that a scan of it reads only the elements from there.
    */
  def seeked(tree: tpd.Tree): Boolean = peeled(tree) match {
    case application: tpd.Apply =>
      Calls.selected(application.fun).exists(select =>
        select.name.toString == "drop" && application.args.headOption.exists(searched(_))
      )
    case _ => false
  }

  private def searched(index: tpd.Tree): Boolean = peeled(index) match {
    case application: tpd.Apply => seeks(application.fun.symbol)
    case _                      => false
  }

  private def root(cls: Symbol): String =
    if (cls.is(Flags.Module)) Calls.qualified(cls)
    else if (cls == owner.enclosingClass) "this"
    else s"${cls.fullName}.this"

  private def field(symbol: Symbol): Boolean =
    symbol.isTerm && !symbol.is(Flags.Method) && !symbol.is(Flags.Module) && !symbol.is(Flags.Package)

  /** The path a value is read from: a parameter, `this` or a module, followed by the fields read from it. */
  def path(tree: tpd.Tree, scope: Scope): Option[String] = tree match {
    case tpd.Typed(expression, _)                                    => path(expression, scope)
    case tpd.This(_)                                                 => Some(root(tree.symbol))
    case _: (tpd.Ident | tpd.Select) if tree.symbol.is(Flags.Module) => Some(root(tree.symbol.moduleClass))
    case tpd.Ident(_)                                                =>
      scope.paths.get(tree.symbol).orElse(
        Option.when(tree.symbol.owner.isClass && field(tree.symbol))(s"${root(tree.symbol.owner)}.${tree.symbol.name}")
      )
    case select: tpd.Select if field(select.symbol) =>
      path(select.qualifier, scope).map(base => s"$base.${select.name}")
    case _ => None
  }

  /** The size of a collection-valued expression, when it follows from a parameter, a field or a local value. */
  def size(tree: tpd.Tree, scope: Scope): Option[Polynomial] = tree match {
    case tpd.Typed(expression, _)                                                 => size(expression, scope)
    case tpd.Inlined(_, Nil, expansion)                                           => size(expansion, scope)
    case tpd.Block(Nil, expression)                                               => size(expression, scope)
    case tpd.Literal(_)                                                           => Some(Polynomial.One)
    case tpd.Apply(function, List(wrapped)) if function.symbol.is(Flags.Implicit) => size(wrapped, scope)
    case tpd.Ident(_) if scope.variables.contains(tree.symbol) => variable(scope.variables(tree.symbol), scope)
    case tpd.Ident(_) if scope.sizes.contains(tree.symbol)     => scope.sizes.get(tree.symbol)
    case _ => derived(tree, scope).orElse(constructedField(tree, scope)).orElse(accessed(tree, scope))
  }

  /** The application of a primary constructor that `tree` is, or the local value it was bound to. */
  def construction(tree: tpd.Tree, scope: Scope): Option[tpd.Apply] = peeled(tree) match {
    case ident: tpd.Ident                                                  => scope.built.get(ident.symbol)
    case application: tpd.Apply if application.symbol.isPrimaryConstructor => Some(application)
    case _                                                                 => None
  }

  /** The size of a field of a freshly constructed object, from the constructor arguments the class body reads. */
  private def constructedField(tree: tpd.Tree, scope: Scope): Option[Polynomial] = tree match {
    case select: tpd.Select if library.sized(tree.tpe) && field(select.symbol) =>
      construction(select.qualifier, scope).flatMap { application =>
        val constructor = application.symbol
        val known       =
          Calls.termClauses(constructor).flatten.zip(Calls.arguments(application)).flatMap((parameter, argument) =>
            size(argument, scope).map(parameter -> _)
          )
        val bound = known.flatMap((parameter, count) =>
          (parameter :: constructor.owner.info.decls.toList.filter(member =>
            member.isTerm && member.is(Flags.ParamAccessor) && member.name == parameter.name
          )).map(_ -> count)
        ).toMap
        if (select.symbol.is(Flags.ParamAccessor)) bound.get(select.symbol)
        else initialisers.get(select.symbol).flatMap(size(_, Scope(sizes = bound)))
      }
    case _ => None
  }

  private def variable(variable: Scope.Variable, scope: Scope): Option[Polynomial] =
    if (variable.shrinking || scope.loops.size <= variable.level) variable.initial
    else
      variable.initial.map(_ + Polynomial.product(scope.loops.drop(variable.level).map(_.trips)) * variable.growth)

  private def accessed(tree: tpd.Tree, scope: Scope): Option[Polynomial] =
    if (!library.sized(tree.tpe)) None
    else if (tree.symbol.is(Flags.Module)) Some(Polynomial.One)
    else declaredBound(tree).orElse {
      if (library.growable(tree.tpe)) path(tree, scope).map(Atoms.size(_))
      else if (static(tree)) Some(Polynomial.One)
      else fixedSize(tree.symbol).orElse(path(tree, scope).map(Atoms.size(_)))
    }

  // a member the build declares to hold a bounded number of elements, read as a field or a selection
  private def declaredBound(tree: tpd.Tree): Option[Polynomial] = tree match {
    case _: (tpd.Ident | tpd.Select) => declared(tree.symbol)
    case _                           => None
  }

  // a table reached from a module through immutable members is the same for every input
  def static(tree: tpd.Tree): Boolean = tree match {
    case tpd.Typed(expression, _)          => static(expression)
    case _ if tree.symbol.is(Flags.Module) => true
    case tpd.Ident(_) => tree.symbol.owner.is(Flags.Module) && field(tree.symbol) && !tree.symbol.is(Flags.Mutable)
    case select: tpd.Select => field(select.symbol) && !select.symbol.is(Flags.Mutable) && static(select.qualifier)
    case _                  => false
  }

  // immutable members of a class that its initialiser builds from literals have a fixed size
  private def fixedSize(symbol: Symbol): Option[Polynomial] =
    if (!symbol.exists || !symbol.owner.isClass || !field(symbol) || symbol.is(Flags.Mutable)) None
    else
      fixed.getOrElse(
        symbol, {
          fixed(symbol) = None
          val result = initialisers.get(symbol).flatMap(initial =>
            size(initial, Scope()).collect { case bound if bound.terms.keys.forall(_.isEmpty) => Polynomial.One }
          )
          fixed(symbol) = result
          result
        }
      )

  @annotation.tailrec
  private def derived(tree: tpd.Tree, scope: Scope): Option[Polynomial] = tree match {
    case tpd.TypeApply(inner, _) => derived(inner, scope)
    case select: tpd.Select if select.name.toString == "empty" && library.sized(select.tpe.widen.finalResultType) =>
      Some(Polynomial.One)
    case select: tpd.Select if library.traversal(select.qualifier.tpe) => resulting(select, Nil, scope)
    case application: tpd.Apply                                        =>
      Calls.selected(application.fun) match {
        case Some(select) if library.traversal(select.qualifier.tpe) =>
          resulting(select, Calls.arguments(application), scope)
        case Some(select) if Ranges(select.name.toString) && library.sized(application.tpe) =>
          Calls.arguments(application).headOption.flatMap(lengthOf(_, scope))
        case _ => constructed(application, scope)
      }
    case _ => None
  }

  private def constructed(application: tpd.Apply, scope: Scope): Option[Polynomial] = {
    val function = application.fun.symbol
    val factory  = Calls.selected(application.fun).exists(select =>
      select.qualifier.tpe.widenDealias.typeSymbol.is(Flags.Module)
    )
    if (!library.sized(application.tpe)) None
    else if (function.isConstructor && application.args.isEmpty) Some(Polynomial.One)
    else if (factory && Sized(function.name.toString))
      Calls.arguments(application).headOption.flatMap(lengthOf(_, scope))
    else if (factory && Set("apply", "empty")(function.name.toString))
      application.args match {
        case List(tpd.Typed(tpd.SeqLiteral(_, _), _))                  => Some(Polynomial.One)
        case List(tpd.Typed(spliced, _)) if library.sized(spliced.tpe) => size(spliced, scope)
        case _                                                         => Some(Polynomial.One)
      }
    else None
  }

  private def resulting(select: tpd.Select, arguments: List[tpd.Tree], scope: Scope): Option[Polynomial] = {
    val name = select.name.toString
    if (
      (name == "take" && arguments.flatMap(literal(_)).nonEmpty) ||
      (name == "filter" && arguments.flatMap(Calls.lambda(_)).exists(keyed(_))) ||
      (name == "slice" && arguments.size == 2 && arguments.forall(literal(_).isDefined)) ||
      (name == "takeWhile" && seeked(select.qualifier))
    ) Some(Polynomial.One)
    else if (Joins(name))
      for {
        left  <- size(select.qualifier, scope)
        right <- arguments.headOption.flatMap(size(_, scope))
      } yield left + right
    else if (Preserving(name) || Methods.Conversions(name)) size(select.qualifier, scope)
    else None
  }

  /** The value of an integer expression as a size, when it is a collection's size or a constant. */
  def lengthOf(tree: tpd.Tree, scope: Scope): Option[Polynomial] = peeled(tree) match {
    case tpd.Literal(constant) if constant.tag == Constants.IntTag => Some(Polynomial.One)
    case select: tpd.Select if Sizing(select.name.toString) && library.traversal(select.qualifier.tpe) =>
      size(select.qualifier, scope)
    case tpd.Ident(_) => scope.counts.get(tree.symbol)
    case tpd.Apply(tpd.Select(left, operator), List(right)) if Arithmetic(operator.toString) =>
      (lengthOf(left, scope), lengthOf(right, scope)) match {
        case (Some(l), Some(r)) if operator.toString == "+" => Some(l + r)
        case (Some(l), Some(r)) if operator.toString == "*" => Some(l * r)
        case (Some(l), Some(_))                             => Some(l)
        case _                                              => None
      }
    case _ => None
  }

  /** The upper bound of the turns of a loop, from the counter, iterator or shrinking collection its condition tests. */
  def trips(condition: tpd.Tree, body: tpd.Tree, scope: Scope): (Polynomial, Option[Symbol]) = {
    val bounds = conjuncts(condition).flatMap(bounded(_, body, scope))
    bounds.find(_._1 == Polynomial.One).orElse(bounds.minByOption(_._1.weight)) match {
      case Some((bound, driver))          => (bound, Some(driver))
      case None if probe(condition, body) => (Polynomial.One, None)
      case None                           => (Atoms.Unknown, None)
    }
  }

  /** A probe of an open-addressing table: the index wraps around the mask of a mutable array and stops at an empty
    * entry or at the key. The table doubles when its occupancy passes a fraction of its capacity, so a probe is short.
    */
  private def probe(condition: tpd.Tree, body: tpd.Tree): Boolean =
    conjuncts(condition).exists {
      case tpd.Apply(tpd.Select(tpd.Apply(tpd.Select(table, apply), List(index)), operator), List(_)) =>
        apply.toString == "apply" && Probes(operator.toString) && peeled(index).symbol.is(Flags.Mutable) &&
        peeled(table).symbol.is(Flags.Mutable) && exists(body) {
          case tpd.Assign(target, value) => target.symbol == peeled(index).symbol && wraps(value, target.symbol)
          case _                         => false
        }
      case _ => false
    }

  private def wraps(value: tpd.Tree, index: Symbol): Boolean = peeled(value) match {
    case tpd.Apply(tpd.Select(tpd.Apply(tpd.Select(source, plus), List(step)), and), List(_)) =>
      and.toString == "&" && plus.toString == "+" && source.symbol == index && literal(step).contains(1)
    case _ => false
  }

  /** A comparison of a count with a fraction of the capacity of a mutable array, as a table tests before it doubles. */
  def occupancy(test: tpd.Tree): Boolean = peeled(test) match {
    case tpd.Apply(tpd.Select(left, operator), List(capacity)) =>
      Set(">", ">=", "<", "<=")(operator.toString) && isCapacity(capacity) &&
      (peeled(left) match {
        case tpd.Apply(tpd.Select(_, times), List(factor)) => times.toString == "*" && literal(factor).exists(_ > 0)
        case _                                             => false
      })
    case _ => false
  }

  private def isCapacity(tree: tpd.Tree): Boolean = peeled(tree) match {
    case select: tpd.Select => select.name.toString == "length" && peeled(select.qualifier).symbol.is(Flags.Mutable)
    case _                  => false
  }

  /** The variables the code assigns something not derived from their own value, which start a loop over. */
  def reset(body: tpd.Tree): Set[Symbol] = {
    def assigned(tree: tpd.Tree): List[Symbol] =
      (tree match {
        case tpd.Assign(target, value) if !exists(value)(_.symbol == target.symbol) => List(target.symbol)
        case _                                                                      => Nil
      }) ++ subtrees.of(tree).flatMap(assigned(_))
    assigned(body).toSet
  }

  def conjuncts(condition: tpd.Tree): List[tpd.Tree] = peeled(condition) match {
    case tpd.Apply(tpd.Select(left, name), List(right)) if name.toString == "&&" =>
      conjuncts(left) ++ conjuncts(right)
    case other => List(other)
  }

  private def counter(tree: tpd.Tree): Option[Symbol] = peeled(tree) match {
    case ident: tpd.Ident if ident.symbol.is(Flags.Mutable) => Some(ident.symbol)
    case _                                                  => None
  }

  private val Mirrored    = Map("<" -> ">", "<=" -> ">=", ">" -> "<", ">=" -> "<=", "!=" -> "!=")
  private val Sizing      = Set("length", "size")
  private val Arithmetic  = Set("+", "-", "*", "/")
  private val Comparisons = Set("<", "<=", ">", ">=", "==", "!=")
  private val Sized       = Set("fill", "tabulate")
  private val Probes      = Set("!=", "==")
  private val Up          = Set("<", "<=", "!=")
  private val Down        = Set(">", ">=", "!=")

  private def bounded(test: tpd.Tree, body: tpd.Tree, scope: Scope): Option[(Polynomial, Symbol)] = test match {
    case tpd.Apply(tpd.Select(left, operator), List(right)) =>
      val name = operator.toString
      def against(variable: Option[tpd.Tree], limit: tpd.Tree, op: String): Option[(Polynomial, Symbol)] =
        variable.flatMap(counter(_)).flatMap { symbol =>
          if (Up(op) && stepped(body, symbol, "+")) lengthOf(limit, scope).map(_ -> symbol)
          else if (Down(op) && stepped(body, symbol, "-")) scope.counts.get(symbol).map(_ -> symbol)
          else None
        }
      Mirrored.get(name).flatMap(mirrored =>
        against(Some(left), right, name).orElse(against(Some(right), left, mirrored))
      )
    case select: tpd.Select if select.name.toString == "hasNext" =>
      Option.when(exists(body)(called(_, select.qualifier.symbol, "next")))(scope.sizes.get(select.qualifier.symbol))
        .flatten.map(_ -> select.qualifier.symbol)
    case _ => emptiness(test).flatMap(shrinking(_, body, scope))
  }

  private def called(tree: tpd.Tree, symbol: Symbol, method: String): Boolean = tree match {
    case select: tpd.Select => select.name.toString == method && select.qualifier.symbol == symbol
    case _                  => false
  }

  private def stepped(body: tpd.Tree, symbol: Symbol, operator: String): Boolean =
    exists(body) {
      case tpd.Assign(target, tpd.Apply(tpd.Select(source, name), List(step))) =>
        target.symbol == symbol && source.symbol == symbol && name.toString == operator &&
        literal(step).exists(_ > 0)
      case _ => false
    }

  /** The collection a condition says is not empty. */
  private def emptiness(test: tpd.Tree): Option[Symbol] = test match {
    case select: tpd.Select if select.name.toString == "nonEmpty" => Some(select.qualifier.symbol)
    case tpd.Apply(tpd.Select(inner: tpd.Select, bang), Nil)
        if bang.toString == "unary_!" && inner.name.toString == "isEmpty" =>
      Some(inner.qualifier.symbol)
    case tpd.Apply(tpd.Select(inner: tpd.Select, operator), List(zero))
        if operator.toString == ">" && Sizing(inner.name.toString) && literal(zero).contains(0) =>
      Some(inner.qualifier.symbol)
    case tpd.Apply(tpd.Select(collection, operator), List(nil))
        if operator.toString == "!=" && nil.symbol.name.toString == "Nil" =>
      Some(collection.symbol)
    case _ => None
  }

  private def shrinking(symbol: Symbol, body: tpd.Tree, scope: Scope): Option[(Polynomial, Symbol)] =
    scope.variables.get(symbol).filter(_.shrinking).flatMap(variable => variable.initial).collect {
      case initial
          if exists(body) {
            case tpd.Assign(target, _) => target.symbol == symbol
            case _                     => false
          } => initial -> symbol
    }

  /** Whether every value assigned to `symbol` is a part of the value it had. */
  def shrinks(symbol: Symbol, assigned: List[tpd.Tree]): Boolean = {
    def part(tree: tpd.Tree): Boolean = peeled(tree) match {
      case ident: tpd.Ident        => ident.symbol == symbol
      case tpd.TypeApply(inner, _) => part(inner)
      case select: tpd.Select      => Shrinking(select.name.toString) && part(select.qualifier)
      case application: tpd.Apply  =>
        Calls.selected(application.fun).exists(select => Shrinking(select.name.toString) && part(select.qualifier))
      case _ => false
    }
    assigned.forall(part(_))
  }

  def equality(test: tpd.Tree): Boolean = peeled(test) match {
    case tpd.Apply(tpd.Select(_, name), List(_)) => name.toString == "=="
    case _                                       => false
  }

  private def constant(tree: tpd.Tree): Boolean = peeled(tree) match {
    case tpd.Literal(_) => true
    case _              => false
  }

  // a predicate that fixes two attributes to literals picks out a key, which matches a single element
  private def keyed(function: tpd.DefDef): Boolean =
    conjuncts(function.rhs).count {
      case tpd.Apply(tpd.Select(left, name), List(right)) if name.toString == "==" =>
        constant(left) || constant(right)
      case _ => false
    } >= 2

  /** Whether an integer argument is the parameter stepped by a positive constant with `operator`. */
  def steps(argument: tpd.Tree, parameter: Symbol, operator: String): Boolean = peeled(argument) match {
    case tpd.Apply(tpd.Select(source, name), List(step)) =>
      source.symbol == parameter && name.toString == operator && literal(step).exists(_ > 0)
    case _ => false
  }

  /** The bound a comparison in `body` puts on an integer parameter, when it is a size or a constant. */
  def limit(body: tpd.Tree, parameter: Symbol, scope: Scope): Option[Polynomial] = {
    def limits(tree: tpd.Tree): List[Polynomial] =
      (tree match {
        case tpd.Apply(tpd.Select(left, name), List(right)) if Comparisons(name.toString) =>
          if (left.symbol == parameter) lengthOf(right, scope).toList
          else if (right.symbol == parameter) lengthOf(left, scope).toList
          else Nil
        case _ => Nil
      }) ++ subtrees.of(tree).flatMap(limits(_))
    limits(body).headOption
  }

  /** How much one turn of a loop adds to the collection `symbol`: the sizes of the collections joined onto it. */
  def growth(symbol: Symbol, trees: List[tpd.Tree], scope: Scope): Polynomial = {
    def self(tree: tpd.Tree): Boolean            = peeled(tree).symbol == symbol
    def pieces(tree: tpd.Tree): List[Polynomial] =
      (tree match {
        case application: tpd.Apply =>
          Calls.selected(application.fun).filter(select => Joins(select.name.toString)).toList.flatMap { select =>
            val operands = select.qualifier :: Calls.arguments(application).take(1)
            if (operands.exists(self(_)))
              operands.collect { case operand if !self(operand) => size(operand, scope).getOrElse(Atoms.Unknown) }
            else Nil
          }
        case _ => Nil
      }) ++ subtrees.of(tree).flatMap(pieces(_))
    val added = trees.flatMap(pieces(_))
    if (added.isEmpty) Polynomial.One else Polynomial.sum(added)
  }
}
