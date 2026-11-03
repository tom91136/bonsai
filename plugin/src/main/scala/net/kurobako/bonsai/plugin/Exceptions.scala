package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameKinds.DefaultGetterName
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.rewrites.Rewrites.ActionPatch
import java.util.concurrent.ConcurrentHashMap
import net.kurobako.bonsai.effects.Partials
import net.kurobako.bonsai.effects.Throwing
import net.kurobako.bonsai.lifting.Calls
import net.kurobako.bonsai.lifting.Classes
import scala.collection.mutable
import scala.util.matching.Regex

/** The patterns of the methods that throw, with the patterns each qualified name matches kept once found. */
private class ThrowingTable(patterns: List[(Regex, List[String])]) {
  private val matched = ConcurrentHashMap[String, List[(Regex, List[String])]]()

  def matching(name: String): List[(Regex, List[String])] =
    matched.computeIfAbsent(name, _ => patterns.filter((pattern, _) => pattern.matches(name)))
}

/** Exceptions used for control flow where a result type can carry the failure: throws a surrounding handler turns into
  * a result, throws standing in for an Option or Either's absent value, helpers that throw only for their callers'
  * handlers, partial calls inside handlers, catch-alls around calls with a documented exception, and handlers that drop
  * the exception an error value has a place for.
  */
private class Exceptions(
    checks: Checks,
    partials: Map[String, Partials.Partial],
    throwing: ThrowingTable
)(using Context)
    extends tpd.TreeTraverser {

  /** A region whose exceptions `cases` handle, or the non-fatal ones when `tried` it is the argument of `Try`. */
  private class Handler(val cases: List[tpd.CaseDef], val tried: Boolean) {
    val partial: mutable.ArrayBuffer[tpd.Select]                         = mutable.ArrayBuffer.empty
    val boundary: mutable.ArrayBuffer[(tpd.Tree, List[String], Boolean)] = mutable.ArrayBuffer.empty
    val sharers: mutable.Set[Type]                                       = mutable.Set.empty
  }

  // The handlers around the current tree, innermost first, and the result type of the enclosing function; a named
  // method, class or a function not run at once starts afresh.
  private val handlers = mutable.Stack(List.empty[Handler])
  private val results  = mutable.Stack(Option.empty[Type])
  private val reported = mutable.Set.empty[tpd.Tree]
  private val quiet    = mutable.Set.empty[tpd.Tree]

  private val helpers  = mutable.Map.empty[Symbol, (tpd.DefDef, Set[Type])]
  private val calls    = mutable.Map.empty[Symbol, mutable.ArrayBuffer[(Set[Type], List[Handler])]]
  private val bodies   = mutable.Map.empty[Symbol, tpd.DefDef]
  private val escaping = mutable.Map.empty[Symbol, Set[Type]]

  private val NoSuchElement = Classes.required("java.util.NoSuchElementException").typeRef
  private val NumberFormat  = Classes.required("java.lang.NumberFormatException").typeRef
  private val FutureClass   = Classes.required("scala.concurrent.Future")
  private val PartialClass  = Classes.required("scala.PartialFunction")

  def run(tree: tpd.Tree): Unit = {
    tree.foreachSubTree {
      case definition: tpd.DefDef if !definition.symbol.isAnonymousFunction => bodies(definition.symbol) = definition
      case _                                                                => ()
    }
    throwingHelpers(tree)
    traverse(tree)
    calls.foreach { (helper, sites) =>
      helpers.get(helper).filter(_ =>
        sites.nonEmpty && sites.forall((types, around) => covered(types, around)) &&
          !sites.forall((types, around) => shared(types, around))
      ).foreach((definition, types) =>
        checks(
          Check.ThrowingHelper,
          s"${helper.name} throws ${types.map(_.classSymbol.name).toList.sortBy(_.toString).mkString(", ")} only for " +
            "the handlers around its calls to turn into a result; return the failure in its result instead",
          definition.namePos
        )
      )
    }
  }

  override def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
    case definition: tpd.DefDef                => within(Some(definition.tpt.tpe))(traverseChildren(tree))
    case _: tpd.TypeDef                        => within(None)(traverseChildren(tree))
    case tpd.Try(expression, cases, finalizer) =>
      val handler = Handler(cases, tried = false)
      region(handler)(traverse(expression))
      finish(handler, tree)
      cases.foreach(traverse(_))
      traverse(finalizer)
    case application @ tpd.Apply(function, List(argument))
        if Calls.qualified(function.symbol) == "scala.util.Try.apply" =>
      val handler = Handler(Nil, tried = true)
      region(handler)(traverse(argument))
      finish(handler, application)
    case application @ tpd.Apply(function, arguments) if arguments.exists(handling(_).isDefined) =>
      val (lambda, rest) = arguments.partition(handling(_).isDefined)
      val handler        = Handler(lambda.flatMap(handling(_)).flatten, tried = false)
      // The earlier argument lists hold what the handler protects, even when they take it by name.
      region(handler) {
        function match {
          case tpd.Apply(inner, protectedArguments) =>
            traverse(inner)
            protectedArguments.foreach(traverse(_))
          case other => traverse(other)
        }
        rest.foreach(traverse(_))
      }
      finish(handler, application)
      lambda.foreach(traverse(_))
    case thrown @ tpd.Apply(function, List(exception)) if function.symbol == defn.throwMethod =>
      local(thrown, exception.tpe.widen)
      traverseChildren(tree)
    case application @ tpd.Apply(function, arguments) =>
      redundant(application)
      default(application)
      traverse(function)
      val parameters = Trees.parameters(application)
      arguments.zipWithIndex.foreach { (argument, index) =>
        if (Calls.lambda(argument).isDefined || parameters.lift(index).exists(Trees.byName(_))) {
          if (Trees.eager(function))
            Calls.lambda(argument).fold(traverse(argument))(definition => traverse(definition.rhs))
          else within(Calls.lambda(argument).map(_.tpt.tpe))(traverse(argument))
        } else traverse(argument)
      }
    case select: tpd.Select =>
      redundant(select)
      partialCall(select)
      helperCall(select.symbol)
      throwsCall(select)
      traverseChildren(tree)
    case reference: tpd.Ident =>
      helperCall(reference.symbol)
      throwsCall(reference)
    case _ => traverseChildren(tree)
  }

  private def within(result: Option[Type])(body: => Unit): Unit = {
    handlers.push(Nil)
    results.push(result)
    try body
    finally {
      handlers.pop()
      results.pop(): Unit
    }
  }

  private def region(handler: Handler)(body: => Unit): Unit = {
    handlers.push(handler :: handlers.top)
    try body
    finally handlers.pop(): Unit
  }

  // A handler argument: a function of a Throwable, written as cases.
  private def handling(argument: tpd.Tree)(using Context): Option[List[tpd.CaseDef]] =
    Calls.lambda(argument).filter(definition =>
      argument.tpe.widen.derivesFrom(PartialClass) ||
        definition.termParamss.flatten.headOption.exists(_.tpt.tpe.widen =:= defn.ThrowableType)
    ).flatMap(_.rhs match {
      case tpd.Match(_, cases) => Some(cases)
      case _                   => None
    })

  /** Whether a type carries failure as a value: Option, Either, Try or Future, or an effect producing one. */
  private def channel(tpe: Type)(using Context): Boolean = {
    val result = tpe.widenDealias.finalResultType
    Trees.optional(result) || result.derivesFrom(FutureClass) || result.argInfos.lastOption.exists(channel(_))
  }

  @annotation.tailrec
  private def wildcard(pattern: tpd.Tree): Boolean = pattern match {
    case tpd.Ident(name)   => name == nme.WILDCARD
    case tpd.Bind(_, body) => wildcard(body)
    case _                 => false
  }

  @annotation.tailrec
  private def nonFatal(pattern: tpd.Tree)(using Context): Boolean = pattern match {
    case tpd.UnApply(function, _, _) =>
      function.symbol.owner.fullName.toString.startsWith("scala.util.control.NonFatal")
    case tpd.Bind(_, body) => nonFatal(body)
    case _                 => false
  }

  private def catchAll(clause: tpd.CaseDef)(using Context): Boolean = clause.guard.isEmpty && {
    @annotation.tailrec
    def all(pattern: tpd.Tree): Boolean = pattern match {
      case tpd.Typed(_, tpt) =>
        tpt.tpe =:= defn.ThrowableType || tpt.tpe.classSymbol == defn.ExceptionClass
      case tpd.Bind(_, body) => all(body)
      case other             => wildcard(other) || nonFatal(other)
    }
    all(clause.pat)
  }

  private def covers(clause: tpd.CaseDef, thrown: Type)(using Context): Boolean = clause.guard.isEmpty && {
    def matches(pattern: tpd.Tree): Boolean = pattern match {
      case tpd.Typed(_, tpt)             => thrown <:< tpt.tpe
      case tpd.Bind(_, body)             => matches(body)
      case tpd.Alternative(alternatives) => alternatives.exists(matches(_))
      case other if nonFatal(other)      => thrown.derivesFrom(defn.ExceptionClass)
      case unapply: tpd.UnApply          => unapply.fun.tpe.widen.firstParamTypes.headOption.exists(thrown <:< _)
      case other                         => wildcard(other)
    }
    matches(clause.pat)
  }

  private def handles(handler: Handler, thrown: Type)(using Context): Boolean =
    if (handler.tried) thrown.derivesFrom(defn.ExceptionClass)
    else handler.cases.exists(clause => covers(clause, thrown) && !rethrows(clause.body))

  @annotation.tailrec
  private def rethrows(body: tpd.Tree)(using Context): Boolean = body match {
    case tpd.Apply(function, _)   => function.symbol == defn.throwMethod
    case tpd.Block(_, expression) => rethrows(expression)
    case _                        => false
  }

  private def local(thrown: tpd.Tree, tpe: Type)(using Context): Unit =
    if (!reported(thrown) && handlers.top.exists(handles(_, tpe)))
      checks(
        Check.LocalThrow,
        s"${tpe.classSymbol.name} is thrown here only for the handler around it to turn into a result; " +
          "return the result instead",
        thrown.srcPos
      )

  // A throw standing in for the absent value of an Option, Either or Try in code that returns failure as a value.
  private def default(application: tpd.Apply)(using Context): Unit = {
    val thrown = application match {
      case tpd.Apply(tpd.Select(receiver, name), List(argument))
          if name.toString == "getOrElse" && Trees.optional(receiver.tpe.widen) => Some(argument)
      case tpd.Apply(tpd.TypeApply(tpd.Select(receiver, name), _), List(argument))
          if name.toString == "getOrElse" && Trees.optional(receiver.tpe.widen) => Some(argument)
      case tpd.Apply(tpd.Apply(tpd.TypeApply(tpd.Select(receiver, name), _), List(argument)), _)
          if name.toString == "fold" && receiver.tpe.widen.derivesFrom(defn.OptionClass) => Some(argument)
      case _ => None
    }
    thrown.map(Calls.peelInlined(_)).collect {
      case raised @ tpd.Apply(function, _) if function.symbol == defn.throwMethod => raised
    }.filter(_ => results.top.exists(channel(_))).foreach { raised =>
      reported += raised
      checks(
        Check.ThrowInDefault,
        "a throw stands in for the absent value in code returning failure as a value; " +
          "return it instead, as toRight does",
        raised.srcPos
      )
    }
  }

  private val known = mutable.HashMap.empty[Symbol, Option[Partials.Partial]]

  private def partial(symbol: Symbol)(using Context): Option[Partials.Partial] =
    if (!symbol.exists) None
    else
      known.getOrElseUpdate(
        symbol,
        (symbol :: symbol.allOverriddenSymbols.toList).view.flatMap(candidate =>
          partials.get(Calls.qualified(candidate))
        ).headOption
      )

  private def partialCall(select: tpd.Select)(using Context): Unit =
    if (partial(select.symbol).isDefined) handlers.top.headOption.foreach(_.partial += select)

  private def throwsCall(tree: tpd.Tree)(using Context): Unit = if (tree.symbol.exists) {
    val name    = Calls.qualified(tree.symbol)
    val matched = throwing.matching(name)
    if (matched.nonEmpty)
      handlers.top.headOption.foreach(_.boundary +=
        ((
          tree,
          matched.flatMap(_._2),
          matched.exists((pattern, _) => Throwing.varies(pattern.regex))
        )))
  }

  private def helperCall(symbol: Symbol)(using Context): Unit = helpers.get(symbol) match {
    case Some((_, types)) => calls.getOrElseUpdate(symbol, mutable.ArrayBuffer.empty) += (types -> handlers.top)
    case None             => handlers.top.foreach(_.sharers ++= escapes(symbol))
  }

  private def covered(types: Set[Type], around: List[Handler])(using Context): Boolean =
    types.forall(tpe => around.exists(handles(_, tpe)))

  // A helper's failure is shared when each handler that covers it also covers another call that throws the same type.
  private def shared(types: Set[Type], around: List[Handler])(using Context): Boolean =
    around.filter(handler => types.exists(handles(handler, _))).forall(handler =>
      types.forall(tpe => handler.sharers.exists(tpe <:< _))
    )

  /** The exceptions a build method throws outside its handlers, directly or through the build methods it calls. */
  private def escapes(symbol: Symbol)(using Context): Set[Type] =
    escaping.getOrElse(
      symbol,
      bodies.get(symbol).fold(Set.empty[Type]) { definition =>
        escaping(symbol) = Set.empty
        val thrown = Thrown()
        val result = thrown.of(definition.rhs) ++ thrown.invoked.toSet.flatMap(escapes)
        escaping(symbol) = result
        result
      }
    )

  // A Try around one partial call whose failure is then discarded, where the call has an Option alternative.
  private def redundant(tree: tpd.Tree)(using Context): Unit = {
    def tried(candidate: tpd.Tree): Option[(tpd.Tree, tpd.Tree)] = Calls.peelInlined(candidate) match {
      case application @ tpd.Apply(function, List(argument))
          if Calls.qualified(function.symbol) == "scala.util.Try.apply" =>
        Some(application -> Calls.peelInlined(argument))
      case _ => None
    }
    val shape: Option[(tpd.Tree, String => String)] = tree match {
      case tpd.Select(qualifier, name) if name.toString == "toOption" || name.toString == "isSuccess" =>
        Some(qualifier -> (alternative => if (name.toString == "toOption") alternative else s"$alternative.isDefined"))
      case tpd.Apply(tpd.TypeApply(tpd.Select(qualifier, name), _), List(fallback)) if name.toString == "getOrElse" =>
        Some(qualifier -> (alternative => s"$alternative.getOrElse(${Trees.text(fallback)})"))
      case tpd.Apply(tpd.TypeApply(tpd.Select(tpd.Select(tpd.Select(qualifier, either), left), map), _), List(function))
          if either.toString == "toEither" && left.toString == "left" && map.toString == "map" =>
        Calls.lambda(function).filter(definition =>
          definition.termParamss.flatten.forall(parameter =>
            !definition.rhs.existsSubTree(_.symbol == parameter.symbol)
          )
        ).map(definition => qualifier -> (alternative => s"$alternative.toRight(${Trees.text(definition.rhs)})"))
      case _ => None
    }
    shape.flatMap((qualifier, replace) => tried(qualifier).map((call, argument) => (call, argument, replace))).foreach {
      (call, argument, replace) =>
        val (converted, member, arguments) = argument match {
          case tpd.Apply(select @ tpd.Select(receiver, _), arguments) => (receiver, select, arguments)
          case select @ tpd.Select(receiver, _)                       => (receiver, select, Nil)
          case other                                                  => (other, other, Nil)
        }
        // Methods such as StringOps.toInt reach their receiver through an implicit conversion.
        val receiver = converted match {
          case tpd.Apply(function, List(inner)) if function.symbol.is(Flags.Implicit) => inner
          case other                                                                  => other
        }
        partial(member.symbol).filter(_.optional).filterNot(_ =>
          receiver.existsSubTree(inner => partial(inner.symbol).isDefined)
        ).foreach { alternative =>
          quiet += call
          val stable = (receiver :: arguments).forall(stableTree(_))
          val text   = s"${Trees.text(receiver)}.${alternative.alternative}" +
            (if (arguments.isEmpty) "" else arguments.map(Trees.text(_)).mkString("(", ", ", ")"))
          checks(
            Check.RedundantTry,
            s"Try around ${member.symbol.name} only discards its failure; use ${alternative.alternative}",
            tree.srcPos,
            if (stable) List(ActionPatch(tree.sourcePos, replace(text))) else Nil
          )
        }
    }
  }

  private def stableTree(tree: tpd.Tree)(using Context): Boolean = tree match {
    case _: tpd.Ident | _: tpd.Literal | _: tpd.This => true
    case tpd.Select(qualifier, _)                    => tree.symbol.isStableMember && stableTree(qualifier)
    case _                                           => false
  }

  private def finish(handler: Handler, tree: tpd.Tree)(using Context): Unit = {
    val catches = handler.tried || handler.cases.exists(clause =>
      catchAll(clause) || covers(clause, NoSuchElement) || covers(clause, NumberFormat)
    )
    if (
      catches && handler.partial.nonEmpty && !quiet(tree) && (!handler.tried || handler.partial.map(_.symbol).size > 1)
    ) {
      val ordered      = handler.partial.sortBy(_.span.end)
      val names        = ordered.map(_.symbol.name.toString).distinct
      val alternatives = ordered.flatMap(select => partial(select.symbol).map(_.alternative)).distinct
      checks(
        Check.PartialInTry,
        s"${names.mkString(", ")} throw for missing or malformed values inside a handler; use " +
          s"${alternatives.mkString(", ")} and keep the handler for what must throw",
        ordered.head.srcPos
      )
    }
    if (handler.boundary.nonEmpty) {
      val exceptions = handler.boundary.flatMap(_._2).distinct.map(_.split('.').last)
      val callees    = handler.boundary.map(_._1.symbol.name.toString).distinct
      val platforms  =
        if (handler.boundary.exists(_._3))
          " on this platform; other platforms throw differently, so cross-built code classifies them in one place"
        else ""
      val message = s"catching everything here also turns defects into results; catch ${exceptions.mkString(" | ")}, " +
        s"which ${callees.mkString(", ")} throw$platforms"
      handler.cases.filter(clause => catchAll(clause) && value(clause.body)).foreach(clause =>
        checks(Check.BroadCatch, message, clause.pat.srcPos)
      )
    }
    handler.cases.foreach(discarded(_))
  }

  private def value(body: tpd.Tree)(using Context): Boolean = {
    val tpe = body.tpe.widen
    !tpe.isRef(defn.UnitClass) && !tpe.isRef(defn.NothingClass) && !rethrows(body)
  }

  // An error value with a place for the cause, built without it while the caught exception goes unnamed or unused.
  private def discarded(clause: tpd.CaseDef)(using Context): Unit = {
    val bound = clause.pat.filterSubTrees(_.isInstanceOf[tpd.Bind]).map(_.symbol)
    if (!bound.exists(symbol => clause.body.existsSubTree(_.symbol == symbol))) {
      val dropped = clause.body.filterSubTrees {
        case application: tpd.Apply => constructs(application)
        case _                      => false
      }
      dropped.headOption.foreach(construction =>
        checks(
          Check.DiscardedCause,
          s"the caught exception is dropped although ${construction.symbol.owner.name.toString.stripSuffix("$")} has a " +
            "place for its cause; pass it on",
          construction.srcPos
        )
      )
    }
  }

  private def constructs(application: tpd.Apply)(using Context): Boolean = {
    val callee = application.fun.symbol
    Trees.constructs(callee) &&
    Trees.parameters(application).zip(application.args).exists((parameter, argument) =>
      cause(parameter.info) && dropped(argument)
    )
  }

  private def cause(tpe: Type)(using Context): Boolean = {
    val widened = tpe.widen
    widened <:< defn.ThrowableType ||
    widened.derivesFrom(defn.OptionClass) && widened.argInfos.headOption.exists(_ <:< defn.ThrowableType)
  }

  private def dropped(argument: tpd.Tree)(using Context): Boolean = argument match {
    case tpd.Literal(constant) => constant.tag == Constants.NullTag
    case other                 =>
      other.symbol.name.is(DefaultGetterName) || other.symbol == defn.NoneModule
  }

  /** Private and local methods that throw for none of their own handlers, with what they throw. */
  private def throwingHelpers(tree: tpd.Tree)(using Context): Unit = {
    val candidates = tree.filterSubTrees {
      case definition: tpd.DefDef =>
        val symbol = definition.symbol
        !symbol.isAnonymousFunction && !symbol.is(Flags.Synthetic) && (symbol.is(Flags.Private) || symbol.owner.isTerm)
      case _ => false
    }.collect { case definition: tpd.DefDef => definition }
    val declared = candidates.toSet
    // An exception the unit declares for itself and throws unhandled from visible methods is its failure channel,
    // which helpers may share.
    val channel = tree.filterSubTrees {
      case definition: tpd.DefDef =>
        !definition.symbol.isAnonymousFunction && !definition.symbol.is(Flags.Synthetic) &&
        !declared(definition)
      case _ => false
    }.view.collect { case definition: tpd.DefDef => definition }
      .flatMap(definition => Thrown().of(definition.rhs).filter(_.classSymbol.source == tree.source)).toSet
    candidates.foreach(definition =>
      Thrown().of(definition.rhs) match {
        case types if types.nonEmpty && !types.forall(tpe => channel.exists(tpe <:< _)) =>
          helpers(definition.symbol) = definition -> types
        case _ => ()
      }
    )
  }

  /** What a body throws outside its own handlers and the functions it defines. */
  private class Thrown extends tpd.TreeTraverser {
    private val found = mutable.Set.empty[Type]
    val invoked       = mutable.Set.empty[Symbol]

    def of(body: tpd.Tree)(using Context): Set[Type] = {
      traverse(body)
      found.toSet
    }

    override def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
      case tpd.Try(_, _, _)                                                            => ()
      case definition: tpd.DefDef if definition.symbol.isAnonymousFunction             => ()
      case tpd.Apply(function, List(exception)) if function.symbol == defn.throwMethod =>
        found += exception.tpe.widen
      case reference: (tpd.Ident | tpd.Select) =>
        invoked += reference.symbol
        traverseChildren(tree)
      case _ => traverseChildren(tree)
    }
  }
}
