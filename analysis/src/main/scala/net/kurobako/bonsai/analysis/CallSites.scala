package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameKinds.DefaultGetterName
import dotty.tools.dotc.core.Names.DerivedName
import dotty.tools.dotc.core.Names.TermName
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.AppliedType
import dotty.tools.dotc.core.Types.ConstantType
import dotty.tools.dotc.core.Types.ExprType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeParamRef
import net.kurobako.bonsai.lifting.Calls
import scala.collection.mutable

/** A construction or call kept until every precondition, binding and default argument of the build is known. */
private case class Pending(
    callee: Symbol,
    method: Symbol,
    arguments: List[tpd.Tree],
    test: Boolean,
    own: Boolean,
    location: String
)

/** A callee of the build: the key its sites are recorded under, its number of parameters, and the name, node and
  * suppressions its findings carry.
  */
private[analysis] case class Callee(key: String, arity: Int, name: String, node: String, suppressed: Set[String])

/** The calls and constructions of the build, counted and classified by what they pass and drop. */
private[analysis] class CallSites(symbols: Symbols, lines: Lines) {
  import symbols.*

  private val pending      = mutable.ArrayBuffer.empty[Pending]
  private val sites        = mutable.Map.empty[String, Vector[Algebra.Site]]
  private val explicit     = mutable.Map.empty[(Symbol, String, String), mutable.ArrayBuffer[Algebra.Site]]
  private val spares       = mutable.ArrayBuffer.empty[Smells.Spare]
  private val results      = mutable.Map.empty[Symbol, Option[String]]
  private val calls        = mutable.Map.empty[Symbol, (Int, Int)]
  private val discards     = mutable.Map.empty[Symbol, Int]
  private val negations    = mutable.Set.empty[tpd.Tree]
  private val called       = mutable.Set.empty[tpd.Tree]
  private val negatedCalls = mutable.Map.empty[Symbol, Int]
  private val tracked      = mutable.Map.empty[Symbol, Boolean]
  private val bindings     = mutable.Map.empty[Symbol, tpd.Tree]
  private val defaults     = mutable.Map.empty[Symbol, tpd.Tree]
  private val generics     = mutable.Map.empty[Symbol, (String, String, Vector[String])]
  private val arguments    = mutable.Map.empty[(Symbol, Int), mutable.Map[String, Int]]
  private val testUses     = mutable.Map.empty[(Symbol, Int), Int]
  private val varied       = mutable.Set.empty[(Symbol, Int)]

  def recorded: Map[String, Vector[Algebra.Site]] = sites.toMap

  // Redundant arguments are dropped first, so an unused default only shows once they are gone.
  def spared(defaults: Vector[Smells.Default]): Vector[Smells.Spare] = {
    val redundant = defaults.map(default => default.node -> default.parameter).toSet
    spares.filterNot(spare => redundant((spare.node, spare.parameter))).toVector
  }

  private def site(call: Pending, arguments: Vector[Algebra.Argument] = Vector.empty): Algebra.Site =
    Algebra.Site(arguments, call.test, call.own, call.location)

  /** Classifies the calls kept, with `callees` those that have a model, and notes the default arguments they repeat and
    * the defaults no call overrides.
    */
  def align(callees: collection.Map[Symbol, Callee])(using Context): Unit = {
    for {
      (callee, recorded) <- pending.groupBy(_.callee)
      subject            <- callees.get(callee)
      aligned = recorded.filter(_.arguments.size == subject.arity) if aligned.nonEmpty
    } sites(subject.key) = aligned.map(call => site(call, call.arguments.map(classify(_)).toVector)).toVector
    for {
      call              <- pending if call.method.name != nme.copy && defaulted(call.method)
      (argument, index) <- call.arguments.zipWithIndex if !omitted(argument) && droppable(call.arguments, index)
      default           <- defaults.get(defaultGetter(call.method, index))
      value             <- same(argument, default)
      parameter         <- fieldsOf(call.callee).lift(index)
    } explicit.getOrElseUpdate((call.callee, parameter.name, value), mutable.ArrayBuffer.empty) +=
      site(call)
    spares ++= spareDefaults(callees)
    pending.clear()
  }

  def usages(methods: collection.Map[Symbol, Smells.Method]): Vector[Smells.Usage] =
    calls.toVector.flatMap { case (symbol, (total, tests)) =>
      for {
        method <- methods.get(symbol)
        result <- results.getOrElse(symbol, None)
      } yield Smells.Usage(
        method,
        result,
        total,
        discards.getOrElse(symbol, 0),
        tests,
        negatedCalls.getOrElse(symbol, 0)
      )
    }

  def instantiations: Vector[Smells.Instantiation] =
    arguments.toVector.flatMap { case ((symbol, index), types) =>
      generics.get(symbol).map((owner, node, parameters) =>
        Smells.Instantiation(
          owner,
          node,
          parameters(index),
          types.toMap,
          varied((symbol, index)),
          testUses.getOrElse((symbol, index), 0)
        )
      )
    }

  def defaulting(callees: collection.Map[Symbol, Callee]): Vector[Smells.Default] =
    explicit.toVector.flatMap { case ((callee, parameter, value), recorded) =>
      callees.get(callee).map(subject =>
        Smells.Default(subject.name, subject.node, parameter, value, recorded.toVector)
      )
    }

  private def spareDefaults(callees: collection.Map[Symbol, Callee])(using Context): Vector[Smells.Spare] =
    pending.groupBy(_.method).toVector.flatMap((method, calls) => spare(method, calls.toVector, callees))

  private def spare(method: Symbol, calls: Vector[Pending], callees: collection.Map[Symbol, Callee])(using
      Context
  ): Vector[Smells.Spare] = {
    val plain = method.is(Flags.Method) && !method.isConstructor && !method.is(Flags.Synthetic) &&
      method.isEffectivelyFinal && !method.allOverriddenSymbols.hasNext && !exported(method)
    val clause    = method.paramSymss.find(_.forall(_.isTerm)).getOrElse(Nil).toVector
    val sites     = calls.map(site(_))
    val arguments = calls.map(_.arguments.toVector)
    callees.get(method).filter(_ => plain && calls.exists(!_.own)).toVector.flatMap { subject =>
      clause.zipWithIndex.collect {
        case (parameter, index)
            if parameter.is(Flags.HasDefault) &&
              arguments.forall(args => args.sizeIs == clause.size && !omitted(args(index))) =>
          Smells.Spare(subject.name, subject.node, parameter.name.toString, sites, subject.suppressed)
      }
    }
  }

  /** Keeps an application the code inverts with `!`, which is how a boolean method every caller negates is found. */
  def negate(call: tpd.Tree): Unit = negations += innermost(call)

  @annotation.tailrec
  private def innermost(call: tpd.Tree): tpd.Tree = call match {
    case tpd.Apply(function: tpd.Apply, _) => innermost(function)
    case other                             => other
  }

  /** Counts a reference to a method without parameter lists, which is a call that no application shows. */
  def invoke(reference: tpd.Tree, from: Option[String])(using Context): Unit = {
    val method = reference.symbol
    if (method.is(Flags.Method) && method.info.isInstanceOf[ExprType] && !called(reference) && result(method).isDefined)
      count(method, from.flatMap(nodes.get).exists(_.test), negations.contains(reference))
  }

  private def count(method: Symbol, test: Boolean, negated: Boolean): Unit = {
    val (total, tests) = calls.getOrElse(method, (0, 0))
    calls(method) = (total + 1, tests + (if (test) 1 else 0))
    if (negated) negatedCalls(method) = negatedCalls.getOrElse(method, 0) + 1
  }

  /** Counts a statement whose value is dropped, for methods whose results are tracked. */
  def discard(
      statement: tpd.Tree
  )(using Context): Unit = { // `val _ = value` is typed as `value match { case _ => () }`.
    val dropped = statement match {
      case tpd.Match(selector, List(tpd.CaseDef(tpd.Ident(name), tpd.EmptyTree, _))) if name.toString == "_" =>
        selector
      case other => other
    }
    val method = invoked(dropped)
    if (result(method).isDefined) discards(method) = discards.getOrElse(method, 0) + 1
  }

  @annotation.tailrec
  private def invoked(tree: tpd.Tree)(using Context): Symbol = Calls.peel(tree) match {
    case tpd.Apply(function, _)     => invoked(function)
    case tpd.TypeApply(function, _) => invoked(function)
    case other                      => other.symbol
  }

  /** Records the type arguments of a call or construction of a generic definition of the build. */
  def instantiate(target: Symbol, types: List[Type], clause: Int, owner: Option[String])(using Context): Unit = {
    val generic = Some(constructs(target)).filter(_.exists).getOrElse(target)
    // Extension methods take a type clause of their own after the extension's, each filled by its own application.
    val clauses =
      if (generic.isClass) List(generic.typeParams) else generic.paramSymss.filter(_.headOption.exists(_.isType))
    val parameters = clauses.lift(clause).getOrElse(Nil)
    // The compiler, not the caller, chooses the type arguments of a given; default getters repeat those of their
    // method, which is reported instead; and other builds give their own to what a library exports.
    if (
      parameters.nonEmpty && parameters.size == types.size && !generic.isOneOf(Flags.Synthetic | Flags.Given) &&
      !generic.name.is(DefaultGetterName) && !exported(generic)
    )
      key(generic).filter(_ => output(generic).isDefined).foreach { node =>
        generics.getOrElseUpdate(
          generic,
          (qualifiedName(generic), node, clauses.flatten.map(_.name.toString).toVector)
        ): Unit
        val test   = owner.flatMap(nodes.get).exists(_.test)
        val offset = clauses.take(clause).map(_.size).sum
        types.zipWithIndex.foreach { (argument, index) =>
          val slot = generic -> (offset + index)
          // A literal type such as the 16 of FloatingPoint[16] is an argument of its own, not its class.
          val bare = argument.dealias match {
            case literal: ConstantType => literal
            case _                     => argument.widenDealias
          }
          if (!bare.typeSymbol.isClass || bare.existsPart(_.typeSymbol.is(Flags.TypeParam))) varied += slot
          else if (!bare.isRef(defn.NothingClass)) {
            val counts = arguments.getOrElseUpdate(slot, mutable.Map.empty)
            val shown  = bare.show
            counts(shown) = counts.getOrElse(shown, 0) + 1
            if (test) testUses(slot) = testUses.getOrElse(slot, 0) + 1
          }
        }
      }
  }

  def bind(symbol: Symbol, value: tpd.Tree): Unit = bindings(symbol) = value

  def default(getter: Symbol, value: tpd.Tree): Unit = defaults(getter) = value

  /** Keeps a construction of a case class, or a call to a method of the build, for classification at the end. A
    * construction is `own` inside its class or companion, a call is `own` inside the method itself.
    */
  def record(application: tpd.Apply, owner: Option[String], enclosing: Symbol => Boolean)(using Context): Unit =
    application.fun match {
      case tpd.Apply(_, _) => ()
      case _               =>
        val method      = application.symbol
        val constructed = constructs(method)
        val test        = owner.flatMap(nodes.get).exists(_.test)
        called += application.fun
        if (!constructed.exists && result(method).isDefined) count(method, test, negations.contains(application))
        val callee =
          if (constructed.exists && constructed.is(Flags.CaseClass)) constructed
          else if (method.is(Flags.Method) && !method.isConstructor) method
          else NoSymbol
        if (
          callee.exists &&
          tracked.getOrElseUpdate(
            callee,
            output(callee).isDefined &&
              (fieldsOf(callee).exists(_.kind != Algebra.Kind.Other) ||
                defaulted(method) || (callee.is(Flags.Method) && fieldsOf(callee).nonEmpty))
          )
        ) {
          val position = application.sourcePos
          val location = lines.location(position)
          val own      = if (constructed.exists) owner.isDefined && owner == key(callee) else enclosing(callee)
          pending += Pending(callee, method, application.args, test, own, location)
        }
    }

  private def defaulted(method: Symbol)(using Context): Boolean =
    method.paramSymss.flatten.exists(_.is(Flags.HasDefault))

  // A named argument reaches the call through a binding whose value may itself be the default getter, and a quote
  // reaches a getter it cannot access through an inline accessor named after it.
  private def omitted(argument: tpd.Tree)(using Context): Boolean = {
    val symbol = Calls.peel(argument).symbol
    @annotation.tailrec
    def getter(name: TermName): Boolean = name.is(DefaultGetterName) ||
      (name match {
        case derived: DerivedName => getter(derived.underlying)
        case _                    => false
      })
    symbol.exists && (getter(symbol.name.toTermName) || bindings.get(symbol).exists(omitted(_)))
  }

  // Dropping an argument would shift any written positional argument after it.
  private def droppable(arguments: List[tpd.Tree], index: Int)(using Context): Boolean =
    arguments.drop(index + 1).forall {
      case tpd.NamedArg(_, _) => true
      case argument           => omitted(argument)
    }

  // A case class `apply` shares the constructor's default getters in the companion.
  private def defaultGetter(method: Symbol, index: Int)(using Context): Symbol = {
    val owner = if (method.isConstructor) method.owner.companionModule.moduleClass else method.owner
    val own   = owner.info.decl(DefaultGetterName(method.name.asTermName, index)).symbol
    if (own.exists || !method.is(Flags.Synthetic)) own
    else owner.info.decl(DefaultGetterName(nme.CONSTRUCTOR, index)).symbol
  }

  /** The case class a primary constructor, synthetic `apply` or `copy` builds. */
  private def constructs(method: Symbol)(using Context): Symbol =
    if (method.isPrimaryConstructor || method.is(Flags.Synthetic) && method.name == nme.copy) method.owner
    else if (method.is(Flags.Synthetic) && method.name == nme.apply && method.owner.is(Flags.ModuleClass))
      method.owner.companionClass
    else NoSymbol

  /** The rendered result type of a build method whose callers might use it; fluent and `Unit` results are skipped. */
  private def result(method: Symbol)(using Context): Option[String] =
    if (!method.exists || !method.is(Flags.Method)) None
    else
      results.getOrElseUpdate(
        method, {
          val tpe     = method.info.finalResultType.widen
          val trivial = tpe.isRef(defn.UnitClass) || tpe.isRef(defn.NothingClass) || tpe.isRef(defn.NullClass)
          val fluent  = method.owner.isClass && tpe.classSymbol.exists && tpe.classSymbol.derivesFrom(method.owner)
          // an effect of Unit, as in a tagless-final F[Unit], has no value for a caller to use
          val effect = tpe match {
            case applied: AppliedType =>
              applied.args.sizeIs == 1 && applied.args.head.isRef(defn.UnitClass) &&
              (applied.tycon.isInstanceOf[TypeParamRef] || applied.tycon.typeSymbol.is(Flags.TypeParam))
            case _ => false
          }
          Option.when(output(method).isDefined && !trivial && !fluent && !effect)(tpe.show)
        }
      )

  // Named and default arguments reach the call through synthetic local bindings and default getters.
  private def classify(tree: tpd.Tree, depth: Int = 4)(using Context): Algebra.Argument =
    Predicates.argument(tree).getOrElse {
      binding(tree) match {
        case Some(value) if depth > 0 => classify(value, depth - 1)
        case _                        => Algebra.Argument.Unknown
      }
    }

  private def binding(tree: tpd.Tree)(using Context): Option[tpd.Tree] = {
    val symbol = Calls.peel(tree).symbol
    bindings.get(symbol).orElse(defaults.get(symbol))
  }

  // The tree a named argument's binding or a default getter stands for.
  private def resolved(tree: tpd.Tree, depth: Int = 4)(using Context): tpd.Tree =
    binding(tree).filter(_ => depth > 0).fold(tree)(resolved(_, depth - 1))

  /** The value two arguments are both known to have, shown; an option is known by its contents. */
  private def same(left: tpd.Tree, right: tpd.Tree)(using Context): Option[String] = {
    val (first, second)          = (resolved(left), resolved(right))
    def contents(tree: tpd.Tree) = Calls.peel(tree) match {
      case tpd.Apply(_, List(inner)) => Some(inner)
      case _                         => None
    }
    (classify(first), classify(second)) match {
      case (Algebra.Argument.Present, Algebra.Argument.Present) =>
        contents(first).zip(contents(second)).flatMap(same(_, _)).map(inner => s"Some($inner)")
      case (value, other) => Option.when(value != Algebra.Argument.Unknown && value == other)(value.shown)
    }
  }
}
