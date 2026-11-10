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
import net.kurobako.bonsai.lifting.*
import scala.annotation.targetName
import scala.collection.mutable

private object CallSites {

  /** A construction or call kept until every precondition, binding and default argument of the build is known. */
  private case class Pending(
      callee: Symbol,
      method: Symbol,
      arguments: List[tpd.Tree],
      test: Boolean,
      own: Boolean,
      location: String,
      scope: Option[Symbol],
      helper: Option[Symbol]
  )

  /** The method a construction sits in and the positions of its arguments among that method's parameters. */
  case class Forward(helper: String, passed: Vector[Int])

  /** A call or construction, with `omitted` telling which of its arguments are default getters, when the callee has
    * defaults.
    */
  case class Call(site: Algebra.Site, omitted: Vector[Boolean], forward: Option[Forward])

  /** How often a method returning a value is called, discarded, tested, negated and unwrapped. */
  case class Count(result: String, calls: Int, tests: Int, discards: Int, negated: Int, unwrapped: Int) {
    def +(other: Count): Count = Count(
      result,
      calls + other.calls,
      tests + other.tests,
      discards + other.discards,
      negated + other.negated,
      unwrapped + other.unwrapped
    )
  }

  object Evidence {
    val empty: Evidence = Evidence(Map.empty, Map.empty, Map.empty)
  }

  /** What the code of a run records of its callees, by the keys they go by, so that runs which read the definitions and
    * the uses of a callee apart still count them together.
    */
  case class Evidence(
      calls: Map[String, Vector[Call]],
      counts: Map[String, Count],
      explicit: Map[(String, Int, String), Vector[Algebra.Site]]
  ) {
    @targetName("merge")
    def ++(other: Evidence): Evidence = Evidence(
      Maps.join(calls, other.calls)(_ ++ _),
      Maps.join(counts, other.counts)(_ + _),
      Maps.join(explicit, other.explicit)(_ ++ _)
    )

    def excluding(keys: Set[String]): Evidence = Evidence(
      calls.filterNot((key, _) => keys(key)),
      counts.filterNot((key, _) => keys(key)),
      explicit.filterNot { case ((key, _, _), _) => keys(key) }
    )
  }
}

/** The calls and constructions of the build, counted and classified by what they pass and drop. */
private[analysis] class CallSites(symbols: Symbols, lines: Lines) {
  import CallSites.*
  import symbols.*

  private val pending      = mutable.ArrayBuffer.empty[Pending]
  private val sites        = mutable.Map.empty[String, Vector[Algebra.Site]]
  private val forwarded    = mutable.Map.empty[String, Vector[Algebra.Site]]
  private val spares       = mutable.ArrayBuffer.empty[Smells.Spare]
  private val results      = mutable.Map.empty[Symbol, Option[String]]
  private val calls        = mutable.Map.empty[Symbol, (Int, Int)]
  private val discards     = mutable.Map.empty[Symbol, Int]
  private val negations    = mutable.Set.empty[tpd.Tree]
  private val unwraps      = mutable.Set.empty[tpd.Tree]
  private val unwrapped    = mutable.Map.empty[Symbol, Int]
  private val called       = mutable.Set.empty[tpd.Tree]
  private val negatedCalls = mutable.Map.empty[Symbol, Int]
  private val tracked      = mutable.Map.empty[Symbol, Boolean]
  private val bindings     = mutable.Map.empty[Symbol, tpd.Tree]
  private val locals       = mutable.Map.empty[Symbol, tpd.Tree]
  private val defaults     = mutable.Map.empty[Symbol, tpd.Tree]
  private val generics     = mutable.Map.empty[Symbol, (String, String, Vector[String])]
  private val arguments    = mutable.Map.empty[(Symbol, Int), mutable.Map[String, Int]]
  private val testUses     = mutable.Map.empty[(Symbol, Int), Int]
  private val varied       = mutable.Set.empty[(Symbol, Int)]
  private var concluded    = Evidence.empty               // scalafix:ok DisableSyntax.var
  private var repeated     = Vector.empty[Smells.Default] // scalafix:ok DisableSyntax.var

  def recorded: Map[String, Vector[Algebra.Site]] = sites.toMap

  /** The constructions of each case class, with the arguments a method forwards to the construction from its own
    * parameters read off the calls of that method.
    */
  def expanded: Map[String, Vector[Algebra.Site]] = forwarded.toMap

  // Redundant arguments are dropped first, so an unused default only shows once they are gone.
  def spared(defaults: Vector[Smells.Default]): Vector[Smells.Spare] = {
    val redundant = defaults.map(default => default.node -> default.parameter).toSet
    spares.filterNot(spare => redundant((spare.node, spare.parameter))).toVector
  }

  private def site(
      call: Pending,
      arguments: Vector[Algebra.Argument] = Vector.empty,
      derivations: Map[Int, Algebra.Derivation] = Map.empty
  ): Algebra.Site = Algebra.Site(arguments, call.test, call.own, call.location, derivations)

  /** The evidence of this run: its calls and constructions of `callees` and of the callees that `foreign` holds, the
    * counts of its calls to methods that return values, and the defaults its calls repeat.
    */
  def collect(callees: collection.Map[Symbol, Callee], foreign: Symbol => Boolean)(using Context): Evidence = {
    val passed = pending.filter(_.scope.isDefined).flatMap { call =>
      call.arguments.zipWithIndex.collect {
        case (argument, index) if !omitted(argument) => (call.callee, call.scope, index) -> argument
      }
    }.groupMap(_._1)(_._2)
    val helpers                               = pending.flatMap(_.helper).toSet
    val keys                                  = mutable.Map.empty[Symbol, Option[String]]
    def keyOf(callee: Symbol): Option[String] = keys.getOrElseUpdate(
      callee,
      callees.get(callee).map(_.key).orElse(Option.when(foreign(callee) || helpers(callee))(calleeKey(callee)))
    )
    val withDefaults                = mutable.Map.empty[Symbol, Boolean]
    def hasDefaults(callee: Symbol) = withDefaults.getOrElseUpdate(
      callee,
      (if (callee.isClass) callee.primaryConstructor else callee).paramSymss.flatten.exists(_.is(Flags.HasDefault))
    )
    val calls = pending.flatMap(call => keyOf(call.callee).map(key => key -> call)).groupMap(_._1)(_._2).view
      .mapValues(_.map { call =>
        val derived = if (call.callee.isClass) derivations(call.arguments) else Map.empty[Int, Algebra.Derivation]
        val flags   = if (hasDefaults(call.callee)) call.arguments.map(omitted(_)).toVector else Vector.empty
        val onward  = forward(call).flatMap((helper, passed) => keyOf(helper).map(Forward(_, passed)))
        Call(site(call, call.arguments.map(classify(_)).toVector, derived), flags, onward)
      }.toVector).toMap
    val explicit = mutable.Map.empty[(String, Int, String), Vector[Algebra.Site]]
    for {
      call              <- pending if call.method.name != nme.copy && defaulted(call.method)
      key               <- keyOf(call.callee)
      (argument, index) <- call.arguments.zipWithIndex if !omitted(argument) && droppable(call.arguments, index)
      value             <- valueOf(argument)
      if !varied(argument, passed.getOrElse((call.callee, call.scope, index), Nil))
    } explicit((key, index, value)) = explicit.getOrElse((key, index, value), Vector.empty) :+ site(call)
    val counts = this.calls.view.flatMap { (method, total) =>
      for {
        key    <- keyOf(method)
        result <- results.getOrElse(method, None)
      } yield key -> Count(
        result,
        calls = total._1,
        tests = total._2,
        discards = discards.getOrElse(method, 0),
        negated = negatedCalls.getOrElse(method, 0),
        unwrapped = unwrapped.getOrElse(method, 0)
      )
    }.toMap
    pending.clear()
    Evidence(calls, counts, explicit.toMap)
  }

  private def forward(call: Pending)(using Context): Option[(Symbol, Vector[Int])] =
    call.helper.filter(_ => call.callee.isClass).map { helper =>
      val parameters = helper.paramSymss.find(_.forall(_.isTerm)).getOrElse(Nil).toVector
      helper -> call.arguments.map(argument => parameters.indexOf(Calls.peel(argument).symbol)).toVector
    }

  /** The key under which the calls of `callee` are recorded. */
  def calleeKey(callee: Symbol)(using Context): String =
    if (callee.isClass) qualifiedName(callee) else methodKey(callee, fieldsOf(callee))

  /** Classifies the calls of `evidence` to `callees`, and notes the defaults no call overrides. */
  def conclude(evidence: Evidence, callees: collection.Map[Symbol, Callee])(using Context): Unit = {
    for {
      (callee, subject) <- callees
      recorded          <- evidence.calls.get(subject.key)
      aligned = recorded.filter(_.site.arguments.sizeIs == subject.arity) if aligned.nonEmpty
    } {
      sites(subject.key) = aligned.map(_.site)
      if (callee.isClass) forwarded(subject.key) = aligned.flatMap(expand(_, evidence))
    }
    for {
      (callee, subject) <- callees
      recorded          <- evidence.calls.get(subject.key)
    } {
      if (!callee.isClass) spares ++= spare(callee, recorded, subject)
      spares ++= fixed(callee, recorded, subject)
    }
    concluded = evidence
    repeated = defaulting(callees)
  }

  def repeatedDefaults: Vector[Smells.Default] = repeated

  private def bare(call: Call): Algebra.Site = call.site.copy(arguments = Vector.empty, derivations = Map.empty)

  // A construction inside a method passes that method's parameters on, so each call of the method is a construction
  // with the arguments of that call.
  private def expand(call: Call, evidence: Evidence): Vector[Algebra.Site] = {
    val direct = call.site.copy(derivations = Map.empty)
    call.forward.map(forward => forward.passed -> evidence.calls.getOrElse(forward.helper, Vector.empty)) match {
      case Some((passed, callers)) if callers.nonEmpty && !passed.forall(_ < 0) =>
        callers.map { caller =>
          val arguments = direct.arguments.zip(passed).map((value, index) =>
            caller.site.arguments.lift(index).getOrElse(value)
          )
          Algebra.Site(arguments, call.site.test || caller.site.test, call.site.own, call.site.location)
        }
      case _ => Vector(direct)
    }
  }

  def usages(methods: collection.Map[Symbol, Smells.Method]): Vector[Smells.Usage] =
    methods.values.toVector.flatMap { method =>
      concluded.counts.get(method.key).map(count =>
        Smells.Usage(
          method,
          count.result,
          count.calls,
          count.discards,
          count.tests,
          negated = count.negated,
          unwrapped = count.unwrapped
        )
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

  private def defaulting(callees: collection.Map[Symbol, Callee])(using Context): Vector[Smells.Default] = {
    val byKey = callees.map((symbol, subject) => subject.key -> (symbol, subject)).toMap
    concluded.explicit.toVector.flatMap { case ((key, index, value), recorded) =>
      for {
        (callee, subject) <- byKey.get(key)
        default <- defaults.get(defaultGetter(if (callee.isClass) callee.primaryConstructor else callee, index))
        if valueOf(default).contains(value)
        parameter <- fieldsOf(callee).lift(index)
      } yield Smells.Default(subject.name, subject.node, parameter.name, value, recorded, subject.line)
    }
  }

  private def plain(method: Symbol)(using Context): Boolean =
    method.is(Flags.Method) && !method.isConstructor && !method.is(Flags.Synthetic) && method.isEffectivelyFinal &&
      !method.allOverriddenSymbols.hasNext && !exported(method)

  // A case class that derives an instance may be read from data that sets its defaults.
  private def fixable(callee: Symbol)(using Context): Boolean =
    if (callee.isClass)
      !exported(callee) &&
      !callee.companionModule.moduleClass.info.decls.toList.exists(_.isOneOf(Flags.Given | Flags.Implicit))
    else plain(callee)

  private def fixed(callee: Symbol, calls: Vector[Call], subject: Callee)(using Context): Vector[Smells.Spare] =
    if (calls.exists(!_.site.own) && fixable(callee)) {
      val clause = (if (callee.isClass) callee.primaryConstructor else callee).paramSymss.find(_.forall(_.isTerm))
        .getOrElse(Nil).toVector
      val sites = calls.map(bare(_))
      clause.zipWithIndex.collect {
        case (parameter, index)
            if parameter.is(Flags.HasDefault) &&
              calls.forall(call => call.omitted.sizeIs == clause.size && call.omitted(index)) =>
          Smells.Spare(
            subject.name,
            subject.node,
            parameter.name.toString,
            sites,
            subject.suppressed,
            lines.line(callee.sourcePos),
            fixed = true
          )
      }
    } else Vector.empty

  private def spare(method: Symbol, calls: Vector[Call], subject: Callee)(using Context): Vector[Smells.Spare] =
    if (plain(method) && calls.exists(!_.site.own)) {
      val clause = method.paramSymss.find(_.forall(_.isTerm)).getOrElse(Nil).toVector
      val sites  = calls.map(bare(_))
      clause.zipWithIndex.collect {
        case (parameter, index)
            if parameter.is(Flags.HasDefault) &&
              calls.forall(call => call.omitted.sizeIs == clause.size && !call.omitted(index)) =>
          Smells.Spare(
            subject.name,
            subject.node,
            parameter.name.toString,
            sites,
            subject.suppressed,
            lines.line(method.sourcePos)
          )
      }
    } else Vector.empty

  /** Keeps an application the code inverts with `!`, which is how a boolean method every caller negates is found. */
  def negate(call: tpd.Tree): Unit = negations += innermost(call)

  /** Keeps an application the code unwraps with `get`, which is how a `Try` every production caller unwraps is found.
    */
  def unwrap(call: tpd.Tree): Unit = unwraps += innermost(call)

  @annotation.tailrec
  private def innermost(call: tpd.Tree): tpd.Tree = call match {
    case tpd.Apply(function: tpd.Apply, _) => innermost(function)
    case tpd.Block(_, expression)          => innermost(expression)
    case tpd.Typed(expression, _)          => innermost(expression)
    case other                             => other
  }

  /** Counts a reference to a method without parameter lists, which is a call that no application shows. */
  def invoke(reference: tpd.Tree, from: Option[String])(using Context): Unit = {
    val method = reference.symbol
    if (method.is(Flags.Method) && method.info.isInstanceOf[ExprType] && !called(reference) && result(method).isDefined)
      count(method, from.flatMap(nodes.get).exists(_.test), negations.contains(reference), unwraps.contains(reference))
  }

  private def count(method: Symbol, test: Boolean, negated: Boolean, unwrap: Boolean): Unit = {
    val (total, tests) = calls.getOrElse(method, (0, 0))
    calls(method) = (total + 1, tests + (if (test) 1 else 0))
    if (negated) negatedCalls(method) = negatedCalls.getOrElse(method, 0) + 1
    if (unwrap && !test) unwrapped(method) = unwrapped.getOrElse(method, 0) + 1
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

  def local(symbol: Symbol, value: tpd.Tree): Unit = locals(symbol) = value

  def default(getter: Symbol, value: tpd.Tree): Unit = defaults(getter) = value

  /** Keeps a construction of a case class, or a call to a method of the build, for classification at the end. A
    * construction is `own` inside its class or companion, a call is `own` inside the method itself.
    */
  def record(
      application: tpd.Apply,
      owner: Option[String],
      enclosing: Symbol => Boolean,
      scope: Option[Symbol],
      helper: Option[Symbol]
  )(using Context): Unit =
    application.fun match {
      case tpd.Apply(_, _) => ()
      case _               =>
        val method      = application.symbol
        val constructed = constructs(method)
        val test        = owner.flatMap(nodes.get).exists(_.test)
        called += application.fun
        if (!constructed.exists && result(method).isDefined)
          count(method, test, negations.contains(application), unwraps.contains(application))
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
          pending += Pending(callee, method, application.args, test, own, location, scope, helper)
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

  // A reordered named argument reaches the call through a binding, an in-order one as itself.
  private def named(argument: tpd.Tree)(using Context): Boolean = argument match {
    case tpd.NamedArg(_, _) => true
    case _                  => bindings.contains(Calls.peel(argument).symbol)
  }

  // A named argument that another call to the same method varies is spelled out on purpose.
  private def varied(argument: tpd.Tree, others: Iterable[tpd.Tree])(using Context): Boolean =
    named(argument) && others.exists(same(argument, _).isEmpty)

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

  // The tree a local value or a named argument's binding stands for.
  private def origin(tree: tpd.Tree, depth: Int = 3)(using Context): tpd.Tree = {
    val bare = Calls.peel(tree)
    locals.get(bare.symbol).orElse(bindings.get(bare.symbol)).filter(_ => depth > 0).fold(bare)(origin(_, depth - 1))
  }

  // The arguments that are a plain value are the sources the others may be computed from.
  private def derivations(arguments: List[tpd.Tree])(using Context): Map[Int, Algebra.Derivation] = {
    val sources = arguments.zipWithIndex.collect {
      case (argument, index) if sourceable(argument) => Calls.peel(argument).symbol -> index
    }.toMap
    if (sources.isEmpty) Map.empty
    else
      arguments.zipWithIndex.flatMap { (argument, index) =>
        val used = mutable.SortedSet.empty[Int]
        Option.unless(omitted(argument))(origin(argument)).flatMap(template(_, sources, index, used)).collect {
          case text if used.nonEmpty && !Single.matches(text) => index -> Algebra.Derivation(used.toVector, text)
        }
      }.toMap
  }

  private val Single = "\\$\\d+".r

  private val WorldPackages = Vector("java.io.", "java.nio.file.", "java.net.http.", "scala.io.", "scala.sys.")

  private def sourceable(argument: tpd.Tree)(using Context): Boolean = {
    val bare = Calls.peel(argument)
    bare.isInstanceOf[tpd.Ident] && bare.symbol.exists && bare.symbol.isTerm &&
    !bare.symbol.isOneOf(Flags.Method | Flags.Module) && !omitted(argument)
  }

  // The expression as text when it only applies methods to the sources and constants, which is how a field computed
  // from others reads the same wherever it is built.
  private def template(tree: tpd.Tree, sources: Map[Symbol, Int], own: Int, used: mutable.SortedSet[Int])(using
      Context
  ): Option[String] = {
    def all(trees: List[tpd.Tree]): Option[List[String]] =
      trees.foldRight(Option(List.empty[String]))((tree, rest) => rest.flatMap(tail => text(tree).map(_ :: tail)))
    def text(tree: tpd.Tree): Option[String] = Calls.peel(tree) match {
      case ident @ tpd.Ident(name) =>
        sources.get(ident.symbol).filter(_ != own) match {
          case Some(index)                                               => used += index; Some(s"$$$index")
          case None if ident.symbol.isOneOf(Flags.Method | Flags.Module) => Some(name.toString)
          case None                                                      => None
        }
      case select @ tpd.Select(qualifier, name) if select.symbol.exists && !readsWorld(select.symbol) =>
        text(qualifier).map(inner => s"$inner.$name")
      case tpd.TypeApply(function, _)   => text(function)
      case tpd.Apply(function, applied) =>
        text(function).flatMap(inner => all(applied).map(rest => s"$inner(${rest.mkString(", ")})"))
      case literal: tpd.Literal        => Some(literal.show)
      case tpd.SeqLiteral(elements, _) => all(elements).map(rest => rest.mkString(", "))
      case _                           => None
    }
    text(tree)
  }

  // A value read from the file system or network is a snapshot of it, which later reads need not agree with.
  private def readsWorld(symbol: Symbol)(using Context): Boolean =
    WorldPackages.exists(symbol.owner.fullName.toString.startsWith(_))

  /** The value an argument is known to have, shown; an option is known by its contents. */
  private def valueOf(argument: tpd.Tree)(using Context): Option[String] = {
    val tree = resolved(argument)
    classify(tree) match {
      case Algebra.Argument.Present =>
        Calls.peel(tree) match {
          case tpd.Apply(_, List(inner)) => valueOf(inner).map(shown => s"Some($shown)")
          case _                         => None
        }
      case Algebra.Argument.Unknown => None
      case value                    => Some(value.shown)
    }
  }

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
