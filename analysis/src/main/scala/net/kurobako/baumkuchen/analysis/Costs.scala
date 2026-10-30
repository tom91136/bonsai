package net.kurobako.baumkuchen.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import scala.collection.mutable

private object Costs {

  object Bound {
    val Zero: Bound = Bound(Polynomial.Zero, Polynomial.Zero)
  }

  /** Time and allocation bounds; allocation counts the copies of collections an operation makes. */
  case class Bound(time: Polynomial, space: Polynomial) {
    def +(that: Bound): Bound = Bound(time + that.time, space + that.space)

    def *(factor: Polynomial): Bound = Bound(time * factor, space * factor)

    /** The terms that are not above linear in sizes, which is what a caller inherits from a callee. */
    def linear: Bound = Bound(kept(time), kept(space))

    private def kept(bound: Polynomial): Polynomial =
      Polynomial(bound.terms.filter((monomial, _) => Atoms.degreeOf(monomial) <= 1))
  }

  /** One costly operation, its cost already multiplied by the turns of the loops around it. */
  case class Charge(
      what: String,
      within: Option[String],
      time: Polynomial,
      space: Polynomial,
      linear: Boolean,
      callback: Boolean,
      terminal: Boolean,
      line: Int
  ) {
    def scaled(factor: Polynomial): Charge =
      if (terminal) this else copy(time = time * factor, space = space * factor)

    def hit(terms: Vector[Complexity.Term]): Complexity.Hit = Complexity.Hit(terms, what, within, line)
  }

  enum Recursion {
    case Absent
    case Bounded(factor: Polynomial)
    case Unbounded
  }
}

/** Upper bounds on the time and allocation of build methods, as polynomials in the sizes of collections.
  *
  * Every operation that costs something is a charge, multiplied by the turns of the loops around it. A traversal runs
  * its function once per element of its receiver, so it is a loop of that many turns, and a call costs its callee's
  * bound with the callee's parameters and `this` rebound to the arguments and receiver. Loops of unknown length,
  * recursion that does not shrink a collection and sizes that cannot be followed contribute the atom `?`, and terms
  * containing it are never reported.
  *
  * Allocation counts the copies an operation makes of what it works on, not the results it hands back.
  */
private class Costs(
    definitions: collection.Map[Symbol, tpd.DefDef],
    initialisers: collection.Map[Symbol, tpd.Tree],
    bounded: Vector[Complexity.Bounded]
)(using Context) {
  import Costs.*

  private val library   = Library()
  private val summaries = mutable.Map.empty[Symbol, Bound]
  private val pending   = mutable.Set.empty[Symbol]
  private val subtrees  = Children()
  private val fixed     = mutable.Map.empty[Symbol, Option[Polynomial]]
  private val globs     = bounded.map(entry => Intended.qualifiedNames(entry.member) -> entry)
  private val reached   = mutable.Set.empty[Complexity.Bounded]
  private val declared  = mutable.Map.empty[Symbol, Option[Polynomial]]

  private def sizesOf(method: Symbol): Sizes = Sizes(library, method, initialisers, fixed, declaredSize, searching)

  /** A sorted search, or a method whose body makes one, locates the element a scan starts from. */
  private def searching(method: Symbol): Boolean =
    searchCall(method) || definitions.get(method).exists(definition =>
      sizesOf(method).exists(definition.rhs) {
        case application: tpd.Apply => searchCall(application.fun.symbol)
        case _                      => false
      }
    )

  private def searchCall(method: Symbol): Boolean =
    (method.name.toString == "binarySearch" && method.owner.fullName.toString.startsWith("java.util.Arrays")) ||
      (method.name.toString == "search" && method.owner.name.toString.contains("Search"))

  /** The size of the collections of a member or parameter that the build declares bounded, matching its qualified name
    * or its owner's.
    */
  private def declaredSize(member: Symbol): Option[Polynomial] =
    if (!member.owner.isClass && !member.is(Flags.Param)) None
    else
      declared.getOrElseUpdate(
        member, {
          val owner = member.owner.fullName.toString.replace("$.", ".").stripSuffix("$")
          val name  = s"$owner.${member.name}"
          val hits  = globs.collect {
            case (glob, entry) if glob.matches(s"$name.") || glob.matches(s"$owner.") => entry
          }
          reached ++= hits
          hits.minByOption(_.max).map(entry => Polynomial.constant(entry.max))
        }
      )

  /** The declared bounds that no member the analysis reached matches. */
  def unmatched: Vector[Complexity.Bounded] = bounded.filterNot(reached)

  private val Searches = Set("contains", "indexOf", "lastIndexOf", "containsSlice", "indexOfSlice", "sameElements")
  private val Sorts    = Set("sorted", "sortBy", "sortWith")
  private val Copies   =
    Set(
      "toList",
      "toVector",
      "toSet",
      "toMap",
      "toArray",
      "toSeq",
      "toIndexedSeq",
      "toBuffer",
      "distinct",
      "reverse",
      "flatten"
    )
  private val Aggregates = Copies ++ Set("sum", "product", "max", "min", "mkString")
  private val Walks      = Set("apply", "length", "size", "last")
  private val Shifts     = Set("remove", "insert", "insertAll", "prepend", "prependAll", "+=:", "++=:")
  private val Appends    = Set(":+", "appended", "++", "appendedAll", "concat")
  private val Prepends   = Set(":::", "++:", "prependedAll")
  private val Folds      = Set("foldLeft", "fold", "scanLeft", "reduceLeft", "reduce", "aggregate")
  private val Guards     = Set("scala.Option$.when", "scala.Option$.unless")
  private val Cons       = Set("::", "+:")
  private val Compilers  = Set("scala.util.matching.Regex.<init>", "java.util.regex.Pattern$.compile")

  /** The time and allocation hits of `method`, from its own body and its callees' linear costs. */
  def summary(method: Symbol): Option[Complexity.Bounds] = definitions.get(method).map { definition =>
    val walk = Walk(method, callee => bound(callee).linear)
    walk.run(definition)
    val charges = recursion(method, definition, walk.recursions.toVector) match {
      case Recursion.Bounded(factor) => walk.charges.toVector.map(_.scaled(factor))
      case _                         => walk.charges.toVector
    }
    Complexity.Bounds(
      hit(charges, _.time),
      hit(charges, _.space),
      charges.find(charge => charge.callback && charge.linear).map(_.hit(Vector.empty))
    )
  }.filter(_.nonEmpty)

  private def hit(charges: Vector[Charge], select: Charge => Polynomial): Option[Complexity.Hit] = {
    val candidates = charges.flatMap(charge => Atoms.terms(select(charge)).map(_ -> charge))
    val top        = candidates.map(_._1.degree).maxOption.getOrElse(0)
    Option.when(top >= 2) {
      val best = candidates.filter(_._1.degree == top)
      best.head._2.hit(best.map(_._1).distinct)
    }
  }

  /** The bound of `method` in the sizes of its own parameters; zero outside the build. */
  private def bound(method: Symbol): Bound = summaries.get(method) match {
    case Some(known)             => known
    case None if pending(method) => Bound(Atoms.Unknown, Atoms.Unknown)
    case None                    => definitions.get(method).fold(Bound.Zero) { definition =>
        pending += method
        val result =
          try summarise(method, definition)
          finally pending -= method
        summaries(method) = result
        result
      }
  }

  private def summarise(method: Symbol, definition: tpd.DefDef): Bound = {
    val walk = Walk(method, bound(_))
    walk.run(definition)
    def sum(charges: Iterable[Charge]): Bound =
      charges.foldLeft(Bound.Zero)((total, charge) => total + Bound(charge.time, charge.space))
    val (exits, frames) = walk.charges.partition(_.terminal)
    val total           = sum(frames)
    recursion(method, definition, walk.recursions.toVector) match {
      case Recursion.Absent          => total + sum(exits)
      case Recursion.Bounded(factor) => (total + Bound(Polynomial.One, Polynomial.Zero)) * factor + sum(exits)
      case Recursion.Unbounded       => Bound(Atoms.Unknown, Atoms.Unknown)
    }
  }

  private def name(parameter: Symbol): String = s"${parameter.name}#${parameter.id}"

  private def sizeOf(parameter: Symbol): Polynomial =
    declaredSize(parameter).getOrElse(Atoms.size(name(parameter)))

  private def line(tree: tpd.Tree): Int = {
    val position = tree.sourcePos
    if (position.exists) position.line + 1 else 0
  }

  private def argumentLists(application: tpd.Apply): List[tpd.Tree] = application.fun match {
    case inner: tpd.Apply                   => argumentLists(inner) ++ application.args
    case tpd.TypeApply(inner: tpd.Apply, _) => argumentLists(inner) ++ application.args
    case _                                  => application.args
  }

  /** How a method recurs on itself: not at all, once per element of a parameter that shrinks, or unboundedly. */
  private def recursion(method: Symbol, definition: tpd.DefDef, calls: Vector[(tpd.Apply, Scope)]): Recursion =
    if (calls.isEmpty) Recursion.Absent
    else if (depth(definition.rhs, method) > 1) Recursion.Unbounded
    else {
      val sizes = sizesOf(method)
      definition.termParamss.flatten.zipWithIndex.collectFirst {
        case (parameter, index)
            if library.sized(parameter.tpt.tpe) &&
              calls.forall((call, scope) =>
                argumentLists(call).lift(index).exists(shrunk(sizes, _, parameter.symbol, scope))
              ) => Recursion.Bounded(sizeOf(parameter.symbol))
      }.orElse(counted(definition, calls, sizes)).getOrElse(Recursion.Unbounded)
    }

  /** Recursion on an integer parameter that steps down towards zero, or steps up towards a size it is compared to. */
  private def counted(
      definition: tpd.DefDef,
      calls: Vector[(tpd.Apply, Scope)],
      sizes: Sizes
  ): Option[Recursion] =
    definition.termParamss.flatten.zipWithIndex.collectFirst {
      case (parameter, index) if parameter.tpt.tpe.widen.derivesFrom(defn.IntClass) =>
        def all(operator: String): Boolean =
          calls.forall((call, _) => argumentLists(call).lift(index).exists(sizes.steps(_, parameter.symbol, operator)))
        if (all("-")) Some(Recursion.Bounded(sizeOf(parameter.symbol)))
        else if (all("+")) sizes.limit(definition.rhs, parameter.symbol, entry(definition)).map(Recursion.Bounded(_))
        else None
    }.flatten

  @annotation.tailrec
  private def shrunk(sizes: Sizes, argument: tpd.Tree, parameter: Symbol, scope: Scope): Boolean =
    sizes.peeled(argument) match {
      case ident: tpd.Ident        => scope.smaller.get(ident.symbol).contains(parameter)
      case tpd.TypeApply(inner, _) => shrunk(sizes, inner, parameter, scope)
      case select: tpd.Select      =>
        Set("tail", "init")(select.name.toString) &&
        (select.qualifier.symbol == parameter || shrunk(sizes, select.qualifier, parameter, scope))
      case application: tpd.Apply =>
        Traversals.selected(application.fun) match {
          case Some(select) if Set("drop", "dropRight")(select.name.toString) =>
            application.args.headOption.flatMap(sizes.literal(_)).exists(_ > 0) &&
            (select.qualifier.symbol == parameter || shrunk(sizes, select.qualifier, parameter, scope))
          case _ => false
        }
      case _ => false
    }

  /** The most calls of `method` to itself that one run of `tree` makes, counting a call in a function as many. */
  private def depth(tree: tpd.Tree, method: Symbol): Int = tree match {
    case tpd.If(condition, thenp, elsep) =>
      depth(condition, method) + depth(thenp, method).max(depth(elsep, method))
    case tpd.Match(selector, cases) =>
      depth(selector, method) +
        cases.map(branch => depth(branch.guard, method) + depth(branch.body, method)).maxOption.getOrElse(0)
    case definition: tpd.DefDef =>
      if (definition.symbol.isAnonymousFunction && depth(definition.rhs, method) > 0) 2 else 0
    case application: tpd.Apply if application.fun.symbol == method =>
      1 + subtrees.of(tree).map(depth(_, method)).sum
    case _ => subtrees.of(tree).map(depth(_, method)).sum
  }

  /** The branches in tail position that end the recursion, which run once rather than in every frame. */
  private def exitsOf(tree: tpd.Tree, method: Symbol): List[tpd.Tree] = {
    def split(branches: List[tpd.Tree]): List[tpd.Tree] =
      if (branches.exists(depth(_, method) > 0))
        branches.flatMap(branch => if (depth(branch, method) > 0) exitsOf(branch, method) else List(branch))
      else Nil
    tree match {
      case tpd.Block(statements, expression) if statements.forall(depth(_, method) == 0) =>
        exitsOf(expression, method)
      case tpd.If(_, thenp, elsep) => split(List(thenp, elsep))
      case tpd.Match(_, cases)     => split(cases.map(_.body))
      case _                       => Nil
    }
  }

  private def entry(definition: tpd.DefDef): Scope = {
    val parameters = definition.termParamss.flatten
    Scope(
      sizes = parameters.filter(parameter => library.sized(parameter.tpt.tpe)).map(parameter =>
        parameter.symbol -> sizeOf(parameter.symbol)
      ).toMap,
      paths = parameters.map(parameter => parameter.symbol -> name(parameter.symbol)).toMap
    )
  }

  private class Walk(current: Symbol, callee: Symbol => Bound) {
    val charges: mutable.ArrayBuffer[Charge]                = mutable.ArrayBuffer.empty
    val recursions: mutable.ArrayBuffer[(tpd.Apply, Scope)] = mutable.ArrayBuffer.empty
    private val sizes                                       = sizesOf(current)

    def run(definition: tpd.DefDef): Unit = {
      exits ++= exitsOf(definition.rhs, current)
      visit(definition.rhs, entry(definition))
    }

    private def label(tree: tpd.Tree, operation: String, count: Option[Polynomial]): String =
      s"$operation over ${count.fold("?")(Atoms.show)} (line ${line(tree)})"

    // a branch taken when two values are equal runs for few of the turns of the loops around it
    private def guarded(condition: tpd.Tree, scope: Scope): Scope =
      if (scope.loops.nonEmpty && sizes.conjuncts(condition).exists(sizes.equality(_)))
        scope.pushed(Scope.Loop(Polynomial.One, "", absorbs = 1, hidden = true))
      else scope

    // a loop that grows a table runs once per doubling, so it costs a constant share of each operation
    private def amortizing(condition: tpd.Tree, scope: Scope): Scope =
      if (sizes.conjuncts(condition).exists(sizes.occupancy(_))) scope.copy(amortized = Some(scope.loops.size))
      else scope

    private def entered(scope: Scope, body: tpd.Tree): Scope = scope.copy(resets = scope.resets ++ sizes.reset(body))

    private def charge(
        tree: tpd.Tree,
        what: String,
        time: Polynomial,
        space: Polynomial,
        linear: Boolean,
        scope: Scope
    ): Unit = charge(tree, what, time, space, linear, scope, scope)

    private def charge(
        tree: tpd.Tree,
        what: String,
        time: Polynomial,
        space: Polynomial,
        linear: Boolean,
        scope: Scope,
        counted: Scope
    ): Unit = {
      val factor    = counted.factor
      val causeWhat = if (scope.inFallback) s"$what (in orElse fallback)" else what
      charges += Charge(
        causeWhat,
        scope.loops.findLast(!_.hidden).map(_.label),
        factor * time,
        factor * space,
        linear,
        scope.callback,
        scope.terminal,
        line(tree)
      )
    }

    private val exits = mutable.Set.empty[tpd.Tree]

    private def visit(tree: tpd.Tree, scope: Scope): Unit =
      visited(tree, if (exits(tree)) scope.copy(terminal = true) else scope)

    private def visited(tree: tpd.Tree, scope: Scope): Unit = tree match {
      case tpd.Block(statements, expression)   => visit(expression, block(statements, expression, scope))
      case tpd.Inlined(_, bindings, expansion) => visit(expansion, block(bindings, expansion, scope))
      case tpd.WhileDo(condition, body)        =>
        val (trips, driver) = sizes.trips(condition, body, scope)
        val what            = s"while loop over ${Atoms.show(trips)} (line ${line(tree)})"
        val absorbed        =
          driver.filterNot(scope.resets).fold(0)(symbol => scope.loops.size - scope.levels.getOrElse(symbol, 0))
        val inner = entered(scope.pushed(Scope.Loop(trips, what, absorbed.max(0))), body)
        charge(tree, what, Polynomial.One, Polynomial.Zero, linear = false, scope, inner)
        visit(condition, inner)
        visit(body, inner)
      case tpd.If(condition, thenp, elsep) =>
        visit(condition, scope)
        visit(thenp, amortizing(condition, guarded(condition, scope)))
        visit(elsep, scope)
      case tpd.Match(selector, cases) =>
        visit(selector, scope)
        cases.foreach { branch =>
          val inner = matched(selector, branch.pat, scope)
          visit(branch.guard, inner)
          visit(branch.body, inner)
        }
      case application: tpd.Apply => applied(application, scope)
      case select: tpd.Select     =>
        visit(select.qualifier, scope)
        if (library.traversal(select.qualifier.tpe))
          operation(select, select.name.toString, select.qualifier, Nil, scope)
      case tpd.DefDef(_, _, _, _) => ()
      case _                      => subtrees.of(tree).foreach(visit(_, scope))
    }

    @annotation.tailrec
    private def block(statements: List[tpd.Tree], expression: tpd.Tree, scope: Scope): Scope = statements match {
      case Nil                         => scope
      case (value: tpd.ValDef) :: rest =>
        visit(value.rhs, scope)
        block(
          rest,
          expression,
          bind(value, expression :: rest, scope.copy(levels = scope.levels.updated(value.symbol, scope.loops.size)))
        )
      case tpd.DefDef(_, _, _, _) :: rest => block(rest, expression, scope)
      case statement :: rest              =>
        visit(statement, scope)
        block(rest, expression, scope)
    }

    private def bind(value: tpd.ValDef, rest: List[tpd.Tree], scope: Scope): Scope = {
      val symbol     = value.symbol
      val collection = library.sized(value.tpt.tpe) || library.text(value.tpt.tpe)
      val size       = Option.when(collection)(sizes.size(value.rhs, scope)).flatten
      val counts     =
        if (collection) scope.counts
        else sizes.lengthOf(value.rhs, scope).fold(scope.counts)(count => scope.counts.updated(symbol, count))
      if (!symbol.is(Flags.Mutable))
        scope.copy(
          sizes = size.fold(scope.sizes)(scope.sizes.updated(symbol, _)),
          paths = sizes.path(value.rhs, scope).fold(scope.paths)(scope.paths.updated(symbol, _)),
          counts = counts
        )
      else if (collection)
        scope.copy(variables =
          scope.variables.updated(
            symbol,
            Scope.Variable(
              scope.loops.size,
              size,
              sizes.shrinks(symbol, rest),
              sizes.growth(symbol, assignedTo(symbol, rest), scope)
            )
          )
        )
      else scope.copy(counts = counts)
    }

    private def assignedTo(symbol: Symbol, trees: List[tpd.Tree]): List[tpd.Tree] =
      trees.flatMap { tree =>
        (tree match {
          case tpd.Assign(target, value) if target.symbol == symbol => List(value)
          case _                                                    => Nil
        }) ++ assignedTo(symbol, subtrees.of(tree))
      }

    @annotation.tailrec
    private def matched(selector: tpd.Tree, pattern: tpd.Tree, scope: Scope): Scope = pattern match {
      case tpd.Bind(_, inner)  => matched(selector, inner, scope)
      case tpd.Typed(inner, _) => matched(selector, inner, scope)
      case tpd.UnApply(function, _, List(_, tail @ tpd.Bind(_, _)))
          if Cons.exists(function.symbol.owner.name.toString.startsWith(_)) =>
        val subject = sizes.peeled(selector).symbol
        scope.copy(
          smaller = scope.smaller.updated(tail.symbol, subject),
          sizes = scope.sizes.get(subject).fold(scope.sizes)(scope.sizes.updated(tail.symbol, _))
        )
      case _ => scope
    }

    private def applied(application: tpd.Apply, scope: Scope): Unit = {
      val method    = application.fun.symbol
      val select    = Traversals.selected(application.fun)
      val arguments = argumentLists(application)
      val functions = arguments.flatMap(Traversals.lambda(_))
      select.foreach(selected => visit(selected.qualifier, scope))
      val isFallback = Set("orElse", "getOrElse")(method.name.toString)
      val argScope   = if (isFallback) scope.copy(inFallback = true) else scope
      val plain      = arguments.filter(Traversals.lambda(_).isEmpty)
      plain match {
        case List(condition, body) if Guards(method.fullName.toString) =>
          visit(condition, scope)
          visit(body, guarded(condition, scope))
        case _ => plain.foreach(visit(_, argScope))
      }
      if (method == current) recursions += ((application, scope))
      else if (definitions.contains(method)) {
        functions.foreach(function => visit(function.rhs, argScope))
        call(application, method, select, arguments, scope)
      } else
        select match {
          case Some(selected) if library.traversal(selected.qualifier.tpe) =>
            traversed(application, selected, arguments, functions, scope)
          case _ =>
            functions.foreach(function => visit(function.rhs, argScope))
            select.filter(selected => library.text(selected.qualifier.tpe)).foreach(selected =>
              operation(application, selected.name.toString, selected.qualifier, arguments, scope)
            )
            external(application, method, arguments, scope)
        }
    }

    private def call(
        application: tpd.Apply,
        method: Symbol,
        select: Option[tpd.Select],
        arguments: List[tpd.Tree],
        scope: Scope
    ): Unit = {
      val parameters = method.paramSymss.filter(_.headOption.exists(_.isTerm)).flatten
      val receiver   = select.fold(Some("this"))(selected => sizes.path(selected.qualifier, scope))
      val bindings   = parameters.zip(arguments).map { (parameter, argument) =>
        name(parameter) -> Atoms.Binding(
          sizes.path(argument, scope),
          sizes.size(argument, scope).orElse(sizes.lengthOf(argument, scope)),
          sizes.static(argument)
        )
      }.toMap + ("this" -> Atoms.Binding(receiver, None, select.exists(selected => sizes.static(selected.qualifier))))
      val inherited    = callee(method)
      val time         = Atoms.rebind(inherited.time, bindings)
      val space        = Atoms.rebind(inherited.space, bindings)
      val nonPrimitive = !method.info.finalResultType.widen.derivesFrom(defn.UnitClass) &&
        !method.info.finalResultType.widen.isPrimitiveValueType
      val retainedResult =
        nonPrimitive && scope.loops.nonEmpty &&
          scope.enclosingResults.exists(enclosing => isRetained(application, enclosing))
      if (!retainedResult)
        charge(
          application,
          s"call of ${method.name} (line ${line(application)})",
          time,
          space,
          Atoms.terms(time).exists(_.degree == 1),
          scope
        )
    }

    private def isRetained(target: tpd.Tree, in: tpd.Tree): Boolean =
      if (in eq target) true
      else in match {
        case app: tpd.Apply =>
          val name = app.fun.symbol.name.toString
          if (
            Searches(name) ||
            Set("count", "head", "last", "size", "length", "isEmpty", "nonEmpty", "exists", "forall", "find")(name)
          )
            false
          else
            subtrees.of(in).exists(isRetained(target, _))
        case _ =>
          subtrees.of(in).exists(isRetained(target, _))
      }

    private def traversed(
        application: tpd.Apply,
        select: tpd.Select,
        arguments: List[tpd.Tree],
        functions: List[tpd.DefDef],
        scope: Scope
    ): Unit = {
      val name = select.name.toString
      operation(application, name, select.qualifier, arguments, scope)
      if (functions.nonEmpty) {
        val count =
          if (name == "takeWhile" && sizes.seeked(select.qualifier)) Some(Polynomial.One)
          else sizes.size(select.qualifier, scope)
        val extent     = count.getOrElse(Atoms.Unknown)
        val what       = label(application, name, count)
        val collecting = Set("map", "flatMap", "scanLeft")(name) &&
          (library.sized(application.tpe) || library.traversal(application.tpe))
        val retainedResult = collecting &&
          scope.loops.nonEmpty && scope.enclosingResults.exists(enclosing => isRetained(application, enclosing))
        if (!retainedResult && !functions.exists(dropsOne(name, _)))
          charge(application, what, extent, Polynomial.Zero, linear = false, scope)
        val inner         = scope.pushed(Scope.Loop(extent, what)).copy(callback = true)
        val collectsOuter = Set("map", "flatMap", "foldLeft", "scanLeft", "collect", "foreach")(name)
        functions.foreach { function =>
          val innerWithResult =
            if (collectsOuter) inner.copy(enclosingResults = function.rhs :: inner.enclosingResults)
            else inner
          visit(function.rhs, accumulating(name, function, arguments, scope, entered(innerWithResult, function.rhs)))
        }
      }
    }

    /** A filter that drops only the elements equal to a value bound outside it keeps all but one per turn of the loop
      * around it, so its output is as large as its work.
      */
    private def dropsOne(name: String, function: tpd.DefDef): Boolean =
      Set("filter", "withFilter")(name) && function.termParamss.flatten.headOption.exists { parameter =>
        sizes.peeled(function.rhs) match {
          case tpd.Apply(tpd.Select(left, operator), List(right)) if operator.toString == "!=" =>
            (isParameter(left, parameter.symbol) && outside(right)) ||
            (isParameter(right, parameter.symbol) && outside(left))
          case _ => false
        }
      }

    private def isParameter(tree: tpd.Tree, parameter: Symbol): Boolean = sizes.peeled(tree) match {
      case ident: tpd.Ident => ident.symbol == parameter
      case _                => false
    }

    private def outside(tree: tpd.Tree): Boolean = sizes.peeled(tree) match {
      case ident: tpd.Ident => ident.symbol.owner.isTerm
      case _                => false
    }

    private def accumulating(
        name: String,
        function: tpd.DefDef,
        arguments: List[tpd.Tree],
        outer: Scope,
        inner: Scope
    ): Scope = {
      val parameters  = function.termParamss.flatten
      val accumulator =
        if (Folds(name)) parameters.headOption
        else if (name == "foldRight" || name == "reduceRight") parameters.lift(1)
        else None
      accumulator.filter(parameter => library.sized(parameter.tpt.tpe) || library.text(parameter.tpt.tpe))
        .fold(inner) { parameter =>
          val initial = arguments.headOption.flatMap(sizes.size(_, outer))
          inner.copy(variables =
            inner.variables.updated(
              parameter.symbol,
              Scope.Variable(
                outer.loops.size,
                initial,
                shrinking = false,
                sizes.growth(parameter.symbol, List(function.rhs), outer)
              )
            )
          )
        }
    }

    private def external(application: tpd.Apply, method: Symbol, arguments: List[tpd.Tree], scope: Scope): Unit =
      if (Compilers(method.fullName.toString)) {
        val count = arguments.headOption.flatMap(sizes.size(_, scope))
        charge(
          application,
          label(application, "regex compilation", count),
          count.getOrElse(Atoms.Unknown),
          Polynomial.Zero,
          linear = true,
          scope
        )
      } else if (method.owner.name.toString == "StringContext" && Set("s", "f", "raw")(method.name.toString))
        arguments.flatMap {
          case tpd.Typed(tpd.SeqLiteral(elements, _), _) => elements
          case other                                     => List(other)
        }.foreach(grown(application, "interpolation", _, scope))

    private def grown(tree: tpd.Tree, name: String, receiver: tpd.Tree, scope: Scope): Unit =
      if (library.text(receiver.tpe) && scope.variables.contains(sizes.peeled(receiver).symbol)) {
        val count  = sizes.size(receiver, scope)
        val extent = count.getOrElse(Atoms.Unknown)
        charge(tree, label(tree, name, count), extent, extent, linear = true, scope)
      }

    private def operation(
        tree: tpd.Tree,
        name: String,
        receiver: tpd.Tree,
        arguments: List[tpd.Tree],
        scope: Scope
    ): Unit = {
      val tpe                                                                        = receiver.tpe
      val count                                                                      = sizes.size(receiver, scope)
      val extent                                                                     = count.getOrElse(Atoms.Unknown)
      val linear                                                                     = count.exists(Atoms.degree(_) > 0)
      def emit(time: Polynomial, space: Polynomial, flagged: Boolean = linear): Unit =
        charge(tree, label(tree, name, count), time, space, flagged, scope)
      if ((Walks(name) && library.linked(tpe)) || (name == "apply" && library.listBuffer(tpe)))
        emit(extent, Polynomial.Zero)
      else if (Sorts(name) && library.traversal(tpe)) emit(extent * Atoms.log(extent), extent)
      else if (Searches(name) && library.searchable(tpe)) emit(extent, Polynomial.Zero)
      else if (Shifts(name) && library.arrayBuffer(tpe) && front(name, arguments)) emit(extent, extent)
      else if (library.linked(tpe) && Appends(name)) emit(extent, extent)
      else if (library.linked(tpe) && Prepends(name))
        arguments.headOption.map(sizes.size(_, scope).getOrElse(Atoms.Unknown)).foreach(copied => emit(copied, copied))
      else if (library.array(tpe) && (Appends(name) || Prepends(name) || Set("+:", "prepended")(name)))
        emit(extent, extent)
      else if (library.text(tpe) && Set("+", "concat")(name)) grown(tree, name, receiver, scope)
      else if (name == "r" && library.text(tpe))
        charge(tree, label(tree, "regex compilation", count), extent, Polynomial.Zero, linear = true, scope)
      else if (Aggregates(name) && library.traversal(tpe) && !library.unchanged(name, tpe))
        emit(extent, if (Copies(name)) extent else Polynomial.Zero)
    }

    private def front(name: String, arguments: List[tpd.Tree]): Boolean =
      !Set("remove", "insert", "insertAll")(name) || arguments.headOption.flatMap(sizes.literal(_)).contains(0)
  }
}
