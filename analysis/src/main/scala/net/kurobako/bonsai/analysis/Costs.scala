package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import net.kurobako.bonsai.collections.Methods
import net.kurobako.bonsai.lifting.Calls
import scala.collection.mutable

/** Upper bounds on the time and allocation of build methods, as polynomials in the sizes of collections.
  *
  * Every operation that costs something is a charge, multiplied by the turns of the loops around it. A traversal runs
  * its function once per element of its receiver, so it is a loop of that many turns, and a call costs its callee's
  * bound with the callee's parameters and `this` rebound to the arguments and receiver. Loops of unknown length,
  * recursion that does not shrink a collection and sizes that cannot be followed contribute the atom `?`, and terms
  * containing it are never reported. A collection the build declares bounded has a constant size, so work on it or per
  * element of it adds no factor.
  *
  * Time counts every call and traversal wherever its result goes. Space counts the copies an operation makes of what it
  * works on, and only for as long as they are kept: a copy that the method returns, stores or accumulates is live
  * across the turns of the loops it is kept over, and one that only reaches a call that holds nothing is the space of a
  * single turn. A caller inherits the space its callee returns or stores.
  */
private class Costs(
    definitions: collection.Map[Symbol, tpd.DefDef],
    initialisers: collection.Map[Symbol, tpd.Tree],
    declared: Complexity.Declared,
    java: JavaCosts
)(using Context) {
  import Charges.*

  private val library  = Library()
  private val memo     = new Memo[Symbol]
  private val subtrees = Children()
  private val flows    =
    Flows(library, definitions, method => definitions.contains(method) || known(method).isDefined, subtrees)
  private val fixed    = mutable.Map.empty[Symbol, Option[Polynomial]]
  private val sized    = mutable.Map.empty[Symbol, Option[Polynomial]]
  private val searched = mutable.Map.empty[Symbol, Boolean]
  private val keyed    = mutable.Map.empty[Symbol, Option[String]]

  private lazy val definedKeys: Map[String, Symbol] =
    definitions.keys.flatMap(method =>
      Option.when(method.owner.isClass)(method).flatMap(Keys.of).map(_ -> method)
    ).toMap

  private val resolve: JavaCosts.Resolver = key =>
    definedKeys.get(key).map(method =>
      Callee(bound(method), definitions(method).termParamss.flatten.map(parameter => name(parameter.symbol)))
    )

  /** What is known of a method the build holds no body of, from the trees of the Java code. */
  private def known(method: Symbol): Option[Callee] =
    if (!java.nonEmpty || definitions.contains(method) || !method.is(Flags.Method)) None
    else keyed.getOrElseUpdate(method, Keys.of(method)).flatMap(key => java.callee(key, resolve))

  /** The summaries of the methods of the Java code, bound against the methods of the build. */
  def javaSummaries: Vector[Complexity.Summary] = java.summaries(resolve)

  private def sizesOf(method: Symbol): Sizes = Sizes(library, method, initialisers, fixed, declaredSize, searching)

  /** A sorted search, or a method whose body makes one, locates the element a scan starts from. */
  private def searching(method: Symbol): Boolean = searched.getOrElseUpdate(
    method,
    searchCall(method) || definitions.get(method).exists(definition =>
      sizesOf(method).exists(definition.rhs) {
        case application: tpd.Apply => searchCall(application.fun.symbol)
        case _                      => false
      }
    )
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
      sized.getOrElseUpdate(
        member, {
          val owner = Calls.qualified(member.owner)
          declared.size(s"$owner.${member.name}", owner)
        }
      )

  private val Searches   = Methods.Searches
  private val Peels      = Set("tail", "init")
  private val Drops      = Set("drop", "dropRight")
  private val Sorts      = Methods.Sorts
  private val Copies     = Methods.Copies
  private val Aggregates = Copies ++ Set("sum", "product", "max", "min", "mkString")
  private val Shifts     = Set("remove", "insert", "insertAll", "prepend", "prependAll", "+=:", "++=:")
  private val Prepends   = Set(":::", "++:", "prependedAll")
  private val Folds      = Set("foldLeft", "fold", "scanLeft", "reduceLeft", "reduce", "aggregate")
  private val Guards     = Set("scala.Option$.when", "scala.Option$.unless")
  private val Cons       = Set("::", "+:")
  private val Compilers  = Set("scala.util.matching.Regex.<init>", "java.util.regex.Pattern$.compile")

  /** The time and allocation hits of `method`, from its own body and its callees' linear costs. */
  def summary(method: Symbol): Option[Complexity.Bounds] = definitions.get(method).map { definition =>
    val walk = Walk(method, callee => bound(callee).linear)
    walk.run(definition)
    Charges.bounds(recursion(method, definition, walk.recursions.toVector), walk.charges.toVector)
  }.filter(_.nonEmpty)

  /** The bound of `method` in the sizes of its own parameters; zero outside the build. */
  private def bound(method: Symbol): Bound = memo(method) {
    definitions.get(method).fold(known(method).fold(Bound.Zero)(_.bound))(summarise(method, _))
  }

  private def summarise(method: Symbol, definition: tpd.DefDef): Bound = {
    val walk = Walk(method, bound(_))
    walk.run(definition)
    total(recursion(method, definition, walk.recursions.toVector), walk.charges)
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
        Peels(select.name.toString) &&
        (select.qualifier.symbol == parameter || shrunk(sizes, select.qualifier, parameter, scope))
      case application: tpd.Apply =>
        Calls.selected(application.fun) match {
          case Some(select) if Drops(select.name.toString) =>
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
    val bound = definition.termParamss.flatten.flatMap(parameter =>
      (parameter.symbol :: accessors(definition.symbol, parameter.symbol)).map(_ -> parameter)
    )
    Scope(
      sizes = bound.collect {
        case (symbol, parameter) if library.sized(parameter.tpt.tpe) => symbol -> sizeOf(parameter.symbol)
      }.toMap,
      paths = bound.map((symbol, parameter) => symbol -> name(parameter.symbol)).toMap
    )
  }

  /** The members of a class that a primary constructor parameter is stored in, which the class body reads it by. */
  private def accessors(constructor: Symbol, parameter: Symbol): List[Symbol] =
    if (!constructor.isPrimaryConstructor) Nil
    else
      constructor.owner.info.decls.toList.filter(member =>
        member.isTerm && member.is(Flags.ParamAccessor) && member.name == parameter.name
      )

  private class Walk(current: Symbol, callee: Symbol => Bound) {
    val charges: mutable.ArrayBuffer[Charge]                = mutable.ArrayBuffer.empty
    val recursions: mutable.ArrayBuffer[(tpd.Apply, Scope)] = mutable.ArrayBuffer.empty
    private val sizes                                       = sizesOf(current)
    private val flow                                        = flows.of(current)

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

    private def charge(tree: tpd.Tree, work: Work, scope: Scope, counted: Scope): Unit = {
      val cause = if (scope.inFallback) work.copy(what = s"${work.what} (in orElse fallback)") else work
      charges += Charge.at(scope, counted, cause, line(tree), flow.of(tree))
    }

    private val exits = mutable.Set.empty[tpd.Tree]

    private def visit(tree: tpd.Tree, scope: Scope): Unit =
      visited(tree, if (exits(tree)) scope.copy(terminal = true) else scope)

    private def visited(tree: tpd.Tree, scope: Scope): Unit = tree match {
      case tpd.Block(statements, expression)   => visit(expression, block(statements, expression, scope))
      case tpd.Inlined(_, bindings, expansion) => visit(expansion, block(bindings, expansion, scope))
      case tpd.WhileDo(condition, body)        =>
        val (trips, driver) = sizes.trips(condition, body, scope)
        val what            = label(tree, "while loop", Some(trips))
        val absorbed        =
          driver.filterNot(scope.resets).fold(0)(symbol => scope.loops.size - scope.levels.getOrElse(symbol, 0))
        val inner = entered(scope.pushed(Scope.Loop(trips, what, absorbed.max(0))), body)
        charge(tree, Work(what, Polynomial.One, Polynomial.Zero, linear = false), scope, inner)
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
        else if (select.symbol.is(Flags.Method)) nullary(select, select.symbol, Some(select), scope)
      case ident: tpd.Ident if ident.symbol.is(Flags.Method) => nullary(ident, ident.symbol, None, scope)
      case tpd.DefDef(_, _, _, _)                            => ()
      case _                                                 => subtrees.of(tree).foreach(visit(_, scope))
    }

    private def block(statements: List[tpd.Tree], expression: tpd.Tree, scope: Scope): Scope =
      block(statements, expression, scope, Assigned(expression :: statements))

    @annotation.tailrec
    private def block(statements: List[tpd.Tree], expression: tpd.Tree, scope: Scope, assigned: Assigned): Scope =
      statements match {
        case Nil                         => scope
        case (value: tpd.ValDef) :: rest =>
          visit(value.rhs, scope)
          block(
            rest,
            expression,
            bind(value, assigned, scope.copy(levels = scope.levels.updated(value.symbol, scope.loops.size))),
            assigned
          )
        case tpd.DefDef(_, _, _, _) :: rest => block(rest, expression, scope, assigned)
        case statement :: rest              =>
          visit(statement, scope)
          block(rest, expression, scope, assigned)
      }

    private def bind(value: tpd.ValDef, assigned: Assigned, scope: Scope): Scope = {
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
          counts = counts,
          built = sizes.construction(value.rhs, scope).fold(scope.built)(scope.built.updated(symbol, _))
        )
      else if (collection)
        scope.tracking(
          symbol,
          Scope.Variable(
            scope.loops.size,
            size,
            sizes.shrinks(symbol, assigned.to(symbol)),
            sizes.growth(symbol, assigned.to(symbol), scope)
          )
        )
      else scope.copy(counts = counts)
    }

    /** The values assigned to each variable within `trees`, indexed once and only when a variable asks. */
    private class Assigned(trees: List[tpd.Tree]) {
      private lazy val values = assignments(trees).groupMap(_._1)(_._2)

      def to(symbol: Symbol): List[tpd.Tree] = values.getOrElse(symbol, Nil)
    }

    private def assignments(trees: List[tpd.Tree]): List[(Symbol, tpd.Tree)] =
      trees.flatMap { tree =>
        (tree match {
          case tpd.Assign(target, value) => List(target.symbol -> value)
          case _                         => Nil
        }) ++ assignments(subtrees.of(tree))
      }

    @annotation.tailrec
    private def matched(selector: tpd.Tree, pattern: tpd.Tree, scope: Scope): Scope = pattern match {
      case tpd.Bind(_, inner)  => matched(selector, inner, scope)
      case tpd.Typed(inner, _) => matched(selector, inner, scope)
      case tpd.UnApply(function, _, List(_, tail @ tpd.Bind(_, _)))
          if Cons.exists(Calls.owner(function.symbol).name.toString.startsWith(_)) =>
        val subject = sizes.peeled(selector).symbol
        scope.copy(
          smaller = scope.smaller.updated(tail.symbol, subject),
          sizes = scope.sizes.get(subject).fold(scope.sizes)(scope.sizes.updated(tail.symbol, _))
        )
      case _ => scope
    }

    private def applied(application: tpd.Apply, scope: Scope): Unit = {
      val method    = application.fun.symbol
      val select    = Calls.selected(application.fun)
      val arguments = argumentLists(application)
      val functions = arguments.flatMap(Calls.lambda(_))
      select.foreach(selected => visit(selected.qualifier, scope))
      val isFallback = Set("orElse", "getOrElse")(method.name.toString)
      val argScope   = if (isFallback) scope.copy(inFallback = true) else scope
      val plain      = arguments.filter(Calls.lambda(_).isEmpty)
      plain match {
        case List(condition, body) if Guards(method.fullName.toString) =>
          visit(condition, scope)
          visit(body, guarded(condition, scope))
        case _ => plain.foreach(visit(_, argScope))
      }
      if (method == current) recursions += ((application, scope))
      else if (definitions.contains(method) || known(method).isDefined) {
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

    private def nullary(reference: tpd.Tree, method: Symbol, select: Option[tpd.Select], scope: Scope): Unit =
      if (definitions.contains(method) || known(method).isDefined) call(reference, method, select, Nil, scope)

    private def call(
        application: tpd.Tree,
        method: Symbol,
        select: Option[tpd.Select],
        arguments: List[tpd.Tree],
        scope: Scope
    ): Unit = {
      val roots = known(method).fold(
        Shapes.termClauses(method).flatten.map(name)
      )(_.roots)
      val receiver = select.fold(Some("this"))(selected => sizes.path(selected.qualifier, scope))
      val bindings = roots.zip(arguments).map { (root, argument) =>
        root -> Atoms.Binding(
          sizes.path(argument, scope),
          sizes.size(argument, scope).orElse(sizes.lengthOf(argument, scope)),
          sizes.static(argument)
        )
      }.toMap + ("this" -> Atoms.Binding(receiver, None, select.exists(selected => sizes.static(selected.qualifier))))
      val inherited = callee(method)
      val time      = Atoms.rebind(inherited.time, bindings)
      val space     = Atoms.rebind(inherited.space, bindings)
      charge(
        application,
        Work(s"call of ${method.name} (line ${line(application)})", time, space, linearIn(time)),
        scope,
        scope
      )
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
        val extent = count.getOrElse(Atoms.Unknown)
        val what   = label(application, name, count)
        if (!functions.exists(dropsOne(name, _)))
          charge(application, Work(what, extent, Polynomial.Zero, linear = false), scope, scope)
        val inner = scope.pushed(Scope.Loop(extent, what)).copy(callback = true)
        functions.foreach(function =>
          visit(function.rhs, accumulating(name, function, arguments, scope, entered(inner, function.rhs)))
        )
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
          inner.tracking(
            parameter.symbol,
            Scope.Variable(
              outer.loops.size,
              initial,
              shrinking = false,
              sizes.growth(parameter.symbol, List(function.rhs), outer)
            )
          )
        }
    }

    private def external(application: tpd.Apply, method: Symbol, arguments: List[tpd.Tree], scope: Scope): Unit =
      if (Compilers(method.fullName.toString)) {
        val count = arguments.headOption.flatMap(sizes.size(_, scope))
        charge(
          application,
          Work(
            label(application, "regex compilation", count),
            count.getOrElse(Atoms.Unknown),
            Polynomial.Zero,
            linear = true
          ),
          scope,
          scope
        )
      } else if (Calls.owner(method).name.toString == "StringContext" && Set("s", "f", "raw")(method.name.toString))
        arguments.flatMap {
          case tpd.Typed(tpd.SeqLiteral(elements, _), _) => elements
          case other                                     => List(other)
        }.foreach(grown(application, "interpolation", _, scope))

    private def grown(tree: tpd.Tree, name: String, receiver: tpd.Tree, scope: Scope): Unit =
      if (library.text(receiver.tpe) && scope.variables.contains(sizes.peeled(receiver).symbol)) {
        val count  = sizes.size(receiver, scope)
        val extent = count.getOrElse(Atoms.Unknown)
        charge(tree, Work(label(tree, name, count), extent, extent, linear = true), scope, scope)
      }

    private def operation(
        tree: tpd.Tree,
        name: String,
        receiver: tpd.Tree,
        arguments: List[tpd.Tree],
        scope: Scope
    ): Unit = {
      val tpe                                           = receiver.tpe
      val count                                         = sizes.size(receiver, scope)
      val extent                                        = count.getOrElse(Atoms.Unknown)
      val linear                                        = growsWith(count)
      def emit(kind: Operation, size: Polynomial): Unit =
        charge(tree, kind.work(label(tree, name, count), size, linear), scope, scope)
      if ((Methods.Walks(name) && library.linked(tpe)) || (name == "apply" && library.listBuffer(tpe)))
        emit(Operation.Search, extent)
      else if (Sorts(name) && library.traversal(tpe)) emit(Operation.Sort, extent)
      else if (Searches(name) && library.searchable(tpe)) emit(Operation.Search, extent)
      else if (
        (Shifts(name) && library.arrayBuffer(tpe) && front(name, arguments)) ||
        (library.linked(tpe) && Methods.Appends(name))
      ) emit(Operation.Copy, extent)
      else if (library.linked(tpe) && Prepends(name))
        arguments.headOption
          .map(sizes.size(_, scope).getOrElse(Atoms.Unknown))
          .foreach(copied => emit(Operation.Copy, copied))
      else if (library.array(tpe) && (Methods.Appends(name) || Prepends(name) || Set("+:", "prepended")(name)))
        emit(Operation.Copy, extent)
      else if (library.text(tpe) && Set("+", "concat")(name)) grown(tree, name, receiver, scope)
      else if (name == "r" && library.text(tpe))
        charge(
          tree,
          Work(label(tree, "regex compilation", count), extent, Polynomial.Zero, linear = true),
          scope,
          scope
        )
      else if (Aggregates(name) && library.traversal(tpe) && !library.unchanged(name, tpe))
        emit(if (Copies(name)) Operation.Copy else Operation.Search, extent)
    }

    private def front(name: String, arguments: List[tpd.Tree]): Boolean =
      !Set("remove", "insert", "insertAll")(name) || arguments.headOption.flatMap(sizes.literal(_)).contains(0)
  }
}
