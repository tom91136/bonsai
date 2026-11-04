package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameKinds.DefaultGetterName
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.AppliedType
import dotty.tools.dotc.core.Types.MethodType
import dotty.tools.dotc.core.Types.MethodicType
import dotty.tools.dotc.core.Types.NoPrefix
import dotty.tools.dotc.core.Types.PolyType
import dotty.tools.dotc.core.Types.TermRef
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.util.Spans.Span
import net.kurobako.bonsai.collections.*
import net.kurobako.bonsai.effects.*
import net.kurobako.bonsai.lifting.*
import scala.collection.mutable

private object Loops {
  val Checks: Set[Check] = Memberships.Checks ++ Set(
    Check.AccumulatingConcat,
    Check.BoxingFold,
    Check.InvariantInLoop,
    Check.KeyedLookupInLoop,
    Check.LinearInLoop,
    Check.LiteralTableInDef,
    Check.LoopBoxing,
    Check.RegexInDef,
    Check.TupleDestructure
  )
}

/** Reports work repeated on every iteration of a loop: a `while` body, or a function passed to a collection's
  * traversal. Linear operations there multiply the loop's cost, strict transformation chains allocate their
  * intermediate collections each time, primitives passed to the generic parameters of collections box each time, work
  * that does not depend on the loop is redone, accumulators that copy on every append grow quadratically, and searches
  * by key rescan what a map already indexes. Regexes compiled from literals on every call are reported wherever they
  * are, and boxing folds in the classes the build marks hot. Boxed primitives in loops are reported only in those
  * classes when the build marks any.
  */
private class Loops(
    checks: Checks,
    hot: String => Boolean,
    hotDeclared: Boolean,
    cold: String => Boolean,
    declared: (String, String) => Boolean
)(using Context)
    extends tpd.TreeTraverser {
  // Whether each enclosing tree runs once per iteration of some loop, and the symbols the innermost loop binds afresh on
  // each iteration; a collection reached through them differs between iterations, so walking it is not repeated work.
  private val looping = mutable.Stack(false)
  private val bound   = mutable.Stack(mutable.Set.empty[Symbol])
  // A fold's accumulator carries one growing collection across iterations, so it is never bound afresh.
  private val carried = mutable.Set.empty[Symbol]
  // Variables a loop assigns and collections it updates, which differ between its iterations.
  private val written = mutable.Set.empty[Symbol]
  // Whether each enclosing tree runs only when needed, as an argument passed by name or a lazy value does, rather than
  // once per iteration; and the work already reported as invariant, inside which nothing is reported again.
  private val deferred = mutable.Stack(false)
  private val hoisted  = mutable.ListBuffer.empty[Span]
  // The lines of each enclosing loop, which unswitching it on a match copies once per case.
  private val extents = mutable.Stack(0)
  // The symbols the pattern of each enclosing case binds while its guard is traversed.
  private val guarded     = mutable.Stack(Set.empty[Symbol])
  private val guarding    = mutable.Stack(false)
  private val memberships = Memberships(checks)
  // The method whose recursion each enclosing loop is, the iterations of which are its calls.
  private val recursing = mutable.Stack(Option.empty[Symbol])
  // The extents of the branches of recursive methods that make no call of the method, which run once.
  private val leaves = mutable.ListBuffer.empty[Span]
  // The settled collection each enclosing loop iterates.
  private val iterated = mutable.Stack(Option.empty[Symbol])
  // The windows of a collection that grow with the innermost loop, by the value they are bound to.
  private val windows = mutable.Map.empty[Symbol, tpd.Tree]
  // The scans and windows already reported.
  private val scans = mutable.Set.empty[Span]
  // Constants a match or a branch returns, which make it the lookup, and operands of comparisons, which are not tables.
  private val exempt = mutable.Set.empty[Span]
  // Whether each inline method runs what each inline parameter receives more than once, false while it is decided.
  private val repeating = mutable.Map.empty[(Symbol, Symbol), Boolean]

  private val MaximumUnswitched = 12

  private val IterableOnce      = Classes.required("scala.collection.IterableOnce")
  private val IterableOnceOps   = Classes.required("scala.collection.IterableOnceOps")
  private val Iterable          = Classes.required("scala.collection.Iterable")
  private val Seq               = Classes.required("scala.collection.Seq")
  private val LinearSeq         = Classes.required("scala.collection.LinearSeq")
  private val ImmutableLinear   = Classes.required("scala.collection.immutable.LinearSeq")
  private val View              = Classes.required("scala.collection.View")
  private val LazyList          = Classes.required("scala.collection.immutable.LazyList")
  private val Range             = Classes.required("scala.collection.immutable.Range")
  private val NumericRange      = Classes.required("scala.collection.immutable.NumericRange")
  private val ArrayOps          = Classes.required("scala.collection.ArrayOps")
  private val StringOps         = Classes.required("scala.collection.StringOps")
  private val ImmutableIterable = Classes.required("scala.collection.immutable.Iterable")
  private val Growable          = Classes.required("scala.collection.mutable.Growable")
  private val ArraySeq          = Classes.required("scala.collection.immutable.ArraySeq")
  private val Map               = Classes.required("scala.collection.Map")
  private val IndexedSeq        = Classes.required("scala.collection.IndexedSeq")
  private val Iterator          = Classes.required("scala.collection.Iterator")

  private val LeftFolds  = Set("foldLeft", "scanLeft", "fold", "scan", "reduceLeft", "reduce")
  private val Stopping   = Set("collectFirst")
  private val RightFolds = Set("foldRight", "scanRight", "reduceRight")

  def run(tree: tpd.Tree): Unit = {
    Callers().traverse(tree)
    traverse(tree)
    memberships.report()
  }

  // The private and local methods of a unit, with the method each call sits in, or none for an initialiser.
  private val callers = mutable.Map.empty[Symbol, mutable.ListBuffer[Option[Symbol]]]
  // The methods of the unit with a body, so the writes of a call can follow the methods it reaches.
  private val definitions = mutable.Map.empty[Symbol, tpd.DefDef]
  // The methods that call themselves, with their definitions, and what the loop of their calls changes.
  private val recursive  = mutable.Map.empty[Symbol, tpd.DefDef]
  private val recursions = mutable.Map.empty[Symbol, Option[(Set[Symbol], List[Span])]]
  // Whether each private or local method runs once, false while it is decided.
  private val once = mutable.Map.empty[Symbol, Boolean]
  // The right-hand sides of the values that never change, which a bounded collection may be derived through.
  private val bindings = mutable.Map.empty[Symbol, tpd.Tree]
  // The bodies of the cases of functions that stop at the first case matching, which run at most once.
  private val stopped = mutable.Set.empty[Span]

  private class Callers extends tpd.TreeTraverser {
    // The definitions whose right-hand side is being traversed, and the right-hand sides to enter.
    private val open   = mutable.Map.empty[Symbol, tpd.DefDef]
    private val bodies = java.util.IdentityHashMap[tpd.Tree, tpd.DefDef]()

    override def traverse(tree: tpd.Tree)(using Context): Unit = {
      val entered = Option(bodies.remove(tree))
      entered.foreach(body => open(body.symbol) = body)
      tree match {
        case reference @ (_: tpd.Ident | _: tpd.Select) =>
          val symbol = reference.symbol
          if (symbol.is(Flags.Method) && confined(symbol))
            callers.getOrElseUpdate(symbol, mutable.ListBuffer.empty) += enclosing
          open.get(symbol).foreach(recursive(symbol) = _)
        case definition: tpd.DefDef if !definition.rhs.isEmpty =>
          definitions(definition.symbol) = definition
          bodies.put(definition.rhs, definition): Unit
        case value: tpd.ValDef if !value.rhs.isEmpty && !value.symbol.is(Flags.Mutable) =>
          bindings(value.symbol) = value.rhs
        case _ => ()
      }
      traverseChildren(tree)
      entered.foreach(body => open -= body.symbol)
    }
  }

  private def confined(method: Symbol)(using Context): Boolean = method.is(Flags.Private) || method.owner.isTerm

  private def references(tree: tpd.Tree, method: Symbol)(using Context): Boolean = tree.existsSubTree {
    case reference @ (_: tpd.Ident | _: tpd.Select) => reference.symbol == method
    case _                                          => false
  }

  override def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
    case definition: tpd.DefDef if definition.symbol.is(Flags.Synthetic) && !definition.symbol.isAnonymousFunction =>
      ()
    case definition: tpd.DefDef if recursive.contains(definition.symbol) =>
      written ++= writes(definition.rhs)
      recursion(definition) match {
        case Some((variants, branches)) =>
          leaves ++= branches
          within(mutable.Set.from(variants), definition, method = Some(definition.symbol))(traverse(definition.rhs))
        case None => traverseChildren(tree)
      }
    case tpd.WhileDo(condition, body) =>
      written ++= writes(tree)
      within(mutable.Set.empty, tree)(traverse(condition))
      within(mutable.Set.empty, tree)(traverse(body))
    case application @ tpd.Apply(function, arguments)
        if traversal(function) ||
          arguments.indices.exists(index => closure(arguments(index)) && inlined(application, index)) =>
      visit(application)
      traverse(function)
      arguments.zipWithIndex.foreach { (argument, index) =>
        if (closure(argument) && (traversal(function) || inlined(application, index))) {
          val over = Option.when(traversal(function))(Calls.selected(function)).flatten.flatMap(receiver =>
            settledPath(converted(receiver.qualifier))
          )
          carried ++= accumulators(function, argument)
          written ++= writes(argument)
          if (Stopping(Calls.selected(function).fold("")(_.name.toString)))
            Calls.lambda(argument).map(_.rhs).foreach {
              case tpd.Match(_, cases) => stopped ++= cases.map(_.body.span).filter(_.exists)
              case _                   => ()
            }
          within(mutable.Set.empty, argument, over = over)(traverse(argument))
        } else passed(application, argument, index)
      }
    case binding @ (_: tpd.ValDef | _: tpd.Bind) if looping.top && !carried(binding.symbol) =>
      bound.top += binding.symbol
      binding match {
        case value: tpd.ValDef =>
          destructured(value)
          framed(value)
        case _ => ()
      }
      if (binding.symbol.is(Flags.Lazy)) deferring(traverseChildren(tree))
      else traverseChildren(tree)
    case matched @ tpd.Match(selector, cases) if looping.top =>
      exempt ++= ends(matched).map(_.span)
      decoded(matched)
      traverse(selector)
      cases.foreach { clause =>
        carried ++= threaded(selector, clause.pat)
        traverse(clause)
      }
    case clause: tpd.CaseDef =>
      traverse(clause.pat)
      guarded.push(clause.pat.filterSubTrees(_.isInstanceOf[tpd.Bind]).map(_.symbol).toSet)
      guarding.push(true)
      try traverse(clause.guard)
      finally {
        guarding.pop()
        guarded.pop(): Unit
      }
      if (stopped(clause.body.span)) {
        looping.push(false)
        try traverse(clause.body)
        finally looping.pop(): Unit
      } else traverse(clause.body)
    case assignment: tpd.Assign =>
      accumulated(assignment)
      traverseChildren(tree)
    case application: tpd.Apply =>
      visit(application)
      threads(application)
      traverse(application.fun)
      application.args.zipWithIndex.foreach((argument, index) => passed(application, argument, index))
    case select: tpd.Select =>
      linear(select)
      windowed(select)
      if (compiled(select)) recompiled(select)
      hoistable(select)
      searched(select)
      folded(select)
      traverseChildren(tree)
    case application: tpd.TypeApply =>
      hoistable(application)
      traverseChildren(tree)
    case branch @ (_: tpd.If | _: tpd.Match) =>
      exempt ++= ends(branch).map(_.span)
      traverseChildren(tree)
    case _ => traverseChildren(tree)
  }

  private def visit(application: tpd.Apply)(using Context): Unit = {
    val repetition =
      if (looping.top && !deferred.top && reached(application)) Memberships.Repetition.Looped
      else Memberships.Repetition.Once
    hoisted ++= memberships.observe(application, repetition, guarded.top)
    compared(application)
    tabulated(application)
    boxing(application)
    rebuilt(application)
    equated(application)
    if (compiled(application)) recompiled(application)
    reparsed(application)
    concatenated(application)
    hoistable(application)
    searched(application)
    folded(application)
    scanned(application)
    walked(application)
    derived(application)
  }

  private def within(
      fresh: mutable.Set[Symbol],
      loop: tpd.Tree,
      method: Option[Symbol] = None,
      over: Option[Symbol] = None
  )(body: => Unit): Unit = {
    looping.push(true)
    bound.push(fresh)
    deferred.push(false)
    extents.push(if (loop.span.exists) loop.sourcePos.endLine - loop.sourcePos.startLine + 1 else 1)
    recursing.push(method)
    iterated.push(over)
    try body
    finally {
      looping.pop()
      bound.pop()
      deferred.pop()
      recursing.pop()
      iterated.pop()
      extents.pop(): Unit
    }
  }

  // The iterations of a loop that has some, as opposed to the calls of a recursive method.
  private def iterating: Boolean = looping.top && recursing.top.isEmpty

  // Whether a tree runs on every call of the recursion it is in, if any, rather than in a branch that ends it.
  private def reached(tree: tpd.Tree): Boolean = recursing.top.isEmpty || !leaves.exists(_.contains(tree.span))

  // Arguments passed by name and lazy values run when needed, if at all, so their work is not repeated every iteration.
  private def deferring(body: => Unit): Unit = {
    deferred.push(true)
    try body
    finally deferred.pop(): Unit
  }

  private def passed(application: tpd.Apply, argument: tpd.Tree, index: Int)(using Context): Unit =
    if (Trees.parameters(application).lift(index).exists(Trees.byName(_)))
      deferring(traverse(argument))
    else traverse(argument)

  private def accumulators(function: tpd.Tree, argument: tpd.Tree)(using Context): List[Symbol] = {
    val name       = Calls.selected(function).fold("")(_.name.toString)
    val parameters = Calls.lambda(argument).fold(Nil)(_.termParamss.flatten.map(_.symbol))
    if (LeftFolds(name)) parameters.take(1) else if (RightFolds(name)) parameters.slice(1, 2) else Nil
  }

  // A function written as cases matches its parameters as a tuple, so the binders matching an accumulator carry it too.
  private def threaded(selector: tpd.Tree, pattern: tpd.Tree)(using Context): List[Symbol] = (selector, pattern) match {
    case (reference: tpd.Ident, _) if carried(reference.symbol) =>
      pattern.filterSubTrees(_.isInstanceOf[tpd.Bind]).map(_.symbol)
    case (tpd.Apply(_, elements), tpd.UnApply(_, _, patterns))
        if defn.isTupleClass(selector.tpe.widen.classSymbol) && elements.size == patterns.size =>
      elements.zip(patterns).flatMap(threaded(_, _))
    case _ => Nil
  }

  private def retained(receiver: tpd.Tree)(using Context): Boolean = !receiver.existsSubTree {
    case reference: tpd.Ident => bound.top(reference.symbol)
    case _                    => false
  }

  // A collection reached through a variable of an enclosing loop is built afresh by that loop, so an index of it would
  // be built on every iteration of that loop too.
  private def anchored(receiver: tpd.Tree)(using Context): Boolean = receiver.existsSubTree {
    case reference: tpd.Ident => bound.exists(_(reference.symbol))
    case _                    => false
  }

  /** Whether `function` selects a method of a collection, whose function arguments run once per element. */
  private def traversal(function: tpd.Tree)(using Context): Boolean = Calls.selected(function).exists { select =>
    val receiver = select.qualifier.tpe.widen
    !receiver.derivesFrom(defn.OptionClass) && !single(select.qualifier) &&
    (receiver.derivesFrom(IterableOnce) || receiver.derivesFrom(ArrayOps) || receiver.derivesFrom(StringOps))
  }

  private val Singles = Set("toVector", "toList", "toSeq", "toIterable", "iterator")

  /** An option viewed as a collection, of at most one element. */
  private def single(tree: tpd.Tree)(using Context): Boolean = Calls.peelInlined(tree) match {
    case tpd.Select(option, name)                 => Singles(name.toString) && optional(option)
    case tpd.Apply(tpd.Select(option, name), Nil) => Singles(name.toString) && optional(option)
    case _                                        => false
  }

  private def optional(tree: tpd.Tree)(using Context): Boolean = converted(tree).tpe.widen.derivesFrom(defn.OptionClass)

  private def closure(tree: tpd.Tree)(using Context): Boolean = Calls.lambda(tree).isDefined

  private def inlined(application: tpd.Apply, index: Int)(using Context): Boolean =
    application.fun.symbol.is(Flags.Inline) && Trees.parameters(application).lift(index)
      .exists(parameter => parameter.is(Flags.Inline) && repeats(application.fun.symbol, parameter))

  /** Whether an inline method may run what its inline `parameter` receives more than once: where its body loops, or
    * where it passes it on to a traversal or to an inline method that does. A method without a body to read, as an
    * abstract one, is not known to, and its instances may run the function once.
    */
  private def repeats(method: Symbol, parameter: Symbol)(using Context): Boolean =
    repeating.getOrElse(
      (method, parameter), {
        repeating((method, parameter)) = false
        val repeated =
          method.getAnnotation(defn.BodyAnnot).map(_.tree).filterNot(Calls.peelInlined(_).isEmpty).exists { body =>
            val regions = body.filterSubTrees {
              case _: tpd.WhileDo | _: tpd.Apply => true
              case _                             => false
            }.flatMap {
              case loop: tpd.WhileDo      => List(loop.span)
              case application: tpd.Apply =>
                application.args.indices.collect {
                  case index if traversal(application.fun) || inlined(application, index) =>
                    application.args(index).span
                }
              case _ => Nil
            }
            body.existsSubTree {
              case reference: tpd.Ident => reference.symbol == parameter && regions.exists(_.contains(reference.span))
              case _                    => false
            }
          }
        repeating((method, parameter)) = repeated
        repeated
      }
    )

  // Arrays reach their transformations through an implicit conversion to ArrayOps, and some transformations take
  // inferred implicit arguments, such as a ClassTag, in a later argument list.
  @annotation.tailrec
  private def converted(tree: tpd.Tree)(using Context): tpd.Tree = tree match {
    case tpd.Apply(function, List(wrapped)) if function.symbol.is(Flags.Implicit)       => converted(wrapped)
    case tpd.Apply(inner: tpd.Apply, arguments) if arguments.forall(_.span.isSynthetic) => converted(inner)
    case other                                                                          => other
  }

  private def strict(receiver: Type)(using Context): Boolean =
    receiver.derivesFrom(ArrayOps) ||
      receiver.derivesFrom(Iterable) && !receiver.derivesFrom(View) && !receiver.derivesFrom(LazyList)

  private def linear(select: tpd.Select)(using Context): Unit = if (iterating) {
    val name = select.name.toString
    if (
      (Methods.Walks(name) || Methods.Appends(name) || name == "contains") && retained(select.qualifier) &&
      !fixed(select.qualifier) && !bounded(select.qualifier)
    ) {
      val receiver = select.qualifier.tpe.widen
      val kind     = article(receiver.classSymbol.name.toString)
      if (receiver.derivesFrom(LinearSeq) && Methods.Walks(name))
        checks(
          Check.LinearInLoop,
          s"$name on $kind walks it on every iteration; index an IndexedSeq instead",
          select.srcPos
        )
      else if (receiver.derivesFrom(ImmutableLinear) && Methods.Appends(name))
        checks(
          Check.LinearInLoop,
          s"$name on $kind copies it on every iteration; prepend and reverse, or use a builder",
          select.srcPos
        )
      else if (name == "contains" && scanned(receiver))
        checks(Check.LinearInLoop, s"contains on $kind scans it on every iteration; use a Set", select.srcPos)
    }
  }

  // An existence test for equality with a value scans the sequence as contains does.
  private def equated(application: tpd.Apply)(using Context): Unit = if (iterating) {
    Calls.selected(application.fun).foreach { select =>
      val name     = select.name.toString
      val receiver = select.qualifier.tpe.widen
      if (
        (name == "exists" || name == "forall") && application.args.sizeIs == 1 && scanned(receiver) &&
        retained(select.qualifier) && !fixed(select.qualifier) && !bounded(select.qualifier) &&
        comparing(application.args.head, if (name == "exists") nme.EQ else nme.NE)
      ) {
        scans += application.span
        checks(
          Check.LinearInLoop,
          s"$name on ${article(receiver.classSymbol.name.toString)} scans it on every iteration; use a Set",
          select.srcPos
        )
      }
    }
  }

  /** Whether `function` compares its parameter with `operator` to a value that does not mention the parameter. */
  private def comparing(function: tpd.Tree, operator: Name)(using Context): Boolean =
    Calls.lambda(function).exists { definition =>
      definition.termParamss.flatten match {
        case List(parameter) => Calls.peelInlined(definition.rhs) match {
            case tpd.Apply(tpd.Select(left: tpd.Ident, `operator`), List(value)) =>
              left.symbol == parameter.symbol && !value.existsSubTree(_.symbol == parameter.symbol)
            case _ => false
          }
        case _ => false
      }
    }

  // A value that names an existing collection rather than computing one, and that no iteration changes.
  private def settled(tree: tpd.Tree)(using Context): Boolean = tree match {
    case reference: tpd.Ident     => !carried(reference.symbol) && !written(reference.symbol)
    case tpd.Select(qualifier, _) => tree.symbol.isStableMember && !written(tree.symbol) && settled(qualifier)
    case _: tpd.This              => true
    case _                        => false
  }

  // Concatenating collections that stay the same across iterations builds the same collection each time.
  private def rebuilt(application: tpd.Apply)(using Context): Unit = if (iterating) {
    val select = application.fun match {
      case select: tpd.Select                   => Some(select)
      case tpd.TypeApply(select: tpd.Select, _) => Some(select)
      case _                                    => None
    }
    select.filter(select =>
      Methods.Appends(select.name.toString) && strict(select.qualifier.tpe.widen) &&
        !select.qualifier.tpe.widen.derivesFrom(ImmutableLinear) &&
        (select.qualifier :: application.args).forall(operand => settled(operand) && retained(operand)) &&
        !(select.qualifier :: application.args).forall(bounded(_))
    ).foreach(select =>
      checks(
        Check.LinearInLoop,
        s"${select.name} on ${article(select.qualifier.tpe.widen.classSymbol.name.toString)} rebuilds the same " +
          "collection on every iteration; build it once before the loop",
        application.srcPos
      )
    )
  }

  private def article(word: String): String = if ("AEIOU".contains(word.head.toUpper)) s"an $word" else s"a $word"

  // Boxing matters where primitives go into or are looked up in collections, which hold them boxed.
  private def collection(owner: Symbol)(using Context): Boolean =
    owner.derivesFrom(IterableOnceOps) || owner.derivesFrom(Growable) || owner.derivesFrom(ArrayOps)

  // Int and narrower values in loops are overwhelmingly indices and counts, which their valueOf methods serve from a
  // cache; Long values tend to be masks and addresses beyond it, and Double and Float values are never cached.
  private def wide(tpe: Type)(using Context): Boolean =
    tpe.isRef(defn.LongClass) || tpe.isRef(defn.DoubleClass) || tpe.isRef(defn.FloatClass)

  // Boxing a constant from -128 to 127 returns a shared instance rather than allocating.
  private def cached(argument: tpd.Tree): Boolean = argument match {
    case tpd.Literal(constant) if constant.tag == Constants.LongTag =>
      constant.longValue >= Byte.MinValue && constant.longValue <= Byte.MaxValue
    case _ => false
  }

  private def scanned(receiver: Type)(using Context): Boolean =
    (receiver.derivesFrom(Seq) || receiver.derivesFrom(ArrayOps)) && !receiver.derivesFrom(Range) &&
      !receiver.derivesFrom(NumericRange)

  // A sequence built from elements listed in the source has the same few elements on every iteration.
  @annotation.tailrec
  private def fixed(tree: tpd.Tree)(using Context): Boolean = tree match {
    case tpd.Apply(function, arguments) if function.symbol.name == nme.apply      => arguments.forall(listed(_))
    case tpd.Apply(function, List(wrapped)) if function.symbol.is(Flags.Implicit) => fixed(wrapped)
    case _                                                                        => false
  }

  @annotation.tailrec
  private def listed(tree: tpd.Tree): Boolean = tree match {
    case tpd.Typed(expression, _) => listed(expression)
    case _: tpd.SeqLiteral        => true
    case _                        => false
  }

  private def boxing(application: tpd.Apply)(using Context): Unit = if (iterating && (!hotDeclared || heated)) {
    val method = application.fun.symbol
    if (method.exists && collection(method.owner)) {
      Trees.parameters(application).zip(application.args).foreach { (parameter, argument) =>
        if (
          parameter.info.typeSymbol.isTypeParam && !parameter.info.isRepeatedParam &&
          wide(argument.tpe.widen) && !cached(argument)
        )
          checks(
            Check.LoopBoxing,
            s"${Trees.text(argument)} is boxed for the generic parameter ${parameter.name} of ${method.name} " +
              "on every iteration; use a primitive array or a specialised structure",
            argument.srcPos
          )
      }
    }
  }

  /** A regex compiled from a literal, by `.r` or `Pattern.compile`. */
  private def compiled(tree: tpd.Tree)(using Context): Boolean = tree match {
    case select @ tpd.Select(receiver, name) =>
      name.toString == "r" && select.symbol.owner == StringOps && string(converted(receiver)).isDefined
    case tpd.Apply(function, pattern :: _) =>
      Calls.qualified(function.symbol) == "java.util.regex.Pattern.compile" && string(pattern).isDefined
    case _ => false
  }

  // An interpolation that interpolates nothing, as `raw"\d+"` does, is a literal too.
  private def string(tree: tpd.Tree)(using Context): Option[String] = Calls.peelInlined(tree) match {
    case tpd.Literal(constant) if constant.tag == Constants.StringTag => Some(constant.stringValue)
    case tpd.Apply(tpd.Select(context @ tpd.Apply(_, List(parts)), name), List(arguments))
        if context.tpe.widen.isRef(defn.StringContextClass) && Interpolators(name.toString) &&
          elements(arguments).isEmpty =>
      elements(parts) match {
        case List(part) =>
          string(part).map(text => if (name.toString == "s") StringContext.processEscapes(text) else text)
        case _ => None
      }
    case _ => None
  }

  private val Interpolators = Set("s", "raw")

  private def elements(arguments: tpd.Tree)(using Context): List[tpd.Tree] = Calls.peelInlined(arguments) match {
    case tpd.SeqLiteral(elements, _) => elements
    case other                       => List(other)
  }

  // A method runs its body on every call, while a val, a lazy val or an object initialises once; local values and
  // lambdas run as often as the definition around them.
  private def perCall(using Context): Boolean = enclosing.exists(!runsOnce(_))

  private def enclosing(using Context): Option[Symbol] = ctx.owner.ownersIterator.find(owner =>
    !owner.isAnonymousFunction && !(owner.isTerm && !owner.is(Flags.Method) && !owner.owner.isClass)
  ).filter(_.is(Flags.Method))

  /** Whether a method runs once each time the code around its definition does: a constructor, which initialises as a
    * val does, or a private or local method that only such code calls, as methods that split a large initialiser do.
    */
  private def runsOnce(method: Symbol)(using Context): Boolean =
    method.isConstructor || confined(method) && once.getOrElse(
      method, {
        once(method) = false
        val result = callers.get(method).exists(_.forall(_.forall(runsOnce(_))))
        once(method) = result
        result
      }
    )

  private def recompiled(tree: tpd.Tree)(using Context): Unit = if (perCall)
    checks(
      Check.RegexInDef,
      s"${Trees.text(tree)} compiles the pattern on every call; hoist the pattern into a val",
      tree.srcPos
    )

  private val Matching = Set("replaceAll", "replaceFirst", "split", "matches")

  // The JDK splits on one character, or on one escaped non-alphanumeric character, without a regex.
  private def verbatim(pattern: String): Boolean =
    pattern.length == 1 && !".$|()[{^?*+\\".contains(pattern.head) ||
      pattern.length == 2 && pattern.head == '\\' && !pattern(1).isLetterOrDigit

  private def reparsed(application: tpd.Apply)(using Context): Unit = if (iterating) application match {
    case tpd.Apply(select @ tpd.Select(_, name), pattern :: _)
        if Matching(name.toString) && select.symbol.owner == defn.StringClass &&
          string(pattern).exists(pattern => name.toString != "split" || !verbatim(pattern)) =>
      checks(
        Check.RegexInDef,
        s"$name compiles its literal regex on every iteration; hoist the pattern into a val",
        application.srcPos
      )
    case _ => ()
  }

  /** The variables a tree assigns, and the receivers of the updates it makes, through the methods it reaches. */
  private def writes(tree: tpd.Tree)(using Context): List[Symbol] = {
    val reached                            = mutable.Set.empty[Symbol]
    def reach(methods: List[Symbol]): Unit =
      methods.foreach(method => if (reached.add(method)) reach(calls(method)))
    reach(called(tree))
    val found = mutable.ListBuffer.from(direct(tree))
    reached.foreach(method => found ++= ownWrites.getOrElseUpdate(method, direct(definitions(method).rhs)))
    found.toList
  }

  private val ownWrites = mutable.Map.empty[Symbol, List[Symbol]]
  private val ownCalls  = mutable.Map.empty[Symbol, List[Symbol]]

  private def calls(method: Symbol)(using Context): List[Symbol] =
    ownCalls.getOrElseUpdate(method, called(definitions(method).rhs))

  private def called(tree: tpd.Tree)(using Context): List[Symbol] = tree.filterSubTrees {
    case reference @ (_: tpd.Ident | _: tpd.Select) => definitions.contains(reference.symbol)
    case _                                          => false
  }.map(_.symbol).distinct

  private def direct(tree: tpd.Tree)(using Context): List[Symbol] = tree.filterSubTrees {
    case _: tpd.Assign          => true
    case tpd.Apply(function, _) => Calls.selected(function).exists(select => Axioms.mutates(select.name.toString))
    case _                      => false
  }.flatMap {
    case tpd.Assign(target, _)  => List(target.symbol)
    case application: tpd.Apply =>
      Calls.selected(application.fun).map(select => converted(select.qualifier).symbol).toList
    case _ => Nil
  }

  // Option and Either hand a carried accumulator on to the functions mapping it.
  private def threads(application: tpd.Apply)(using Context): Unit =
    if (
      Calls.selected(application.fun).exists(select =>
        Calls.peelInlined(select.qualifier) match {
          case reference: tpd.Ident => carried(reference.symbol) && Trees.optional(reference.tpe.widen)
          case _                    => false
        }
      )
    ) carried ++= application.args.flatMap(arg => Calls.lambda(arg).toList.flatMap(_.termParamss.flatten.map(_.symbol)))

  private val Appends = Methods.Appends + "+"

  private def appended(tree: tpd.Tree)(using Context): Option[(tpd.Select, tpd.Tree)] = tree match {
    case tpd.Apply(function @ (_: tpd.Select | _: tpd.TypeApply), _) =>
      Calls.selected(function).filter(select => Appends(select.name.toString)).map(select =>
        select -> converted(select.qualifier)
      )
    case _ => None
  }

  private def concatenated(application: tpd.Apply)(using Context): Unit = if (iterating)
    appended(application).foreach {
      case (select, receiver: tpd.Ident) if carried(receiver.symbol) => copying(select, receiver, application)
      case _                                                         => ()
    }

  // A variable declared outside the loop accumulates across its iterations.
  private def accumulated(assignment: tpd.Assign)(using Context): Unit = if (iterating) assignment match {
    case tpd.Assign(target: tpd.Ident, value) if !bound.top(target.symbol) =>
      appended(Calls.peelInlined(value)).filter((_, receiver) => receiver.symbol == target.symbol).foreach(
        (select, receiver) => copying(select, receiver, assignment)
      )
    case _ => ()
  }

  // Appending to an array, an ArraySeq or a String copies everything it holds, so appending on every step is
  // quadratic. Lists are left to linear-in-loop, and Vector and Queue append in effectively constant time.
  private def copying(select: tpd.Select, receiver: tpd.Tree, tree: tpd.Tree)(using Context): Unit = {
    val accumulator = receiver.tpe.widen
    if (
      accumulator.derivesFrom(ArraySeq) || accumulator.derivesFrom(defn.ArrayClass) ||
      accumulator.derivesFrom(defn.StringClass)
    )
      checks(
        Check.AccumulatingConcat,
        s"${select.name} on the ${accumulator.classSymbol.name} accumulator copies it on every step, which is " +
          "quadratic; use a builder",
        tree.srcPos
      )
  }

  // A pattern definition binds its tuple to a synthetic value; a tuple built in each branch is allocated where they
  // merge, which escape analysis cannot remove, while one built directly is scalar-replaced.
  private def destructured(value: tpd.ValDef)(using Context): Unit =
    if (
      iterating && (!hotDeclared || heated) && value.symbol.is(Flags.Synthetic) && defn.isTupleNType(value.tpt.tpe) &&
      merged(value.rhs)
    )
      checks(
        Check.TupleDestructure,
        "the tuple destructured here is built in every branch and allocated where they merge, which the JIT cannot " +
          "scalar-replace; bind each component separately",
        value.srcPos
      )

  // A single branch that completes, as in an extractor pattern, builds its tuple where nothing merges.
  private def merged(tree: tpd.Tree)(using Context): Boolean = {
    val completing = ends(tree).filterNot(_.tpe.widen.isRef(defn.NothingClass))
    completing.size > 1 && completing.forall {
      case application: tpd.Apply =>
        application.symbol.name == nme.apply && defn.isTupleClass(application.tpe.widen.classSymbol)
      case _ => false
    }
  }

  /** The expressions the value of `tree` comes from, through blocks and branches. */
  private def ends(tree: tpd.Tree)(using Context): List[tpd.Tree] = Calls.peelInlined(tree) match {
    case tpd.Block(_, expression)                               => ends(expression)
    case tpd.If(_, left, right)                                 => ends(left) ++ ends(right)
    case matched @ tpd.Match(selector, _) if rematched(matched) => ends(selector)
    case tpd.Match(_, cases)                                    => cases.flatMap(clause => ends(clause.body))
    case other                                                  => List(other)
  }

  // A value the compiler cannot prove a tuple, as when a branch throws, is matched against the pattern again, and
  // before Scala 3.7 the match rebuilds the tuple from the components it extracts.
  private def rematched(matched: tpd.Match)(using Context): Boolean = matched.cases match {
    case List(tpd.CaseDef(binding: tpd.Bind, _, body))    => Calls.peelInlined(body).symbol == binding.symbol
    case List(tpd.CaseDef(pattern: tpd.UnApply, _, body)) =>
      val components = pattern.filterSubTrees(_.isInstanceOf[tpd.Bind]).map(_.symbol).toSet
      Calls.peelInlined(body) match {
        case application @ tpd.Apply(_, arguments) =>
          defn.isTupleClass(application.tpe.widen.classSymbol) && arguments.nonEmpty &&
          arguments.forall(argument => components(Calls.peelInlined(argument).symbol))
        case _ => false
      }
    case _ => false
  }

  /** A value that is the same on every iteration of the innermost loop: a literal, or a stable path through values the
    * loop neither binds nor changes.
    */
  private def stable(tree: tpd.Tree)(using Context): Boolean = tree match {
    case _: tpd.Literal | _: tpd.This => true
    case tpd.Typed(expression, _)     => stable(expression)
    case tpd.SeqLiteral(elements, _)  => elements.forall(stable(_))
    case reference: tpd.Ident         =>
      !reference.symbol.is(Flags.Method) && !Trees.byName(reference.symbol) && !varying(reference.symbol)
    case tpd.Select(qualifier, _) => tree.symbol.isStableMember && !written(tree.symbol) && stable(qualifier)
    case tpd.Apply(function, List(argument)) if function.symbol.is(Flags.Implicit) => stable(argument)
    case _                                                                         => false
  }

  private def varying(symbol: Symbol): Boolean = bound.top(symbol) || carried(symbol) || written(symbol)

  private def varies(tree: tpd.Tree, entered: Set[Symbol] = Set.empty)(using Context): Boolean = tree.existsSubTree {
    case reference: tpd.Ident =>
      varying(reference.symbol) || Trees.byName(reference.symbol) || captures(reference.symbol, entered)
    case _ => false
  }

  private def captures(method: Symbol, entered: Set[Symbol])(using Context): Boolean =
    method.is(Flags.Method) && method.owner.isTerm && !method.isAnonymousFunction && !entered(method) &&
      definitions.get(method).exists { definition =>
        val own = definition.filterSubTrees(_.isInstanceOf[tpd.DefTree]).map(_.symbol).toSet
        definition.rhs.existsSubTree {
          case reference: tpd.Ident => !own(reference.symbol) && varies(reference, entered + method)
          case _                    => false
        }
      }

  // The arguments a call is written with, in every argument list, without the implicit ones and the defaults.
  private def supplied(tree: tpd.Tree)(using Context): List[tpd.Tree] = tree match {
    case tpd.Apply(function, arguments) =>
      val implicitly = function.tpe.widen match {
        case method: MethodType => method.isImplicitMethod
        case _                  => false
      }
      supplied(function) ++
        (if (implicitly) Nil else arguments.filterNot(_.symbol.name.is(DefaultGetterName)))
    case tpd.TypeApply(function, _) => supplied(function)
    case _                          => Nil
  }

  // A call takes its last argument list in the tree whose type is no longer a method's.
  private def complete(tree: tpd.Tree)(using Context): Boolean = !tree.tpe.widen.isInstanceOf[MethodicType]

  private val Derived =
    Methods.Conversions ++ Set(
      "map",
      "flatMap",
      "filter",
      "filterNot",
      "collect",
      "to",
      "groupBy",
      "groupMap",
      "sorted",
      "sortBy",
      "sortWith",
      "distinct"
    )

  private val Aggregates = Set(
    "exists",
    "forall",
    "count",
    "find",
    "indexWhere",
    "sum",
    "product",
    "min",
    "max",
    "minBy",
    "maxBy",
    "mkString",
    "foldLeft",
    "foldRight",
    "reduce",
    "reduceLeft"
  )

  /** Allocations, decoding and derived collections that every iteration repeats with the same inputs. Arguments passed
    * by name and lazy values run only when needed, and linear-in-loop reports concatenation.
    */
  private def hoistable(tree: tpd.Tree)(using Context): Unit =
    if (
      looping.top && !deferred.top && !guarding.top && complete(tree) && !hoisted.exists(_.contains(tree.span)) &&
      reached(tree) && (allocated(tree) || traversed(tree)) && !(tiny(tree) && (chilled || hotDeclared && !heated))
    ) {
      hoisted += tree.span
      val reason = recursing.top.fold("the loop")(method => s"what the recursion of ${method.name} changes")
      checks(Check.InvariantInLoop, s"${label(tree)} does not depend on $reason; hoist it", tree.srcPos)
    }

  /** A match on what the loop does not change. One whose arms depend on the loop hoists only by unswitching the loop, a
    * copy of it per case, which pays only where the code runs hot and the loop is short.
    */
  private def decoded(matched: tpd.Match)(using Context): Unit =
    if (
      iterating && !deferred.top && !Calls.peelInlined(matched.selector).isInstanceOf[tpd.Literal] &&
      stable(matched.selector) &&
      !rematched(matched) && !hoisted.exists(_.contains(matched.span))
    )
      if (!matched.cases.exists(varies(_)))
        checks(
          Check.InvariantInLoop,
          s"the match on ${label(matched.selector)} does not depend on the loop; hoist it",
          matched.srcPos
        )
      else if (heated && extents.top <= MaximumUnswitched)
        checks(
          Check.InvariantInLoop,
          s"the match on ${label(matched.selector)} does not depend on the loop, but its cases do; unswitch the loop, " +
            "with a copy of it in each case",
          matched.srcPos
        )

  // Building a few constants costs less than naming them outside the code the build says runs hot.
  private def tiny(tree: tpd.Tree)(using Context): Boolean =
    Trees.built(tree).orElse(tupled(tree))
      .exists(elements => elements.sizeIs <= MaximumTiny && elements.forall(Trees.constant(_)))

  private def tupled(tree: tpd.Tree)(using Context): Option[List[tpd.Tree]] = Calls.peelInlined(tree) match {
    case tpd.Apply(_, elements) if defn.isTupleNType(tree.tpe.widen) => Some(elements)
    case _                                                           => None
  }

  private val MaximumTiny = 4

  private def allocated(tree: tpd.Tree)(using Context): Boolean = tree match {
    case select: tpd.Select =>
      select.name.toString == "r" && select.symbol.owner == StringOps && stable(converted(select.qualifier)) &&
      !(compiled(select) && perCall)
    case application: tpd.Apply =>
      val method    = application.symbol
      val cls       = application.tpe.widen.classSymbol
      val arguments = supplied(application)
      val factory   = method.isConstructor || method.name == nme.apply &&
        Calls.selected(application.fun).exists(_.qualifier.tpe.widen.classSymbol.is(Flags.Module)) &&
        (method.is(Flags.Synthetic) && cls.is(Flags.Case) || cls.derivesFrom(Iterable))
      arguments.forall(stable(_)) &&
      (factory && substantial(arguments) && immutable(cls) && (recursing.top.isEmpty || cls.derivesFrom(Iterable)) ||
        Calls.qualified(method) == "java.util.regex.Pattern.compile" && !(compiled(application) && perCall))
    case _ => false
  }

  /** A collection of constants, or of tuples and collections of them, built in code that runs on every call. */
  private def tabulated(application: tpd.Apply)(using Context): Unit =
    if (perCall && !exempt(application.span) && !hoisted.exists(_.contains(application.span)))
      Trees.built(application).filter(rows =>
        rows.size > 1 && rows.forall(datum(_)) && (rows.size >= MinimumConstants || !rows.forall(Trees.constant(_)))
      ).foreach { _ =>
        hoisted += application.span
        checks(
          Check.LiteralTableInDef,
          s"${label(application)} is a table of constants built on every call; make it a val named for what it holds",
          application.srcPos
        )
      }

  // Two constants are as often a pair of options as a table, while rows of tuples or collections are a table.
  private val MinimumConstants = 3

  private val Comparisons = Set("==", "!=", "equals", "sameElements", "subsetOf")

  private def compared(application: tpd.Apply)(using Context): Unit = application match {
    case tpd.Apply(select: tpd.Select, List(argument)) if Comparisons(select.name.toString) =>
      exempt ++= List(converted(select.qualifier), Calls.peelInlined(argument)).map(_.span)
    case _ => ()
  }

  private def datum(tree: tpd.Tree)(using Context): Boolean = Trees.constant(tree) ||
    (Calls.peelInlined(tree) match {
      case tuple: tpd.Apply if tuple.symbol.name == nme.apply && defn.isTupleClass(tuple.symbol.owner.companionClass) =>
        tuple.args.forall(datum(_))
      // An extension method takes the key as an argument, while ArrowAssoc wraps it as the receiver.
      case pair @ tpd.Apply(function, List(value)) if pair.symbol.name.toString == "->" =>
        val receiver = function match {
          case keyed: tpd.Apply                   => keyed
          case tpd.TypeApply(keyed: tpd.Apply, _) => keyed
          case _ => Calls.selected(function).fold(function)(select => Calls.peelInlined(select.qualifier))
        }
        datum(value) &&
        (receiver match {
          case tpd.Apply(_, List(key)) => datum(key)
          case _                       => false
        })
      case other => Trees.built(other).exists(_.forall(datum(_)))
    })

  // Wrapping one value, as Some(x) or Vector(x) do, costs too little to hoist.
  private def substantial(arguments: List[tpd.Tree])(using Context): Boolean =
    arguments.map(Calls.peelInlined(_) match {
      case tpd.SeqLiteral(elements, _) => elements.size
      case _                           => 1
    }).sum > 1

  // Mutable objects, exceptions and local classes, which capture their scope, may be meant to be fresh each time, and
  // the compiler rewrites the StringContext of an interpolation away.
  private def immutable(cls: Symbol)(using Context): Boolean =
    cls.isClass && !cls.is(Flags.JavaDefined) && !cls.owner.isTerm && cls != defn.StringContextClass &&
      !cls.derivesFrom(defn.ThrowableClass) && !Purity.stateful(cls.typeRef) &&
      !cls.info.decls.toList.exists(field =>
        field.is(Flags.Mutable) || field.isTerm && !field.is(Flags.Method) && Purity.stateful(field.info)
      )

  // Elements with fields the loop can change give a function of them a new result each time.
  private def traversed(tree: tpd.Tree)(using Context): Boolean = Calls.selected(tree).exists(select =>
    (Derived(select.name.toString) || Aggregates(select.name.toString)) && strict(select.qualifier.tpe.widen) &&
      stable(converted(select.qualifier)) &&
      !supplied(tree).exists(argument => varies(argument) || drawn(argument)) &&
      !element(select.qualifier.tpe.widen).exists(changing(_))
  )

  // State shows as a variable or a mutable collection, or, where an implementation holds it, as methods called for
  // their effect.
  private def changing(tpe: Type)(using Context): Boolean = {
    val cls = tpe.classSymbol
    cls.isClass && !immutableValue(cls) && cls.info.decls.toList.exists(member =>
      member.is(Flags.Mutable) || member.isTerm && !member.is(Flags.Method) && Purity.stateful(member.info) ||
        member.is(Flags.Method) && !member.isConstructor && !cls.is(Flags.JavaDefined) &&
        member.info.finalResultType.isRef(defn.UnitClass)
    )
  }

  // Strings, options, tuples and the immutable collections have no state, whatever their implementation does inside.
  private def immutableValue(cls: Symbol)(using Context): Boolean =
    cls == defn.StringClass || cls.fullName.toString.startsWith("scala.") &&
      (cls == StringOps || cls.derivesFrom(defn.OptionClass) || defn.isTupleClass(cls) ||
        cls.derivesFrom(ImmutableIterable))

  /** A function that draws from a random source, an iterator or a mutable collection, or calls a method of an object
    * with state of its own, gives a new result each time.
    */
  private def drawn(function: tpd.Tree)(using Context): Boolean = !unchanging(function) ||
    function.existsSubTree {
      case tpd.Select(receiver, _) if receiver.isTerm && receiver.symbol.isTerm => changing(receiver.tpe.widen)
      case _                                                                    => false
    }

  private def label(tree: tpd.Tree)(using Context): String = {
    val text = Trees.text(tree)
    if (text.length <= 60 && !text.contains('\n')) text
    else if (tree.symbol.isConstructor) s"new ${tree.symbol.owner.name}(...)"
    else s"${Calls.selected(tree).fold(tree.symbol.name.toString)(_.name.toString)}(...)"
  }

  private val Searches = Set("find", "exists", "indexWhere")
  private val Firsts   = Set("head", "headOption")

  /** Searches of a settled sequence for the element whose field equals a key, where the sequence's owner already keys
    * its elements by that field: in a map, or in a method taking the key last and returning the element as an option.
    */
  private def searched(tree: tpd.Tree)(using Context): Unit = if (iterating) tree match {
    case tpd.Apply(function @ (_: tpd.Select | _: tpd.TypeApply), predicate :: _) =>
      Calls.selected(function).filter(select => Searches(select.name.toString)).foreach(lookup(tree, _, predicate))
    case tpd.Select(tpd.Apply(function @ (_: tpd.Select | _: tpd.TypeApply), List(predicate)), name)
        if Firsts(name.toString) =>
      Calls.selected(function).filter(_.name.toString == "filter").foreach(lookup(tree, _, predicate))
    case _ => ()
  }

  private def lookup(tree: tpd.Tree, search: tpd.Select, predicate: tpd.Tree)(using Context): Unit = {
    val receiver = search.qualifier
    if (receiver.tpe.widen.derivesFrom(Seq) && settled(receiver) && retained(receiver))
      for {
        function <- Calls.lambda(predicate)
        element  <- Parameters.sole(function)
        key      <- compared(function.rhs, element)
        owner    <- receiver.tpe match {
          case reference: TermRef if reference.prefix ne NoPrefix => Some(reference.prefix)
          case _                                                  => None
        }
        member <- index(owner, receiver.symbol, key, element.info)
      } {
        scans += tree.span
        checks(
          Check.KeyedLookupInLoop,
          s"${search.name} over ${Trees.text(receiver)} scans it on every iteration; use ${member.name} instead",
          tree.srcPos
        )
      }
  }

  // The type of the element's field an equality predicate compares with a value not derived from the element.
  private def compared(body: tpd.Tree, element: Symbol)(using Context): Option[Type] = Calls.peelInlined(body) match {
    case tpd.Apply(tpd.Select(left, name), List(right)) if name.toString == "==" =>
      def field(tree: tpd.Tree) = tree match {
        case tpd.Select(reference: tpd.Ident, _) => reference.symbol == element && !tree.symbol.is(Flags.Method)
        case _                                   => false
      }
      def independent(tree: tpd.Tree) = !tree.existsSubTree {
        case reference: tpd.Ident => reference.symbol == element
        case _                    => false
      }
      if (field(left) && independent(right)) Some(left.tpe.widen)
      else if (field(right) && independent(left)) Some(right.tpe.widen)
      else None
    case _ => None
  }

  private def index(owner: Type, collection: Symbol, key: Type, element: Type)(using Context): Option[Symbol] =
    owner.allMembers.view.map(_.symbol).find(member =>
      member.isTerm && member != collection && !ctx.owner.ownersIterator.contains(member) &&
        member.isAccessibleFrom(owner) &&
        (owner.memberInfo(member) match {
          case method: MethodType =>
            method.paramInfoss.flatten.lastOption.exists(key <:< _) &&
            method.finalResultType.baseType(defn.OptionClass).argInfos.headOption.exists(_ =:= element)
          case _: PolyType => false
          case value       =>
            value.widenExpr.baseType(Map).argInfos match {
              case List(keys, values) => key <:< keys && values =:= element
              case _                  => false
            }
        })
    )

  private val Scans = Searches ++ Set("filter", "filterNot", "count", "collect", "collectFirst")

  private val SetClass = Classes.required("scala.collection.Set")

  /** Scans of a collection the loop leaves as it is: of the one it iterates, which makes the loop quadratic, or one
    * whose elements the predicate relates to a value of the loop by equality or membership, which an index built once
    * answers.
    */
  private def scanned(tree: tpd.Tree)(using Context): Unit = if (iterating) tree match {
    case tpd.Apply(function @ (_: tpd.Select | _: tpd.TypeApply), predicate :: _) =>
      Calls.selected(function).filter(select => Scans(select.name.toString)).foreach(scan(tree, _, predicate))
    case _ => ()
  }

  private def scan(tree: tpd.Tree, search: tpd.Select, predicate: tpd.Tree)(using Context): Unit = {
    val receiver = converted(search.qualifier)
    val kind     = receiver.tpe.widen
    if (
      !scans.exists(_.contains(tree.span)) && (strict(kind) || kind.derivesFrom(defn.ArrayClass)) &&
      !kind.derivesFrom(Range) &&
      !kind.derivesFrom(NumericRange) && settled(receiver) && retained(receiver) && !fixed(receiver) &&
      !bounded(receiver)
    ) {
      val shown = s"${search.name} over ${Trees.text(receiver)}"
      if (receiver.symbol.exists && iterated.exists(_.contains(receiver.symbol))) {
        scans += tree.span
        checks(
          Check.LinearInLoop,
          s"$shown scans the collection the loop iterates, which makes it quadratic; build an index of it first",
          tree.srcPos
        )
      } else if (!chilled && !anchored(receiver) && Calls.lambda(predicate).exists(relating(_))) {
        scans += tree.span
        checks(
          Check.KeyedLookupInLoop,
          s"$shown relates its elements to the loop variable and scans them on every iteration; build an index of " +
            "them first",
          tree.srcPos
        )
      }
    }
  }

  private def settledPath(tree: tpd.Tree)(using Context): Option[Symbol] = tree match {
    case _: (tpd.Ident | tpd.Select) if tree.symbol.exists && settled(tree) && retained(tree) => Some(tree.symbol)
    case _                                                                                    => None
  }

  /** Whether a predicate holds only when an element-derived value equals, or is a member of, a value of the loop. */
  private def relating(function: tpd.DefDef)(using Context): Boolean = {
    val elements =
      (function.termParamss.flatten.map(_.symbol) ++
        function.rhs.filterSubTrees(_.isInstanceOf[tpd.Bind]).map(_.symbol)).toSet
    conjuncts(function.rhs).exists(relates(_, elements))
  }

  private def conjuncts(tree: tpd.Tree)(using Context): List[tpd.Tree] = Calls.peelInlined(tree) match {
    case tpd.Apply(tpd.Select(left, name), List(right)) if name.toString == "&&" =>
      conjuncts(left) ++ conjuncts(right)
    case matched: tpd.Match     => matched.cases.flatMap(clause => conjuncts(clause.guard) ++ conjuncts(clause.body))
    case empty if empty.isEmpty => Nil
    case other                  => List(other)
  }

  private def relates(conjunct: tpd.Tree, elements: Set[Symbol])(using Context): Boolean = conjunct match {
    case tpd.Apply(tpd.Select(left, name), List(right))
        if name.toString == "==" || name.toString == "contains" ||
          name == nme.apply && left.tpe.widen.derivesFrom(SetClass) =>
      def element(side: tpd.Tree) = mentions(side, elements) && !varies(side)
      def loop(side: tpd.Tree)    = varies(side) && !mentions(side, elements)
      element(left) && loop(right) || name.toString == "==" && loop(left) && element(right)
    case _ => false
  }

  private def mentions(tree: tpd.Tree, symbols: Set[Symbol])(using Context): Boolean = tree.existsSubTree {
    case reference: tpd.Ident => symbols(reference.symbol)
    case _                    => false
  }

  /** The head of a call written in several argument lists, with the arguments of each list. */
  @annotation.tailrec
  private def unapplied(tree: tpd.Tree, lists: List[List[tpd.Tree]] = Nil)(using
      Context
  ): (tpd.Tree, List[List[tpd.Tree]]) =
    tree match {
      case tpd.Apply(function, arguments) => unapplied(function, arguments :: lists)
      case tpd.TypeApply(function, _)     => unapplied(function, lists)
      case other                          => (other, lists)
    }

  /** A call that descends a recursive structure the loop does not change, for a value of the loop, walks all of it. */
  private def walked(application: tpd.Apply)(using Context): Unit = if (iterating && complete(application)) {
    val (head, lists) = unapplied(application)
    val method        = head.symbol
    for {
      definition    <- recursive.get(method).filterNot(_ => method.isConstructor)
      (variants, _) <- recursion(definition)
      parameters = Trees.termParameters(method)
      if parameters.map(_.size) == lists.map(_.size)
      pairs =
        parameters.flatten.zip(lists.flatten).filterNot((parameter, _) => parameter.isOneOf(Flags.GivenOrImplicit))
      (_, root) <- pairs.find((parameter, argument) =>
        variants(parameter) && branching(parameter.info) && settled(argument) && retained(argument) &&
          !fixed(argument) && Trees.text(argument).nonEmpty &&
          pairs.exists((_, value) => (value ne argument) && varies(value)) &&
          pairs.forall((other, value) => (value eq argument) || !varies(value) || key(other.info))
      )
    } checks(
      Check.LinearInLoop,
      s"${method.name} walks ${Trees.text(root)} on every iteration; build an index of it first",
      application.srcPos
    )
  }

  /** Whether a value of the type is a single thing to look up by, rather than a collection, a function or a structure.
    */
  private def key(tpe: Type)(using Context): Boolean =
    !tpe.derivesFrom(IterableOnce) && !tpe.derivesFrom(ArrayOps) && !tpe.derivesFrom(defn.ArrayClass) &&
      !defn.isFunctionType(tpe.widenDealias) && !structure(tpe)

  /** Whether a type, or the elements of a collection of it, has instances that hold instances of it. */
  private def structure(tpe: Type)(using Context): Boolean = linked(tpe)((_, fields) => fields.nonEmpty)

  /** Whether a structure splits, as a tree does, rather than being a chain of wrappers, which is as long as it is
    * nested and read in constant time in practice.
    */
  private def branching(tpe: Type)(using Context): Boolean =
    linked(tpe)((cls, fields) => fields.sizeIs > 1 || fields.exists(field => !field.info.derivesFrom(cls)))

  private def linked(tpe: Type)(test: (Symbol, List[Symbol]) => Boolean)(using Context): Boolean = {
    val cls = element(tpe).getOrElse(tpe).widenDealias.classSymbol
    cls.isClass && !cls.is(Flags.JavaDefined) && !cls.fullName.toString.startsWith("scala.") &&
    (cls :: cls.children).exists(candidate =>
      candidate.isClass && test(
        cls,
        candidate.info.decls.toList.filter(field =>
          field.isTerm && !field.is(Flags.Method) && field.info.existsPart(_.derivesFrom(cls))
        )
      )
    )
  }

  private val Totals = Set(
    "sum",
    "product",
    "min",
    "max",
    "minBy",
    "maxBy",
    "foldLeft",
    "foldRight",
    "reduce",
    "reduceLeft",
    "mkString",
    "count"
  )
  private val Windows = Set("slice", "take")
  private val Passes  = Set("map", "filter", "filterNot", "flatMap", "collect", "view")

  /** An aggregate over a window of a collection whose end moves with the innermost loop, which the next iteration
    * computes again from the start.
    */
  private def windowed(select: tpd.Select)(using Context): Unit = if (iterating && Totals(select.name.toString)) {
    window(select.qualifier).filterNot(found => scans(found.span)).foreach { found =>
      scans += found.span
      val name = Calls.selected(found).fold("slice")(_.name.toString)
      checks(
        Check.LinearInLoop,
        s"$name recomputes the aggregate over a window that grows by one element on every iteration; keep a " +
          "running total",
        found.srcPos
      )
    }
  }

  @annotation.tailrec
  private def window(tree: tpd.Tree)(using Context): Option[tpd.Tree] = Calls.peelInlined(converted(tree)) match {
    case application @ tpd.Apply(function, _) =>
      Calls.selected(function) match {
        case Some(select) if Passes(select.name.toString)                                  => window(select.qualifier)
        case Some(select) if Windows(select.name.toString) && growing(application, select) => Some(application)
        case _                                                                             => None
      }
    case reference: tpd.Ident => windows.get(reference.symbol)
    case _                    => None
  }

  private def growing(application: tpd.Apply, select: tpd.Select)(using Context): Boolean =
    !bounded(select.qualifier) &&
      (application.args match {
        case List(end)        => select.name.toString == "take" && !retained(end) && retained(select.qualifier)
        case List(start, end) => retained(start) && !retained(end) && retained(select.qualifier)
        case _                => false
      })

  private def framed(value: tpd.ValDef)(using Context): Unit = if (iterating)
    Calls.peelInlined(value.rhs) match {
      case application @ tpd.Apply(function, _) =>
        Calls.selected(function).filter(select => Windows(select.name.toString) && growing(application, select))
          .foreach(_ => windows(value.symbol) = application)
      case _ => ()
    }

  private val Derivations = Derived ++ Set("mkString", "toLowerCase", "toUpperCase")

  /** Work on the elements of a field alone, which a method recomputes on every call. */
  private def derived(application: tpd.Apply)(using Context): Unit =
    if (!looping.top && (!hotDeclared || heated) && perCall && traversal(application.fun)) {
      Calls.selected(application.fun).map(select => converted(select.qualifier)).filter(receiver =>
        fieldPath(receiver) && strict(receiver.tpe.widen) && unchanging(receiver) &&
          !element(receiver.tpe.widen).exists(held => Purity.stateful(held) || !standard(held) && changing(held))
      ).foreach { receiver =>
        application.args.flatMap(Calls.lambda(_)).foreach { function =>
          val parameters = function.termParamss.flatten.map(_.symbol).toSet
          val found = function.rhs.filterSubTrees(derivation(_, parameters)).groupBy(_.span).values.map(_.head).toList
          found.filterNot(inner => found.exists(outer => outer.span != inner.span && outer.span.contains(inner.span)))
            .foreach(tree =>
              checks(
                Check.InvariantInLoop,
                s"${label(tree)} depends only on the elements of ${Trees.text(receiver)}; compute it once in a field",
                tree.srcPos
              )
            )
        }
      }
    }

  /** Whether a tree reads no source of new values, as a random generator, an iterator or a mutable collection is. */
  private def unchanging(tree: tpd.Tree)(using Context): Boolean = !tree.existsSubTree {
    case reference @ (_: tpd.Ident | _: tpd.Select) if reference.symbol.isTerm && !reference.symbol.is(Flags.Method) =>
      val tpe = reference.tpe.widen
      Purity.stateful(tpe)
    case _ => false
  }

  private def standard(tpe: Type)(using Context): Boolean = {
    val name = tpe.widen.classSymbol.fullName.toString
    name.startsWith("scala.") || name.startsWith("java.")
  }

  private def fieldPath(tree: tpd.Tree)(using Context): Boolean = tree match {
    case _: tpd.This                                => true
    case reference @ (_: tpd.Ident | _: tpd.Select) =>
      val symbol = reference.symbol
      symbol.exists && symbol.isTerm && !symbol.is(Flags.Method) && !symbol.is(Flags.Mutable) &&
      symbol.owner.isClass && !symbol.owner.is(Flags.Package) &&
      (reference match {
        case tpd.Select(qualifier, _) => fieldPath(Calls.peelInlined(qualifier)) || qualifier.symbol.is(Flags.Module)
        case _                        => true
      })
    case _ => false
  }

  private def derivation(tree: tpd.Tree, parameters: Set[Symbol])(using Context): Boolean =
    Calls.selected(tree).exists(select => Derivations(select.name.toString)) && complete(tree) &&
      (tree.isInstanceOf[tpd.Apply] || tree.isInstanceOf[tpd.TypeApply]) && unchanging(tree) && {
        val defined    = tree.filterSubTrees(_.isInstanceOf[tpd.DefTree]).map(_.symbol).toSet
        val references = tree.filterSubTrees(_.isInstanceOf[tpd.Ident]).map(_.symbol)
        references.exists(parameters) && references.forall(symbol =>
          parameters(symbol) || defined(symbol) ||
            !symbol.is(Flags.Method) && !symbol.is(Flags.Mutable) && (symbol.owner.isClass || symbol.is(Flags.Module))
        )
      }

  /** The parameters a recursive method passes unchanged to each of its calls are the same in all of them, so work that
    * reads only those, and values the method does not change, repeats; the parameters that differ, and the branches
    * that make no call, are returned with them.
    */
  private def recursion(definition: tpd.DefDef)(using Context): Option[(Set[Symbol], List[Span])] =
    recursions.getOrElseUpdate(definition.symbol, analysed(definition))

  private def analysed(definition: tpd.DefDef)(using Context): Option[(Set[Symbol], List[Span])] = {
    val method  = definition.symbol
    val callers = Recursive(method)
    callers.traverse(definition.rhs)
    val calls      = callers.calls
    val parameters = definition.termParamss.flatten.map(_.symbol)
    if (calls.isEmpty || callers.opaque.nonEmpty) None
    else {
      val passed = (parameter: Symbol, index: Int) =>
        calls.forall(arguments =>
          arguments.sizeIs == parameters.size && Calls.peelInlined(arguments(index)).symbol == parameter
        )
      val variants =
        parameters.zipWithIndex.collect { case (parameter, index) if !passed(parameter, index) => parameter }
      Option.when(variants.nonEmpty)((variants.toSet, branches(definition.rhs, method)))
    }
  }

  private class Recursive(method: Symbol) extends tpd.TreeTraverser {
    val calls: mutable.ListBuffer[List[tpd.Tree]] = mutable.ListBuffer.empty[List[tpd.Tree]]
    val opaque: mutable.ListBuffer[tpd.Tree]      = mutable.ListBuffer.empty[tpd.Tree]

    def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
      case application: tpd.Apply =>
        val (head, lists) = unapplied(application)
        if (head.symbol == method && (head.isInstanceOf[tpd.Ident] || head.isInstanceOf[tpd.Select])) {
          if (own(head, method)) calls += lists.flatten else opaque += tree
          lists.flatten.foreach(argument => traverse(argument))
        } else traverseChildren(tree)
      case reference @ (_: tpd.Ident | _: tpd.Select) if reference.symbol == method => opaque += tree
      case _                                                                        => traverseChildren(tree)
    }
  }

  private def own(head: tpd.Tree, method: Symbol)(using Context): Boolean = head match {
    case tpd.Select(qualifier, _) =>
      Calls.peelInlined(qualifier) match {
        case _: tpd.This => true
        case other       => other.symbol.is(Flags.Module) && other.symbol.moduleClass == method.owner
      }
    case _ => true
  }

  /** The branches of conditionals and matches in a method body that make no call of the method. */
  private def branches(body: tpd.Tree, method: Symbol)(using Context): List[Span] = {
    val silent = Silent(method)
    silent.traverse(body)
    silent.found.toList
  }

  private class Silent(method: Symbol) extends tpd.TreeTraverser {
    val found: mutable.ListBuffer[Span] = mutable.ListBuffer.empty[Span]

    private def branch(tree: tpd.Tree)(using Context): Unit =
      if (references(tree, method)) traverse(tree) else found += tree.span
    def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
      case tpd.If(condition, left, right) =>
        traverse(condition)
        branch(left)
        branch(right)
      case tpd.Match(selector, cases) =>
        traverse(selector)
        cases.foreach { clause =>
          traverse(clause.guard)
          branch(clause.body)
        }
      case _ => traverseChildren(tree)
    }
  }

  private val Folds   = Set("foldLeft", "foldRight", "fold", "reduce", "scanLeft")
  private val Pairing = Set("tabulate", "zipWithIndex", "zip")

  /** In the classes the build marks hot, folds over indexed sequences and iterators whose accumulator the unspecialised
    * Function2 boxes on every step, and calls building a tuple or an option of primitives for every element. An
    * accumulator threading an error, or an optional reference, is boxed by design.
    */
  private def folded(tree: tpd.Tree)(using Context): Unit = Calls.selected(tree).filter(select =>
    (Folds(select.name.toString) || Pairing(select.name.toString)) && complete(tree) && heated
  ).foreach { select =>
    val name     = select.name.toString
    val receiver = select.qualifier.tpe.widen
    if (
      Folds(name) &&
      (receiver.derivesFrom(IndexedSeq) || receiver.derivesFrom(ArrayOps) || receiver.derivesFrom(Iterator))
    )
      typeArguments(tree).headOption.filter(accumulator => wide(accumulator) || boxed(accumulator)).foreach(
        accumulator =>
          checks(
            Check.BoxingFold,
            s"$name with ${article(accumulator.show)} accumulator boxes it on every step; use a @tailrec method " +
              "over primitives",
            tree.srcPos
          )
      )
    else if (Pairing(name))
      element(tree.tpe.widen).filter(boxed(_)).foreach(value =>
        checks(
          Check.BoxingFold,
          s"$name builds a boxed ${unannotated(value).show} for every element; use a @tailrec method over " +
            "primitive arrays",
          tree.srcPos
        )
      )
  }

  // Library signatures annotate type arguments for variance checking, which is noise in a message.
  private def unannotated(tpe: Type)(using Context): Type = tpe match {
    case AppliedType(constructor, arguments) => constructor.appliedTo(arguments.map(_.stripAnnots))
    case other                               => other
  }

  private val Shrinking = Set(
    "filter",
    "filterNot",
    "take",
    "drop",
    "takeWhile",
    "dropWhile",
    "slice",
    "distinct",
    "distinctBy",
    "tail",
    "init",
    "reverse",
    "sorted",
    "sortBy",
    "sortWith",
    "collect"
  )

  /** Whether the build declares the collection a member holds to be small, or the collection is taken from one that is
    * by an operation that cannot grow it, directly or through a value.
    */
  private def bounded(tree: tpd.Tree)(using Context): Boolean = {
    val member = converted(tree).symbol
    shrunk(converted(tree)) ||
    member.exists &&
    ((member.owner.isClass || member.is(Flags.Param)) && {
      val owner = Calls.qualified(member.owner)
      declared(s"$owner.${member.name}", owner)
    } || bindings.get(member).exists(shrunk(_)))
  }

  private def shrunk(tree: tpd.Tree)(using Context): Boolean =
    Calls.selected(tree).exists(select => Shrinking(select.name.toString) && bounded(select.qualifier))

  private def heated(using Context): Boolean =
    Trees.enclosedBy(ctx.owner, owner => !owner.isAnonymousClass && hot(Calls.qualified(owner)))

  private def chilled(using Context): Boolean =
    Trees.enclosedBy(ctx.owner, owner => !owner.isAnonymousClass && cold(Calls.qualified(owner)))

  @annotation.tailrec
  private def typeArguments(tree: tpd.Tree): List[Type] = tree match {
    case tpd.Apply(function, _)      => typeArguments(function)
    case tpd.TypeApply(_, arguments) => arguments.map(_.tpe)
    case _                           => Nil
  }

  private def boxed(tpe: Type)(using Context): Boolean = {
    val value = tpe.widen.dealias
    defn.isTupleNType(value) && value.argInfos.forall(Trees.primitive(_)) ||
    value.derivesFrom(defn.OptionClass) && value.baseType(defn.OptionClass).argInfos.exists(Trees.primitive(_))
  }

  private def element(tpe: Type)(using Context): Option[Type] =
    (if (tpe.derivesFrom(defn.ArrayClass)) tpe.baseType(defn.ArrayClass) else tpe.baseType(IterableOnce)).argInfos
      .headOption
}
