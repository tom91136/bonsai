package net.kurobako.baumkuchen.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.Type
import scala.collection.mutable

/** Reports work repeated on every iteration of a loop: a `while` body, or a function passed to a collection's
  * traversal. Linear operations there multiply the loop's cost, strict transformation chains allocate their
  * intermediate collections each time, and primitives passed to the generic parameters of collections box each time.
  */
private class Loops(checks: Checks)(using Context) extends tpd.TreeTraverser {
  // Whether each enclosing tree runs once per iteration of some loop, and the symbols the innermost loop binds afresh on
  // each iteration; a collection reached through them differs between iterations, so walking it is not repeated work.
  private val looping = mutable.Stack(false)
  private val bound   = mutable.Stack(mutable.Set.empty[Symbol])
  // A fold's accumulator carries one growing collection across iterations, so it is never bound afresh.
  private val carried = mutable.Set.empty[Symbol]

  private val IterableOnce    = requiredClass("scala.collection.IterableOnce")
  private val IterableOnceOps = requiredClass("scala.collection.IterableOnceOps")
  private val Iterable        = requiredClass("scala.collection.Iterable")
  private val Seq             = requiredClass("scala.collection.Seq")
  private val LinearSeq       = requiredClass("scala.collection.LinearSeq")
  private val ImmutableLinear = requiredClass("scala.collection.immutable.LinearSeq")
  private val View            = requiredClass("scala.collection.View")
  private val LazyList        = requiredClass("scala.collection.immutable.LazyList")
  private val Range           = requiredClass("scala.collection.immutable.Range")
  private val NumericRange    = requiredClass("scala.collection.immutable.NumericRange")
  private val ArrayOps        = requiredClass("scala.collection.ArrayOps")
  private val StringOps       = requiredClass("scala.collection.StringOps")
  private val Growable        = requiredClass("scala.collection.mutable.Growable")

  private val LeftFolds  = Set("foldLeft", "scanLeft", "fold", "scan", "reduceLeft", "reduce")
  private val RightFolds = Set("foldRight", "scanRight", "reduceRight")

  private val Walks  = Set("apply", "length", "size", "last")
  private val Copies = Set(":+", "appended", "++", "concat", "appendedAll", ":++")

  override def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
    case definition: tpd.DefDef if definition.symbol.is(Flags.Synthetic) && !definition.symbol.isAnonymousFunction =>
      ()
    case tpd.WhileDo(condition, body) =>
      within(true, mutable.Set.empty)(traverse(condition))
      within(true, mutable.Set.empty)(traverse(body))
    case application @ tpd.Apply(function, arguments) if traversal(function) =>
      boxing(application)
      strictChain(application)
      rebuilt(application)
      traverse(function)
      arguments.foreach { argument =>
        if (closure(argument)) {
          carried ++= accumulators(function, argument)
          within(true, mutable.Set.empty)(traverse(argument))
        } else within(looping.top, bound.top)(traverse(argument))
      }
    case binding @ (_: tpd.ValDef | _: tpd.Bind) if looping.top && !carried(binding.symbol) =>
      bound.top += binding.symbol
      traverseChildren(tree)
    case tpd.Match(selector, cases) if looping.top =>
      traverse(selector)
      cases.foreach { clause =>
        carried ++= threaded(selector, clause.pat)
        traverse(clause)
      }
    case application: tpd.Apply =>
      boxing(application)
      strictChain(application)
      rebuilt(application)
      traverseChildren(tree)
    case select: tpd.Select =>
      linear(select)
      traverseChildren(tree)
    case _ => traverseChildren(tree)
  }

  private def within(inside: Boolean, fresh: mutable.Set[Symbol])(body: => Unit): Unit = {
    looping.push(inside)
    bound.push(fresh)
    try body
    finally {
      looping.pop()
      bound.pop(): Unit
    }
  }

  private def accumulators(function: tpd.Tree, argument: tpd.Tree)(using Context): List[Symbol] = {
    val name       = Trees.selected(function).fold("")(_.name.toString)
    val parameters = Trees.lambda(argument).fold(Nil)(_.termParamss.flatten.map(_.symbol))
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

  private def iterated(receiver: tpd.Tree)(using Context): Boolean = receiver.existsSubTree {
    case reference: tpd.Ident => bound.top(reference.symbol)
    case _                    => false
  }

  /** Whether `function` selects a method of a collection, whose function arguments run once per element. */
  private def traversal(function: tpd.Tree)(using Context): Boolean = Trees.selected(function).exists { select =>
    val receiver = select.qualifier.tpe.widen
    !receiver.derivesFrom(defn.OptionClass) &&
    (receiver.derivesFrom(IterableOnce) || receiver.derivesFrom(ArrayOps) || receiver.derivesFrom(StringOps))
  }

  private def closure(tree: tpd.Tree)(using Context): Boolean = Trees.lambda(tree).isDefined

  private val StrictTransforms = Set("map", "flatMap", "filter", "filterNot", "collect")

  /** Two strict transformations in a row, reported at the second so a longer chain is reported once. */
  private def strictChain(tree: tpd.Apply)(using Context): Unit = if (looping.top) for {
    outer <- transformation(tree)
    inner <- transformation(converted(outer.qualifier)) if transformation(converted(inner.qualifier)).isEmpty
  } checks(
    Check.StrictChain,
    s"${inner.name} then ${outer.name} builds an intermediate collection; use .view or a single pass",
    tree.srcPos
  )

  // Only the application that names the transformation counts, not one that supplies its implicit arguments.
  private def transformation(tree: tpd.Tree)(using Context): Option[tpd.Select] = tree match {
    case tpd.Apply(function: (tpd.Select | tpd.TypeApply), _) => Trees.selected(function).filter(select =>
        StrictTransforms(select.name.toString) && strict(select.qualifier.tpe.widen)
      )
    case _ => None
  }

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

  private def linear(select: tpd.Select)(using Context): Unit = if (looping.top) {
    val name = select.name.toString
    if (
      (Walks(name) || Copies(name) || name == "contains") && !iterated(select.qualifier) && !fixed(select.qualifier)
    ) {
      val receiver = select.qualifier.tpe.widen
      val kind     = article(receiver.classSymbol.name.toString)
      if (receiver.derivesFrom(LinearSeq) && Walks(name))
        checks(
          Check.LinearInLoop,
          s"$name on $kind walks it on every iteration; index an IndexedSeq instead",
          select.srcPos
        )
      else if (receiver.derivesFrom(ImmutableLinear) && Copies(name))
        checks(
          Check.LinearInLoop,
          s"$name on $kind copies it on every iteration; prepend and reverse, or use a builder",
          select.srcPos
        )
      else if (name == "contains" && scanned(receiver))
        checks(Check.LinearInLoop, s"contains on $kind scans it on every iteration; use a Set", select.srcPos)
    }
  }

  // A value that names an existing collection rather than computing one, and that no iteration changes.
  private def settled(tree: tpd.Tree)(using Context): Boolean = tree match {
    case reference: tpd.Ident     => !carried(reference.symbol)
    case tpd.Select(qualifier, _) => tree.symbol.isStableMember && settled(qualifier)
    case _: tpd.This              => true
    case _                        => false
  }

  // Concatenating collections that stay the same across iterations builds the same collection each time.
  private def rebuilt(application: tpd.Apply)(using Context): Unit = if (looping.top) {
    val select = application.fun match {
      case select: tpd.Select                   => Some(select)
      case tpd.TypeApply(select: tpd.Select, _) => Some(select)
      case _                                    => None
    }
    select.filter(select =>
      Copies(select.name.toString) && strict(select.qualifier.tpe.widen) &&
        !select.qualifier.tpe.widen.derivesFrom(ImmutableLinear) &&
        (select.qualifier :: application.args).forall(operand => settled(operand) && !iterated(operand))
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

  private def boxing(application: tpd.Apply)(using Context): Unit = if (looping.top) {
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
}
