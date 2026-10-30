package net.kurobako.baumkuchen.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import scala.collection.mutable
import scala.util.hashing.MurmurHash3

/** Finds duplicated code the way IntelliJ's duplicate locator does: subtrees and statement windows of method bodies are
  * hashed bottom-up with local definitions anonymised, and fragments sharing a hash are confirmed with a hash that
  * numbers locals by first occurrence, so `a + a` and `a + b` differ while consistent renamings match. Fragments that
  * also match once the values of literals are ignored are near duplicates, reported with the literals that differ as
  * holes.
  */
class Fragments(using Context) {

  /** Where a fragment comes from, as `Clones.Fragment` describes it. */
  private case class Origin(node: String, test: Boolean, family: String, specialised: Boolean, method: Boolean)

  private case class Candidate(trees: List[tpd.Tree], cost: Int, origin: Origin, shape: Long)
  private case class Copy(fragment: Clones.Fragment, span: (Int, Int), trees: List[tpd.Tree], method: Boolean)

  // Runs of statements must be long to matter, while a single dense expression is worth reporting from two lines.
  private val WindowCost      = 50
  private val WindowLines     = 4
  private val ExpressionCost  = 30
  private val ExpressionLines = 2
  private val MaximumWindow   = 20
  // Short bodies alike but for numbers are a family of methods, one per width or mask, once there are three; short
  // bodies alike but for strings are a table of names or messages.
  private val FamilyCost   = 12
  private val FamilyCopies = 3
  // Each hole becomes a parameter of the shared method, which pays when enough code comes with it, while rows of a
  // table differ in most of their literals.
  private val MaximumHoles = 3
  private val HoleCost     = 16
  private val Local        = 0x5bd1e995L
  // Many fragments alike but for their literals are a table of rows, not copies a shared method would replace, and
  // grouping them grows with the square of the rows.
  private val MaximumNear = 32

  private val subtrees   = Children()
  private val candidates = mutable.LongMap.empty[mutable.ArrayBuffer[Candidate]]
  private val stable     = mutable.Map.empty[Symbol, Long]

  /** Hashes a member body owned by `node`; a method's body may form a family with others alike but for numbers, while a
    * value's initialiser, such as an array of some size, is no method a family would share.
    */
  def add(body: tpd.Tree, node: String, test: Boolean, method: Boolean, family: String, specialised: Boolean): Unit =
    if (!row(body)) shape(body, Origin(node, test, family, specialised, method), whole = method): Unit

  /** Confirmed groups of non-overlapping copies, without groups whose copies all lie inside a larger reported copy,
    * then near duplicates that no larger near duplicate overlaps.
    */
  def groups(): Vector[Clones.Group] = {
    val similar = candidates.values.filter(_.size > 1).map(_.toVector).toVector
    val exact   = similar.flatMap(_.filter(_.cost >= ExpressionCost).groupBy(_.shape).values.filter(_.size > 1))
      .flatMap(confirm(_, Literals.Valued))
    val near = similar.filter(_.size <= MaximumNear).flatMap(confirm(_, Literals.Holes)).flatMap { (cost, copies) =>
      val holes = copies.map(copy => literals(copy.trees)).transpose.filter(_.distinct.size > 1).distinct
      Option.when(parameterised(cost, copies, holes))((cost, copies, holes.map(_.distinct.map(_.show))))
    }
    report(exact.map((cost, copies) => (cost, copies, Vector.empty)))((outer, inner) =>
      outer._1 <= inner._1 && inner._2 <= outer._2
    ) ++ report(near)((outer, inner) => outer._1 < inner._2 && inner._1 < outer._2)
  }

  /** Whether literals are hashed by value, as exact duplicates need, or as holes near duplicates may fill. */
  private enum Literals {
    case Valued, Holes
  }

  private def confirm(similar: Vector[Candidate], literals: Literals): Iterable[(Int, Vector[Copy])] =
    similar.groupBy(candidate => exact(candidate.trees, literals == Literals.Valued)).values.flatMap { same =>
      val copies = disjoint(same.flatMap(locate(_)))
      val lines  =
        if (same.head.trees.size > 1) WindowLines else if (same.head.cost >= ExpressionCost) ExpressionLines else 1
      Option.when(copies.size > 1 && copies.forall(_.fragment.lines >= lines))(same.head.cost -> copies)
    }

  /** The largest groups first, without those whose every copy is covered by a copy reported before. */
  private def report(confirmed: Vector[(Int, Vector[Copy], Vector[Vector[String]])])(
      covers: ((Int, Int), (Int, Int)) => Boolean
  ): Vector[Clones.Group] =
    confirmed.sortBy((cost, copies, _) => (-cost, -copies.size))
      .foldLeft((Map.empty[String, Vector[(Int, Int)]], Vector.empty[Clones.Group])) {
        case ((reported, groups), (cost, copies, holes)) =>
          val covered = copies.forall(copy => reported.get(copy.fragment.path).exists(_.exists(covers(_, copy.span))))
          if (covered) (reported, groups)
          else
            (
              copies.foldLeft(reported)((seen, copy) =>
                seen.updated(copy.fragment.path, seen.getOrElse(copy.fragment.path, Vector.empty) :+ copy.span)
              ),
              groups :+ Clones.Group(cost, copies.map(_.fragment), holes, mapping = isMap(copies))
            )
      }._2

  private def isMap(copies: Vector[Copy]): Boolean =
    copies.exists(_.trees.exists(hasMap))

  private def hasMap(tree: tpd.Tree): Boolean = {
    val tpe = tree.tpe
    (tpe != null &&
      tpe.widen.baseClasses.exists(cls => cls.name.toString == "Map" || cls.name.toString.endsWith("Map"))) ||
    subtrees.of(tree).exists(hasMap)
  }

  private def shape(tree: tpd.Tree, origin: Origin, whole: Boolean, pattern: Boolean = false): (Long, Long, Int) = {
    val children = subtrees.of(tree)
    val shapes   = tree match {
      case tpd.CaseDef(pat, _, _) => children.map(child => shape(child, origin, whole = false, pattern = child eq pat))
      case _                      => children.map(shape(_, origin, whole = false, pattern))
    }
    val cost  = weight(tree) + shapes.map(_._3).sum
    val hash  = combine(own(tree, None, values = true), shapes.map(_._1))
    val loose = combine(own(tree, None, values = false), shapes.map(_._2))
    if (!pattern && (cost >= ExpressionCost || whole && cost >= FamilyCost) && tree.isTerm)
      keep(loose, Candidate(List(tree), cost, origin, hash))
    tree match {
      case tpd.Block(statements, expression) =>
        val items = (statements :+ expression).zip(shapes).toVector
        for {
          start <- items.indices
          end   <- start + 1 until (start + MaximumWindow).min(items.size)
          window = items.slice(start, end + 1)
          total  = window.map(_._2._3).sum if total >= WindowCost
        } keep(
          combine(17L, window.map(_._2._2)),
          Candidate(window.map(_._1).toList, total, origin, combine(17L, window.map(_._2._1)))
        )
      case _ => ()
    }
    (hash, loose, cost)
  }

  private def keep(hash: Long, candidate: Candidate): Unit =
    candidates.getOrElseUpdate(hash, mutable.ArrayBuffer.empty) += candidate: Unit

  /** The hash of a fragment with its locals numbered in order of first occurrence. */
  private def exact(trees: List[tpd.Tree], values: Boolean): Long = {
    val locals                   = mutable.Map.empty[Symbol, Long]
    def go(tree: tpd.Tree): Long =
      combine(
        own(tree, Some(locals), values),
        subtrees.of(tree).map(go(_))
      )
    combine(17L, trees.map(go(_)))
  }

  private def parameterised(cost: Int, copies: Vector[Copy], holes: Vector[Vector[Constant]]): Boolean =
    holes.nonEmpty &&
      (holes.size <= MaximumHoles || holes.size * HoleCost <= cost) &&
      (cost >= ExpressionCost || copies.size >= FamilyCopies && holes.flatten.forall(_.tag != Constants.StringTag)) &&
      !called(copies)

  // Calls of one function that differ only in the literals they pass to it, or in the strings they pass anywhere,
  // already share that function.
  private def called(copies: Vector[Copy]): Boolean =
    copies.forall(copy => !copy.method && applications(copy.trees)) || sharedCallee(copies)

  private def applications(trees: List[tpd.Tree]): Boolean = trees match {
    case List(tree) =>
      Predicates.unwrap(tree) match {
        case tpd.Apply(_, _)             => true
        case tpd.SeqLiteral(elements, _) => elements.forall(Predicates.unwrap(_).isInstanceOf[tpd.Apply])
        case _                           => false
      }
    case _ => false
  }

  private def sharedCallee(copies: Vector[Copy]): Boolean =
    copies.map(_.trees match {
      case List(tree) =>
        Predicates.unwrap(tree) match {
          case application: tpd.Apply =>
            val computed = application.args.filterNot(Predicates.unwrap(_).isInstanceOf[tpd.Literal])
            Some(literals(application.fun :: computed).collect {
              case constant if constant.tag != Constants.StringTag => constant.show
            })
          case _ => None
        }
      case _ => None
    }).distinct match {
      case Vector(Some(_)) => true
      case _               => false
    }

  /** The literals that may differ between near duplicates, in the order `exact` visits them. */
  private def literals(trees: List[tpd.Tree]): Vector[Constant] = trees.toVector.flatMap {
    case tpd.Literal(constant) if hole(constant) => Vector(constant)
    // an enum case passes its ordinal and name to `$new`, which the compiler writes
    case tpd.Apply(fun, _) if fun.symbol.name.toString == "$new" => Vector.empty
    case tree                                                    => literals(subtrees.of(tree))
  }

  // A class literal names a type, which is as much part of the fragment as a reference is.
  private def hole(constant: Constant): Boolean = constant.tag != Constants.ClazzTag

  /** The hash of `tree` itself; `locals` numbers the symbols local to a fragment in order of first occurrence, which
    * are otherwise all alike.
    */
  private def own(tree: tpd.Tree, locals: Option[mutable.Map[Symbol, Long]], values: Boolean): Long = {
    val symbol = tree match {
      case typeTree: tpd.TypeTree => typeTree.tpe.typeSymbol
      case other                  => other.symbol
    }
    val reference =
      if (!symbol.exists) 0L
      else if (symbol.owner.isTerm) locals.fold(Local)(numbers => numbers.getOrElseUpdate(symbol, numbers.size + 1L))
      else stable.getOrElseUpdate(symbol, identity(symbol))
    val literal = tree match {
      case tpd.Literal(constant) if values || !hole(constant) =>
        constant.tag * 31L + String.valueOf(constant.value).hashCode
      case tpd.Literal(constant) => constant.tag.toLong
      case _                     => 0L
    }
    mix(mix(tree.getClass.getName.hashCode.toLong * 31 + reference) * 31 + literal)
  }

  // Overloads share a full name, so methods also hash their signature.
  private def identity(symbol: Symbol): Long = {
    val name = symbol.fullName.toString + (if (symbol.is(Flags.Method)) symbol.signature.toString else "")
    (MurmurHash3.stringHash(name).toLong << 32) | (name.hashCode & 0xffffffffL)
  }

  private def weight(tree: tpd.Tree): Int = tree match {
    case tpd.TypeTree() | tpd.Typed(_, _) | tpd.Inlined(_, _, _) => 0
    case _                                                       => 1
  }

  // An enum case without written members is a row of constructor arguments, which its compiler-written members repeat.
  private def row(tree: tpd.Tree): Boolean = tree match {
    case tpd.Block(List(definition @ tpd.TypeDef(_, template: tpd.Template)), _) =>
      definition.symbol.asClass.parentSyms.exists(_.is(Flags.Enum)) && template.body.forall(_.span.isSynthetic)
    case _ => false
  }

  // A for comprehension places a function's parameter at its generator, before the collection it ranges over, so a
  // function is found where its body is.
  private def located(tree: tpd.Tree): tpd.Tree = tree match {
    case tpd.Block(List(definition: tpd.DefDef), tpd.Closure(_, _, _)) => definition.rhs
    case definition: tpd.DefDef                                        => definition.rhs
    case other                                                         => other
  }

  private def locate(candidate: Candidate): Option[Copy] = {
    val (first, last) = (located(candidate.trees.head), located(candidate.trees.last))
    Option.when(first.span.exists && last.span.exists && first.source.exists) {
      val fragment = Clones.Fragment(
        candidate.origin.node,
        first.source.path,
        first.sourcePos.startLine + 1,
        last.sourcePos.endLine + 1,
        candidate.origin.test,
        candidate.origin.family,
        candidate.origin.specialised
      )
      Copy(fragment, (first.span.start, last.span.end), candidate.trees, candidate.origin.method)
    }
  }

  /** Copies that do not overlap each other, as sliding windows over repeated statements would. */
  private def disjoint(copies: Vector[Copy]): Vector[Copy] =
    copies.sortBy(copy => (copy.fragment.path, copy.span._1)).foldLeft(Vector.empty[Copy]) { (kept, copy) =>
      val overlaps =
        kept.lastOption.exists(last => last.fragment.path == copy.fragment.path && copy.span._1 < last.span._2)
      if (overlaps) kept else kept :+ copy
    }

  private def combine(seed: Long, hashes: Iterable[Long]): Long =
    hashes.foldLeft(seed)((hash, child) => mix(hash * 31 + child))

  private def mix(value: Long): Long = {
    val first  = (value ^ (value >>> 33)) * 0xff51afd7ed558ccdL
    val second = (first ^ (first >>> 33)) * 0xc4ceb9fe1a85ec53L
    second ^ (second >>> 33)
  }

}
