package net.kurobako.baumkuchen.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import scala.collection.mutable
import scala.util.hashing.MurmurHash3

/** Finds duplicated code the way IntelliJ's duplicate locator does: subtrees and statement windows of method bodies are
  * hashed bottom-up with local definitions anonymised, and fragments sharing a hash are confirmed with a hash that
  * numbers locals by first occurrence, so `a + a` and `a + b` differ while consistent renamings match.
  */
class Fragments(using Context) {
  private case class Candidate(trees: List[tpd.Tree], cost: Int, node: String, test: Boolean)

  // Runs of statements must be long to matter, while a single dense expression is worth reporting from two lines.
  private val WindowCost      = 50
  private val WindowLines     = 4
  private val ExpressionCost  = 30
  private val ExpressionLines = 2
  private val MaximumWindow   = 20
  private val Local           = 0x5bd1e995L

  private val subtrees   = Children()
  private val candidates = mutable.LongMap.empty[mutable.ArrayBuffer[Candidate]]
  private val stable     = mutable.Map.empty[Symbol, Long]

  /** Hashes a member body owned by `node`. */
  def add(body: tpd.Tree, node: String, test: Boolean): Unit = shape(body, node, test): Unit

  /** Confirmed groups of non-overlapping copies, without groups whose copies all lie inside a larger reported copy. */
  def groups(): Vector[Clones.Group] = {
    val confirmed = candidates.values.filter(_.size > 1).toVector.flatMap { similar =>
      similar.toVector.groupBy(candidate => exact(candidate.trees)).values.flatMap { same =>
        val copies = disjoint(same.flatMap(locate(_)))
        val lines  = if (same.head.trees.size > 1) WindowLines else ExpressionLines
        Option.when(copies.size > 1 && copies.forall(_._1.lines >= lines))(same.head.cost -> copies)
      }
    }.sortBy((cost, copies) => (-cost, -copies.size))
    confirmed.foldLeft((Map.empty[String, Vector[(Int, Int)]], Vector.empty[Clones.Group])) {
      case ((reported, groups), (cost, copies)) =>
        val inside = copies.forall((fragment, span) =>
          reported.get(fragment.path).exists(_.exists((start, end) => start <= span._1 && span._2 <= end))
        )
        if (inside) (reported, groups)
        else
          (
            copies.foldLeft(reported)((seen, copy) =>
              seen.updated(copy._1.path, seen.getOrElse(copy._1.path, Vector.empty) :+ copy._2)
            ),
            groups :+ Clones.Group(cost, copies.map(_._1))
          )
    }._2
  }

  private def shape(tree: tpd.Tree, node: String, test: Boolean): (Long, Int) = {
    val children = subtrees.of(tree)
    val shapes   = children.map(shape(_, node, test))
    val cost     = weight(tree) + shapes.map(_._2).sum
    val hash     = shapes.foldLeft(own(tree, _ => Local))((hash, child) => mix(hash * 31 + child._1))
    if (cost >= ExpressionCost && tree.isTerm) keep(hash, Candidate(List(tree), cost, node, test))
    tree match {
      case tpd.Block(statements, expression) =>
        val items = (statements :+ expression).zip(shapes).toVector
        for {
          start <- items.indices
          end   <- start + 1 until (start + MaximumWindow).min(items.size)
          window = items.slice(start, end + 1)
          total  = window.map(_._2._2).sum if total >= WindowCost
        } keep(
          window.foldLeft(17L)((hash, item) => mix(hash * 31 + item._2._1)),
          Candidate(window.map(_._1).toList, total, node, test)
        )
      case _ => ()
    }
    (hash, cost)
  }

  private def keep(hash: Long, candidate: Candidate): Unit =
    candidates.getOrElseUpdate(hash, mutable.ArrayBuffer.empty) += candidate: Unit

  /** The hash of a fragment with its locals numbered in order of first occurrence. */
  private def exact(trees: List[tpd.Tree]): Long = {
    val locals                   = mutable.Map.empty[Symbol, Long]
    def go(tree: tpd.Tree): Long =
      subtrees.of(tree).foldLeft(own(tree, symbol => locals.getOrElseUpdate(symbol, locals.size + 1L)))((hash, child) =>
        mix(hash * 31 + go(child))
      )
    trees.foldLeft(17L)((hash, tree) => mix(hash * 31 + go(tree)))
  }

  private def own(tree: tpd.Tree, local: Symbol => Long): Long = {
    val symbol = tree match {
      case typeTree: tpd.TypeTree => typeTree.tpe.typeSymbol
      case other                  => other.symbol
    }
    val reference =
      if (!symbol.exists) 0L
      else if (symbol.owner.isTerm) local(symbol)
      else stable.getOrElseUpdate(symbol, identity(symbol))
    val literal = tree match {
      case tpd.Literal(constant) => constant.tag * 31L + String.valueOf(constant.value).hashCode
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
    case tpd.TypeTree() | tpd.Typed(_, _) | _: tpd.Inlined => 0
    case _                                                 => 1
  }

  private def locate(candidate: Candidate): Option[(Clones.Fragment, (Int, Int))] = {
    val (first, last) = (candidate.trees.head, candidate.trees.last)
    Option.when(first.span.exists && last.span.exists && first.source.exists) {
      val fragment = Clones.Fragment(
        candidate.node,
        first.source.path,
        first.sourcePos.startLine + 1,
        last.sourcePos.endLine + 1,
        candidate.test
      )
      fragment -> (first.span.start, last.span.end)
    }
  }

  /** Copies that do not overlap each other, as sliding windows over repeated statements would. */
  private def disjoint(copies: Vector[(Clones.Fragment, (Int, Int))]): Vector[(Clones.Fragment, (Int, Int))] =
    copies.sortBy((fragment, span) => (fragment.path, span._1)).foldLeft(Vector.empty[(Clones.Fragment, (Int, Int))]) {
      (kept, copy) =>
        val overlaps = kept.lastOption.exists((fragment, span) => fragment.path == copy._1.path && copy._2._1 < span._2)
        if (overlaps) kept else kept :+ copy
    }

  private def mix(value: Long): Long = {
    val first  = (value ^ (value >>> 33)) * 0xff51afd7ed558ccdL
    val second = (first ^ (first >>> 33)) * 0xc4ceb9fe1a85ec53L
    second ^ (second >>> 33)
  }

}
