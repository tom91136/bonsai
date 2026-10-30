package net.kurobako.baumkuchen.analysis

import net.kurobako.baumkuchen.java.{Model => JavaModel}
import scala.collection.Seq

/** Groups the fragments of the Java code of a build into the copies of the code that recur, the way the fragments of
  * the Scala code are grouped: the fragments that share a hash are copies of one another, and the fragments that also
  * share a hash once the values of the literals are ignored are near copies, which differ in the literals named as
  * holes.
  *
  * The grouping is done here, and not by the javac plugin, as the copies of the code of one module are of the code of
  * the whole build: the same block of code written in two modules of it is as much a copy as one written twice in one.
  */
object JavaFragments {

  // A copy must be long enough to be worth reporting.
  private val MinimumLines = 4
  // Each hole becomes a parameter of the shared method, which pays when enough code comes with it.
  private val MaximumHoles = 3
  private val HoleCost     = 16

  /** A fragment of the Java code, with the flag that tells the code of the test sources from the rest. */
  private case class Found(fragment: JavaModel.Fragment, test: Boolean)

  /** The groups of copies the fragments of the Java code make, which the rules read alike with those of the Scala code.
    */
  def groups(fragments: Seq[(JavaModel.Fragment, Boolean)]): Vector[Clones.Group] = {
    val found = fragments.toVector.map((fragment, test) => Found(fragment, test))
    val exact = found.groupBy(_.fragment.exact).values.toVector.filter(_.size > 1).map(copies => disjoint(copies))
      .filter(copies => copies.size > 1 && copies.forall(copy => lines(copy) >= MinimumLines))
      .map(copies => (copies.head.fragment.cost, copies, Vector.empty[Vector[String]]))
    val near = found.groupBy(_.fragment.loose).values.toVector.filter(_.size > 1).map(copies => disjoint(copies))
      .filter(copies => copies.size > 1 && copies.forall(copy => lines(copy) >= MinimumLines))
      .flatMap { copies =>
        val holes = differing(copies)
        Option.when(holes.nonEmpty &&
          (holes.size <= MaximumHoles || holes.size * HoleCost <= copies.head.fragment.cost))(
          (copies.head.fragment.cost, copies, holes)
        )
      }
    report(exact)((outer, inner) => outer.path == inner.path && outer.from <= inner.from && inner.to <= outer.to) ++
      report(near)((outer, inner) => outer.path == inner.path && outer.from < inner.to && inner.from < outer.to)
  }

  /** The copies of one fragment, of which the ones that lie within each other are one copy, as the runs of statements a
    * block holds overlap each other.
    */
  private def disjoint(copies: Vector[Found]): Vector[Found] =
    copies.sortBy(copy => (copy.fragment.path, copy.fragment.from)).foldLeft(Vector.empty[Found]) { (kept, copy) =>
      val overlaps =
        kept.lastOption.exists(last =>
          last.fragment.path == copy.fragment.path && copy.fragment.from < last.fragment.to
        )
      if overlaps then kept else kept :+ copy
    }

  /** The literals the copies of a fragment differ in, which are the holes of a near duplicate. */
  private def differing(copies: Vector[Found]): Vector[Vector[String]] = {
    val literals = copies.map(_.fragment.literals)
    if literals.exists(_.size != literals.head.size) then Vector.empty
    else literals.transpose.map(_.distinct).filter(_.size > 1).distinct
  }

  /** The groups of copies, the largest first, without those whose every copy lies within a copy reported before. */
  private def report(groups: Vector[(Int, Vector[Found], Vector[Vector[String]])])(
      covers: (JavaModel.Fragment, JavaModel.Fragment) => Boolean
  ): Vector[Clones.Group] = {
    val reported =
      groups.sortBy((cost, copies, _) => (-cost, -copies.size)).foldLeft(Vector.empty[(Vector[Found], Clones.Group)]) {
        (reported, group) =>
          val (cost, copies, holes) = group
          val covered               = copies.forall(copy =>
            reported.exists((others, _) => others.exists(other => covers(other.fragment, copy.fragment)))
          )
          if covered then reported
          else
            reported :+
              (copies -> Clones.Group(
                cost,
                copies.map(copy =>
                  Clones.Fragment(
                    copy.fragment.node,
                    copy.fragment.path,
                    copy.fragment.start,
                    copy.fragment.end,
                    copy.test
                  )
                ),
                holes
              ))
      }
    reported.map(_._2)
  }

  /** The number of the lines a fragment of code holds, which is what makes it worth reporting. */
  private def lines(copy: Found): Int = copy.fragment.end - copy.fragment.start + 1
}
