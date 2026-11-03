package net.kurobako.bonsai.collections

import scala.annotation.targetName

object Cost {

  /** Whether `after` is no worse than `before` in every component. */
  def neverWorse(before: Cost, after: Cost): Boolean = after.measures.zip(before.measures).forall(_ <= _)

  /** Whether `after` is no worse than `before` in every component and better in one. */
  def improves(before: Cost, after: Cost): Boolean =
    neverWorse(before, after) && after.measures.zip(before.measures).exists(_ < _)

  /** The gain of `after` over `before` in passes and intermediate collections, such as "two passes and an intermediate
    * collection become one pass".
    */
  def gain(before: Cost, after: Cost): String =
    s"${passes(before.passes)} and ${intermediates(before.allocations - after.allocations)} become ${passes(after.passes)}"

  private val words = Vector("no", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")

  private def number(n: Int): String = words.applyOrElse(n, (_: Int).toString)

  private def passes(n: Int): String = s"${number(n)} ${if (n == 1) "pass" else "passes"}"

  private def intermediates(n: Int): String =
    if (n == 1) "an intermediate collection" else s"${number(n)} intermediate collections"
}

/** What running steps costs: `passes` over the elements, `allocations` of collections, how many of the passes stop
  * `early` at a deciding element, `sorts` of n log n steps and `scans`, linear searches made per element, which make
  * the work quadratic.
  */
case class Cost(passes: Int = 0, allocations: Int = 0, early: Int = 0, sorts: Int = 0, scans: Int = 0) {
  @targetName("plus")
  def +(that: Cost): Cost =
    Cost(
      passes + that.passes,
      allocations + that.allocations,
      early + that.early,
      sorts + that.sorts,
      scans + that.scans
    )

  /** The passes that always run to the end. */
  def full: Int = passes - early

  private def measures: Vector[Int] = Vector(passes, allocations, full, sorts, scans)
}
