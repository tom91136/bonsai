package net.kurobako.bonsai.collections

/** The scale clone detection sizes code on, which the collectors of both languages count in. A unit is one node of the
  * syntax the author wrote: a name, literal, call, operator, assignment, declaration, statement, branch or loop, with
  * what the language leaves implicit left out (types, modifiers, wrappers around statements, the lambda and
  * construction scaffolding of Scala), so equivalent code in Scala and Java weighs the same.
  */
object CloneCost {

  /** The cost at which a single expression, or a block of statements, is a copy of its own, where lighter ones only
    * form families of near duplicates.
    */
  val Expression: Int = 30

  /** The cost a run of statements must reach to be worth reporting as a copy, which is the cost of a block: a run is
    * the part of a block that recurs.
    */
  val Window: Int = Expression

  /** The fewest units that make a chain of calls worth a name. */
  val Small: Int = 6

  /** The fewest units of the whole body of a member that is a copy of it worth a name. */
  val Tiny: Int = 4

  /** The fewest units of a body that calls of it may replace. */
  val Helper: Int = 8

  /** The most units of such a body. */
  val MaximumHelper: Int = 120

  /** The most statements in a run reported as a copy. */
  private val MaximumWindow: Int = 20

  /** The runs of statements, given the cost of each, that are worth reporting as copies: every run of two or more
    * statements, no more than `MaximumWindow` of them, that reaches the window cost, as the position it starts at and
    * the position after it ends. Longer runs are kept beside the shortest, so the longest copy covers those within it.
    */
  def windows(costs: IndexedSeq[Int]): Iterator[(Int, Int)] =
    Iterator.range(0, costs.size).flatMap { start =>
      val totals = costs.slice(start, start + MaximumWindow).scanLeft(0)(_ + _)
      Iterator.range(2, totals.size).filter(totals(_) >= Window).map(length => (start, start + length))
    }
}
