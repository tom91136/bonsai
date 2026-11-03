package net.kurobako.bonsai.collections

/** Where the value of an expression ends up: the outermost loop, by depth, whose turns it stays live across, whether
  * the method returns it, and whether it is stored in a field or a variable of an outer method.
  */
case class Sink(level: Int, returned: Boolean, stored: Boolean) {
  def escapes: Boolean = returned || stored

  def join(that: Sink): Sink = Sink(level.min(that.level), returned || that.returned, stored || that.stored)

  /** The sink of what a traversal accumulates over its turns, the loop at `depth` being its own. */
  def accumulated(depth: Int): Sink = copy(level = level.min(depth))
}

object Sink {
  val Dropped: Sink  = Sink(Int.MaxValue, returned = false, stored = false)
  val Returned: Sink = Sink(0, returned = true, stored = false)
  val Stored: Sink   = Sink(0, returned = false, stored = true)
}
