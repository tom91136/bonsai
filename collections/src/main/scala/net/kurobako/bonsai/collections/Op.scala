package net.kurobako.bonsai.collections

/** A method of the collection chains: its `shape`, what it costs on a strict receiver and what it costs `lazily`, on a
  * lazy receiver, where a transformer defers its work to the step that forces it and a terminal does the work it left
  * behind. A `hashed` cost, when there is one, is what the method costs on a keyed or distinct receiver, which it looks
  * up rather than searches. On a single receiver, which holds at most one element, every method costs nothing.
  */
case class Op(shape: Shape, cost: Cost, lazily: Cost, hashed: Option[Cost] = None) {
  def elementWise: Boolean = shape == Shape.Map || shape == Shape.Filter || shape == Shape.Flat
}
