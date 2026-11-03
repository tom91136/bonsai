package net.kurobako.bonsai.collections

/** An argument of a step, or the receiver of a chain, with what its front end knows of it. `pure` holds when calling it
  * has no effect a rewrite could reorder, repeat or drop; front ends say so only when they know.
  */
trait Arg {
  def text: String
  def pure: Boolean
  def identity: Boolean = false

  /** The number of the tuple component the function selects, so `_._1` is `Some(1)`. */
  def projection: Option[Int] = None
  def literal: Option[Any]    = None

  /** The equality test against a constant the function is, if it is one. */
  def comparison: Option[Comparison] = None

  /** The source of the set whose `contains` the function is, if it is one. */
  def membership: Option[String] = None

  /** The call of this function on `value`, written as source. */
  def applied(value: String): String
}
