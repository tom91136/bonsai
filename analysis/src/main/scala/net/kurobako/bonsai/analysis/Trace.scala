package net.kurobako.bonsai.analysis

/** What a method does to objects it may not own, found by following values through its locals and stack. The vectors
  * hold the origin of the receiver of each invocation and of the holder of each field access, in the order of
  * `ClassFiles.Method.invocations` and `ClassFiles.Method.fields`. `exceptional` marks the invocations that only lead
  * to a throw, `loops` a backward jump on a path that returns, and `allocates` an array created on such a path.
  */
private[analysis] case class Trace(
    receivers: Vector[Origin],
    exceptional: Vector[Boolean],
    holders: Vector[Origin],
    stores: Set[Origin],
    loads: Set[Origin],
    locked: Set[Origin],
    throws: Boolean,
    loops: Boolean,
    allocates: Boolean
)
