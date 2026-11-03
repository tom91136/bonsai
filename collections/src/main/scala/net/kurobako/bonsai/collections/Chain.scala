package net.kurobako.bonsai.collections

/** The calls of a chain in the order they run, on `source`; `asserted` when the value of the chain is an argument of an
  * equality assertion, where a sequence and its `toSeq` are interchangeable.
  */
case class Chain(source: Arg, steps: Vector[Step], asserted: Boolean = false)
