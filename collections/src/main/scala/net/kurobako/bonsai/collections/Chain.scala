package net.kurobako.bonsai.collections

/** The calls of a chain in the order they run, on `source`; `asserted` when the value of the chain is an argument of an
  * equality assertion, where a sequence and its `toSeq` are interchangeable; `hot` when it is in a class the build
  * marks hot; `yielded` when it is what the function of a generator in a for-comprehension returns, so that a
  * `desugared` map at its end is the yield of a comprehension with more than one generator.
  */
case class Chain(
    source: Arg,
    steps: Vector[Step],
    asserted: Boolean = false,
    hot: Boolean = false,
    yielded: Boolean = false
)
