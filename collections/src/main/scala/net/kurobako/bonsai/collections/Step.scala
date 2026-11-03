package net.kurobako.bonsai.collections

/** A call in a chain: `method` with its arguments, on a receiver of one kind, giving a result of another. A comparison
  * that ends a chain, such as `== 0` or `!= None`, is lowered by the front end to the method it states, such as
  * `isEmpty`. Other forms are lowered to the pseudo methods that `Ops` keys: a tuple component, `0 until xs.size` and
  * `0 to xs.size - 1` (both on a chain from `xs`, with no arguments), and `foldLeft(true)((acc, x) => acc && p(x))`
  * with the predicate `x => p(x)` as its one argument. A size or a count compared with zero takes the predicate as its
  * one argument for a count, a comparison with `None` or `Some(x)` takes `x` as its argument for `Some`, and
  * `xs(xs.size - 1)` and `xs.lift(xs.size - 1)` are the last element. The operator of the comparison picks its key. A
  * `typed` step was written with explicit type arguments, which a rewrite of it would drop; a `desugared` step is a
  * call the compiler wrote for a for-comprehension, which has no text of its own to rewrite.
  */
case class Step(
    method: String,
    args: Vector[Arg],
    receiver: Kind,
    result: Kind,
    typed: Boolean = false,
    desugared: Boolean = false
)
