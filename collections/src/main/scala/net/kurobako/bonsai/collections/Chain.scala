package net.kurobako.bonsai.collections

/** What the receiver or the result of a step is, as far as the laws care: `library` for a standard collection rather
  * than a user type with the same method names, `strict` unless it is a view, an iterator or a lazy list, `ordered`
  * when it keeps an order, `distinct` for a set, `keyed` for a map, `single` for an option and `sorted` when a
  * comparator rather than equality decides which elements are the same. The `name` is the standard type the value has
  * when the front end knows it exactly, written below `scala.collection` such as `immutable.List`, `Seq` or `Array`,
  * and empty otherwise.
  */
case class Kind(
    library: Boolean,
    strict: Boolean = true,
    ordered: Boolean = true,
    distinct: Boolean = false,
    keyed: Boolean = false,
    single: Boolean = false,
    name: String = "",
    sorted: Boolean = false
)

/** A function that tests its parameter for equality with a value (`equal`) or for inequality, where `constant` is the
  * source of a value that is the same on every call, cannot throw and does not mention the parameter, and `assignable`
  * when its type conforms to the parameter's, so a method that takes the element accepts it.
  */
case class Comparison(equal: Boolean, constant: String, assignable: Boolean = true)

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

/** The calls of a chain in the order they run, on `source`; `asserted` when the value of the chain is an argument of an
  * equality assertion, where a sequence and its `toSeq` are interchangeable.
  */
case class Chain(source: Arg, steps: Vector[Step], asserted: Boolean = false)

/** What a law says of its steps in Java: the `message`, and the `render` of the expression that replaces them from the
  * text of their receiver, when the law has an exact rewrite in Java.
  */
case class Java(message: String, render: Option[String => String])

/** What a law found for the steps from `from` until `until`, by the law's `id`. The compiler plugin reports what is
  * `lint`, and the whole-build analysis what is `analysis`; the javac plugin reports what has a `java` form.
  */
enum Outcome {
  def law: String
  def from: Int
  def until: Int
  def message: String
  def lint: Boolean
  def analysis: Boolean
  def java: Option[Java]

  /** An exact replacement: `render` gives the expression that replaces the steps, from the text of their receiver. */
  case Rewrite(
      law: String,
      from: Int,
      until: Int,
      message: String,
      render: String => String,
      lint: Boolean = true,
      analysis: Boolean = false,
      java: Option[Java] = None
  )

  /** An improvement whose conditions do not all hold, or that changes how the code evaluates, so it is reported. A
    * report that is `viewable` finds steps a view runs in one pass, and is `pure` when none of their functions has an
    * effect that running the steps in turn on each element would reorder.
    */
  case Report(
      law: String,
      from: Int,
      until: Int,
      message: String,
      lint: Boolean = true,
      analysis: Boolean = false,
      viewable: Boolean = false,
      pure: Boolean = false,
      java: Option[Java] = None
  )
}
