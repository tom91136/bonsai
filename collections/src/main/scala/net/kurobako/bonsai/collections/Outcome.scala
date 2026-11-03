package net.kurobako.bonsai.collections

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
