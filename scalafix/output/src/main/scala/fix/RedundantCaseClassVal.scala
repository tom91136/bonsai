// format: off
package fix

object RedundantCaseClassVal {
  case class Plain(value: Int)
  case class Curried(value: Int)(val extra: Int)
  case class Hidden(private val value: Int)
  case class Mutable(var value: Int)
  case class Annotated(@deprecated("fixture", "1") val value: Int)
  class Ordinary(val value: Int)
}
