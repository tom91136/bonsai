/*
rules = DisableSyntax
DisableSyntax.noAsInstanceOf = true
DisableSyntax.noIsInstanceOf = true
 */
package fix

object NoTypeCasting {
  val value: Any = 1
  val tested     = value.isInstanceOf[Int] // assert: DisableSyntax.isInstanceOf
  val cast       = value.asInstanceOf[Int] // assert: DisableSyntax.asInstanceOf
  val matched    = value match {
    case integer: Int => Some(integer)
    case _            => None
  }
}
