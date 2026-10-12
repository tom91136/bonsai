/*
rule = AnonymousGiven
 */
package fix

import scala.quoted.*

object AnonymousGiven {
  case class Celsius(value: Int)
  case class Kelvin(value: Int)
  case class Fahrenheit(value: Int)

  private given ascending: Ordering[Celsius] = Ordering.by(_.value) // assert: AnonymousGiven.anonymous-given
  private given descending: Ordering[Kelvin] = Ordering.by(-_.value)
  given exported: Ordering[Fahrenheit]       = Ordering.by(-_.value)

  def sorted(values: List[Celsius]): List[Celsius] = values.sorted
  def kelvins(values: List[Kelvin]): List[Kelvin]  = values.sortWith(descending.lt)

  def max(xs: List[Celsius])(using order: Ordering[Celsius]) = xs.max // assert: AnonymousGiven.anonymous-using
  def smallest(values: List[Celsius])(using order: Ordering[Celsius]): Celsius = values.minBy(identity)(using order)
  def anonymous(values: List[Celsius])(using Ordering[Celsius]): Celsius       = values.max
  def mixed(values: List[Celsius])(using order: Ordering[Celsius], numeric: Numeric[Int]): Celsius = /* assert: AnonymousGiven.anonymous-using
                                                                   ^^^^^^^
numeric is never referenced by name; give it a clause of its own, `(using Numeric[Int])`
*/
    values.min(using order)

  def local(values: List[Kelvin]): Kelvin = {
    given byValue: Ordering[Kelvin] = Ordering.by(_.value) // assert: AnonymousGiven.anonymous-given
    values.max
  }

  def clashing(values: List[Kelvin]): Kelvin = {
    given first: Ordering[Kelvin]  = Ordering.by(_.value)
    given second: Ordering[Kelvin] = Ordering.by(-_.value)
    values.max(using first)
  }

  trait Sorter { def sort(values: List[Celsius])(using order: Ordering[Celsius]): List[Celsius] }

  class Ranking(using order: Ordering[Celsius]) { // assert: AnonymousGiven.anonymous-using
    def rank(values: List[Celsius]): List[Celsius] = values.sorted
  }
  class Explicit(using order: Ordering[Celsius]) {
    def rank(values: List[Celsius]): List[Celsius] = values.sorted(using order)
  }
  class Field(using val order: Ordering[Celsius])

  object Macros {
    def describeImpl[A: Type](base: Expr[Ordering[A]])(using Quotes): Expr[String] = Expr("ordering")
    inline def describe[A](using base: Ordering[A]): String                        = ${ describeImpl[A]('base) }
  }
}
