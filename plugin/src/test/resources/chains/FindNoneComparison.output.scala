// format: off
package fix

object FindNoneComparison {
  val list = List(1, 2, 3)
  val compared = list.exists(_ > 0)
}
