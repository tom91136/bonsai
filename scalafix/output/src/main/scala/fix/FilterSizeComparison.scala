// format: off
package fix

object FilterSizeComparison {
  val list = List(1, 2, 3)
  val positive = list.filter(_ > 0).nonEmpty
  val none = list.filter(_ > 0).isEmpty
  val some = list.filter(_ > 0).nonEmpty
  val counted = list.count(_ > 0) > 1
}
