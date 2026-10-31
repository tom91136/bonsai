// format: off
package fix

object FilterSizeComparison {
  val list = List(1, 2, 3)
  val positive = list.exists(_ > 0)
  val none = !list.exists(_ > 0)
  val some = list.exists(_ > 0)
  val rejected = !list.forall(_ > 0)
  val all = list.forall(_ > 0)
  val notAll = !list.forall(_ > 0)
  val counted = list.count(_ > 0) > 1
}
