// format: off
package fix

object FilterSizeComparison {
  val list = List(1, 2, 3)
  val positive = list.filter(_ > 0).size > 0// assert: size-to-non-empty
  val none = list.filter(_ > 0).size == 0// assert: size-to-is-empty
  val some = list.filter(_ > 0).length != 0// assert: size-to-non-empty
  val rejected = list.filterNot(_ > 0).size > 0// assert: size-to-non-empty
  val all = list.filterNot(_ > 0).size == 0// assert: size-to-is-empty
  val notAll = list.filterNot(_ > 0).length != 0// assert: size-to-non-empty
  val counted = list.filter(_ > 0).size > 1// assert: filter-to-count
}
