/*
rules = [CollectionChains, CollectionIdioms]
 */
// format: off
package fix

object FilterSizeComparison {
  val list = List(1, 2, 3)
  val positive = list.filter(_ > 0).size > 0// assert: CollectionIdioms.size-to-non-empty
  val none = list.filter(_ > 0).size == 0// assert: CollectionIdioms.size-to-is-empty
  val some = list.filter(_ > 0).length != 0// assert: CollectionIdioms.size-to-non-empty
  val counted = list.filter(_ > 0).size > 1// assert: CollectionChains.filter-to-count
}
