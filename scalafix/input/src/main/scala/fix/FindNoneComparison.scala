/*
rules = [CollectionChains, OptionIdioms]
 */
// format: off
package fix

object FindNoneComparison {
  val list = List(1, 2, 3)
  val compared = list.find(_ > 0) != None// assert: CollectionChains.find-to-exists
}
