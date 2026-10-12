/*
rule = CollectionChains
 */
// format: off
package fix

object CollectionChains {
  val lists = List(List(1, 2), List(3))
  val list  = List(1, 2, 3)
  val map   = Map(1 -> "a", 2 -> "b")
  val pairs = List((1, "a"), (2, "b"))
  val triples = List((1, "a", true), (2, "b", false))

  val flattened = lists.map(_.map(_ * 2)).flatten// assert: CollectionChains.map-flatten-to-flat-map
  val flattenedIdentity = lists.flatMap(identity)// assert: CollectionChains.flat-map-identity-to-flatten
  val count = list.filter(_ > 0).size// assert: CollectionChains.filter-to-count
  val length = list.filter(_ > 0).length// assert: CollectionChains.filter-to-count
  val defined = list.find(_ > 0).isDefined// assert: CollectionChains.find-to-exists
  val missing = list.find(_ > 0).isEmpty// assert: CollectionChains.find-to-exists
  val compared = list.find(_ > 0) != None// assert: CollectionChains.find-to-exists
  val tail = list.reverse.take(2).reverse// assert: CollectionChains.reverse-take-to-take-right
  val keys = map.map(_._1)// assert: CollectionChains.map-to-keys
  val values = map.map(_._2)// assert: CollectionChains.map-to-values
  val distinctSeq = list.toSet.toSeq// assert: CollectionChains.to-set-and-back-to-distinct
  val distinctList = list.toSet.toList// assert: CollectionChains.to-set-and-back-to-distinct
  val indexed = list.zip(list.indices)// assert: CollectionChains.zip-indices-to-zip-with-index
  val range = 0 until list.size// assert: CollectionChains.range-to-indices
  val inclusive = 0 to list.length - 1// assert: CollectionChains.range-to-indices
  val first = pairs.unzip._1// assert: CollectionChains.unzip-to-map
  val second = pairs.unzip._2// assert: CollectionChains.unzip-to-map
  val middle = triples.unzip3._2// assert: CollectionChains.unzip-to-map
  val allPositive = list.foldLeft(true)((acc, value) => acc && value > 0)// assert: CollectionChains.fold-true-and-to-forall

  val emptiness = list.filter(_ > 0).isEmpty// assert: CollectionChains.filter-emptiness
  val presence = list.filterNot(_ > 0).nonEmpty// assert: CollectionChains.filter-emptiness
  val head = list.filter(_ > 0).headOption// assert: CollectionChains.filter-head-option
  val collected = list.collect { case value if value > 0 => value }.headOption// assert: CollectionChains.collect-head-option
  val any = list.map(_ > 0).contains(true)// assert: CollectionChains.map-contains
  val last = list.reverse.find(_ > 0)// assert: CollectionChains.reverse-find
  val smallest = list.sorted.head// assert: CollectionChains.sorted-min-max
  val largest = list.sorted.last// assert: CollectionChains.sorted-min-max
  val longest = List("a", "bb").sortBy(_.length).last// assert: CollectionChains.sorted-min-max
  val option = list.sorted.headOption// assert: CollectionChains.sorted-min-max

  class ApplicationMethods {
    def filter(predicate: Int => Boolean): ApplicationMethods = this
    def map[A](function: Int => A): ApplicationMethods        = this
    def reverse: ApplicationMethods                           = this
    def take(n: Int): ApplicationMethods                      = this
    def sorted: ApplicationMethods                            = this
    def find(predicate: Int => Boolean): ApplicationMethods   = this
    def size: Int                                             = 0
    def head: Int                                             = 0
    def headOption: Option[Int]                               = None
    def toSet: ApplicationMethods                             = this
    def toSeq: ApplicationMethods                             = this
  }
  val pairList = List((1, "a"))
  val custom = new ApplicationMethods
  val customCount = custom.filter(_ > 0).size
  val customReverse = custom.reverse.take(2).reverse
  val customSorted = custom.sorted.head
  val customFind = custom.find(_ > 0).headOption
  val customSet = custom.toSet.toSeq
  val customMap = pairList.map(_._1)
  val customZip = list.zip(List(1))
  val unrelatedRange = 0 until 3
  val foldFalse = list.foldLeft(false)((acc, value) => acc || value > 0)
}
