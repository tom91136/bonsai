// format: off
package fix

object CollectionChains {
  val lists = List(List(1, 2), List(3))
  val list  = List(1, 2, 3)
  val map   = Map(1 -> "a", 2 -> "b")
  val pairs = List((1, "a"), (2, "b"))
  val triples = List((1, "a", true), (2, "b", false))

  val flattened = lists.map(_.map(_ * 2)).flatten// assert: map-flatten-to-flat-map
  val flattenedIdentity = lists.flatMap(identity)// assert: flat-map-identity-to-flatten
  val count = list.filter(_ > 0).size// assert: filter-to-count
  val length = list.filter(_ > 0).length// assert: filter-to-count
  val defined = list.find(_ > 0).isDefined// assert: find-to-exists
  val missing = list.find(_ > 0).isEmpty// assert: find-to-exists
  val compared = list.find(_ > 0) != None// assert: find-to-exists
  val tail = list.reverse.take(2).reverse// assert: reverse-take-to-take-right
  val keys = map.map(_._1)// assert: map-to-keys
  val values = map.map(_._2)// assert: map-to-values
  val distinctSeq = list.toSet.toSeq// assert: to-set-and-back-to-distinct
  val distinctList = list.toSet.toList// assert: to-set-and-back-to-distinct
  val indexed = list.zip(list.indices)// assert: zip-indices-to-zip-with-index
  val range = 0 until list.size// assert: range-to-indices
  val inclusive = 0 to list.length - 1// assert: range-to-indices
  val first = pairs.unzip._1// assert: unzip-to-map
  val second = pairs.unzip._2// assert: unzip-to-map
  val middle = triples.unzip3._2// assert: unzip-to-map
  val allPositive = list.foldLeft(true)((acc, value) => acc && value > 0)// assert: fold-true-and-to-forall

  val emptiness = list.filter(_ > 0).isEmpty// assert: filter-emptiness
  val presence = list.filterNot(_ > 0).nonEmpty// assert: filter-emptiness
  val head = list.filter(_ > 0).headOption// assert: filter-head-option
  val collected = list.collect { case value if value > 0 => value }.headOption// assert: collect-head-option
  val doubled = list.filter(_ > 0).map(_ * 2)// assert: filter-map-to-collect
  val rejected = list.filterNot(_ > 0).map(_ * 2)// assert: filter-map-to-collect
  val chosen = Option(1).filter(_ > 0).map(_ * 2)// assert: filter-map-to-collect
  val lazily = list.withFilter(_ > 0).map(_ * 2)
  val any = list.map(_ > 0).contains(true)// assert: map-contains
  val last = list.reverse.find(_ > 0)// assert: reverse-find
  val smallest = list.sorted.head// assert: sorted-min-max
  val largest = list.sorted.last// assert: sorted-min-max
  val longest = List("a", "bb").sortBy(_.length).last// assert: sorted-min-max
  val option = list.sorted.headOption// assert: sorted-min-max
  val mappedTwice = list.map(_ + 1).map(_ * 2)// assert: map-map-to-map
  val filteredTwice = list.filter(_ > 0).filter(_ < 9)// assert: filter-filter-to-filter
  val rejectedTwice = list.filterNot(_ > 0).filterNot(_ < 9)// assert: filter-not-filter-not-to-filter-not
  def twice(value: Int) = value * 2
  val effectfulMap = list.map(twice).map(_ + 1)
  val effectfulFilter = list.filter(twice(_) > 0).filter(_ < 9)

  class ApplicationMethods {
    def filter(predicate: Int => Boolean): ApplicationMethods = this
    def map[A](function: Int => A): ApplicationMethods        = this
    def filterNot(predicate: Int => Boolean): ApplicationMethods = this
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
  val customCollect = custom.filter(_ > 0).map(_ * 2)
  val customRejected = custom.filterNot(_ > 0).map(_ * 2)
  val customReverse = custom.reverse.take(2).reverse
  val customSorted = custom.sorted.head
  val customFind = custom.find(_ > 0).headOption
  val customSet = custom.toSet.toSeq
  val customMap = pairList.map(_._1)
  val customZip = list.zip(List(1))
  val unrelatedRange = 0 until 3
  val stepped = 0 until list.size by 2
  val steppedInclusive = 0 to list.length - 1 by 2
  val foldFalse = list.foldLeft(false)((acc, value) => acc || value > 0)
}
