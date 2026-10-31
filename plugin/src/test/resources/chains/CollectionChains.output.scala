// format: off
package fix

object CollectionChains {
  val lists = List(List(1, 2), List(3))
  val list  = List(1, 2, 3)
  val map   = Map(1 -> "a", 2 -> "b")
  val pairs = List((1, "a"), (2, "b"))
  val triples = List((1, "a", true), (2, "b", false))

  val flattened = lists.flatMap(_.map(_ * 2))
  val flattenedIdentity = lists.flatten
  val count = list.count(_ > 0)
  val length = list.count(_ > 0)
  val defined = list.exists(_ > 0)
  val missing = !list.exists(_ > 0)
  val compared = list.exists(_ > 0)
  val tail = list.takeRight(2)
  val keys = map.keys
  val values = map.values
  val distinctSeq = list.distinct
  val distinctList = list.distinct
  val indexed = list.zipWithIndex
  val range = list.indices
  val inclusive = list.indices
  val first = pairs.map(_._1)
  val second = pairs.map(_._2)
  val middle = triples.map(_._2)
  val allPositive = list.forall(value => value > 0)

  val emptiness = list.filter(_ > 0).isEmpty
  val presence = list.filterNot(_ > 0).nonEmpty
  val head = list.filter(_ > 0).headOption
  val collected = list.collect { case value if value > 0 => value }.headOption
  val doubled = list.filter(_ > 0).map(_ * 2)
  val rejected = list.filterNot(_ > 0).map(_ * 2)
  val chosen = Option(1).filter(_ > 0).map(_ * 2)
  val lazily = list.withFilter(_ > 0).map(_ * 2)
  val any = list.map(_ > 0).contains(true)
  val last = list.reverse.find(_ > 0)
  val smallest = list.sorted.head
  val largest = list.sorted.last
  val longest = List("a", "bb").sortBy(_.length).last
  val option = list.sorted.headOption
  val mappedTwice = list.map(x => (x + 1) * 2)
  val filteredTwice = list.filter(x => x > 0 && x < 9)
  val rejectedTwice = list.filterNot(x => x > 0 || x < 9)
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
