// format: off
package fix

object ForComprehensions {
  val option: Option[Int] = Some(1)
  val empty: Option[Int] = None
  val list = List(1, 2, 3)
  val other = List(4, 5)
  val pairs = List((1, "a"), (2, "b"))
  val map = Map(1 -> "a")
  val nested = List(List(1), List(2))

  def pure(value: Int): Int = value + 1
  def positive(value: Int): Boolean = value > 0
  def many(value: Int): List[Int] = List(value)

  val idOne = for (x <- option) yield x// assert: redundant-identity-map
  val idList = for (x <- list) yield x
  val idTwo = for { a <- option; b <- empty } yield b
  val idGuard = for (x <- option if x > 1) yield x// assert: redundant-identity-map
  val idBind = for { x <- option; y = x } yield y
  val idPattern = for ((a, b) <- pairs) yield (a, b)
  val idPatternOption = for ((a, b) <- Option((1, 2))) yield (a, b)// assert: redundant-identity-map
  val idPatternSwap = for ((a, b) <- Option((1, 2))) yield (b, a)
  val idCalled = (for (x <- option) yield x).getOrElse(0)// assert: redundant-identity-map
  val idMapped = option.map(identity)// assert: redundant-identity-map
  val idFolded = option.map(x => x).getOrElse(0)// assert: redundant-identity-map

  val projectedFor = for (entry <- map) yield entry._1// assert: map-to-keys
  val projectedPattern = for ((key, _) <- map) yield key// assert: map-to-keys
  val projectedValues = for ((_, value) <- map) yield value// assert: map-to-values
  val projectedPairs = for ((a, _) <- pairs) yield a
  val projectedNarrow = for (case (key, "a") <- map) yield key
  val projectedList = for (entry <- pairs) yield entry._2

  val foldedFor = (for (x <- option) yield x + 1).getOrElse(0)// assert: map-get-or-else-to-fold
  val foldedTwo = (for { a <- option; b <- empty } yield a + b).getOrElse(0)

  val mapMapFor = (for (x <- list) yield pure(x)).map(pure)
  val mapMapAfter = for (x <- list.map(pure)) yield pure(x)
  val mapMapGuard = for (x <- list.map(pure) if x > 1) yield pure(x)
  val mapMapTwo = for { x <- list.map(pure); y <- other.map(pure) } yield x + y
  val mapMapNest = for { x <- list.map(_ + 1).map(_ * 2); y <- other } yield x + y// assert: map-map-to-map
  val mapMapLater = for { x <- list; y <- other.map(_ + 1).map(_ * 2) } yield x + y// assert: map-map-to-map
  val filterFilterPattern = for ((a, b) <- pairs.filter(_._1 > 0).filter(_._1 > 1)) yield b// assert: filter-filter-to-filter
  val filterFilterLater = for { x <- list; y <- other.filter(_ > 0).filter(_ > 1) } yield x + y// assert: filter-filter-to-filter
  val filterFilterBound = for { x <- list.filter(_ > 0).filter(_ > 1); y = x + 1 if y > 1 } println(y)// assert: filter-filter-to-filter
  val filterFilter = for (x <- list.filter(positive)) yield x

  val flattened = (for (x <- list) yield many(x)).flatten// assert: map-flatten-to-flat-map
  val flatId = (for (x <- list) yield x).flatMap(many)
  val nestedFor = (for (x <- list; y <- many(x)) yield y).map(pure)
  val nestedFilter = (for (x <- list; y <- many(x)) yield y).filter(positive)
  val flatFirst = list.flatMap(many).map(pure)
  val flatThenFor = for (x <- list.flatMap(many)) yield pure(x)

  val sizeFor = (for (x <- list) yield x).size
  val filterSize = (for (x <- list if x > 1) yield x).size
  val findFor = (for (x <- list) yield x).find(positive).isDefined// assert: find-to-exists
  val headFor = (for (x <- list if x > 1) yield x).headOption
  val reverseFor = (for (x <- list) yield x).reverse.find(positive)// assert: reverse-find
  val sortedFor = (for (x <- list) yield x).sorted.head// assert: sorted-min-max
  val toSetFor = (for (x <- list) yield x).toSet.toList// assert: to-set-and-back-to-distinct
  val convFor = (for (x <- list) yield x).toList// assert: redundant-collection-conversion
  val indexFor = (for (x <- list) yield x)(0)// assert: zero-index-to-head
  val liftFor = (for (x <- list) yield x).lift(0)// assert: head-or-last-option
  val zipFor = (for (x <- list) yield x).zip(other.indices)
  val dropFor = (for (x <- list) yield x).drop(0)// assert: drop-zero
  val takeFor = (for (x <- list) yield x).take(1).drop(1)// assert: take-drop-to-slice
  val countFor = (for (x <- list) yield x).filter(positive).size// assert: filter-to-count
  val equalsFor = (for (x <- option) yield x + 1) == Some(1)// assert: some-to-contains
  val noneFor = (for (x <- option) yield x + 1) == None// assert: none-check
  val getFor = (for (x <- option) yield x).get// assert: redundant-identity-map

  val fuseFor = (for (x <- list) yield x + 1).map(_ * 2)
  val fuseAfter = for (x <- list.map(_ + 1)) yield x * 2
  val fuseTwo = for { x <- list.map(_ + 1); y <- other.map(_ * 2) } yield x + y
  val fuseGuard = for (x <- list.map(_ + 1) if x > 1) yield x * 2
  val fuseInner = for (x <- list) yield other.map(_ + 1).map(_ * 2)// assert: map-map-to-map
  val fuseOpt = for (x <- option.map(_ + 1)) yield x * 2
  val fuseOptTwo = for { x <- option.map(_ + 1); y <- empty.map(_ * 2) } yield x + y
  val fuseFilter = (for (x <- list if x > 1) yield x).filter(_ > 2)
  val filterThenFor = for (x <- list.filter(_ > 1).filter(_ > 2)) yield x// assert: filter-filter-to-filter
  val nestedPure = (for { x <- list; y <- List(x) } yield y).map(_ + 1)
  val nestedPureFilter = (for { x <- list; y <- List(x) } yield y).filter(_ > 1)
  val flatFor = for (x <- list.flatMap(List(_))) yield x + 1
  val flatInner = for (x <- list) yield other.flatMap(List(_)).map(_ + 1)
  val flatTwo = for { x <- list.flatMap(List(_)); y <- other } yield x + y
  val flatIdentity = for (x <- nested.flatMap(identity)) yield x
  val flatIdentityFor = for (row <- nested; x <- row) yield x// assert: flat-map-identity-to-flatten
  val flatIdentityOption = for (a <- Option(option); b <- a) yield b// assert: flat-map-identity-to-flatten
  val flatIdentityPlus = for (row <- nested; x <- row) yield x + 1
  val flatIdentityOther = for (row <- nested; x <- other) yield x
  val flatIdentityEach = for (row <- nested; x <- row) println(x)
  val flatIdentityMapped = (for (row <- nested) yield row).flatMap(identity)// assert: flat-map-identity-to-flatten
  val flatMapIdFor = for (row <- nested.flatMap(x => x)) yield row + 1
  val mapFlatten = for (x <- list.map(List(_)).flatten) yield x + 1// assert: map-flatten-to-flat-map
  val mapFlattenIn = for (x <- nested.map(_.map(List(_)))) yield x.flatten

  val viewFor = for (x <- list.map(_ + 1).filter(_ > 1)) yield x * 2
  val viewPat = for ((a, b) <- pairs.map(p => (p._2.length, p._1)).filter(_._1 > 1)) yield a * 2// assert: filter-map-to-collect
  val viewNest = for (x <- list.map(_ + 1).filter(_ > 1); y <- other) yield x * y
  val guardYield = for (x <- list if x > 1) yield pure(x)
  val guardNot = for (x <- list if !positive(x)) yield pure(x)
  val guardTwo = for (x <- list if x > 1 if x < 9) yield pure(x)
  val guardOption = for (x <- option if x > 1) yield pure(x)
  val guardSet = for (x <- list.toSet if x > 1) yield pure(x)
  val guardAfterMap = for (x <- list.map(pure) if x > 1) yield x
  val guardAfterFilter = for (x <- list.filter(positive) if x > 1) yield pure(x)
  val guardPattern = for ((a, b) <- pairs if a > 1) yield b
  val guardNarrow = for (case (a, "a") <- pairs) yield a
  val guardNarrowTwo = for (case (a, "a") <- pairs if a > 1) yield a
  val guardOptions = for (case Some(v) <- List(option) if v > 1) yield v
  val guardPatternIdentity = for ((a, b) <- pairs if a > 1) yield (a, b)
  val guardBound = for { x <- list; y = x + 1 if y > 1 } yield y
  val guardBoundTwo = for { x <- list; y = x + 1; z = y * 2 if z > 1 } yield z
  val guardBoundCalled = for { x <- list; y = pure(x) if y > 1 } yield y
  val guardBoundThen = for { x <- list; y = x + 1; z <- other } yield y + z
  val guardNested = for { x <- list if x > 1; y <- other } yield x + y
  val guardInner = for { x <- list; y <- other if y > 1 } yield x + y
  val innerGuardCall = for { x <- list; y <- other if y > x } yield pure(y)
  val innerGuardTwo = for { x <- list; y <- other if y > 1 if y < 9 } yield pure(y)
  val innerGuardThree = for { x <- list; y <- other; z <- list if z > y } yield x + y + z
  val innerGuardOption = for { x <- option; y <- empty if y > 1 } yield x + y
  val innerPatternGuard = for { x <- list; (a, b) <- pairs if a > 1 } yield a + x
  val innerPatternKey = for { x <- list; (key, _) <- map } yield key// assert: map-to-keys
  val innerPatternValue = for { x <- list; (_, value) <- map } yield value// assert: map-to-values
  val innerPatternComputed = for { x <- list; (key, _) <- map } yield key + x
  val innerRefutable = for { x <- list; case Some(v) <- List(option) } yield v + x
  val innerRefutableGuard = for { x <- list; case Some(v) <- List(option) if v > x } yield v
  val innerNarrow = for { x <- list; case (a, "a") <- pairs } yield a + x
  val innerBound = for { x <- list; y <- other; z = y + 1 if z > 1 } yield z
  val innerBoundEffect = for { x <- list; y <- other; z = y + pure(x) if z > 1 } yield z
  val innerMapMap = for { x <- list; y <- other.map(_ + 1).map(_ * 2) } yield x + y// assert: map-map-to-map
  val innerFilterMap = for { x <- list; y <- other.filter(positive) } yield x + y
  val innerKinds = for { x <- list; y <- option } yield x + y
  val innerKindsSet = for { x <- list; y <- other.toSet } yield x + y
  val innerKindsGuard = for { x <- list; y <- other.toSet if y > 1 } yield y + x
  val innerKindsMap = for { x <- list; y <- map } yield x + y._1
  val innerKindsKeys = for { x <- list; y <- map } yield y._1// assert: map-to-keys
  val innerEach = for { x <- list; y <- other if y > 1 } println(x + y)
  val guardSized = (for (x <- list if x > 1) yield pure(x)).size
  val guardHead = (for (x <- list if x > 1) yield pure(x)).headOption
  val guardSorted = (for (x <- list if x > 1) yield pure(x)).sorted
  val guardFlat = (for (x <- list if x > 1) yield many(x)).flatten
  val guardFilter = (for (x <- list if x > 1) yield pure(x)).filter(positive)
  val guardMapMap = (for (x <- list if x > 1) yield pure(x)).map(pure)

  def loops(): Unit = {
    for (x <- list) println(x)
    for { x <- list; y <- other } println(x + y)
    for (x <- list if x > 1) println(x)
    for (x <- list if x > 1 if x < 9) println(x)
    for (x <- list.filter(_ > 1) if x < 9) println(x)
    for (x <- list.map(pure) if x < 9) println(x)
    for (x <- list.filter(positive)) println(x)
    for (x <- list.map(pure)) println(x)
    for (x <- list.reverse) println(x)
    for ((a, b) <- pairs.map(p => p).map(p => p)) println(a)// assert: map-map-to-map
    for { x <- list; (a, b) <- pairs.filter(_._1 > 0).filter(_._1 > 1) } println(a + x)// assert: filter-filter-to-filter
    for { x <- list; y = x + 1 if y > 1 } println(y)
    for ((a, b) <- pairs if a > 1) println(a)
    for (x <- list.indices) println(x)
    for (i <- 0 until list.size) println(list(i))// assert: range-to-indices
    for (i <- 0 to list.size - 1) println(list(i))// assert: range-to-indices
    for (x <- option) println(x)
    for { x <- option; y = x + 1 } println(y)
    for (x <- list.map(_ + 1).map(_ * 2)) println(x)// assert: map-map-to-map
    for (x <- list.filter(_ > 1).filter(_ > 2)) println(x)// assert: filter-filter-to-filter
    for (x <- list.flatMap(List(_)).map(_ + 1)) println(x)
    for (x <- list.toSet.toList) println(x)// assert: to-set-and-back-to-distinct
    for (x <- list.filter(_ > 1).headOption) println(x)// assert: filter-head-option
    for (x <- list.reverse.find(_ > 1)) println(x)// assert: reverse-find
    for (x <- list.find(_ > 1).toList) println(x)
    for (x <- list.take(1).drop(1)) println(x)// assert: take-drop-to-slice
    for (x <- list.sorted.headOption) println(x)// assert: sorted-min-max
  }
}
