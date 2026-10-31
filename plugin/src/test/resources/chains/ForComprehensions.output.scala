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

  val idOne = for (x <- option) yield x
  val idList = for (x <- list) yield x
  val idTwo = for { a <- option; b <- empty } yield b
  val idGuard = for (x <- option if x > 1) yield x
  val idBind = for { x <- option; y = x } yield y
  val idPattern = for ((a, b) <- pairs) yield (a, b)
  val idPatternOption = for ((a, b) <- Option((1, 2))) yield (a, b)
  val idPatternSwap = for ((a, b) <- Option((1, 2))) yield (b, a)
  val idCalled = (for (x <- option) yield x).getOrElse(0)
  val idMapped = option
  val idFolded = option.getOrElse(0)

  val projectedFor = for (entry <- map) yield entry._1
  val projectedPattern = for ((key, _) <- map) yield key
  val projectedValues = for ((_, value) <- map) yield value
  val projectedPairs = for ((a, _) <- pairs) yield a
  val projectedNarrow = for (case (key, "a") <- map) yield key
  val projectedList = for (entry <- pairs) yield entry._2

  val foldedFor = (for (x <- option) yield x + 1).getOrElse(0)
  val foldedTwo = (for { a <- option; b <- empty } yield a + b).getOrElse(0)

  val mapMapFor = (for (x <- list) yield pure(x)).map(pure)
  val mapMapAfter = for (x <- list.map(pure)) yield pure(x)
  val mapMapGuard = for (x <- list.map(pure) if x > 1) yield pure(x)
  val mapMapTwo = for { x <- list.map(pure); y <- other.map(pure) } yield x + y
  val mapMapNest = for { x <- list.map(x => (x + 1) * 2); y <- other } yield x + y
  val mapMapLater = for { x <- list; y <- other.map(x => (x + 1) * 2) } yield x + y
  val filterFilterPattern = for ((a, b) <- pairs.filter(x => x._1 > 0 && x._1 > 1)) yield b
  val filterFilterLater = for { x <- list; y <- other.filter(x => x > 0 && x > 1) } yield x + y
  val filterFilterBound = for { x <- list.filter(x => x > 0 && x > 1); y = x + 1 if y > 1 } println(y)
  val filterFilter = for (x <- list.filter(positive)) yield x

  val flattened = (for (x <- list) yield many(x)).flatten
  val flatId = (for (x <- list) yield x).flatMap(many)
  val nestedFor = (for (x <- list; y <- many(x)) yield y).map(pure)
  val nestedFilter = (for (x <- list; y <- many(x)) yield y).filter(positive)
  val flatFirst = list.flatMap(many).map(pure)
  val flatThenFor = for (x <- list.flatMap(many)) yield pure(x)

  val sizeFor = (for (x <- list) yield x).size
  val filterSize = (for (x <- list if x > 1) yield x).size
  val findFor = (for (x <- list) yield x).exists(positive)
  val headFor = (for (x <- list if x > 1) yield x).headOption
  val reverseFor = (for (x <- list) yield x).reverse.find(positive)
  val sortedFor = (for (x <- list) yield x).sorted.head
  val toSetFor = (for (x <- list) yield x).distinct
  val convFor = for (x <- list) yield x
  val indexFor = (for (x <- list) yield x).head
  val liftFor = (for (x <- list) yield x).headOption
  val zipFor = (for (x <- list) yield x).zip(other.indices)
  val dropFor = for (x <- list) yield x
  val takeFor = (for (x <- list) yield x).slice(1, 1)
  val countFor = (for (x <- list) yield x).count(positive)
  val equalsFor = (for (x <- option) yield x + 1).contains(1)
  val noneFor = (for (x <- option) yield x + 1).isEmpty
  val getFor = (for (x <- option) yield x).get

  val fuseFor = (for (x <- list) yield x + 1).map(_ * 2)
  val fuseAfter = for (x <- list.map(_ + 1)) yield x * 2
  val fuseTwo = for { x <- list.map(_ + 1); y <- other.map(_ * 2) } yield x + y
  val fuseGuard = for (x <- list.map(_ + 1) if x > 1) yield x * 2
  val fuseInner = for (x <- list) yield other.map(x => (x + 1) * 2)
  val fuseOpt = for (x <- option.map(_ + 1)) yield x * 2
  val fuseOptTwo = for { x <- option.map(_ + 1); y <- empty.map(_ * 2) } yield x + y
  val fuseFilter = (for (x <- list if x > 1) yield x).filter(_ > 2)
  val filterThenFor = for (x <- list.filter(x => x > 1 && x > 2)) yield x
  val nestedPure = (for { x <- list; y <- List(x) } yield y).map(_ + 1)
  val nestedPureFilter = (for { x <- list; y <- List(x) } yield y).filter(_ > 1)
  val flatFor = for (x <- list.flatMap(List(_))) yield x + 1
  val flatInner = for (x <- list) yield other.flatMap(List(_)).map(_ + 1)
  val flatTwo = for { x <- list.flatMap(List(_)); y <- other } yield x + y
  val flatIdentity = for (x <- nested.flatMap(identity)) yield x
  val flatIdentityFor = for (row <- nested; x <- row) yield x
  val flatIdentityOption = for (a <- Option(option); b <- a) yield b
  val flatIdentityPlus = for (row <- nested; x <- row) yield x + 1
  val flatIdentityOther = for (row <- nested; x <- other) yield x
  val flatIdentityEach = for (row <- nested; x <- row) println(x)
  val flatIdentityMapped = (for (row <- nested) yield row).flatten
  val flatMapIdFor = for (row <- nested.flatMap(x => x)) yield row + 1
  val mapFlatten = for (x <- list.flatMap(List(_))) yield x + 1
  val mapFlattenIn = for (x <- nested.map(_.map(List(_)))) yield x.flatten

  val viewFor = for (x <- list.map(_ + 1).filter(_ > 1)) yield x * 2
  val viewPat = for ((a, b) <- pairs.map(p => (p._2.length, p._1)).filter(_._1 > 1)) yield a * 2
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
  val innerPatternKey = for { x <- list; (key, _) <- map } yield key
  val innerPatternValue = for { x <- list; (_, value) <- map } yield value
  val innerPatternComputed = for { x <- list; (key, _) <- map } yield key + x
  val innerRefutable = for { x <- list; case Some(v) <- List(option) } yield v + x
  val innerRefutableGuard = for { x <- list; case Some(v) <- List(option) if v > x } yield v
  val innerNarrow = for { x <- list; case (a, "a") <- pairs } yield a + x
  val innerBound = for { x <- list; y <- other; z = y + 1 if z > 1 } yield z
  val innerBoundEffect = for { x <- list; y <- other; z = y + pure(x) if z > 1 } yield z
  val innerMapMap = for { x <- list; y <- other.map(x => (x + 1) * 2) } yield x + y
  val innerFilterMap = for { x <- list; y <- other.filter(positive) } yield x + y
  val innerKinds = for { x <- list; y <- option } yield x + y
  val innerKindsSet = for { x <- list; y <- other.toSet } yield x + y
  val innerKindsGuard = for { x <- list; y <- other.toSet if y > 1 } yield y + x
  val innerKindsMap = for { x <- list; y <- map } yield x + y._1
  val innerKindsKeys = for { x <- list; y <- map } yield y._1
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
    for ((a, b) <- pairs.map(x => x)) println(a)
    for { x <- list; (a, b) <- pairs.filter(x => x._1 > 0 && x._1 > 1) } println(a + x)
    for { x <- list; y = x + 1 if y > 1 } println(y)
    for ((a, b) <- pairs if a > 1) println(a)
    for (x <- list.indices) println(x)
    for (i <- list.indices) println(list(i))
    for (i <- list.indices) println(list(i))
    for (x <- option) println(x)
    for { x <- option; y = x + 1 } println(y)
    for (x <- list.map(x => (x + 1) * 2)) println(x)
    for (x <- list.filter(x => x > 1 && x > 2)) println(x)
    for (x <- list.flatMap(List(_)).map(_ + 1)) println(x)
    for (x <- list.distinct) println(x)
    for (x <- list.filter(_ > 1).headOption) println(x)
    for (x <- list.reverse.find(_ > 1)) println(x)
    for (x <- list.find(_ > 1).toList) println(x)
    for (x <- list.slice(1, 1)) println(x)
    for (x <- list.sorted.headOption) println(x)
  }
}
