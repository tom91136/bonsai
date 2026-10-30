package net.kurobako.bonsai.analysis

class ComplexityPatternsSuite extends AnalysisSuite {
  private lazy val extracted = extract(
    Seq(
      "Main.scala"     -> "object Main { def main(args: Array[String]): Unit = println(Patterns) }",
      "Patterns.scala" ->
        """case class Param(name: String, kind: Int)
          |case class FreeFunction(name: String, params: List[Param])
          |
          |case class Entry(isa: String, name: String)
          |case class Info(encodings: List[Int])
          |
          |object Op {
          |  val meta: Info = Info(List(1, 2))
          |}
          |
          |class Table(entries: List[Int], index: Vector[Int]) {
          |  def duplicates: Int = entries.count(entry => entries.count(_ == entry) > 1)
          |  def lookups(keys: List[Int]): Int = keys.count(key => entries.contains(key))
          |  def viaCall(ff: FreeFunction): Int = ff.params.count(param => touches(ff))
          |  private def touches(ff: FreeFunction): Boolean = ff.params.exists(_.kind == 0)
          |  def viaField: Int = entries.map(entry => scan(entries)).sum
          |  private def scan(values: List[Int]): Int = values.sum
          |  def cheap: Int = index.map(entry => index(entry)).sum
          |}
          |
          |object Patterns {
          |  def resolveFreeFunctionDoc(ff: FreeFunction): Set[String] = {
          |    val countParams = ff.params.collect {
          |      case Param(name = name) if ff.params.exists(_.name == s"${name}Count") => name
          |    }.toSet
          |    countParams
          |  }
          |  def alias(ff: FreeFunction): Int = {
          |    val params = ff.params.filter(_.kind > 0)
          |    params.count(param => ff.params.exists(_.name == param.name))
          |  }
          |  def indexedList(values: List[Int]): Int = {
          |    var total = 0
          |    var index = 0
          |    while (index < values.length) {
          |      total += values(index)
          |      index += 1
          |    }
          |    total
          |  }
          |  def containsInLoop(xs: Vector[Int], ys: List[Int]): Int = {
          |    var total = 0
          |    var index = 0
          |    while (index < xs.length) {
          |      if (ys.contains(xs(index))) total += 1
          |      index += 1
          |    }
          |    total
          |  }
          |  def draining(xs: Vector[Int]): Int = {
          |    val it    = xs.iterator
          |    var total = 0
          |    while (it.hasNext) {
          |      val x = it.next()
          |      total += xs.count(_ == x)
          |    }
          |    total
          |  }
          |  def appended(xs: Vector[Int]): List[Int] = {
          |    var acc = List.empty[Int]
          |    for (x <- xs) acc = acc :+ x
          |    acc
          |  }
          |  def folded(xs: Vector[Int]): List[Int] = xs.foldLeft(List.empty[Int])((acc, x) => acc :+ x)
          |  def concatenated(xs: Vector[Int]): String = {
          |    var text = ""
          |    for (x <- xs) text += x.toString
          |    text
          |  }
          |  def interpolated(xs: Vector[Int]): String = {
          |    var text = ""
          |    for (x <- xs) text = s"$text,$x"
          |    text
          |  }
          |  def sortedInside(xs: List[Int], ys: List[Int]): List[Int] = xs.map(x => ys.sorted.head + x)
          |  def cubic(xs: List[Int]): Int = xs.map(x => xs.map(y => xs.count(_ == x + y))).size
          |  def copying(xs: List[Int], ys: Vector[Int]): List[Boolean] = xs.map(x => ys.toSet.contains(x))
          |  def copiesAndScans(xs: List[Int], ys: List[Int], zs: List[Int]): Int = {
          |    val sets = xs.map(x => ys.toSet)
          |    xs.map(x => ys.map(y => zs.count(_ == y))).size + sets.size
          |  }
          |  def recursive(xs: List[Int]): Int = xs match {
          |    case Nil       => 0
          |    case h :: tail => (if (xs.contains(h)) 1 else 0) + recursive(tail)
          |  }
          |  def dropping(xs: List[Int]): Int =
          |    if (xs.isEmpty) 0 else (if (xs.contains(xs.head)) 1 else 0) + dropping(xs.tail)
          |  def halving(xs: List[Int]): Int =
          |    if (xs.size < 2) 0 else { val (a, b) = xs.splitAt(xs.size / 2); halving(a) + halving(b) + xs.count(_ > 0) }
          |  def fixed(): Int = {
          |    val a = List(1, 2, 3)
          |    val b = List(4, 5)
          |    a.count(x => b.contains(x))
          |  }
          |  def hashed(xs: List[Int], seen: Set[Int]): Int = xs.count(x => seen.contains(x))
          |  def vectorAppend(xs: Vector[Int]): Vector[Int] = {
          |    var acc = Vector.empty[Int]
          |    for (x <- xs) acc = acc :+ x
          |    acc
          |  }
          |  def buffered(xs: Vector[Int]): Int = {
          |    val acc = scala.collection.mutable.ArrayBuffer.empty[Int]
          |    for (x <- xs) acc += x
          |    acc(0)
          |  }
          |  def onceThenSet(xs: List[Int]): List[Int] = {
          |    val seen = xs.toSet
          |    xs.filter(x => seen.contains(x))
          |  }
          |  def taken(xs: List[Int], ys: List[Int]): Int = xs.count(x => ys.take(3).contains(x))
          |  def bounded(xs: Vector[Int], ys: Vector[Int]): Int = {
          |    var total = 0
          |    var index = 0
          |    while (index < xs.length) {
          |      var inner = 0
          |      while (inner < ys.length && inner < 3) { total += ys(inner); inner += 1 }
          |      index += 1
          |    }
          |    total
          |  }
          |  def unbounded(xs: Vector[Int], ys: Vector[Int]): Int = {
          |    var total = 0
          |    var index = 0
          |    while (index < xs.length) {
          |      var inner = 0
          |      while (inner < ys.length) { total += ys(inner); inner += 1 }
          |      index += 1
          |    }
          |    total
          |  }
          |  def merged(xs: Vector[Int], ys: Vector[Int]): Int = {
          |    var total = 0
          |    var left  = 0
          |    var right = 0
          |    while (left < xs.length) {
          |      while (right < ys.length && ys(right) < xs(left)) { total += 1; right += 1 }
          |      left += 1
          |    }
          |    total
          |  }
          |  def restarted(xs: Vector[Int], ys: Vector[Int]): Int = {
          |    var total = 0
          |    var left  = 0
          |    var right = 0
          |    while (left < xs.length) {
          |      right = 0
          |      while (right < ys.length && ys(right) < xs(left)) { total += 1; right += 1 }
          |      left += 1
          |    }
          |    total
          |  }
          |  def lanes(xs: Vector[Int]): Int = xs.count(x => Lanes.All.contains(x)) + Lanes.Filled.count(xs.contains)
          |  def countdown(n: Int, xs: List[Int]): Int =
          |    if (n <= 0) 0 else (if (xs.contains(n)) 1 else 0) + countdown(n - 1, xs)
          |  def walk(xs: List[Int], i: Int): Int =
          |    if (i >= xs.length) 0 else (if (xs.contains(i)) 1 else 0) + walk(xs, i + 1)
          |  def callsCountdown(xs: List[Int]): Int = countdown(3, xs)
          |  def listAppended(xs: Vector[Int]): List[Int] = xs.foldLeft(List.empty[Int])((acc, x) => acc ++ List(x))
          |  def joined(ds: List[List[Int]]): List[Int] = ds.foldLeft(List.empty[Int])((acc, next) => acc ++ next.filter(!acc.contains(_)))
          |  def guardedIf(xs: Vector[Int], ys: List[Int]): Int =
          |    xs.zipWithIndex.map((x, at) => if (at == 0) ys.count(_ == x) else 0).sum
          |  def guardedOption(xs: Vector[Int], ys: List[Int]): Int =
          |    xs.zipWithIndex.flatMap((x, at) => Option.when(at == 0)(ys.count(_ == x))).sum
          |  def unguarded(xs: Vector[Int], ys: List[Int]): Int =
          |    xs.zipWithIndex.map((x, at) => if (at > 0) ys.count(_ == x) else 0).sum
          |  def keyedFilter(es: List[Entry]): List[Entry] =
          |    es.filter(e => e.isa == "A" && e.name == "b").flatMap(e => es.find(_.name == e.name))
          |  def looseFilter(es: List[Entry]): List[Entry] =
          |    es.filter(e => e.isa == "A").flatMap(e => es.find(_.name == e.name))
          |  private def decode(info: Info): Int = info.encodings.sum
          |  def staticArgument(xs: List[Int]): List[Int] = xs.map(x => decode(Op.meta))
          |  def dynamicArgument(xs: List[Int], info: Info): List[Int] = xs.map(x => decode(info))
          |  def gather(lines: Vector[String], idx: Int, acc: List[String]): String =
          |    if (idx >= lines.length) acc.reverse.mkString(" ")
          |    else {
          |      val line = lines(idx)
          |      if (line.isEmpty) acc.reverse.mkString(",") else gather(lines, idx + 1, line :: acc)
          |    }
          |  def vectorIndexed(xs: Vector[Int]): Int = xs.indices.map(index => xs(index)).sum
          |  def sortedOnce(xs: List[Int]): List[Int] = xs.sorted
          |  def separate(xs: List[Int], ys: List[Int]): Int = xs.sum + ys.sum
          |  def renderMatrix(rows: Vector[String], cols: Vector[String], get: (String, String) => String): Vector[Vector[String]] =
          |    rows.map(r => cols.map(c => get(r, c)))
          |  def renderCombinations(operands: Vector[String]): Vector[String] =
          |    operands.foldLeft(Vector(""))((prefixes, op) => prefixes.map(_ + op) ++ prefixes)
          |  def containsDuplicate(items: Vector[String]): Boolean =
          |    items.zipWithIndex.exists { case (a, i) => items.drop(i + 1).contains(a) }
          |}
          |
          |object Lanes {
          |  val All: Array[Int]    = Array.tabulate(32)(identity)
          |  val Filled: Vector[Int] = Vector.fill(4)(0)
          |}
          |
          |object Registry {
          |  val Built: Vector[Int] = scala.util.Random.shuffle(Vector(1, 2, 3))
          |  val Live               = scala.collection.mutable.ArrayBuffer.empty[Int]
          |  def known(xs: Vector[Int]): Int = xs.count(x => Built.contains(x))
          |  def growing(xs: Vector[Int]): Int = xs.count(x => Live.contains(x))
          |}
          |
          |object Tables {
          |  val Names: Vector[String] = Vector("a", "b", "c")
          |  val Pairs: List[(String, Int)] = List("a" -> 1, "b" -> 2)
          |  val Both: Vector[String] = Names ++ Vector("d")
          |  def known(xs: List[String]): Int = xs.count(x => Names.exists(_ == x))
          |  def pairs(xs: List[String]): Int = xs.count(x => Pairs.exists(_._1 == x)) + Both.count(xs.contains)
          |  def scaled(xs: List[String]): Int = Names.count(name => Both.exists(_ == name))
          |}
          |
          |object Hot {
          |  def fetch(): List[Int] = List(1, 2, 3)
          |  def callback(keys: Vector[Int]): Int = fetch().count(x => keys.toSet.contains(x))
          |  def cold(keys: Vector[Int]): Int = fetch().count(x => keys.size > x)
          |  def compiled(): Int = fetch().count(x => "a+b".r.findFirstIn(x.toString).isDefined)
          |  def matching(pattern: String): Int = fetch().count(x => java.util.regex.Pattern.compile(pattern).matcher("a").matches)
          |}""".stripMargin
    ),
    hot = Vector("Hot")
  )

  private lazy val found = Complexity.findings(extracted.complexities, extracted.graph.nodes, extracted.hot)

  private given Finding.Level = Finding.Level.Warning

  private def about(name: String): Vector[Finding] = found.filter(_.detail.startsWith(s"$name "))

  private def silent(names: String*): Unit =
    names.foreach(name => assertEquals(about(name).map(_.detail), Vector.empty))

  test("a traversal whose guard traverses the same field of a parameter is quadratic in that field") {
    assertFinding(
      found,
      "complexity",
      "Patterns.resolveFreeFunctionDoc is O(|ff.params|^2) in time",
      "exists over |ff.params|",
      "inside collect over |ff.params|"
    )
  }

  test("sizes follow fields of this, locals derived from parameters, and calls that pass their arguments on") {
    assertFinding(found, "complexity", "Table.duplicates is O(|this.entries|^2)")
    assertFinding(found, "complexity", "Table.lookups is O(|keys| * |this.entries|)")
    assertFinding(found, "complexity", "Table.viaCall is O(|ff.params|^2)")
    assertFinding(found, "complexity", "Table.viaField is O(|this.entries|^2)")
    assertFinding(found, "complexity", "Patterns.alias is O(|ff.params|^2)")
    silent("Table.cheap")
  }

  test("loops over a counter, an iterator or a recursion on a shrinking list are bounded by the collection") {
    assertFinding(found, "complexity", "Patterns.indexedList is O(|values|^2)")
    assertFinding(found, "complexity", "Patterns.containsInLoop is O(|xs| * |ys|)")
    assertFinding(found, "complexity", "Patterns.draining is O(|xs|^2)")
    assertFinding(found, "complexity", "Patterns.recursive is O(|xs|^2)")
    assertFinding(found, "complexity", "Patterns.dropping is O(|xs|^2)")
    assertFinding(found, "complexity", "Patterns.unbounded is O(|xs| * |ys|)")
    assertFinding(found, "complexity", "Patterns.restarted is O(|xs| * |ys|)")
    assertFinding(found, "complexity", "Patterns.countdown is O(|n| * |xs|)")
    assertFinding(found, "complexity", "Patterns.walk is O(|xs|^2)")
    assertFinding(found, "complexity", "Patterns.listAppended is O(|xs|^2)")
    assertFinding(found, "complexity", "Patterns.unguarded is O(|xs| * |ys|)")
    assertFinding(found, "complexity", "Patterns.looseFilter is O(|es|^2)")
    assertFinding(found, "complexity", "Patterns.dynamicArgument is O(|info.encodings| * |xs|)")
    assertFinding(found, "complexity", "Registry.growing is O(|Registry.Live| * |xs|)")
  }

  test("accumulating with appends or concatenation in a loop is quadratic in time and in allocation") {
    assertFinding(found, "complexity", "Patterns.appended is O(|xs|^2) in time and space")
    assertFinding(found, "complexity", "Patterns.folded is O(|xs|^2) in time and space")
    assertFinding(found, "complexity", "Patterns.concatenated is O(|xs|^2) in time and space")
    assertFinding(found, "complexity", "Patterns.interpolated is O(|xs|^2) in time and space")
  }

  test("a sort or a copy inside a traversal multiplies by the traversed size, and cubic work is an error") {
    assertFinding(found, "complexity", "Patterns.sortedInside is O(|xs| * |ys| * log|ys|)")
    assertFinding(found, "complexity", "Patterns.copying is O(|xs| * |ys|) in time and space")
    assertFinding(found, "complexity", "Patterns.cubic is O(|xs|^3)")(using Finding.Level.Error)
  }

  test("allocation that is below the time bound is reported apart") {
    assertFinding(found, "space-complexity", "Patterns.copiesAndScans allocates O(|xs| * |ys|)")
    assertFinding(found, "complexity", "Patterns.copiesAndScans is O(|xs| * |ys| * |zs|)")(using Finding.Level.Error)
  }

  test("constant-size collections, hashed lookups, vectors, buffers, single passes and early exits stay silent") {
    silent(
      "Patterns.fixed",
      "Patterns.hashed",
      "Patterns.vectorAppend",
      "Patterns.buffered",
      "Patterns.onceThenSet",
      "Patterns.taken",
      "Patterns.bounded",
      "Patterns.vectorIndexed",
      "Patterns.sortedOnce",
      "Patterns.separate",
      "Patterns.halving",
      "Patterns.merged",
      "Patterns.gather",
      "Patterns.callsCountdown",
      "Patterns.joined",
      "Patterns.guardedIf",
      "Patterns.guardedOption",
      "Patterns.keyedFilter",
      "Patterns.staticArgument",
      "Patterns.lanes",
      "Tables.known",
      "Registry.known",
      "Tables.pairs",
      "Tables.scaled"
    )
  }

  test("linear work in a callback is reported in hot classes only") {
    assertFinding(found, "complexity", "Hot.callback does linear work for each element")
    assertFinding(found, "complexity", "Hot.compiled does linear work for each element", "regex compilation")
    assertFinding(found, "complexity", "Hot.matching does linear work for each element", "regex compilation")
    silent("Hot.cold")
  }

  test("output-sized work is not reported as accidental complexity, while accidental scans keep firing") {
    assertFinding(found, "complexity", "Patterns.containsDuplicate is O(|items|^2)")
    silent("Patterns.renderMatrix", "Patterns.renderCombinations")
  }

  private lazy val seeking = extract(
    Seq(
      "Main.scala" -> "object Main { def main(args: Array[String]): Unit = println(Seek) }",
      "Seek.scala" ->
        """object Seek {
          |  def lowerBound(addresses: Array[Long], key: Long): Int = {
          |    val at = java.util.Arrays.binarySearch(addresses, key)
          |    if (at >= 0) at else -at - 1
          |  }
          |  def within(addresses: Array[Long], symbols: Vector[(Long, Long)]): Vector[Vector[Long]] =
          |    symbols.map((start, size) =>
          |      Vector.from(addresses.view.drop(lowerBound(addresses, start)).takeWhile(_ < start + size))
          |    )
          |  def scanned(addresses: Array[Long], symbols: Vector[(Long, Long)]): Vector[Vector[Long]] =
          |    symbols.map((start, size) => addresses.filter(a => a >= start && a < start + size).toVector)
          |  def unsought(addresses: Array[Long], symbols: Vector[(Long, Long)]): Vector[Vector[Long]] =
          |    symbols.map((start, size) => Vector.from(addresses.view.takeWhile(_ < start + size)))
          |  def pairs(xs: Vector[Int], ys: Vector[Int]): Vector[(Int, Int)] = for { x <- xs; y <- ys if y != x } yield (x, y)
          |  def matched(xs: Vector[Int], ys: Vector[Int]): Vector[(Int, Int)] = for { x <- xs; y <- ys if y == x } yield (x, y)
          |}""".stripMargin
    )
  )

  test("a scan after a sorted seek, and a cross product minus the diagonal, cost their output") {
    val seekFound = Complexity.findings(seeking.complexities, seeking.graph.nodes, seeking.hot)
    def seekAbout(name: String): Vector[Finding] = seekFound.filter(_.detail.startsWith(s"$name "))
    assertEquals(seekAbout("Seek.within").map(_.detail), Vector.empty)
    assertEquals(seekAbout("Seek.pairs").map(_.detail), Vector.empty)
    assertEquals(seekAbout("Seek.scanned").size, 1)
    assertEquals(seekAbout("Seek.unsought").size, 1)
    assertEquals(seekAbout("Seek.matched").size, 1)
  }
}
