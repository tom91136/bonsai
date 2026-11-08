package net.kurobako.bonsai.analysis

class PipelinesSuite extends AnalysisSuite {
  private lazy val found = {
    val result = extract(Seq(
      "Main.scala"   -> "object Main { def main(args: Array[String]): Unit = println(Chains) }",
      "Chains.scala" ->
        """object Chains {
          |  final class Box[A](value: A) {
          |    def map[B](f: A => B): Box[B]       = Box(f(value))
          |    def filter(p: A => Boolean): Box[A] = this
          |  }
          |  def pipeline(values: List[Int]): Int = values.map(_ + 1).filter(_ > 0).sum
          |  def single(values: List[Int]): Int = values.map(_ + 1).size
          |  def fused(values: List[Int]): List[Int] = values.map(_ + 1).map(_ * 2)
          |  def filtered(values: List[Int]): List[Int] = values.filter(_ > 0).filter(_ < 9)
          |  def effectful(values: List[Int]): List[Int] = values.map { x => println(x); x }.map(_ * 2)
          |  def counted(values: List[Int]): Int = values.filter(_ > 0).size
          |  def boxed(box: Box[Int]): Box[Int] = box.map(_ + 1).filter(_ > 0)
          |  def narrowed(value: Option[Int]): Option[Int] = value.filter(_ > 0).filter(_ < 9)
          |  def mapped(value: Option[Int]): Option[Int] = value.map(_ + 1).map(_ * 2)
          |  def viewed(values: List[Int]): Int = values.view.map(_ + 1).filter(_ > 0).size
          |  def arrays(values: Array[Int]): Int = values.map(_ + 1).filter(_ > 0).sum
          |  def nested(values: List[List[Int]]): Int = values.map(row => row.filter(_ > 0).size).sum
          |  def longer(rows: Vector[List[(Int, Int)]]): Int =
          |    rows.map(values => values.map(p => (p._1 + 1, p._2)).filter(_._1 > 2).map { case (a, b) => a * b }.sum).sum
          |  def looped(rows: Vector[Array[(Int, Int)]]): Int =
          |    rows.map(values => values.filter(_._1 > 0).map { case (a, b) => a + b }.length).sum
          |  def once(values: List[Int]): Int = values.map(_ + 1).filter(_ > 2).sum
          |  def lone(rows: Vector[List[Int]]): Int = rows.map(values => values.map(_ + 1).sum).sum
          |  def lazily(rows: Vector[List[Int]]): Int = rows.map(values => values.view.map(_ + 1).filter(_ > 2).sum).sum
          |  def iterated(rows: Vector[Iterator[Int]]): Int = rows.map(values => values.map(_ + 1).filter(_ > 2).sum).sum
          |  def optional(rows: Vector[Option[Int]]): Int = rows.map(value => value.map(_ + 1).filter(_ > 2).sum).sum
          |  def local(values: List[Int]): Int = {
          |    def inner(items: List[Int]) = items.map(_ + 1).filter(_ > 0).size
          |    inner(values)
          |  }
          |  def comprehended(values: List[Int]): List[Int] = for (x <- values.map(_ + 1)) yield x * 2
          |  def comprehendedTwo(values: List[Int], pairs: List[(Int, Int)]): List[Int] =
          |    for { x <- values; (a, b) <- pairs.filter(_._1 > 0) } yield x + a
          |  def comprehendedGuard(values: List[Int]): List[Int] = for (x <- values if x > 0) yield x * 2
          |  def comprehendedIdentity(values: List[Int]): List[Int] = for (x <- values) yield x
          |}""".stripMargin
    ))
    Pipelines.findings(result.pipelines, result.graph.nodes)
  }

  private def of(name: String): Vector[Finding] = found.filter(_.detail.startsWith(s"Chains.$name:"))

  test("the chains of a source the reader cannot find are left out, and the rest of the build is still read") {
    val result = extract(
      Seq(
        "Main.scala"   -> "object Main { def main(args: Array[String]): Unit = println(Chains.pipeline(Nil)) }",
        "Chains.scala" -> "object Chains { def pipeline(values: List[Int]): Int = values.map(_ + 1).filter(_ > 0).sum }"
      ),
      removed = Seq("Chains.scala")
    )
    assert(result.pipelines.isEmpty, result.pipelines.toString)
    assert(result.graph.nodes.contains("Chains"), result.graph.nodes.keys.toString)
  }

  test("chains of strict transformations are reported once per method") {
    given Finding.Level = Finding.Level.Info
    assertFinding(found, "intermediate-collections", "Chains.pipeline: map then filter", "use a view")
    assertFinding(found, "intermediate-collections", "Chains.arrays: map then filter", "use a view")
    assertEquals(of("pipeline").size, 1, found.mkString("\n"))
    assertEquals(of("arrays").size, 1, found.mkString("\n"))
  }

  test("chains inside a lambda are reported once, through array conversions and implicit arguments") {
    given Finding.Level = Finding.Level.Info
    assertFinding(found, "intermediate-collections", "Chains.longer: filter then map is a collect")
    assertFinding(found, "intermediate-collections", "Chains.looped: filter then map is a collect")
    assertEquals(of("longer").size, 1, found.mkString("\n"))
    assertEquals(of("looped").size, 1, found.mkString("\n"))
  }

  test("pure functions of a fusable run are advised exactly, effectful ones by their report") {
    given Finding.Level = Finding.Level.Info
    assertFinding(found, "intermediate-collections", "Chains.fused: use one map for map then map")
    assertFinding(found, "intermediate-collections", "Chains.filtered: use one filter for filter then filter")
    assertFinding(found, "intermediate-collections", "Chains.effectful:", "order of their effects would change")
    assertEquals(of("fused").size + of("filtered").size + of("effectful").size, 3, found.mkString("\n"))
  }

  test("a for-comprehension is advised as the calls it stands for, once for each chain of its generators") {
    given Finding.Level = Finding.Level.Info
    assertFinding(found, "intermediate-collections", "Chains.comprehended: use one map for map then map")
    assertFinding(found, "intermediate-collections", "Chains.comprehendedTwo: filter then map is a collect")
    assertEquals(of("comprehended").size, 1, found.mkString("\n"))
    assertEquals(of("comprehendedTwo").size, 1, found.mkString("\n"))
    assertEquals(of("comprehendedIdentity"), Vector.empty[Finding])
    assertEquals(of("comprehendedGuard"), Vector.empty[Finding])
  }

  test("a filter then a map of unrelated shapes is a collect only in a hot class") {
    given Finding.Level = Finding.Level.Info
    val sources         = Seq(
      "Main.scala"   -> "object Main { def main(args: Array[String]): Unit = println(Chains) }",
      "Chains.scala" ->
        """object Chains {
          |  def unrelated(values: List[String]): List[Long] = values.filter(_.nonEmpty).map(_.toLong)
          |}""".stripMargin
    )
    val cold = extract(sources)
    assertEquals(Pipelines.findings(cold.pipelines, cold.graph.nodes), Vector.empty[Finding])
    val hot = extract(sources, hot = Vector("Chains"))
    assertFinding(
      Pipelines.findings(hot.pipelines, hot.graph.nodes),
      "intermediate-collections",
      "Chains.unrelated: filter then map is a collect"
    )
  }

  test("a finding is placed at the line of its chain") {
    assert(of("fused").head.detail.endsWith("(line 8)"), of("fused").head.detail)
  }

  test("single transformations, options, views, idioms, user types and local methods are left alone") {
    assertNoFinding(found)(finding =>
      Vector(
        "single",
        "viewed",
        "nested",
        "local",
        "counted",
        "boxed",
        "narrowed",
        "mapped",
        "lone",
        "lazily",
        "iterated",
        "optional"
      ).exists(name =>
        finding.detail.startsWith(s"Chains.$name:")
      )
    )
  }

  private lazy val scans = {
    val result = extract(Seq(
      "Main.scala"  -> "object Main { def main(args: Array[String]): Unit = println(Scans) }",
      "Scans.scala" ->
        """object Scans {
          |  def nested(entries: List[String]): Int = {
          |    val bytes = entries.filter(e => e.endsWith(".zip")).map(_.length).sum
          |    val any   = entries.exists(e => e.endsWith(".zip"))
          |    if (any) bytes else 0
          |  }
          |  def reversed(entries: List[String]): Boolean = {
          |    val any   = entries.exists(_.endsWith(".zip"))
          |    val names = entries.filter(_.endsWith(".zip"))
          |    any && names.size > 1
          |  }
          |  def other(entries: List[String]): Boolean = {
          |    val names = entries.filter(_.endsWith(".zip"))
          |    entries.exists(_.endsWith(".rar")) && names.size > 1
          |  }
          |  def apart(entries: List[String], others: List[String]): Boolean = {
          |    val names = entries.filter(_.endsWith(".zip"))
          |    others.exists(_.endsWith(".zip")) && names.size > 1
          |  }
          |  def effectful(entries: List[String]): Boolean = {
          |    val names = entries.filter { e => println(e); e.endsWith(".zip") }
          |    entries.exists { e => println(e); e.endsWith(".zip") } && names.size > 1
          |  }
          |  def optional(entry: Option[String]): Boolean = {
          |    val name = entry.filter(_.endsWith(".zip"))
          |    entry.exists(_.endsWith(".zip")) && name.size > 1
          |  }
          |  def only(entries: List[String]): Boolean = entries.exists(_.endsWith(".zip"))
          |}""".stripMargin
    ))
    Pipelines.findings(result.pipelines, result.graph.nodes).filter(_.rule.id == "repeated-predicate")
  }

  test("an exists that repeats the predicate of a filter on the same collection is a nonEmpty of the filtered one") {
    assertFinding(scans, "repeated-predicate", "Scans.nested", "exists", "filter", "nonEmpty")(using
      Finding.Level.Info
    )
    assertFinding(scans, "repeated-predicate", "Scans.reversed")(using Finding.Level.Info)
  }

  test("an exists and a filter that differ in predicate, collection, effects or kind are no repeated predicate") {
    val quiet = Vector("other", "apart", "effectful", "optional", "only")
    assertNoFinding(scans)(finding => quiet.exists(name => finding.detail.startsWith(s"Scans.$name:")))
  }

  private lazy val complements = {
    val result = extract(Seq(
      "Main.scala"   -> "object Main { def main(args: Array[String]): Unit = println(Counts) }",
      "Counts.scala" ->
        """object Counts {
          |  def covers(urls: List[String]): Int = {
          |    val downloaded = urls.map(u => u.length)
          |    urls.size - downloaded.count(_ > 3)
          |  }
          |  def sorted(urls: Vector[String]): Int = {
          |    val lengths = urls.map(_.length).sorted
          |    urls.length - lengths.count(_ > 3)
          |  }
          |  def filtered(urls: List[String]): Int = {
          |    val kept = urls.filter(_.nonEmpty)
          |    urls.size - kept.count(_.length > 3)
          |  }
          |  def apart(urls: List[String], others: List[String]): Int = {
          |    val downloaded = urls.map(u => u.length)
          |    others.size - downloaded.count(_ > 3)
          |  }
          |  def reversed(urls: List[String]): Int = {
          |    val downloaded = urls.map(u => u.length)
          |    downloaded.count(_ > 3) - urls.size
          |  }
          |  def effectful(urls: List[String]): Int = {
          |    val downloaded = urls.map(u => u.length)
          |    urls.size - downloaded.count { n => println(n); n > 3 }
          |  }
          |  def distinct(urls: List[String]): Int = {
          |    val lengths = urls.toSet.map((u: String) => u.length)
          |    urls.size - lengths.count(_ > 3)
          |  }
          |}""".stripMargin
    ))
    Pipelines.findings(result.pipelines, result.graph.nodes).filter(_.rule.id == "complement-count")
  }

  test("a count subtracted from the size of the collection its subject maps one to one is a count of the failures") {
    assertFinding(complements, "complement-count", "Counts.covers", "urls minus downloaded.count", "negated")(using
      Finding.Level.Info
    )
    assertFinding(complements, "complement-count", "Counts.sorted", "urls minus lengths.count")(using
      Finding.Level.Info
    )
  }

  test("a count that is no complement of the size of its source is left alone") {
    val quiet = Vector("filtered", "apart", "reversed", "effectful", "distinct")
    assertNoFinding(complements)(finding => quiet.exists(name => finding.detail.startsWith(s"Counts.$name:")))
  }
}
