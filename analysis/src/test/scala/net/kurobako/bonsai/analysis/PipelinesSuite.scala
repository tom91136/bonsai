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
          |  def longer(rows: Vector[List[Int]]): Int = rows.map(values => values.map(_ + 1).filter(_ > 2).map(_ * 2).sum).sum
          |  def looped(rows: Vector[Array[Int]]): Int = rows.map(values => values.filter(_ > 0).map(_ + 1).length).sum
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
          |  def comprehendedTwo(values: List[Int]): List[Int] =
          |    for { x <- values; y <- values.map(_ + 1).filter(_ > 0) } yield x + y
          |  def comprehendedIdentity(values: List[Int]): List[Int] = for (x <- values) yield x
          |}""".stripMargin
    ))
    Pipelines.findings(result.pipelines, result.graph.nodes)
  }

  private def of(name: String): Vector[Finding] = found.filter(_.detail.startsWith(s"Chains.$name:"))

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
}
