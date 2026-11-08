package net.kurobako.bonsai.analysis

/** Tests of the file system that run before pure tests they could short-circuit. */
class OrderSuite extends AnalysisSuite {

  private lazy val ordered = extract(program(
    "Order",
    """import java.nio.file.Files
      |import java.nio.file.Path
      |object Order {
      |  def regular(p: Path): Boolean = Files.isRegularFile(p)
      |  def ext(p: Path, e: String): Boolean = p.toString.endsWith(e)
      |  def archive(p: Path): Boolean = regular(p) && ext(p, "zip")
      |  def skipped(p: Path, link: Boolean): Boolean = link || !regular(p) || !ext(p, "7z")
      |  def direct(p: Path): Boolean = Files.exists(p) && p.toString.nonEmpty
      |  def pure(p: Path): Boolean = ext(p, "zip") && regular(p)
      |  def twice(p: Path): Boolean = regular(p) && Files.isReadable(p)
      |  def guarded(p: Path, xs: Vector[Int]): Boolean = Files.exists(p) && xs(0) > 0
      |  def written(p: Path): Boolean = Files.deleteIfExists(p) && ext(p, "zip")
      |}""".stripMargin
  ))

  private lazy val orders = Purity.findings(ordered.purity, ordered.graph.nodes)

  test("a test of the file system before a pure test in a conjunction or disjunction is reported") {
    assertFinding(orders, "io-before-pure", "Order.archive", "regular(p)", "ext(p, \"zip\")")(using Finding.Level.Info)
    assertFinding(orders, "io-before-pure", "Order.skipped", "!regular(p)", "!ext(p, \"7z\")")(using Finding.Level.Info)
    assertFinding(orders, "io-before-pure", "Order.direct", "Files.exists(p)")(using Finding.Level.Info)
  }

  test("pure tests first, two tests of the file system, tests that may throw, and effects that act are not reported") {
    val flagged = orders.filter(_.rule.id == "io-before-pure").map(_.detail)
    assertEquals(flagged.size, 3, flagged.mkString("\n"))
  }
}
