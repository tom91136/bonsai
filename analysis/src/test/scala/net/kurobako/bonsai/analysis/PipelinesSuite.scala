package net.kurobako.bonsai.analysis

class PipelinesSuite extends AnalysisSuite {
  private lazy val found = {
    val result = extract(Seq(
      "Main.scala"   -> "object Main { def main(args: Array[String]): Unit = println(Chains) }",
      "Chains.scala" ->
        """object Chains {
          |  def pipeline(values: List[Int]): Int = values.map(_ + 1).filter(_ > 0).size
          |  def single(values: List[Int]): Int = values.map(_ + 1).size
          |  def viewed(values: List[Int]): Int = values.view.map(_ + 1).filter(_ > 0).size
          |  def arrays(values: Array[Int]): Int = values.map(_ + 1).filter(_ > 0).size
          |  def nested(values: List[List[Int]]): Int = values.map(row => row.filter(_ > 0).size).sum
          |  def local(values: List[Int]): Int = {
          |    def inner(items: List[Int]) = items.map(_ + 1).filter(_ > 0).size
          |    inner(values)
          |  }
          |}""".stripMargin
    ))
    Pipelines.findings(result.pipelines, result.graph.nodes)
  }

  test("chains of strict transformations are reported once per method") {
    given Finding.Level = Finding.Level.Info
    assertFinding(
      found,
      "intermediate-collections",
      "Chains.pipeline chains map then filter",
      "use .view or a single pass"
    )
    assertFinding(found, "intermediate-collections", "Chains.arrays chains map then filter")
    assertEquals(found.size, 2, found.mkString("\n"))
  }

  test("single transformations, views and local methods are left alone") {
    assertNoFinding(found)(finding =>
      Vector("single", "viewed", "nested", "local").exists(name => finding.detail.startsWith(s"Chains.$name"))
    )
  }
}
