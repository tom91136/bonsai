package net.kurobako.bonsai.analysis

class ComplexityMemoSuite extends AnalysisSuite {
  private lazy val found = {
    val result = extract(
      program(
        "Memo",
        """import scala.collection.mutable
          |
          |object Memo {
          |class Graph(order: Vector[Int]) {
          |  private val depths = mutable.HashMap.empty[Int, Int]
          |  private val peeked = mutable.HashMap.empty[Int, Int]
          |  private val shifts = mutable.HashMap.empty[Int, Int]
          |  private val cached = mutable.HashMap.empty[Int, Int]
          |
          |  def memoised(nodes: Vector[Int]): Vector[Int] = nodes.map(node => depth(node))
          |  def updating(nodes: Vector[Int]): Vector[Int] = nodes.map(node => cache(node))
          |  def unstored(nodes: Vector[Int]): Vector[Int] = nodes.map(node => peek(node))
          |  def rekeyed(nodes: Vector[Int]): Vector[Int] = nodes.map(node => shifted(node))
          |  def scanned(nodes: Vector[Int]): Vector[Int] = nodes.map(node => order.count(_ == node))
          |
          |  def depth(node: Int): Int = depths.getOrElse(node, visit(node))
          |  private def visit(node: Int): Int = {
          |    depths(node) = 0
          |    val result = order.count(_ == node) + (if (node > 0) depth(node - 1) else 0)
          |    depths(node) = result
          |    result
          |  }
          |
          |  def peek(node: Int): Int = peeked.getOrElse(node, search(node))
          |  private def search(node: Int): Int = order.count(_ == node) + (if (node > 0) peek(node - 1) else 0)
          |
          |  def shifted(node: Int): Int = shifts.getOrElse(node, shift(node))
          |  private def shift(node: Int): Int = {
          |    shifts(0) = 0
          |    order.count(_ == node) + (if (node > 0) shifted(node - 1) else 0)
          |  }
          |
          |  def cache(node: Int): Int = cached.getOrElseUpdate(node, fill(node))
          |  private def fill(node: Int): Int = order.count(_ == node) + (if (node > 0) cache(node - 1) else 0)
          |}
          |}""".stripMargin
      )
    )
    Complexity.findings(result.complexities, result.graph.nodes, result.hot)
  }

  private given Finding.Level = Finding.Level.Warning

  private def about(name: String): Vector[Finding] = found.filter(_.detail.startsWith(s"Graph.$name "))

  test("a call to a memoised search that stores the key it looked up is charged once across a loop") {
    assertEquals(about("memoised").map(_.detail), Vector.empty)
  }

  test("a call to a getOrElseUpdate memo whose miss reaches it again is charged once across a loop") {
    assertEquals(about("updating").map(_.detail), Vector.empty)
  }

  test("a search with the same shape that never stores the key is charged on every turn") {
    assertFinding(found, "complexity", "Graph.unstored is O(|nodes| * |this.order|)")
  }

  test("a search that stores a key other than the one it looked up is charged on every turn") {
    assertFinding(found, "complexity", "Graph.rekeyed is O(|nodes| * |this.order|)")
  }

  test("a scan in a loop is still charged on every turn") {
    assertFinding(found, "complexity", "Graph.scanned is O(|nodes| * |this.order|)")
  }
}
