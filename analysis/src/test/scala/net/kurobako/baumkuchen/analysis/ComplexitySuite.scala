package net.kurobako.baumkuchen.analysis

class ComplexitySuite extends AnalysisSuite {
  private lazy val found = {
    val result = extract(Seq(
      "Main.scala"   -> "object Main { def main(args: Array[String]): Unit = println(Search) }",
      "Search.scala" ->
        """object Search {
                          |  def pairs(rules: List[Int], facts: List[Int]): Int = rules.count(rule => facts.contains(rule))
                          |  def squares(values: Vector[Int]): Int = values.map(value => values.count(_ == value)).sum
                          |  def linear(values: Vector[Int]): Int = values.map(_ + 1).sum
                          |  def nested(rows: Vector[Vector[Int]]): Int = rows.map(row => row.sum).sum
                          |  def caller(values: Vector[Int]): Int = squares(values) + 1
                          |  def composed(left: List[Int], right: Vector[Int]): Int = left.map(value => linear(right)).sum
                          |  def looping(values: Vector[Int]): Int = {
                          |    var total = 0
                          |    while (total < 10) total += values.sum
                          |    total
                          |  }
                          |  def indexed(values: Vector[Int]): Int = values.indices.map(index => values(index)).sum
                          |  def listIndexed(values: List[Int]): Int = values.indices.map(index => values(index)).sum
          |  def filtered(values: Vector[Int]): Int = {
          |    val positive = values.filter(_ > 0)
          |    positive.map(value => positive.count(_ < value)).sum
          |  }
          |  def mutualFirst(values: Vector[Int]): Int = if (values.isEmpty) 0 else mutualSecond(values)
          |  def mutualSecond(values: Vector[Int]): Int = values.count(_ > 0) + mutualFirst(values)
          |  def crossed(rows: List[Int], keys: List[Int]): Int = rows.map(_ => keys.last).sum
          |  def joined(rows: List[Int], keys: List[Int]): Int = rows.map(_ => keys.mkString.length).sum
          |  def arrays(left: Array[Int], right: Array[Int]): Int = left.map(value => right.count(_ == value)).sum
          |}""".stripMargin
    ))
    Complexity.findings(result.complexities, result.graph.nodes)
  }

  private given Finding.Level = Finding.Level.Info

  test("nested traversals and linear operations in loops are super-linear in the sizes they traverse") {
    assertFinding(found, "complexity", "Search.pairs is O(|facts| * |rules|)")
    assertFinding(found, "complexity", "Search.squares is O(|values|^2)")
    assertFinding(found, "complexity", "Search.listIndexed is O(|values|^2)")
    assertFinding(found, "complexity", "Search.filtered is O(|values|^2)")
  }

  test(
    "calls contribute their callee's cost in the caller's sizes, and inherited costs are reported where they arise"
  ) {
    assertFinding(found, "complexity", "Search.composed is O(|left| * |right|)")
    assertNoFinding(found)(_.detail.startsWith("Search.caller"))
  }

  test("linear methods, constant-time indexing and unknown sizes are not reported") {
    assertNoFinding(found)(finding =>
      Vector("linear", "nested", "looping", "indexed ").exists(name => finding.detail.startsWith(s"Search.$name"))
    )
  }

  test("mutually recursive methods are bounded rather than followed forever") {
    assertNoFinding(found)(finding =>
      finding.detail.startsWith("Search.mutualFirst") || finding.detail.startsWith("Search.mutualSecond")
    )
  }

  test("array traversals and walks or aggregates nested in traversals are super-linear") {
    assertFinding(found, "complexity", "Search.arrays is O(|left| * |right|)")
    assertFinding(found, "complexity", "Search.crossed is O(|keys| * |rows|)")
    assertFinding(found, "complexity", "Search.joined is O(|keys| * |rows|)")
  }

  private lazy val bounded = {
    val result = extract(Seq(
      "Main.scala"   -> "object Main { def main(args: Array[String]): Unit = println(Bounds) }",
      "Bounds.scala" ->
        """object Bounds {
          |  def optional(rows: Vector[Int], value: Option[Int]): Int = rows.map(row => value.map(_ + row).sum).sum
          |  def argument(values: Vector[Int]): Int = Math.abs(values.map(value => values.count(_ == value)).sum)
          |  def searched(rows: Vector[Int], keys: Vector[Int]): Int = rows.count(row => keys.contains(row))
          |  def ranged(rows: Vector[Int], range: Range): Int = rows.count(row => range.contains(row))
          |  def checked(rows: Vector[Int], values: Vector[Int]): Int = rows.count(row => values.isEmpty)
          |  def unbounded(values: Vector[Int]): Int = {
          |    var total = 0
          |    while (total < 10) total += values.count(value => values.contains(value))
          |    total
          |  }
          |}""".stripMargin
    ))
    Complexity.findings(result.complexities, result.graph.nodes)
  }

  test(
    "searches of any sequence, and expensive arguments, count; options, ranges, cheap members and unknown loops do not"
  ) {
    assertFinding(bounded, "complexity", "Bounds.argument is O(|values|^2)")
    assertFinding(bounded, "complexity", "Bounds.searched is O(|keys| * |rows|)")
    assertNoFinding(bounded)(finding =>
      Vector("optional", "ranged", "checked", "unbounded").exists(name => finding.detail.startsWith(s"Bounds.$name"))
    )
  }
}
