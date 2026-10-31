package net.kurobako.bonsai.analysis

class CostFactsSuite extends AnalysisSuite {
  private given Finding.Level = Finding.Level.Warning

  private lazy val nullary = extract(
    program(
      "Nullary",
      """object Nullary {
        |  class Holder(val items: Vector[Int]) {
        |    def pairs: Int = items.map(item => items.count(_ == item)).sum
        |    def twice: Int = pairs + pairs
        |    def spin: Int = if (items.isEmpty) 0 else spin
        |    def plain: Int = items.size
        |  }
        |  def run(holder: Holder): Int = holder.pairs
        |  def looped(holder: Holder, keys: Vector[Int]): Int = keys.map(_ => holder.plain).sum
        |  def squared(holder: Holder, keys: Vector[Int]): Int = keys.map(_ => holder.pairs).sum
        |  def spinning(holder: Holder): Int = holder.spin
        |  def generic[T]: Vector[T] = Vector.empty
        |  def viaGeneric(keys: Vector[Int]): Int = keys.map(_ => generic[Int].size).sum
        |}""".stripMargin
    )
  )

  test("a call of a method without parameter lists costs its callee's bound, as an applied call does") {
    val findings = Complexity.findings(nullary.complexities, nullary.graph.nodes, nullary.hot)
    assertFinding(findings, "complexity", "Nullary.squared is O(|holder.items| * |keys|)")
    assertNoFinding(findings)(finding =>
      Vector("looped", "run").exists(name => finding.detail.startsWith(s"Nullary.$name"))
    )
  }

  private def unreported(findings: Vector[Finding], names: String*): Unit =
    assertNoFinding(findings)(finding => names.exists(name => finding.detail.startsWith(s"Constructed.$name")))

  private lazy val constructed = extract(
    program(
      "Constructed",
      """object Constructed {
        |  class Copy(items: Vector[Int]) {
        |    val all = items.toArray
        |    def total: Int = all.length
        |  }
        |  class Walking(items: Vector[Int]) {
        |    items.foreach(item => items.contains(item))
        |  }
        |  class Paired(items: Vector[Int]) {
        |    def this(items: Vector[Int], keys: Vector[Int]) = {
        |      this(items)
        |      keys.foreach(key => items.contains(key))
        |    }
        |  }
        |  class Chained(items: Vector[Int]) {
        |    val all = items.toArray
        |    def this(items: Vector[Int], keys: Vector[Int]) = this(items)
        |  }
        |  class Bag(items: Vector[Int]) extends Iterable[Int] {
        |    val all = items.toArray
        |    def iterator: Iterator[Int] = all.iterator
        |  }
        |  class Plain(val items: Vector[Int])
        |  case class Record(items: Vector[Int])
        |  def spread(rows: Vector[Int], keys: Vector[Int]): Int = keys.count(_ => new Copy(rows).total > 0)
        |  def chained(rows: Vector[Int], keys: Vector[Int]): Int = keys.count(_ => new Chained(rows, keys) != null)
        |  def plain(rows: Vector[Int], keys: Vector[Int]): Int = keys.count(_ => new Plain(rows) != null)
        |  def record(rows: Vector[Int], keys: Vector[Int]): Int = keys.count(_ => Record(rows) != null)
        |  def once(rows: Vector[Int]): Int = new Copy(rows).total
        |  def mapped(rows: Vector[Int], keys: Vector[Int]): Vector[Int] = keys.map(_ => new Copy(rows).total)
        |  def read(rows: Vector[Int], keys: Vector[Int]): Vector[Int] = keys.map(_ => new Copy(rows).all.length)
        |  def folded(rows: Vector[Int], keys: Vector[Int]): Int = keys.foldLeft(0)((n, _) => n + new Copy(rows).total)
        |  def bags(rows: Vector[Int], keys: Vector[Int]): Vector[Bag] = keys.map(_ => new Bag(rows))
        |  def kept(rows: Vector[Int], keys: Vector[Int]): Vector[Copy] = keys.map(_ => new Copy(rows))
        |}""".stripMargin
    )
  )

  test("a constructor costs its field initialisers and body statements, charged to the code that builds the object") {
    val findings = Complexity.findings(constructed.complexities, constructed.graph.nodes, constructed.hot)
    assertFinding(findings, "complexity", "Constructed.spread is O(|keys| * |rows|)")
    assertFinding(findings, "complexity", "Constructed.chained is O(|keys| * |rows|)")
    unreported(findings, "plain", "record", "once")
  }

  test("a constructor called in a traversal is charged whether the traversal keeps the object or reads a field of it") {
    val findings = Complexity.findings(constructed.complexities, constructed.graph.nodes, constructed.hot)
    Vector("mapped", "read", "folded", "kept").foreach(name =>
      assertFinding(findings, "complexity", s"Constructed.$name is O(|keys| * |rows|)")
    )
  }

  test("a collection built per element of a traversal and kept as its result is charged like any other work") {
    val findings = Complexity.findings(constructed.complexities, constructed.graph.nodes, constructed.hot)
    assertFinding(findings, "complexity", "Constructed.bags is O(|keys| * |rows|)")
  }

  test("the statements of the primary and of a secondary constructor are walked like a method body") {
    val findings = Complexity.findings(constructed.complexities, constructed.graph.nodes, constructed.hot)
    assertFinding(findings, "complexity", "Walking", "O(|items|^2)")
    assertFinding(findings, "complexity", "Paired", "O(|items| * |keys|)")
    assertNoFinding(findings)(finding => finding.detail.contains("Plain") || finding.detail.contains("Record"))
  }

  private val retaining = Seq(
    "Main.scala" ->
      "object Main { def main(args: Array[String]): Unit = println(Retaining.pairs(Vector(1), Vector(2))) }",
    "Retaining.scala" ->
      """object Retaining {
        |  class Bag(items: Vector[Int]) extends Iterable[Int] {
        |    val all = items.toArray
        |    def iterator: Iterator[Int] = all.iterator
        |  }
        |  def pairs(xs: Vector[Int], ys: Vector[Int]): Vector[Vector[Int]] = xs.map(x => ys.map(y => y + x))
        |  def bags(xs: Vector[Int], ys: Vector[Int]): Vector[Bag] = xs.map(_ => new Bag(ys))
        |  def sums(xs: Vector[Int], ys: Vector[Int]): Vector[Int] = xs.map(x => ys.map(_ + x).sum)
        |}""".stripMargin
  )

  private def retained(bounded: Vector[Complexity.Bounded]) = {
    val result = extract(retaining, bounded = bounded)
    assertEquals(result.unusedBounds, Vector.empty)
    Complexity.findings(result.complexities, result.graph.nodes, result.hot)
  }

  test("a collection built per element of an unbounded traversal is reported whether or not the result is kept") {
    val findings = retained(Vector.empty)
    Vector("pairs", "bags", "sums").foreach(name =>
      assertFinding(findings, "complexity", s"Retaining.$name is O(|xs| * |ys|)")
    )
  }

  test("a collection built per element of a traversal is linear when the collection built from has a declared bound") {
    val findings = retained(
      Vector(
        Complexity.Bounded("Retaining.pairs.ys", 4, "four at most"),
        Complexity.Bounded("Retaining.bags.ys", 4, "four at most"),
        Complexity.Bounded("Retaining.sums.ys", 4, "four at most")
      )
    )
    assertNoFinding(findings)(_.detail.startsWith("Retaining."))
  }

  test("a bound on the traversed collection makes the work per element a constant") {
    val findings = retained(
      Vector(
        Complexity.Bounded("Retaining.pairs.xs", 4, "four at most"),
        Complexity.Bounded("Retaining.bags.xs", 4, "four at most"),
        Complexity.Bounded("Retaining.sums.xs", 4, "four at most")
      )
    )
    assertNoFinding(findings)(_.detail.startsWith("Retaining."))
  }

  private lazy val fields = extract(
    program(
      "Fields",
      """object Fields {
        |  class Copy(items: Vector[Int]) {
        |    val all = items.toArray
        |  }
        |  class Holder(val items: Vector[Int])
        |  def direct(rows: Vector[Int]): Int = new Copy(rows).all.count(a => rows.contains(a))
        |  def viaLocal(rows: Vector[Int]): Int = {
        |    val copy = new Copy(rows)
        |    copy.all.count(a => rows.contains(a))
        |  }
        |  def passed(rows: Vector[Int]): Int = new Holder(rows).items.count(a => rows.contains(a))
        |  def kept(rows: Vector[Int]): Int = {
        |    val holder = new Holder(rows)
        |    holder.items.count(a => rows.contains(a))
        |  }
        |  def per(rows: Vector[Int], keys: Vector[Int]): Int = keys.map(k => new Copy(rows).all.count(_ == k)).sum
        |  def anonymous(rows: Vector[Int], keys: Vector[Int]): Int = keys.count(_ => new Copy(rows) {}.all.length > 0)
        |  def local(rows: Vector[Int], keys: Vector[Int]): Int = {
        |    class Own(items: Vector[Int]) { val all = items.toArray }
        |    keys.count(_ => new Own(rows).all.length > 0)
        |  }
        |}""".stripMargin
    )
  )

  test("a field of a constructed object keeps the size of the collection it was built from") {
    val findings = Complexity.findings(fields.complexities, fields.graph.nodes, fields.hot)
    Vector("direct", "viaLocal", "passed", "kept").foreach(name =>
      assertFinding(findings, "complexity", s"Fields.$name is O(|rows|^2)")
    )
    assertFinding(findings, "complexity", "Fields.per is O(|keys| * |rows|)")
    assertFinding(findings, "complexity", "Fields.anonymous is O(|keys| * |rows|)")
    assertFinding(findings, "complexity", "Fields.local is O(|keys| * |rows|)")
  }
}
