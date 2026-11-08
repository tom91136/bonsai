package net.kurobako.bonsai.analysis

class ComplexitySuite extends AnalysisSuite {
  private def complexities(name: String, source: String) = {
    val result = extract(program(name, source))
    Complexity.findings(result.complexities, result.graph.nodes, result.hot)
  }

  private lazy val found = complexities(
    "Search",
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
  )

  private given Finding.Level = Finding.Level.Warning

  test("a super-linear site is located at its own line in its own file") {
    val padding = (1 to 20).map(index => s"  val padded$index = $index").mkString("\n")
    val result  = extract(Seq(
      "First.scala" ->
        s"object First {\n$padding\n  def pairs(xs: List[Int]): Int = xs.map(x => xs.count(_ == x)).sum\n}",
      "Second.scala" -> "object Second {\n  def pairs(ys: List[Int]): Int = ys.map(y => ys.count(_ == y)).sum\n}"
    ))
    val located = Complexity.findings(result.complexities, result.graph.nodes, result.hot)
    assertFinding(
      located,
      "complexity",
      "First.pairs is O(|xs|^2) in time: count over |xs| (line 22) inside map over |xs| (line 22)"
    )
    assertFinding(
      located,
      "complexity",
      "Second.pairs is O(|ys|^2) in time: count over |ys| (line 2) inside map over |ys| (line 2)"
    )
  }

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

  private lazy val bounded = complexities(
    "Bounds",
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
  )

  test(
    "searches of any sequence, and expensive arguments, count; options, ranges, cheap members and unknown loops do not"
  ) {
    assertFinding(bounded, "complexity", "Bounds.argument is O(|values|^2)")
    assertFinding(bounded, "complexity", "Bounds.searched is O(|keys| * |rows|)")
    assertNoFinding(bounded)(finding =>
      Vector("optional", "ranged", "checked", "unbounded").exists(name => finding.detail.startsWith(s"Bounds.$name"))
    )
  }

  private lazy val probed = complexities(
    "Table",
    """object Table {
          |  final class Queue {
          |  private var keys  = Array.ofDim[Long](8)
          |  private var count = 0
          |  def find(at: Long): Int = {
          |    val mask = keys.length - 1
          |    var i    = (at & mask).toInt
          |    while (keys(i) != 0L && keys(i) != at) i = (i + 1) & mask
          |    if (keys(i) == at) i else -1
          |  }
          |  def place(at: Long): Unit = {
          |    val mask = keys.length - 1
          |    var i    = (at & mask).toInt
          |    while (keys(i) != 0L) i = (i + 1) & mask
          |    keys(i) = at
          |  }
          |  def add(at: Long): Unit = {
          |    if ((count + 1) * 2 > keys.length) {
          |      val old = keys
          |      keys = Array.ofDim[Long](old.length * 2)
          |      var i = 0
          |      while (i < old.length) {
          |        if (old(i) != 0L) place(old(i))
          |        i += 1
          |      }
          |    }
          |    place(at)
          |    count += 1
          |  }
          |  }
          |  def run(queue: Queue, values: Vector[Long]): Unit = values.foreach(value => queue.add(value))
          |  def load(values: Vector[Long]): Int = values.map(value => values.count(_ == value)).sum
          |}""".stripMargin
  )

  test("probes of a table that doubles as its occupancy passes a fraction of its capacity are constant") {
    assertNoFinding(probed)(finding => finding.detail.startsWith("Table.run"))
    assertFinding(probed, "complexity", "Table.load is O(|values|^2)")
  }

  test("complexity charge in an orElse fallback mentions the orElse fallback") {
    val result = extract(Seq(
      "Main.scala"     -> "object Main { def main(args: Array[String]): Unit = println(Fallback) }",
      "Fallback.scala" ->
        """object Fallback {
          |  def lookup(keys: List[String], primary: Map[String, Int], secondary: List[String]): Int = {
          |    keys.foldLeft(0) { (acc, key) =>
          |      val found = primary.get(key).orElse(secondary.find(_ == key).map(_.length))
          |      acc + found.getOrElse(0)
          |    }
          |  }
          |}""".stripMargin
    ))
    val timed = Complexity.findings(result.complexities, result.graph.nodes, result.hot)
      .filter(_.rule == Finding.Rule.Complexity)
    assertEquals(timed.size, 1)
    assert(
      timed.head.detail.contains("(in orElse fallback)"),
      s"expected detail to mention (in orElse fallback) but was: ${timed.head.detail}"
    )
  }

  private val encodings = Seq(
    "Main.scala"     -> "object Main { def main(args: Array[String]): Unit = println(Limited) }",
    "Encoding.scala" -> "class Encoding(val operands: Vector[Int], val modifiers: Vector[Int], val extra: Vector[Int])",
    "Limited.scala"  ->
      """object Limited {
        |  def nested(encoding: Encoding, xs: Vector[Int], ys: Vector[Int]): Int =
        |    encoding.operands.map(operand => xs.map(x => ys.count(_ == x + operand)).sum).sum
        |  def paired(encoding: Encoding, other: Encoding): Int =
        |    encoding.operands.map(operand => other.modifiers.count(_ == operand)).sum
        |  def unbounded(xs: Vector[Int], ys: Vector[Int]): Int = xs.map(x => ys.count(_ == x)).sum
        |}""".stripMargin
  )

  private lazy val declared = {
    val result = extract(
      encodings,
      bounded = Vector(
        Complexity.Bounded("Encoding.operands", 4, "an instruction has at most four operands"),
        Complexity.Bounded("Encoding.modifiers", 2, "an instruction has at most two modifiers")
      )
    )
    Complexity.findings(result.complexities, result.graph.nodes, result.hot)
  }

  test("a bounded member is a constant, so the unbounded loops of its nest are reported without it") {
    assertFinding(declared, "complexity", "Limited.nested is O(|xs| * |ys|)")
    assertFinding(declared, "complexity", "Limited.unbounded is O(|xs| * |ys|)")
    assertNoFinding(declared)(_.detail.contains("operands"))
  }

  test("two bounded members in one nest are silent") {
    assertNoFinding(declared)(_.detail.startsWith("Limited.paired"))
  }

  test("a bounded member that no collection reaches is reported as unused") {
    val result = extract(
      encodings,
      bounded = Vector(
        Complexity.Bounded("Encoding.operands", 4, "an instruction has at most four operands"),
        Complexity.Bounded("Encoding.extra", 3, "an instruction has at most three extra fields")
      )
    )
    assertEquals(
      result.unusedBounds,
      Vector(Complexity.Bounded("Encoding.extra", 3, "an instruction has at most three extra fields"))
    )
    assertFinding(Build.unused(result.unusedBounds), "unused-bound", "Encoding.extra")
  }

  test("a bounded member of a class nested in an object, selected by this or through a parameter, is a constant") {
    val result = extract(
      Seq(
        "Main.scala"    -> "object Main { def main(args: Array[String]): Unit = println(Machine.run(1)) }",
        "Machine.scala" ->
          """object Machine {
            |  final class Execution(lanes: Int) {
            |    private val arrivals = Array.ofDim[Option[Int]]((lanes + 31) >>> 5)
            |    def step(values: Vector[Int]): Int = values.map(value => arrivals.count(_ == Some(value))).sum
            |    def scan(values: Vector[Int]): Int = values.map(value => this.arrivals.count(_ == Some(value))).sum
            |  }
            |  final class Format(val widths: Vector[Int])
            |  def decode(format: Format, values: Vector[Int]): Int =
            |    format.widths.map(width => format.widths.count(_ == width)).sum + values.size
            |  def run(lanes: Int): Int = new Execution(lanes).step(Vector(lanes))
            |}""".stripMargin
      ),
      bounded = Vector(
        Complexity.Bounded("Machine.Execution.arrivals", 32, "one slot per warp"),
        Complexity.Bounded("Machine.Format.widths", 4, "four widths at most")
      )
    )
    assertEquals(result.unusedBounds, Vector.empty)
    val reported = Complexity.findings(result.complexities, result.graph.nodes, result.hot)
    assertNoFinding(reported)(_.detail.startsWith("Execution."))
    assertNoFinding(reported)(_.detail.startsWith("Machine.decode"))
  }

  private lazy val arrivals = {
    val result = extract(
      Seq(
        "Main.scala"     -> "object Main { def main(args: Array[String]): Unit = println(Barriers.run(1)) }",
        "Barriers.scala" ->
          """object Barriers {
            |  final class Gate(waves: Int) {
            |    private val live    = Array.ofDim[Boolean](waves)
            |    private val arrived = Array.ofDim[Boolean](2, waves)
            |    private def complete(barrier: Int): Unit = if (live.indices.forall(wave => arrived(barrier)(wave))) ()
            |    def drop(wave: Int): Unit =
            |      arrived.indices.foreach(barrier => if (arrived(barrier).contains(true)) complete(barrier))
            |    def indexed(values: Vector[Int]): Int = arrived.indices.map(barrier => values.count(_ == barrier)).sum
            |    def sized(values: Vector[Int]): Int = (0 until arrived.size).map(i => values.count(_ == i)).sum
            |    def iterated(values: Vector[Int]): Int = arrived.iterator.map(row => values.count(_ == row.length)).sum
            |  }
            |  final class Open(waves: Int) {
            |    private val live    = Array.ofDim[Boolean](waves)
            |    private val arrived = Array.ofDim[Boolean](2, waves)
            |    private def complete(barrier: Int): Unit = if (live.indices.forall(wave => arrived(barrier)(wave))) ()
            |    def drop(wave: Int): Unit =
            |      arrived.indices.foreach(barrier => if (arrived(barrier).contains(true)) complete(barrier))
            |  }
            |  def run(waves: Int): Int = {
            |    new Gate(waves).drop(waves)
            |    new Open(waves).drop(waves)
            |    new Gate(waves).indexed(Vector(waves)) + new Gate(waves).sized(Vector(waves))
            |  }
            |}""".stripMargin
      ),
      bounded = Vector(Complexity.Bounded("Barriers.Gate.arrived", 2, "two barriers"))
    )
    Complexity.findings(result.complexities, result.graph.nodes, result.hot)
  }

  test("a loop over the indices, size or iterator of a bounded member is a constant") {
    assertNoFinding(arrivals)(_.detail.startsWith("Gate."))
    assertFinding(arrivals, "complexity", "Open.drop", "|this.arrived|", "|this.live|")
  }

  private val barriersSources = Seq(
    "Main.scala" ->
      "package net.kurobako.gemstone.isa.amdgpu\nobject Main { def main(args: Array[String]): Unit = VMState.run(4) }",
    "VMState.scala" ->
      """package net.kurobako.gemstone.isa.amdgpu
        |private object VMState {
        |  final class WorkgroupBarriers(waves: Int) {
        |    private val live      = Array.ofDim[Boolean](waves)
        |    private val arrived   = Array.ofDim[Boolean](2, waves)
        |    private val completed = Array.ofDim[Boolean](2, waves)
        |    java.util.Arrays.fill(live, true)
        |    private def complete(barrier: Int): Unit =
        |      if (live.indices.forall(wave => !live(wave) || arrived(barrier)(wave))) {
        |        java.util.Arrays.fill(arrived(barrier), false)
        |        java.util.Arrays.fill(completed(barrier), true)
        |      }
        |    def signal(id: Int, wave: Int): Boolean = {
        |      arrived(id)(wave) = true
        |      complete(id)
        |      completed(id).contains(true)
        |    }
        |    def drop(wave: Int): Unit = {
        |      live(wave) = false
        |      arrived.indices.foreach { barrier =>
        |        if (arrived(barrier).contains(true)) complete(barrier)
        |      }
        |    }
        |  }
        |  def run(waves: Int): Unit = new WorkgroupBarriers(waves).drop(0)
        |}""".stripMargin
  )

  private def barriersFound(bounded: Vector[Complexity.Bounded]) = {
    val result = extract(barriersSources, bounded = bounded)
    Complexity.findings(result.complexities, result.graph.nodes, result.hot)
  }

  private lazy val barriersOpen    = barriersFound(Vector.empty)
  private lazy val barriersBounded = barriersFound(
    Vector(
      Complexity.Bounded(
        "net.kurobako.gemstone.isa.amdgpu.VMState.WorkgroupBarriers.arrived",
        2,
        "two barriers"
      )
    )
  )

  test("a bounded field's loops over its indices are constant in a class of a module") {
    assertFinding(barriersOpen, "complexity", "WorkgroupBarriers.drop")
    assertNoFinding(barriersBounded)(_.detail.startsWith("WorkgroupBarriers.drop"))
  }

  private val validation = Seq(
    "Main.scala" ->
      "object Main { def main(args: Array[String]): Unit = println(Validation.bounded(Vector(1), Vector(2))) }",
    "Validation.scala" ->
      """object Validation {
        |  def bounded(forms: Vector[Int], mods: Vector[Int]): Int = forms.map(form => mods.count(_ == form)).sum
        |  def nested(forms: Vector[Int], mods: Vector[Int], others: Vector[Int]): Int =
        |    forms.map(form => mods.map(mod => others.count(_ == mod + form)).sum).sum
        |}""".stripMargin
  )

  private lazy val parameters = {
    val result = extract(
      validation,
      bounded = Vector(
        Complexity.Bounded("Validation.bounded.forms", 4, "four forms at most"),
        Complexity.Bounded("Validation.nested.forms", 4, "four forms at most")
      )
    )
    assertEquals(result.unusedBounds, Vector.empty)
    Complexity.findings(result.complexities, result.graph.nodes, result.hot)
  }

  test("a bounded parameter is a constant, and an unbounded factor of its nest still reports") {
    assertNoFinding(parameters)(_.detail.startsWith("Validation.bounded"))
    assertFinding(parameters, "complexity", "Validation.nested is O(|mods| * |others|)")
  }

  test("a bound naming a parameter that no method has is reported as unused") {
    val result = extract(
      validation,
      bounded = Vector(Complexity.Bounded("Validation.bounded.amounts", 4, "four amounts at most"))
    )
    assertEquals(
      result.unusedBounds,
      Vector(Complexity.Bounded("Validation.bounded.amounts", 4, "four amounts at most"))
    )
    assertFinding(Build.unused(result.unusedBounds), "unused-bound", "Validation.bounded.amounts")
  }

  test("a return in a loop is no call of its method") {
    val early = complexities(
      "Early",
      """object Early {
        |  def first(values: Vector[Int]): Either[String, Int] = {
        |    var index = 0
        |    while (index < values.size) {
        |      if (values(index) < 0) return Left("negative")
        |      index += 1
        |    }
        |    Right(index)
        |  }
        |}""".stripMargin
    )
    assertEquals(early.map(_.detail), Vector.empty)
  }

  test("where the build marks classes hot, quadratic findings elsewhere are dropped and cubic ones stay") {
    def reported(hot: Vector[String]) = {
      val result = extract(
        program(
          "Cold",
          """object Cold {
            |  def squares(values: Vector[Int]): Int = values.map(value => values.count(_ == value)).sum
            |  def cubes(values: Vector[Int]): Int =
            |    values.map(a => values.map(b => values.count(_ == a + b)).sum).sum
            |}
            |object Warm {
            |  def squares(values: Vector[Int]): Int = values.map(value => values.count(_ == value)).sum
            |}""".stripMargin
        ),
        hot = hot
      )
      Complexity.findings(result.complexities, result.graph.nodes, result.hot).map(_.detail.takeWhile(_ != ' '))
    }
    assertEquals(reported(Vector.empty).toSet, Set("Cold.squares", "Cold.cubes", "Warm.squares"))
    assertEquals(reported(Vector("Warm")).toSet, Set("Cold.cubes", "Warm.squares"))
  }
}
