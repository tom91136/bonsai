package net.kurobako.bonsai.analysis

class InvariantsSuite extends AnalysisSuite {
  private def findings(sources: (String, String)*): Vector[Finding] = findingsWithTests(sources, Nil)

  private def findingsWithTests(main: Seq[(String, String)], test: Seq[(String, String)]): Vector[Finding] = {
    val result = extract(main, test)
    Invariants.findings(result.algebra, result.graph.nodes)
  }

  test("an invariant that excludes states of optional fields describes a sum") {
    assertFinding(
      findings(
        "Source.scala" ->
          "case class Source(toolkit: Option[String], image: Option[Int]) { require(toolkit.isEmpty || image.isEmpty) }"
      ),
      "invariant-sum",
      "rejects 1 of 4 states of (toolkit, image)",
      "1 + Int + String valid",
      "as a sum: none | image | toolkit"
    )
  }

  test("a flag equal to an option's presence is redundant") {
    assertFinding(
      findings("Load.scala" ->
        "case class Load(done: Boolean, value: Option[Int]) { require(done == value.isDefined) }"),
      "invariant-redundant",
      "done is determined by (value)"
    )
  }

  test("an invariant that forces a value makes the field constant") {
    assertFinding(
      findings("Ready.scala" -> "case class Ready(value: Option[Int]) { require(value.isDefined) }"),
      "invariant-constant",
      "forces value to Present"
    )
  }

  test("enum tags take part in the decomposition") {
    assertFinding(
      findings(
        "Access.scala" ->
          """enum Mode { case Load, Store }
                            |case class Access(mode: Mode, value: Option[Int]) { require(mode != Mode.Load || value.isEmpty) }""".stripMargin
      ),
      "invariant-sum",
      "rejects 1 of 4 states",
      "as a sum: mode=Load | mode=Store | mode=Store+value"
    )
  }

  test("constructions with literal arguments that break the invariant are violations") {
    assertFinding(
      findings(
        "Size.scala" -> "case class Size(count: Int) { require(count >= 0 && 64 >= count) }",
        "Main.scala" -> "object Main {\n  val ok = Size(2)\n  val negative = Size(-1)\n  val large = Size(65)\n}"
      ),
      "invariant-violation",
      "2 constructions violate the invariant",
      "Main.scala:3",
      "Main.scala:4"
    )
  }

  test("collections kept the same size are parallel") {
    assertFinding(
      findings(
        "Table.scala" ->
          "case class Table(names: Vector[String], values: Vector[Int]) { require(names.size == values.length) }"
      ),
      "parallel-collections",
      "names and values are kept the same size"
    )
  }

  test("constructions outside the companion bypass a validating factory") {
    val found = findings(
      "Port.scala" ->
        "case class Port(number: Int)\nobject Port { def of(number: Int): Option[Port] = Option.when(number > 0)(new Port(number)) }",
      "Main.scala" -> "object Main {\n  val direct = Port(5)\n  val checked = Port.of(3)\n}"
    )
    assertFinding(found, "bypassed-factory", "1 of 2 constructions bypass Port.of", "Main.scala:2")
  }

  test("private constructors cannot be bypassed") {
    val found = findings(
      "Port.scala" ->
        "case class Port private (number: Int)\nobject Port { def of(number: Int): Option[Port] = Option.when(number > 0)(new Port(number)) }",
      "Main.scala" -> "object Main { val checked = Port.of(3) }"
    )
    assertNoFinding(found)(_.rule == Finding.Rule.BypassedFactory)
  }

  test("parts outside the vocabulary leave states undecided") {
    val found = findings(
      "Mixed.scala" ->
        "case class Mixed(label: String, value: Option[Int]) { require(label.startsWith(\"x\") || value.isDefined) }"
    )
    assertEquals(found, Vector.empty)
  }

  test("method preconditions over parameters describe sums like class invariants") {
    assertFinding(
      findings(
        "Harness.scala" -> """object Harness {
                             |  def run(name: String, toolkit: Option[String], image: Option[String]): Unit = {
                             |    require(toolkit.isEmpty || image.isEmpty, "set either toolkit or image")
                             |    println(name)
                             |  }
                             |}""".stripMargin
      ),
      "invariant-sum",
      "Harness.run(name, toolkit, image): the precondition rejects 1 of 4 states of (toolkit, image)"
    )
  }

  test("calls with literal arguments that break a precondition are violations") {
    assertFinding(
      findings(
        "Sizes.scala" -> "object Sizes { def checked(count: Int): Int = { require(count >= 0); count } }",
        "Main.scala"  -> "object Main {\n  val ok = Sizes.checked(1)\n  val bad = Sizes.checked(-1)\n}"
      ),
      "invariant-violation",
      "1 calls violate the precondition",
      "Main.scala:3"
    )
  }

  test("default argument getters and parsers are not validating factories") {
    val found = findings(
      "Settings.scala" ->
        """case class Settings(port: Int, next: Option[Settings] = None)
                            |object Settings { def parse(text: String): Option[Settings] = text.toIntOption.map(Settings(_)) }""".stripMargin,
      "Main.scala" -> "object Main { val settings = List(Settings(1), Settings(2, Some(Settings(3)))) }"
    )
    assertNoFinding(found)(_.rule == Finding.Rule.BypassedFactory)
  }

  test("violations only in tests are deliberate negative tests") {
    val found = findingsWithTests(
      Seq("Sizes.scala"      -> "object Sizes { def checked(count: Int): Int = { require(count >= 0); count } }"),
      Seq("SizesCheck.scala" -> "class SizesCheck { def rejects = Sizes.checked(-1) }")
    )
    assertFinding(found, "invariant-violation", "1 calls violate the precondition", "(all in tests)")(using
      Finding.Level.Info
    )
  }

  test("more than three violating sites are counted, not all listed") {
    val found = findings(
      "Sizes.scala" -> "object Sizes { def checked(count: Int): Int = { require(count >= 0); count } }",
      "Main.scala"  ->
        """object Main {
          |  val first = Sizes.checked(-1)
          |  val second = Sizes.checked(-2)
          |  val third = Sizes.checked(-3)
          |  val fourth = Sizes.checked(-4)
          |}""".stripMargin
    )
    assertFinding(found, "invariant-violation", "4 calls violate the precondition", "1 more")
  }

  test("a bound held in a constant val is read") {
    assertFinding(
      findings(
        "Limits.scala" ->
          """object Limits {
            |  final val Maximum = 10
            |  case class Bounded(count: Int) { require(count <= Maximum) }
            |}""".stripMargin,
        "Main.scala" -> "object Main { val ok = Limits.Bounded(10); val bad = Limits.Bounded(11) }"
      ),
      "invariant-violation",
      "Bounded: 1 constructions violate the invariant"
    )
  }

  test("the state space a decomposition enumerates is capped") {
    def flags(names: String)   = names.map(letter => s"$letter: Boolean").mkString(", ")
    def rejects(names: String) = names.map(letter => s"$letter == false").mkString(" || ")
    val letters                = "abcdefghijklm"
    val found                  = findings(
      "Bits.scala" ->
        s"""case class Small(${flags(letters.take(12))}) {
           |  require(${rejects(letters.take(12))})
           |}
           |case class Big(${flags(letters)}) {
           |  require(${rejects(letters)})
           |}""".stripMargin
    )
    assertFinding(found, "invariant-sum", "Small: the invariant rejects 1 of 4096 states")
    assertNoFinding(found)(_.detail.startsWith("Big:"))
  }
}
