package net.kurobako.baumkuchen.analysis

class MembersSuite extends AnalysisSuite {
  private def findings(main: Seq[(String, String)], test: Seq[(String, String)] = Nil): Vector[Finding] = {
    val result = extract(main, test)
    Members.findings(result.members, result.graph.nodes, Liveness.live(result.graph))
  }

  private def entry(body: String) = "Main.scala" -> s"object Main { def main(args: Array[String]): Unit = { $body } }"

  test("public members nothing references are unused") {
    val found =
      findings(Seq(entry("println(Tools.used)"), "Tools.scala" -> "object Tools { def used = 1; def spare = 2 }"))
    assertFinding(found, "unused-member", "Tools.spare is never referenced")
    assertNoFinding(found)(_.detail.contains("Tools.used"))
  }

  test("members sharing a name are told apart by their parameter types") {
    val found = findings(Seq(
      entry("val bound: Tools.>[Int, 0] = Tools.twice(1); println(bound)"),
      "Tools.scala" ->
        """object Tools {
          |  infix type >[A, N] = A
          |  def twice(value: Int): Int   = value * 2
          |  def twice(value: Long): Long = value * 2
          |  extension (value: Int) def >(other: Long): Boolean = value > other
          |}""".stripMargin
    ))
    assertFinding(found, "unused-member", "Tools.twice(Long) is never referenced")
    assertFinding(found, "unused-member", "Tools.>(Int)(Long) is never referenced")
    assertNoFinding(found)(finding => finding.detail.contains("twice(Int)") || finding.detail.startsWith("Tools.> "))
  }

  test("public and protected members of a library's public types are its API") {
    val result = extract(
      Seq("Client.scala" ->
        """package lib
          |class Client {
          |  def fetch: Int = 1
          |  def spare: Int = 2
          |  def own: Int = spare + Inner().value
          |  class Inner { def value: Int = 7 }
          |  protected def hook: Int = 3
          |  var limit: Int = 4
          |  private[lib] def internal: Int = 5
          |}
          |private[lib] class Hidden { def spare: Int = 6 }
          |object Main { def main(args: Array[String]): Unit = println(Hidden()) }""".stripMargin),
      Seq("ClientCheck.scala" -> "class ClientCheck { val value = lib.Client().fetch }"),
      library = true
    )
    val found = Members.findings(result.members, result.graph.nodes, Liveness.live(result.graph))
    assertEquals(
      found.map(_.detail).sorted,
      Vector("Client.internal is never referenced", "Hidden.spare is never referenced")
    )
  }

  test("dynamic dispatch reaches implementations, and an unused declaration is reported once") {
    val found = findings(Seq(
      entry("val shape: Shape = Square(); println(shape.area)"),
      "Shape.scala" ->
        """trait Shape { def area: Int; def perimeter: Int }
                         |class Square extends Shape { def area = 4; def perimeter = 8; override def toString = "square" }""".stripMargin
    ))
    assertFinding(found, "unused-member", "Shape.perimeter is never referenced")
    assertNoFinding(found)(finding => finding.detail.contains("Square.") || finding.detail.contains("Shape.area"))
  }

  test("members used only by tests are test-only") {
    assertFinding(
      findings(
        Seq(entry("println(Tools.used)"), "Tools.scala" -> "object Tools { def used = 1; def helper = 2 }"),
        Seq("ToolsCheck.scala"                          -> "class ToolsCheck { val value = Tools.helper }")
      ),
      "test-only-member",
      "Tools.helper is referenced only from tests"
    )
  }

  test("members used only inside their own type can be private") {
    val found = findings(Seq(
      entry("Counter().increment()"),
      "Counter.scala" ->
        "class Counter { var count = 0; def increment(): Unit = bump(1); def bump(by: Int): Unit = count += by }"
    ))
    assertFinding(found, "weaker-access", "Counter.bump is referenced only by its own type and companion")(using
      Finding.Level.Info
    )
    assertNoFinding(found)(_.detail.contains("Counter.increment"))
  }

  test("members used only from types nested in their owner can be private") {
    val found = findings(Seq(
      entry("println(Parser.parse(\"1\"))"),
      "Parser.scala" -> """object Parser {
                          |  def parse(text: String): Option[Int] = Reader(text).read
                          |  def number(text: String): Option[Int] = text.toIntOption
                          |  private class Reader(text: String) { def read: Option[Int] = number(text) }
                          |}""".stripMargin
    ))
    assertFinding(found, "weaker-access", "Parser.number is referenced only by its own type")(using Finding.Level.Info)
    assertNoFinding(found)(_.detail.contains("Parser.parse"))
  }

  test("nested types used only inside their owner can be private, unless a visible signature names them") {
    val found = findings(Seq(
      entry("println(Parser.parse(\"1\")); println(Rule(Nil).size); println(Shapes.unit); println(Exposed.make)"),
      "Parser.scala" ->
        """object Parser {
                          |  def parse(text: String): Option[Int] = Reader(text).read
                          |  class Reader(text: String) { def read: Option[Int] = text.toIntOption; def copy: Reader = this }
                          |}
                          |class Rule(boundaries: List[Rule.Boundary]) { def size: Int = boundaries.size }
                          |object Rule { case class Boundary(name: String) }
                          |object Shapes { class Point(x: Int); def unit = Point(1).toString }
                          |object Exposed {
                          |  def make: Inner = Inner()
                          |  class Inner
                          |  private class Hidden
                          |}""".stripMargin,
      "Outside.scala" -> "object Outside { val point = Shapes.Point(2) }"
    ))
    given Finding.Level = Finding.Level.Info
    assertFinding(found, "weaker-access", "Parser.Reader is referenced only inside Parser; it can be private")
    assertNoFinding(found)(finding =>
      finding.rule == Finding.Rule.WeakerAccess &&
        Vector("Boundary", "Point", "Inner", "Hidden").exists(finding.detail.contains(_))
    )
  }

  test("type aliases are members, used through the types that name them") {
    val found = findings(Seq(
      entry("println(Poly(Map.empty).degree); println(Units.metres(1.0))"),
      "Poly.scala" -> """case class Poly(terms: Map[Poly.Mono, BigInt]) { def degree: Int = terms.size }
                        |object Poly { type Mono = Map[String, Int]; type Spare = Int }
                        |object Units { type Metres = Double; def metres(value: Double): Metres = value }""".stripMargin,
      "Report.scala" -> "object Report { val length: Units.Metres = 2.0 }"
    ))
    assertFinding(found, "weaker-access", "Poly.Mono is referenced only by its own type and companion")(using
      Finding.Level.Info
    )
    assertFinding(found, "unused-member", "Poly.Spare is never referenced")
    assertNoFinding(found)(_.detail.contains("Units.Metres"))
  }

  test("type forwarders created by exports are not judged") {
    val found = findings(Seq(
      entry("println(Protocol.Compression.None)"),
      "Protocol.scala" -> """object Formats { enum Compression { case None, Block } }
                            |object Protocol { export Formats.* }""".stripMargin
    ))
    assertNoFinding(found)(_.detail.contains("Protocol.Compression"))
  }

  test("recursion alone does not make a member used") {
    assertFinding(
      findings(Seq(
        entry("println(Tools.used)"),
        "Tools.scala" ->
          "object Tools { def used = 1; def countdown(n: Int): Int = if (n > 0) countdown(n - 1) else 0 }"
      )),
      "unused-member",
      "Tools.countdown is never referenced"
    )
  }

  test("members of unreachable types are left to the type-level finding") {
    val found = findings(Seq(entry("println(1)"), "Dead.scala" -> "class Dead { def spare = 2 }"))
    assertNoFinding(found)(_.detail.contains("Dead.spare"))
  }

  test("vars nothing reassigns can be vals, while vars assigned through other objects cannot") {
    val found = findings(Seq(
      entry("val job = Job(); job.state = 2; println(job.state + job.limit)"),
      "Job.scala" -> "class Job { var state = 0; var limit = 10 }"
    ))
    assertFinding(found, "var-could-be-val", "Job.limit is never reassigned")
    assertNoFinding(found)(finding => finding.detail.contains("Job.state") || finding.detail.contains("_="))
  }

  test("inline members are not judged, since their expansions leave no reference") {
    val found = findings(Seq(
      entry("println(Box.unwrap(Box.wrap(1)))"),
      "Box.scala" -> """object Box {
                       |  opaque type Box = Int
                       |  def wrap(value: Int): Box = value
                       |  transparent inline def unwrap(box: Box): Int = box
                       |  inline def spare(box: Box): Int = box
                       |}""".stripMargin
    ))
    assertNoFinding(found)(finding => finding.detail.contains("unwrap") || finding.detail.contains("spare"))
  }

  test("givens resolved only implicitly can be anonymous") {
    val found = findings(Seq(
      entry(
        """println(Sorting.sorted(List(Celsius(2), Celsius(1)))); println(Codecs.byName.compare(Celsius(1), Celsius(2)))
               |import Codecs.imported
               |println(summon[Ordering[Kelvin]])
               |println(Derived)""".stripMargin
      ),
      "Sorting.scala" ->
        """case class Celsius(value: Int)
                           |case class Kelvin(value: Int)
                           |case class Rankine(value: Int) derives CanEqual
                           |object Sorting {
                           |  given ascending: Ordering[Celsius] = Ordering.by(_.value)
                           |  def sorted(values: List[Celsius]): List[Celsius] = values.sorted
                           |}
                           |object Codecs {
                           |  given byName: Ordering[Celsius] = Ordering.by(-_.value)
                           |  given imported: Ordering[Kelvin] = Ordering.by(_.value)
                           |  given duplicate: Ordering[Rankine] = Ordering.by(_.value)
                           |  given other: Ordering[Rankine] = Ordering.by(-_.value)
                           |}
                           |trait Base { given base: Ordering[Kelvin] }
                           |object Derived extends Base { given base: Ordering[Kelvin] = Ordering.by(-_.value) }""".stripMargin
    ))
    assertFinding(found, "anonymous-given", "Sorting.ascending is never referenced by name")(using Finding.Level.Info)
    assertNoFinding(found)(finding =>
      finding.rule == Finding.Rule.AnonymousGiven &&
        Vector("byName", "imported", "duplicate", "other", "base", "derived").exists(finding.detail.contains(_))
    )
  }

  private def member(name: String, users: Set[String]): Members.Member = Members.Member(
    name,
    name.split('.').init.mkString("."),
    users,
    inherited = false,
    rooted = false,
    overriding = false,
    deferred = false,
    public = true,
    constant = false,
    test = false,
    generated = false,
    suppressed = Set.empty
  )

  private def node(key: String, test: Boolean = false, generated: Boolean = false, exported: Boolean = false) =
    key -> Node(
      key,
      "m",
      if (test) Output.Scope.Test else Output.Scope.Main,
      s"$key.scala",
      line = 1,
      generated = generated,
      exported = exported,
      outer = Some(key.split('.').init.mkString(".")).filter(_.contains('.'))
    )

  private val nodes = Map(
    node("lib.Tools"),
    node("app.Main"),
    node("check.Suite", test = true),
    node("lib.Outer"),
    node("lib.Outer.Plain"),
    node("lib.Outer.Shared"),
    node("lib.Outer.Written", generated = true),
    node("lib.Outer.Published", exported = true)
  )

  private def judged(model: Members.Model): Vector[Finding] = Members.findings(model, nodes, _ => true)

  test("a member some main code uses is not test-only, nor one used from outside the analysed types") {
    val found = judged(Members.Model(
      Vector(
        member("lib.Tools.checked", Set("check.Suite")),
        member("lib.Tools.shared", Set("check.Suite", "app.Main")),
        member("lib.Tools.external", Set("other.Caller"))
      ),
      Vector.empty,
      Vector.empty
    )).filter(_.rule == Finding.Rule.TestOnlyMember)
    assertEquals(found.map(_.detail), Vector("Tools.checked is referenced only from tests"))
  }

  test("members a supertype or other build fixes, or already hidden, are not told to be private") {
    val own   = member("lib.Tools.own", Set("lib.Tools"))
    val found = judged(Members.Model(
      Vector(
        own,
        own.copy(name = "lib.Tools.hidden", public = false),
        own.copy(name = "lib.Tools.overriding", overriding = true),
        own.copy(name = "lib.Tools.deferred", deferred = true),
        own.copy(name = "lib.Tools.inherited", inherited = true)
      ),
      Vector.empty,
      Vector.empty
    )).filter(_.rule == Finding.Rule.WeakerAccess)
    assertEquals(found.map(_.detail.takeWhile(_ != ' ')), Vector("Tools.own"))
  }

  test("nested types used outside their owner, generated or published are not told to be private") {
    def nested(name: String, users: String*) = Members.Nested(s"lib.Outer.$name", "lib.Outer", users.toSet, false)
    val found                                = judged(Members.Model(
      Vector.empty,
      Vector.empty,
      Vector(
        nested("Plain", "lib.Outer"),
        nested("Shared", "lib.Outer", "app.Main"),
        nested("Written", "lib.Outer"),
        nested("Published", "lib.Outer")
      )
    )).filter(_.rule == Finding.Rule.WeakerAccess)
    assertEquals(found.map(_.detail.takeWhile(_ != ' ')), Vector("Outer.Plain"))
  }
}
