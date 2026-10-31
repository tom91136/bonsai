package net.kurobako.bonsai.analysis

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
    assertNoFinding(found)(finding =>
      finding.rule != Finding.Rule.MisplacedMember && finding.detail.contains("Tools.used")
    )
  }

  test("a case class field that nothing reads is reported, however it is built and matched") {
    val found = findings(
      Seq(
        entry(
          """val command: Command = Command.Packed(1, 2, 3, 4)
            |  val shown = command match {
            |    case Command.Packed(a, _, c, _) => a + c
            |    case Command.Plain(value)       => value
            |  }
            |  println(Wrapper(1, 2, 3).first + shown + Tagged(1, 2).productPrefix.length + Counter(1, 2).copy(count = 3).count)
            |  println(Derived(1, 2).read + Mirrored(1, 2).productPrefix)
            |  println(Use.run())
            |  Moved(1, 2) match { case Moved(copy, _) => println(copy) }""".stripMargin
        ),
        "Command.scala" ->
          """sealed trait Command
            |object Command {
            |  case class Packed(first: Int, control: Int, third: Int, fourth: Int) extends Command
            |  case class Plain(value: Int) extends Command
            |}
            |case class Wrapper(first: Int, unused: Int, private val hidden: Int) { def total: Int = first + hidden }
            |case class Tagged(@deprecated("x", "1") tag: Int, other: Int)
            |case class Counter(count: Int, spare: Int)
            |case class Moved(copy: Int, other: Int)
            |case class Derived(read: Int, spare: Int) derives CanEqual
            |trait Codec[A]
            |object Codec { def derived[A](using scala.deriving.Mirror.Of[A]): Codec[A] = new Codec[A] {} }
            |case class Mirrored(first: Int, second: Int) derives Codec
            |trait Named { def name: String }
            |case class Person(name: String, age: Int) extends Named
            |object Names { def of(named: Named): String = named.name }""".stripMargin,
        "Use.scala" -> "object Use { def run(): String = Names.of(Person(\"x\", 3)) }"
      )
    )
    assertEquals(
      found.collect { case finding if finding.rule == Finding.Rule.UnreadField => finding.detail }.sorted,
      Vector(
        "Packed.control is a field nothing reads; remove it, or read it",
        "Packed.fourth is a field nothing reads; remove it, or read it",
        "Counter.spare is a field nothing reads; remove it, or read it",
        "Derived.spare is a field nothing reads; remove it, or read it",
        "Moved.other is a field nothing reads; remove it, or read it",
        "Person.age is a field nothing reads; remove it, or read it",
        "Tagged.other is a field nothing reads; remove it, or read it",
        "Tagged.tag is a field nothing reads; remove it, or read it",
        "Wrapper.unused is a field nothing reads; remove it, or read it"
      ).sorted
    )
  }

  test(
    "fields of case classes collected into buffers and read by projection are unread when no projection names them"
  ) {
    val found = findings(
      Seq(
        entry("println(demo.Flow.run().size)"),
        "Flow.scala" ->
          """package demo
            |import scala.collection.mutable
            |object Flow {
            |  final case class Return(tree: AnyRef, expression: String, method: Option[String])
            |  final case class Compare(tree: AnyRef, subject: String, equal: Boolean)
            |  final case class Variant(name: String, fields: Vector[String])
            |  final case class Kept(tree: AnyRef, subject: String)
            |  def run(): Vector[String] = {
            |    val returns     = mutable.ArrayBuffer.empty[Return]
            |    val comparisons = mutable.ArrayBuffer.empty[Compare]
            |    val kept        = mutable.ArrayBuffer.empty[Kept]
            |    returns += Return("a", "x", None)
            |    comparisons += Compare("b", "y", true)
            |    kept += Kept("c", "z")
            |    val variants = Vector(Variant("v", Vector.empty))
            |    returns.map(r => r.expression + r.method.size).toVector ++ comparisons.map(_.subject) ++
            |      variants.map(_.name) ++ kept.map(k => k.tree.toString + k.subject)
            |  }
            |}""".stripMargin
      )
    )
    assertEquals(
      found.collect { case finding if finding.rule == Finding.Rule.UnreadField => finding.detail }.sorted,
      Vector(
        "Compare.equal is a field nothing reads; remove it, or read it",
        "Compare.tree is a field nothing reads; remove it, or read it",
        "Return.tree is a field nothing reads; remove it, or read it",
        "Variant.fields is a field nothing reads; remove it, or read it"
      )
    )
  }

  test("a case class field is read where a value of it is shown, since its toString writes every field") {
    val found = findings(
      Seq(
        entry(
          """println(Report(1, 2))
            |  val error: Failure = Failure.Parse(Detail(3))
            |  Console.err.println(error)
            |  val text = s"${Some(Wrapped(4))}"
            |  println(text + Silent(5, 6).kept + Quiet(7, 8).kept)""".stripMargin
        ),
        "Model.scala" ->
          """case class Report(lines: Int, bytes: Int)
            |enum Failure {
            |  case Parse(detail: Detail)
            |  case Empty
            |}
            |case class Detail(offset: Int)
            |case class Wrapped(value: Int)
            |case class Silent(kept: Int, dropped: Int)
            |case class Quiet(kept: Int, @SuppressWarnings(Array("bonsai:unread-field")) spare: Int)""".stripMargin
      )
    )
    assertEquals(
      found.collect { case finding if finding.rule == Finding.Rule.UnreadField => finding.detail },
      Vector("Silent.dropped is a field nothing reads; remove it, or read it")
    )
  }

  test("a case class field is read where a value of it is compared, hashed or used as a key, or rendered as text") {
    val found = findings(
      Seq(
        entry(
          """println(Plain(1, 2).productPrefix)
            |  println(Compared(1, 2) == Compared(1, 2))
            |  println(Differing(1, 2) != Differing(3, 4))
            |  println(Hashed(1, 2).hashCode)
            |  println(List(Distinct(1, 2)).distinct.size)
            |  println(Map(Keyed(1, 2) -> 1).size)
            |  println(Set(Elem(1, 2)).size)
            |  println(Map(1 -> Valued(1, 2)).size)
            |  println(scala.collection.immutable.TreeSet(Sorted(1, 2))(using Ordering.by[Sorted, Int](_.first)).size)
            |  println(Some(Nested(Inner(1, 2))) == None)
            |  println(List(Joined(1, 2)).mkString(","))
            |  println(StringBuilder().append(Appended(1, 2)).length)
            |  println(Formatted(1, 2).formatted("%s"))
            |  assert(true, Asserted(1, 2))
            |  println(Tupled(1, 2).productIterator.size)""".stripMargin
        ),
        "Model.scala" ->
          """case class Plain(a: Int, b: Int)
            |case class Compared(a: Int, b: Int)
            |case class Differing(a: Int, b: Int)
            |case class Hashed(a: Int, b: Int)
            |case class Distinct(a: Int, b: Int)
            |case class Keyed(a: Int, b: Int)
            |case class Elem(a: Int, b: Int)
            |case class Valued(first: Int, spare: Int)
            |case class Sorted(first: Int, spare: Int)
            |case class Inner(a: Int, b: Int)
            |case class Nested(inner: Inner)
            |case class Joined(a: Int, b: Int)
            |case class Appended(a: Int, b: Int)
            |case class Formatted(a: Int, b: Int)
            |case class Asserted(a: Int, b: Int)
            |case class Tupled(a: Int, b: Int)""".stripMargin
      )
    )
    assertEquals(
      found.collect { case finding if finding.rule == Finding.Rule.UnreadField => finding.detail }.sorted,
      Vector(
        "Plain.a is a field nothing reads; remove it, or read it",
        "Plain.b is a field nothing reads; remove it, or read it",
        "Sorted.spare is a field nothing reads; remove it, or read it",
        "Valued.first is a field nothing reads; remove it, or read it",
        "Valued.spare is a field nothing reads; remove it, or read it"
      ).sorted
    )
  }

  test("a public member only one other type uses belongs in that type") {
    val found = findings(Seq(
      entry("println(Worker().run + Pair().run)"),
      "Tools.scala" ->
        """object Tools { def helper: Int = 1; def shared: Int = 2; def own: Int = 3; def local: Int = own }
          |class Worker { def run: Int = Tools.helper + Tools.shared + Tools.local }
          |class Pair { def run: Int = Tools.shared }""".stripMargin
    ))
    assertFinding(found, "misplaced-member", "Tools.helper is used only by Worker")(using Finding.Level.Info)
    assertNoFinding(found)(finding =>
      finding.rule == Finding.Rule.MisplacedMember && !finding.detail.startsWith("Tools.helper")
    )
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
    assertNoFinding(found)(finding =>
      finding.rule != Finding.Rule.MisplacedMember &&
        (finding.detail.contains("twice(Int)") || finding.detail.startsWith("Tools.> "))
    )
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
      libraries = Set("m")
    )
    val found = Members.findings(result.members, result.graph.nodes, Liveness.live(result.graph))
    assertEquals(
      found.map(_.detail).sorted,
      Vector("Client.internal is never referenced", "Hidden.spare is never referenced")
    )
  }

  test("types named only in strings, and what they declare, are used when the build declares them reflective") {
    val sources = Seq(
      entry("""println(Class.forName("app.Warp"))"""),
      "Warp.scala" ->
        """package app
          |object Warp {
          |  def lane: Int = 1
          |  class Mask { def bits: Int = 2 }
          |}
          |private[app] object Launch {
          |  def grid: Int = 3
          |  private[app] def block: Int = 4
          |}""".stripMargin
    )
    def found(reflective: Vector[String]) = {
      val result = extract(sources, reflective = reflective)
      Liveness.findings(result.graph) ++
        Members.findings(result.members, result.graph.nodes, Liveness.live(result.graph))
    }
    def unused(reflective: Vector[String]) =
      found(reflective).map(finding => finding.rule.id -> finding.node.name).sorted
    assertEquals(unused(Vector.empty), Vector("unused" -> "app.Launch"))
    assertEquals(unused(Vector("app.Warp")), Vector("unused" -> "app.Launch"))
    assertEquals(found(Vector("app.*")), Vector.empty)
  }

  test("members used only from a platform twin's own sources are used, and shared types stay the JVM module's") {
    val result = extract(
      Seq(entry("println(Model.shown)"), "Model.scala" -> "object Model { def shown = 1; def assemble = 2 }"),
      twins =
        Seq(Seq("Explorer.scala" ->
          "object Explorer { def main(args: Array[String]): Unit = println(Model.assemble) }"))
    )
    assertEquals(result.graph.nodes.get("Model").map(_.module), Some("m"))
    assertEquals(result.graph.nodes.get("Explorer").map(_.module), Some("t0"))
    assertEquals(
      Liveness.findings(result.graph) ++
        Members.findings(result.members, result.graph.nodes, Liveness.live(result.graph))
          .filterNot(_.rule == Finding.Rule.MisplacedMember),
      Vector.empty
    )
  }

  test("a platform twin's replacement of a shared source counts as a user of what only the twin defines") {
    val result = extract(
      Seq(entry("Workarounds.install()"), "Workarounds.scala" -> "object Workarounds { def install(): Unit = () }"),
      twins = Seq(Seq(
        "Workarounds.scala" -> "object Workarounds { def install(): Unit = Relay.install() }",
        "Relay.scala"       -> "object Relay { def install(): Unit = (); def spare(): Unit = () }",
        "Orphan.scala"      -> "class Orphan { def run(): Unit = () }"
      ))
    )
    assertEquals(result.graph.nodes.get("Workarounds").map(_.module), Some("m"))
    val found =
      (Liveness.findings(result.graph) ++
        Members.findings(result.members, result.graph.nodes, Liveness.live(result.graph)))
        .filterNot(_.rule == Finding.Rule.MisplacedMember)
    assertEquals(
      found.map(finding => finding.rule.id -> finding.detail.takeWhile(_ != ' ')).toSet,
      Set("unused" -> "Orphan", "unused-member" -> "Relay.spare")
    )
  }

  test("dynamic dispatch reaches implementations, and an unused declaration is reported once") {
    val found = findings(Seq(
      entry("val shape: Figure = Square(); println(shape.area)"),
      "Figure.scala" ->
        """trait Figure { def area: Int; def perimeter: Int }
                         |class Square extends Figure { def area = 4; def perimeter = 8; override def toString = "square" }""".stripMargin
    ))
    assertFinding(found, "unused-member", "Figure.perimeter is never referenced")
    assertNoFinding(found)(finding => finding.detail.contains("Square.") || finding.detail.contains("Figure.area"))
  }

  test("several members of one type that only tests use are one finding") {
    val found = findings(
      Seq(entry("println(Builder.used)"), "Builder.scala" -> "object Builder { def used = 1; def a = 2; def b = 3 }"),
      Seq("BuilderCheck.scala" -> "class BuilderCheck { val values = (Builder.a, Builder.b) }")
    ).filter(_.rule == Finding.Rule.TestOnlyMember)
    assertEquals(
      found.map(_.detail),
      Vector("2 members of Builder are referenced only from tests: Builder.a, Builder.b")
    )
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

  test("types nested at depth used only inside the type that holds them can be private to it") {
    val found = findings(Seq(
      entry("println(Lints.names); println(Collector.count)"),
      "Lints.scala" ->
        """object Lints {
          |  trait Lint { def name: String }
          |  object Groups {
          |    object Redundant extends Lint { def name = "redundant" }
          |    class Cast(val target: Int)
          |    sealed trait Known
          |    case class Item(value: Int) extends Known
          |  }
          |  val names: Vector[String] = Vector(Groups.Redundant.name, Groups.Cast(1).target.toString)
          |  def items: Vector[Groups.Known] = Vector(Groups.Item(1))
          |}""".stripMargin,
      "Collector.scala" ->
        """object Collector {
          |  object Phase { case class Step(label: String) }
          |  object Pool { def steps: Int = Vector(Phase.Step("a")).size }
          |  def count: Int = Pool.steps
          |}""".stripMargin
    ))
    given Finding.Level = Finding.Level.Info
    assertFinding(found, "weaker-access", "Groups.Redundant is referenced only inside Lints; it can be private[Lints]")
    assertFinding(found, "weaker-access", "Groups.Cast is referenced only inside Lints; it can be private[Lints]")
    assertFinding(
      found,
      "weaker-access",
      "Phase.Step is referenced only inside Collector; it can be private[Collector]"
    )
  }

  test("nested declarations used from another top-level type, or published by a visible signature, stay visible") {
    val found = findings(Seq(
      entry("println(Lints.names); println(Outside.value); println(Exposed.make)"),
      "Lints.scala" ->
        """object Lints {
          |  object Groups {
          |    class Cast(val target: Int)
          |    class Shared(val target: Int)
          |    val shared = 1
          |  }
          |  val names: Vector[String] = Vector(Groups.Cast(1).target.toString, Groups.Shared(2).target.toString)
          |}
          |object Exposed {
          |  object Inner { class Handle }
          |  def make: Inner.Handle = Inner.Handle()
          |}""".stripMargin,
      "Outside.scala" -> "object Outside { val value = Lints.Groups.Shared(3).target + Lints.Groups.shared }"
    ))
    given Finding.Level = Finding.Level.Info
    assertFinding(found, "weaker-access", "Groups.Cast is referenced only inside Lints; it can be private[Lints]")
    assertNoFinding(found)(finding =>
      finding.rule == Finding.Rule.WeakerAccess &&
        Vector("Shared", "Groups.shared", "Handle").exists(finding.detail.contains(_))
    )
  }

  test("nested declarations already private or qualified are not told to be private again") {
    val found = findings(Seq(
      entry("println(Parser.parse(\"1\")); println(Counter(1).next)"),
      "Parser.scala" ->
        """object Parser {
          |  private object Inner { class Reader(val text: String) }
          |  object Open { private[Parser] class Hidden; private val limit = 1 }
          |  def parse(text: String): Option[Int] = Option(Inner.Reader(text).text.length).filter(_ > Open.Hidden().hashCode)
          |}
          |class Counter(val value: Int) { def next: Int = Counter.Step.by + value }
          |object Counter { object Step { val by = 1 } }""".stripMargin
    ))
    assertNoFinding(found)(finding =>
      finding.rule == Finding.Rule.WeakerAccess &&
        Vector("Reader", "Hidden", "limit").exists(finding.detail.contains(_))
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

  test("qualified private givens are reported here and private ones are left to scalafix") {
    val found = findings(Seq(
      entry("println(Hidden.touch)"),
      "Hidden.scala" ->
        """case class Reaumur(value: Int)
          |case class Delisle(value: Int)
          |object Hidden {
          |  private[Hidden] given qualified: Ordering[Reaumur] = Ordering.by(_.value)
          |  private given plain: Ordering[Delisle] = Ordering.by(_.value)
          |  def touch: Int = List(Reaumur(1)).sorted.size + List(Delisle(1)).sorted.size
          |}""".stripMargin
    ))
    assertFinding(found, "anonymous-given", "Hidden.qualified is never referenced by name")(using Finding.Level.Info)
    assertNoFinding(found)(finding => finding.rule == Finding.Rule.AnonymousGiven && finding.detail.contains("plain"))
  }

  test("values of the kind the collections of their siblings register, which nothing lists or uses, are reported") {
    val found = findings(Seq(
      entry(
        """println(Endpoints.all); println(Endpoints.more); println(Endpoints.numbers); println(Endpoints.shared)
          |println(Handlers.registered); println(Stages.listed); println(Stages.indexed)""".stripMargin
      ),
      "Endpoints.scala" ->
        """final class Endpoint[I, O](val path: String)
          |object Endpoints {
          |  type AnyEndpoint = Endpoint[?, ?]
          |  val user: Endpoint[Long, String]       = Endpoint("user")
          |  val illust: Endpoint[Long, Int]        = Endpoint("illust")
          |  val novel: Endpoint[String, String]    = Endpoint("novel")
          |  val forgotten: Endpoint[Long, Boolean] = Endpoint("forgotten")
          |  val shared: Endpoint[Long, Long]       = Endpoint("shared")
          |  val base: Endpoint[Unit, Unit]         = Endpoint("base")
          |  val derived: Endpoint[Unit, String]    = Endpoint(base.path + "/derived")
          |  private val hidden: Endpoint[Int, Int] = Endpoint("hidden")
          |  val limit: Int                         = 10
          |  val other: Int                         = 20
          |  val all: Vector[AnyEndpoint]  = Vector(user, illust, derived)
          |  val more: Vector[AnyEndpoint] = Vector(novel, hidden)
          |  val numbers: List[Int]        = List(limit, 1)
          |}
          |object Handlers {
          |  val first: Endpoint[Int, Int]    = Endpoint("first")
          |  val second: Endpoint[Int, Int]   = Endpoint("second")
          |  val third: Endpoint[Int, Int]    = Endpoint("third")
          |  val registered: Seq[Endpoints.AnyEndpoint] = Seq(first, second)
          |}
          |object Stages {
          |  val one: Endpoint[Long, Long]        = Endpoint("one")
          |  val two: Endpoint[Long, Long]        = Endpoint("two")
          |  val three: Endpoint[Long, Long]      = Endpoint("three")
          |  val listed: Set[Endpoints.AnyEndpoint]         = Set.apply(one, two)
          |  val indexed: IndexedSeq[Endpoints.AnyEndpoint] = IndexedSeq(one, two)
          |}""".stripMargin
    ))
    assertEquals(
      found.collect {
        case finding if finding.rule == Finding.Rule.RegistryDrift => finding.detail.takeWhile(_ != ';')
      }.sorted,
      Vector("Endpoints.forgotten", "Handlers.third", "Stages.three").map(name =>
        s"$name is of the kind the collections of its siblings register, but none lists it"
      )
    )
    assertEquals(
      found.filter(finding =>
        finding.rule == Finding.Rule.UnusedMember && finding.detail.startsWith("Endpoints.forgotten")
      ),
      Vector.empty
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
    detachable = false,
    test = false,
    suppressed = Set.empty
  )

  private def node(
      key: String,
      scope: Output.Scope = Output.Scope.Main,
      generated: Boolean = false,
      exported: Boolean = false
  ) =
    key -> Node(
      key,
      "m",
      scope,
      s"$key.scala",
      line = 1,
      generated = generated,
      exported = exported,
      outer = Some(key.split('.').init.mkString(".")).filter(_.contains('.'))
    )

  private val nodes = Map(
    node("lib.Tools"),
    node("app.Main"),
    node("check.Suite", Output.Scope.Test),
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

  test("nested types used outside their owner, or published, are not told to be private, generated ones are") {
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
    assertEquals(found.map(_.detail.takeWhile(_ != ' ')), Vector("Outer.Plain", "Outer.Written"))
  }

  test("a member named by its full qualified name in a string is used, a short name is not") {
    val layout = "Layout.scala" -> """package probe
                                    |final case class Primitive[A](name: String, function: A)
                                    |object Layout {
                                    |  private def rowOf(value: Long): Long = value * 2
                                    |  val Row: Primitive[Long => Long] = Primitive("probe.Layout.row", rowOf(_))
                                    |  def row(value: Long): Long = rowOf(value)
                                    |  def spare(value: Long): Long = value
                                    |  val label = "Layout.spare"
                                    |}""".stripMargin
    val found = findings(Seq(
      "Main.scala" ->
        "object Main { def main(args: Array[String]): Unit = println(probe.Layout.Row.function(2L) + probe.Layout.label) }",
      layout
    ))
    assertEquals(
      found.collect { case finding if finding.rule == Finding.Rule.UnusedMember => finding.detail },
      Vector("Layout.spare is never referenced")
    )
    assertEquals(found.filter(_.detail.startsWith("Layout.row")), Vector.empty)
  }

  test("a type named by its full qualified name in a string is used, and its members are exempt") {
    val names = "Names.scala" -> """package probe
                                   |trait Frames { def lane: Int = 1 }
                                   |object Names {
                                   |  val frames = "probe.Frames"
                                   |}""".stripMargin
    val result = extract(Seq(entry("println(probe.Names.frames)"), names))
    assertEquals(Liveness.findings(result.graph), Vector.empty)
    assertEquals(
      Members.findings(result.members, result.graph.nodes, Liveness.live(result.graph))
        .filterNot(_.rule == Finding.Rule.MisplacedMember),
      Vector.empty
    )
  }

  test("a type named by its short name in a string is not used") {
    val names = "Names.scala" -> """package probe
                                   |trait Frames { def lane: Int = 1 }
                                   |object Names {
                                   |  val note = "Frames"
                                   |}""".stripMargin
    val result = extract(Seq(entry("println(probe.Names.note)"), names))
    assertEquals(Liveness.findings(result.graph).map(_.node.name), Vector("probe.Frames"))
  }
}
