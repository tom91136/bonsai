package net.kurobako.bonsai.analysis

import java.nio.file.Files
import net.kurobako.bonsai.java.Model as JavaModel
import net.kurobako.bonsai.testing.*

class MembersSuite extends AnalysisSuite {
  private def findings(main: Seq[(String, String)], test: Seq[(String, String)] = Nil): Vector[Finding] = {
    val result = extract(main, test)
    Members.findings(result.members, result.graph.nodes, result.reach)
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

  test("a method called only from a dead method of a live type is unused, however far the chain runs") {
    val found = findings(
      Seq(
        entry("println(Tools.used)"),
        "Tools.scala" ->
          "object Tools { def used = 1; def dead = first; def first = second; def second = 2 }"
      )
    )
    assertEquals(
      found.collect { case finding if finding.rule == Finding.Rule.UnusedMember => finding.detail }.sorted,
      Vector("Tools.dead is never referenced", "Tools.first is never referenced", "Tools.second is never referenced")
    )
  }

  test(
    "a method only an override of a method outside the build calls is used, since the host dispatches to the override"
  ) {
    val found = findings(
      Seq(
        entry("val _ = Spy(1)"),
        "Spy.scala" ->
          """class Spy(val v: Int) extends Comparable[Spy] { def compareTo(other: Spy): Int = Spy.weigh(v) - Spy.weigh(other.v) }
            |object Spy { def weigh(value: Int): Int = scale(value); def scale(value: Int): Int = value * 2 }""".stripMargin
      )
    )
    assertNoFinding(found)(_.rule == Finding.Rule.UnusedMember)
  }

  test("a method only Java code calls is used, and so is what only it calls, by the keys the two languages share") {
    Scratch.using("bonsai-") { directory =>
      val scala = compile(
        directory.resolve("scala"),
        Seq("Tools.scala" -> "package lib\nobject Tools { def fromJava = inner; def inner = 1; def spare = 2 }"),
        Vector.empty
      )
      val java = Seq(
        "demo/Caller.java" ->
          "package demo;\n\nimport lib.Tools;\n\npublic class Caller {\n  public static void main(String[] args) { Tools.fromJava(); }\n}\n"
      )
      write(directory.resolve("java"), java)
      val classes = Files.createDirectories(directory.resolve("java-classes"))
      val model   = directory.resolve("java-model.tsv")
      JavaModel.write(
        javac(java.map((name, _) => directory.resolve("java").resolve(name)), classes, classpath :+ scala),
        model
      )
      val manifest = Manifest(
        Vector(Output("m", Output.Scope.Main, scala), Output("j", Output.Scope.Main, classes)),
        Map("m" -> classpath, "j" -> (classpath :+ scala)),
        Set.empty,
        models = Vector(Output("j", Output.Scope.Main, model)),
        jdk = targetJdk
      )
      val result = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      assertEquals(
        Members.findings(result.members, result.graph.nodes, result.reach).collect {
          case finding if finding.rule == Finding.Rule.UnusedMember => finding.detail
        },
        Vector("Tools.spare is never referenced")
      )
    }
  }

  test("the constructs a program uses are not reported unused, however the bytecode reaches them") {
    val result = extract(
      Seq(
        "Fp.scala" ->
          """
            |package fp
            |
            |import fp.Syntax.doubled
            |import scala.annotation.tailrec
            |
            |enum Colour {
            |  case Red, Green
            |  case Mixed(red: Int, green: Int)
            |
            |  def label: String = this match {
            |    case Red      => "r"
            |    case Green    => "g"
            |    case Mixed(r, g) => s"$r$g"
            |  }
            |}
            |
            |sealed trait Shape
            |case object Dot extends Shape
            |case class Box(side: Int) extends Shape {
            |  def area: Int = side * side
            |}
            |
            |trait Greeter {
            |  def greet(name: String): String = decorate(s"hi $name")
            |  protected def decorate(text: String): String
            |}
            |
            |class Loud extends Greeter {
            |  protected def decorate(text: String): String = text.toUpperCase.trim + suffix
            |  private def suffix: String = "!"
            |}
            |
            |object Syntax {
            |  extension (value: Int) def doubled: Int = value * 2
            |  given ordering: Ordering[Box] = Ordering.by(_.side)
            |  implicit def widen(value: Int): Long = value.toLong
            |  inline def twice(inline value: Int): Int = helper(value) + helper(value)
            |  def helper(value: Int): Int = value + 1
            |  lazy val expensive: Int = compute()
            |  private def compute(): Int = 42
            |  val Limit = 5
            |  final val Constant = 7
            |}
            |
            |object Parse {
            |  def unapply(text: String): Option[Int] = text.toIntOption
            |}
            |
            |class Counter(start: Int = 0, step: Int = defaultStep) {
            |  def next(current: Int): Int = current + step + start
            |}
            |
            |object Counter {
            |  def defaultStep: Int = 1
            |}
            |
            |class Failure(message: String) extends Exception(message)
            |
            |class Resource extends AutoCloseable {
            |  def close(): Unit = release()
            |  private def release(): Unit = ()
            |}
            |
            |object Program {
            |  @tailrec def loop(count: Int, total: Int): Int = if (count == 0) total else loop(count - 1, total + count)
            |
            |  def run(): Int = {
            |    val colours = List(Colour.Red, Colour.Green, Colour.Mixed(1, 2)).map(_.label).mkString
            |    val boxed = Box(3)
            |    val shape: Shape = boxed
            |    val area = shape match {
            |      case Dot     => 0
            |      case Box(s)  => s + boxed.area
            |    }
            |    val sorted = List(Box(2), Box(1)).sorted(using Syntax.ordering)
            |    val widened: Long = 3
            |    val parsed = "12" match {
            |      case Parse(number) => number
            |      case _             => 0
            |    }
            |    val caught =
            |      try throw new Failure("x")
            |      catch { case failure: Failure => 1 }
            |    val resource = new Resource
            |    try resource.close()
            |    finally ()
            |    val counter = new Counter()
            |    val values = Colour.Red.ordinal + Colour.Mixed(1, 2).ordinal
            |    Syntax.twice(1) + Syntax.expensive + Syntax.Limit + Syntax.Constant + 1.doubled + loop(3, 0) + area + parsed + caught +
            |      counter.next(0) + values + widened.toInt + sorted.size + new Loud().greet("x").length + colours.length
            |  }
            |}
            |
            |object Entry {
            |  def main(args: Array[String]): Unit = println(Program.run())
            |}
            |""".stripMargin
      )
    )
    val found = Liveness.findings(result.graph, result.reach) ++
      Members.findings(result.members, result.graph.nodes, result.reach)
    val liveness = Set("unused", "test-only", "unused-member", "test-only-member", "unread-field")
    assertEquals(
      found.collect { case finding if liveness(finding.rule.id) => finding.rule.id -> finding.detail },
      Vector.empty[(String, String)]
    )
  }

  test("a member of a module that serves tests is not test-only") {
    val result = extract(
      Seq(
        entry("println(Tools.used)"),
        "Tools.scala" -> "object Tools { def used = 1; def staged = 2 }"
      ),
      Seq("Check.scala" -> "object Check { def check = Tools.staged }")
    )
    val testOnly = Members.findings(result.members, result.graph.nodes, result.reach, support = Set("m"))
      .filter(_.rule == Finding.Rule.TestOnlyMember)
    assertEquals(testOnly, Vector.empty)
  }

  test("a method only a method reached from tests alone calls is test-only with the method that calls it") {
    val found = findings(
      Seq(
        entry("println(Tools.used)"),
        "Tools.scala" -> "object Tools { def used = 1; def staged = shared; def shared = 2 }"
      ),
      Seq("Check.scala" -> "object Check { def check = Tools.staged }")
    )
    assertEquals(
      found.collect { case finding if finding.rule == Finding.Rule.TestOnlyMember => finding.detail },
      Vector("2 members of Tools are referenced only from tests: Tools.shared, Tools.staged")
    )
  }

  test("a member only an override of a library's public method uses is not test-only") {
    val result = extract(
      Seq(
        "Gate.scala" ->
          """package lib
            |trait Gate { def wrap(n: Int): Gate }
            |object Gate {
            |  def apply: Gate = new Plain
            |  private final class Plain extends Gate {
            |    def limit: Int = 2
            |    def wrap(n: Int): Gate = if (n > limit) new Wrapped else this
            |  }
            |  private final class Wrapped extends Gate { def wrap(n: Int): Gate = this }
            |}""".stripMargin
      ),
      Seq("GateCheck.scala" -> "class GateCheck { def run(gate: lib.Gate): lib.Gate = gate.wrap(1) }"),
      libraries = Set("m")
    )
    assertEquals(
      Members.findings(result.members, result.graph.nodes, result.reach).collect {
        case finding if finding.rule == Finding.Rule.TestOnlyMember => finding.detail
      },
      Vector.empty[String]
    )
  }

  test("a class and its object that declare one signature share a key, so reaching either keeps what the other calls") {
    def unused(objectMethod: String, call: String) = findings(
      Seq(
        entry(s"println(Box.$call)"),
        "Box.scala" ->
          s"""class Box { def stray(value: Int) = secret; def secret = 1 }
             |object Box { $objectMethod }""".stripMargin
      )
    ).collect { case finding if finding.rule == Finding.Rule.UnusedMember => finding.detail }.sorted
    assertEquals(unused("def stray(value: Int) = 2", "stray(1)"), Vector("Box.stray is never referenced"))
    assertEquals(
      unused("def stray(value: Int, other: Int) = 2", "stray(1, 2)"),
      Vector("Box.secret is never referenced", "Box.stray is never referenced")
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
    "a case class derived at an inline call, including one a parent takes, or from a summoned mirror has every field read"
  ) {
    val found = findings(
      Seq(
        entry(
          """import Wires.given
            |  println(Forms.encode[Form] + Mirrors.of[Summoned] + Rows.name + Wires.need[Wired] + Plain(1, 2).a)
            |  println(List.empty[Listed].size + Option.empty[Held].size + Forms.label[Labelled].length + Boxes.name)""".stripMargin
        ),
        "Support.scala" ->
          """object Forms {
            |  inline def encode[A]: String = scala.compiletime.summonInline[scala.deriving.Mirror.Of[A]].toString
            |  inline def label[A]: String = "label"
            |}
            |object Mirrors { def of[A](using scala.deriving.Mirror.Of[A]): String = "mirror" }
            |trait Codec[A]
            |object Wires {
            |  inline given auto[A]: Codec[A] = scala.compiletime.summonFrom {
            |    case _: scala.deriving.Mirror.Of[A] => new Codec[A] {}
            |    case _                               => new Codec[A] {}
            |  }
            |  def need[A](using Codec[A]): String = "wire"
            |}
            |class Table[T](using Codec[T]) { def name: String = "table" }
            |class Box[T] { def name: String = "box" }""".stripMargin,
        "Rows.scala" ->
          """import Wires.given
            |object Rows extends Table[Stored]
            |object Boxes extends Box[Boxed]
            |case class Form(user: String, secret: String)
            |case class Summoned(x: Int, y: Int)
            |case class Stored(id: Int, created: Long)
            |case class Wired(left: Int, right: Int)
            |case class Plain(a: Int, b: Int)
            |case class Listed(c: Int, d: Int)
            |case class Held(e: Int, f: Int)
            |case class Labelled(g: Int, h: Int)
            |case class Boxed(i: Int, j: Int)""".stripMargin
      )
    )
    assertEquals(
      found.collect { case finding if finding.rule == Finding.Rule.UnreadField => finding.detail }.sorted,
      Vector(
        "Plain.b is a field nothing reads; remove it, or read it",
        "Listed.c is a field nothing reads; remove it, or read it",
        "Listed.d is a field nothing reads; remove it, or read it",
        "Held.e is a field nothing reads; remove it, or read it",
        "Held.f is a field nothing reads; remove it, or read it",
        "Labelled.g is a field nothing reads; remove it, or read it",
        "Labelled.h is a field nothing reads; remove it, or read it",
        "Boxed.i is a field nothing reads; remove it, or read it",
        "Boxed.j is a field nothing reads; remove it, or read it"
      ).sorted
    )
  }

  test("case classes a derived class is built from have every field read, and so do the cases of a derived sum") {
    val found = findings(
      Seq(
        entry(
          """val outer = Outer(Inner(1, 2, Deeper(3, 4)), Some(Optional(5, 6)), Vector(Listed(7, 8)), Map("k" -> Keyed(9, 10)), Circle(1, 2, Mark(3, 4)))
            |  val choice: Choice = Choice.Left(1, 2)
            |  println(outer.getClass.getName + choice.getClass.getName + Square(1, 2).getClass.getName + Choice.Right(1).getClass.getName)
            |  println(Stray(1, 2).p + Holder(Held(1, 2)).held.getClass.getName.length)""".stripMargin
        ),
        "Support.scala" ->
          """trait Codec[A]
            |object Codec { def derived[A](using scala.deriving.Mirror.Of[A]): Codec[A] = new Codec[A] {} }
            |case class Outer(
            |    inner: Inner,
            |    maybe: Option[Optional],
            |    many: Vector[Listed],
            |    keyed: Map[String, Keyed],
            |    shape: Shape
            |) derives Codec
            |case class Inner(a: Int, b: Int, deeper: Deeper)
            |case class Deeper(c: Int, d: Int)
            |case class Optional(e: Int, f: Int)
            |case class Listed(g: Int, h: Int)
            |case class Keyed(i: Int, j: Int)
            |sealed trait Shape
            |case class Circle(radius: Int, centre: Int, mark: Mark) extends Shape
            |case class Square(side: Int, corner: Int) extends Shape
            |case class Mark(k: Int, l: Int)
            |sealed trait Choice derives Codec
            |object Choice { case class Left(m: Int, n: Int) extends Choice; case class Right(o: Int) extends Choice }
            |case class Stray(p: Int, q: Int)
            |case class Held(x: Int, y: Int)
            |case class Holder(held: Held)""".stripMargin
      )
    )
    assertEquals(
      found.collect { case finding if finding.rule == Finding.Rule.UnreadField => finding.detail }.sorted,
      Vector(
        "Held.x is a field nothing reads; remove it, or read it",
        "Held.y is a field nothing reads; remove it, or read it",
        "Stray.q is a field nothing reads; remove it, or read it"
      ).sorted
    )
  }

  test("case classes a derived class holds through Option and List have every field read") {
    val found = findings(
      Seq(
        entry(
          """val outer = Outer(Some(Inner(1, 2)), List(Inner(3, 4)))
            |  println(outer.getClass.getName + Stray(1, 2).p)""".stripMargin
        ),
        "Support.scala" ->
          """trait Codec[A]
            |object Codec { def derived[A](using scala.deriving.Mirror.Of[A]): Codec[A] = new Codec[A] {} }
            |case class Outer(maybe: Option[Inner], many: List[Inner]) derives Codec
            |case class Inner(a: Int, b: Int)
            |case class Stray(p: Int, q: Int)""".stripMargin
      )
    )
    assertEquals(
      found.collect { case finding if finding.rule == Finding.Rule.UnreadField => finding.detail }.sorted,
      Vector("Stray.q is a field nothing reads; remove it, or read it")
    )
  }

  test("a field read by name through selectDynamic on a type built from its case class is read") {
    val found = findings(
      Seq(
        entry("println(Query.row.id + Query.row.title.length)"),
        "Query.scala" ->
          """class Record[T](values: Map[String, Any]) extends Selectable {
            |  def selectDynamic(name: String): Any = values(name)
            |}
            |case class Item(id: Int, title: String, hidden: Int)
            |object Query {
            |  type Row = Record[Item] { val id: Int; val title: String }
            |  val row: Row = new Record[Item](Map("id" -> 1, "title" -> "t")).asInstanceOf[Row]
            |}""".stripMargin
      )
    )
    assertEquals(
      found.collect { case finding if finding.rule == Finding.Rule.UnreadField => finding.detail }.sorted,
      Vector("Item.hidden is a field nothing reads; remove it, or read it")
    )
  }

  test("a field main code reads through selectDynamic is not test-only when tests read it plainly") {
    val found = findings(
      Seq(
        entry("println(Query.row.id)"),
        "Query.scala" ->
          """class Record[T](values: Map[String, Any]) extends Selectable {
            |  def selectDynamic(name: String): Any = values(name)
            |}
            |case class Item(id: Int, title: String)
            |object Query {
            |  type Row = Record[Item] { val id: Int }
            |  val row: Row = new Record[Item](Map("id" -> 1)).asInstanceOf[Row]
            |}""".stripMargin
      ),
      Seq("ItemTest.scala" -> "object ItemTest { def check(): Int = Item(1, \"t\").id + Item(2, \"u\").title.length }")
    )
    assertEquals(
      found.collect { case finding if finding.rule == Finding.Rule.TestOnlyMember => finding.detail },
      Vector("Item.title is referenced only from tests")
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
    val found = Members.findings(result.members, result.graph.nodes, result.reach)
    assertEquals(
      found.map(_.detail).sorted,
      Vector("Client.internal is never referenced", "Hidden.spare is never referenced")
    )
  }

  test("members a qualified-private trait gives an exported type are its API, and a hidden type's are not") {
    val result = extract(
      Seq("Client.scala" ->
        """package lib
          |private[lib] trait Shared { def shared: Int = 1 }
          |private[lib] trait Inner { def inner: Int = 2 }
          |class Client extends Shared
          |private[lib] class Hidden extends Inner
          |object Main { def main(args: Array[String]): Unit = println(Hidden()) }""".stripMargin),
      libraries = Set("m")
    )
    val found = Members.findings(result.members, result.graph.nodes, result.reach)
    assertEquals(found.map(_.detail).sorted, Vector("Inner.inner is never referenced"))
  }

  test("a self-typed trait's override is used with the base it overrides, and an unused one still fires") {
    val found = findings(
      Seq(
        entry("val base: Base = new Host; println(base.value)"),
        "Mixins.scala" ->
          """trait Base { def value: Int = 1 }
            |trait Shaped { self: Base => override def value: Int = 2 }
            |trait Lone { self: Base => def spare: Int = 3 }
            |class Host extends Base with Shaped with Lone""".stripMargin
      )
    )
    assertEquals(
      found.collect { case finding if finding.rule == Finding.Rule.UnusedMember => finding.detail },
      Vector("Lone.spare is never referenced")
    )
  }

  test("a mixin's implementation of an abstract member is used when calls reach the abstract member") {
    val found = findings(
      Seq(
        entry("val source: Source = new Host; println(source.next)"),
        "Mixins.scala" ->
          """trait Source { def next: Int }
            |trait Counting { def next: Int = 1 }
            |trait Lone { def spare: Int = 2 }
            |class Host extends Source with Counting with Lone""".stripMargin
      )
    )
    assertEquals(
      found.collect { case finding if finding.rule == Finding.Rule.UnusedMember => finding.detail },
      Vector("Lone.spare is never referenced")
    )
  }

  test("a subclass's override does not use the member it replaces") {
    val found = findings(
      Seq(
        entry("val host = new Host; println(host.next)"),
        "Mixins.scala" ->
          """trait Counting { def next: Int = 1 }
            |class Host extends Counting { override def next: Int = 2 }""".stripMargin
      )
    )
    assertEquals(
      found.collect { case finding if finding.rule == Finding.Rule.UnusedMember => finding.detail },
      Vector("Counting.next is never referenced")
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
      Liveness.findings(result.graph, result.reach) ++
        Members.findings(result.members, result.graph.nodes, result.reach)
    }
    def unused(reflective: Vector[String]) =
      found(reflective).map(finding => finding.rule.id -> finding.node.name).sorted
    assertEquals(unused(Vector.empty), Vector("unused" -> "app.Launch"))
    assertEquals(unused(Vector("app.Warp")), Vector("unused" -> "app.Launch"))
    assertEquals(found(Vector("app.*")), Vector.empty)
  }

  test("members used only from a platform twin's own sources are used, and shared types stay the JVM module's") {
    val result = extract(
      Seq(
        entry("println(Model.shown)"),
        "Model.scala" -> "object Model { def shown = 1; def assemble = helper; def helper = 2 }"
      ),
      twins =
        Seq(Seq("Explorer.scala" ->
          "object Explorer { def main(args: Array[String]): Unit = println(Model.assemble) }"))
    )
    assertEquals(result.graph.nodes.get("Model").map(_.module), Some("m"))
    assertEquals(result.graph.nodes.get("Explorer").map(_.module), Some("t0"))
    assertEquals(
      Liveness.findings(result.graph, result.reach) ++
        Members.findings(result.members, result.graph.nodes, result.reach)
          .filterNot(finding => finding.rule == Finding.Rule.MisplacedMember || finding.level == Finding.Level.Info),
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
      (Liveness.findings(result.graph, result.reach) ++
        Members.findings(result.members, result.graph.nodes, result.reach))
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

  test("a grouped test-only finding sits at the first of the members whichever order they arrive in") {
    val tests = Set("check.Suite")
    val items = Vector("lib.Tools.c" -> 30, "lib.Tools.a" -> 10, "lib.Tools.b" -> 20).map((name, line) =>
      member(name, tests).copy(line = line)
    )
    val lines = items.permutations.map { order =>
      Members.findings(Members.Model(order, Vector.empty, Vector.empty), nodes, Reach.empty)
        .collect { case finding if finding.rule == Finding.Rule.TestOnlyMember => finding.node.line }
    }.toSet
    assertEquals(lines, Set(Vector(10)))
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
    val spare = only(found, "unused-member", "Poly.Spare")
    assertEquals(spare.detail, "Poly.Spare is never referenced")
    assertEquals(spare.node.line, 2)
    assertNoFinding(found)(_.detail.contains("Units.Metres"))
  }

  private def aliases(start: String, extra: String = "") = Seq(
    entry("println(engine.Engine.start)"),
    "Attempt.scala" ->
      """package provider
        |case class Attempt(id: Int)
        |object Attempt { def key(id: Int): String = id.toString }""".stripMargin,
    "Aliases.scala" ->
      """package recovery
        |type Attempt = provider.Attempt
        |val Attempt: provider.Attempt.type = provider.Attempt
        |type Spare = provider.Attempt
        |val Idle: provider.Attempt.type = provider.Attempt""".stripMargin,
    "Engine.scala" ->
      s"""package engine
        |import recovery.*
        |object Engine { def start: String = $start$extra }""".stripMargin
  )

  test("a top-level alias pair is used when the terms or the types naming it are") {
    val found = findings(aliases("Attempt.key(Attempt(1).id) + of(null)", "; def of(a: Attempt): Int = a.id"))
    assertNoFinding(found)(_.detail.contains("recovery.Attempt"))
  }

  private def only(found: Vector[Finding], rule: String, text: String): Finding =
    found.filter(finding => finding.rule.id == rule && finding.detail.contains(text)) match {
      case Vector(single) => single
      case other          => fail(s"expected one finding containing '$text', found ${other.mkString("\n")}")
    }

  test("a top-level type alias nothing names is unused beside its used term alias") {
    val found   = findings(aliases("Attempt.key(Attempt(1).id)"))
    val finding = only(found, "unused-member", "recovery.Attempt")
    assertEquals(finding.detail, "the type alias recovery.Attempt is never referenced; the value of that name is")
    assertEquals(finding.node.line, 2)
  }

  test("a top-level value nothing uses is reported at its own line beside its used type alias") {
    val found   = findings(aliases("of(null).toString", "; def of(a: Attempt): Int = 1"))
    val finding = only(found, "unused-member", "recovery.Attempt")
    assertEquals(finding.detail, "recovery.Attempt is never referenced")
    assertEquals(finding.node.line, 3)
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
    users.map(Use(_)),
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

  private def judged(model: Members.Model): Vector[Finding] = Members.findings(model, nodes, Reach.empty)

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
    assertEquals(Liveness.findings(result.graph, result.reach), Vector.empty)
    assertEquals(
      Members.findings(result.members, result.graph.nodes, result.reach)
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
    assertEquals(Liveness.findings(result.graph, result.reach).map(_.node.name), Vector("probe.Frames"))
  }
}
