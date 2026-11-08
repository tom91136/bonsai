package net.kurobako.bonsai.analysis

import java.nio.file.Files
import net.kurobako.bonsai.analysis.ClassFiles.*
import net.kurobako.bonsai.testing.*
import scala.util.Using

class MegamorphicSuite extends AnalysisSuite {
  private val shapes = """trait Shape { def area: Int }
                         |class Circle extends Shape { def area = 1 }
                         |class Square extends Shape { def area = 2 }
                         |class Triangle extends Shape { def area = 3 }""".stripMargin

  private val several = Seq(
    "Main.scala" ->
      """object Main {
        |  def main(args: Array[String]): Unit = {
        |    val all = List[Shape](Circle(), Square(), Triangle())
        |    println((
        |      total(all), once(Circle()), pairs(List(One(), Two())), sizes(List(Small(), Medium(), Large())),
        |      solos(List(Solo())), looped(all.toArray), shown(all), local(all)
        |    ))
        |  }
        |  def total(shapes: List[Shape]): Int = shapes.map(_.area).sum
        |  def once(shape: Shape): Int = shape.area
        |  def pairs(values: List[Pair]): Int = values.map(_.value).sum
        |  def sizes(values: List[Base]): Int = values.map(_.size).sum
        |  def solos(values: List[Solo]): Int = values.map(_.weight).sum
        |  def looped(shapes: Array[Shape]): Int = {
        |    var total = 0
        |    var index = 0
        |    while (index < shapes.length) {
        |      total += shapes(index).area
        |      index += 1
        |    }
        |    total
        |  }
        |  def shown(shapes: List[Shape]): Int = shapes.map(_.toString.length).sum
        |  def local(shapes: List[Shape]): Int = {
        |    val result = shapes.map(shape => shape.area).sum
        |    result
        |  }
        |}""".stripMargin,
    "Shapes.scala" -> s"""$shapes
                         |trait Pair { def value: Int }
                         |class One extends Pair { def value = 1 }
                         |class Two extends Pair { def value = 2 }
                         |abstract class Base { def size: Int = 0 }
                         |class Small extends Base
                         |class Medium extends Base { override def size = 1 }
                         |class Large extends Base { override def size = 2 }
                         |final class Solo { def weight: Int = 1 }""".stripMargin
  )

  private def megamorphic(
      main: Seq[(String, String)],
      test: Seq[(String, String)] = Nil,
      hot: Vector[String] = Vector("Main")
  ): Vector[Finding] = {
    val result = extract(main, test, hot = hot)
    Megamorphic.findings(result.megamorphic, result.graph.nodes)
  }

  private lazy val found = megamorphic(several)

  private val effects =
    """final class Eff[A](val run: () => A) {
      |  def flatMap[B](f: A => Eff[B]): Eff[B] = Eff(() => f(run()).run())
      |  def >>[B](next: => Eff[B]): Eff[B] = flatMap(_ => next)
      |}
      |object Eff { def apply[A](run: () => A): Eff[A] = new Eff(run) }""".stripMargin

  private def detail(findings: Vector[Finding]): Vector[String] = findings.map(_.detail.takeWhile(_ != ',')).sorted

  test("inline bodies, receivers the loop does not change, and implementations of other type arguments do not count") {
    val result = megamorphic(
      Seq(
        "Main.scala" ->
          """object Main {
            |  def main(args: Array[String]): Unit = {
            |    val all     = List[Shape](Circle(), Square(), Triangle())
            |    val numbers = List[Operands[Long]](Longs(), Wides())
            |    val words   = List[Operands[String]](Names(), Labels())
            |    println((total(all), fixed(Nil, Circle()), longs(numbers), mixed(numbers), mixed(words), held(Nil), expanded(all)))
            |  }
            |  def total(shapes: List[Shape]): Int = shapes.map(_.area).sum
            |  inline def expanded(shapes: List[Shape]): Int = shapes.map(_.area).sum
            |  def fixed(rounds: List[Int], shape: Shape): Int = rounds.map(_ => shape.area).sum
            |  def longs(values: List[Operands[Long]]): Long = values.map(_.first).sum
            |  def mixed[A](values: List[Operands[A]]): Int = values.map(_.first).size
            |  def held(values: List[Int]): Int = { val shape: Shape = Square(); values.map(_ => shape.area).sum }
            |}""".stripMargin,
        "Shapes.scala" ->
          (shapes + """
            |trait Operands[A] { def first: A }
            |class Longs extends Operands[Long] { def first = 1L }
            |class Wides extends Operands[Long] { def first = 2L }
            |class Names extends Operands[String] { def first = "a" }
            |class Labels extends Operands[String] { def first = "b" }""".stripMargin)
      )
    )
    assertEquals(
      detail(result),
      Vector("Main.mixed calls Operands.first in a loop", "Main.total calls Shape.area in a loop")
    )
  }

  test("calls in loops are megamorphic only in the classes the build marks hot, and never in a platform twin's code") {
    val main = "Main.scala" ->
      """object Main {
        |  def main(args: Array[String]): Unit = println(total(List(Circle(), Square(), Triangle())))
        |  def total(shapes: List[Shape]): Int = shapes.map(_.area).sum
        |}""".stripMargin
    assertEquals(megamorphic(Seq(main, "Shapes.scala" -> shapes), hot = Vector.empty), Vector.empty)
    assertEquals(megamorphic(Seq(main, "Shapes.scala" -> shapes), hot = Vector("**", "!Main")), Vector.empty)
    assertEquals(megamorphic(Seq(main, "Shapes.scala" -> shapes), hot = Vector("**")).size, 1)
    val twin = extract(
      Seq("Shapes.scala" -> shapes),
      twins = Seq(Seq("Explorer.scala" ->
        """object Explorer {
          |  def main(args: Array[String]): Unit = println(total(List(Circle(), Square(), Triangle())))
          |  def total(shapes: List[Shape]): Int = shapes.map(_.area).sum
          |}""".stripMargin)),
      hot = Vector("**")
    )
    assertEquals(twin.megamorphic, Vector.empty)
  }

  test("calls in loops to methods with three or more receiver classes are megamorphic") {
    assertFinding(found, "megamorphic-call", "Main.total calls Shape.area in a loop", "3 receiver classes")
    assertFinding(found, "megamorphic-call", "Main.looped calls Shape.area in a loop")
    assertFinding(found, "megamorphic-call", "Main.sizes calls Base.size in a loop", "3 receiver classes")
    assertFinding(found, "megamorphic-call", "Main.local calls Shape.area in a loop")
  }

  test("calls outside loops, bimorphic and final calls, and members every class has are left alone") {
    assertNoFinding(found)(finding =>
      Vector("Main.once", "Main.pairs", "Main.solos", "Main.shown").exists(finding.detail.startsWith(_))
    )
  }

  test("classes the program never instantiates are no receivers") {
    val result = megamorphic(
      Seq(
        "Main.scala" ->
          """object Main {
            |  def main(args: Array[String]): Unit = println(total(List(Circle(), Square())))
            |  def total(shapes: List[Shape]): Int = shapes.map(_.area).sum
            |}""".stripMargin,
        "Shapes.scala" -> shapes
      )
    )
    assertEquals(result, Vector.empty)
  }

  test("a lambda is a receiver of its own") {
    val handlers = "Handlers.scala" ->
      """@FunctionalInterface
        |trait Handler { def handle(value: Int): Int }
        |class Doubling extends Handler { def handle(value: Int): Int = value * 2 }
        |class Negating extends Handler { def handle(value: Int): Int = -value }""".stripMargin
    def main(handlers: String) = "Main.scala" ->
      s"""object Main {
         |  def main(args: Array[String]): Unit = println(run(List($handlers)))
         |  def run(all: List[Handler]): Int = all.map(_.handle(1)).sum
         |}""".stripMargin
    assertEquals(megamorphic(Seq(main("Doubling(), Negating()"), handlers)), Vector.empty)
    val lambdas = megamorphic(Seq(main("Doubling(), Negating(), (value: Int) => value + 1"), handlers))
    assertFinding(
      lambdas,
      "megamorphic-call",
      "Main.run calls Handler.handle in a loop",
      "3 receiver classes",
      "lambda"
    )
  }

  test("a call of a library type is left alone however many classes the world instantiates") {
    val result = megamorphic(
      Seq(
        "Main.scala" ->
          """object Main {
            |  def main(args: Array[String]): Unit = {
            |    val lists = List[java.util.List[Int]](java.util.ArrayList[Int](), java.util.LinkedList[Int](), java.util.Vector[Int]())
            |    println((total(lists), apply(List(_ + 1, _ * 2, _ - 3))))
            |  }
            |  def total(lists: List[java.util.List[Int]]): Int = lists.map(_.size).sum
            |  def apply(functions: List[Int => Int]): Int = functions.map(_(1)).sum
            |}""".stripMargin
      )
    )
    assertEquals(result, Vector.empty)
  }

  test("build classes implementing a library type count where the call is typed by a build type") {
    def total(counters: String) = megamorphic(
      Seq(
        "Main.scala" ->
          s"""object Main {
             |  def main(args: Array[String]): Unit = println(total(List(Zero(), One(), Two())))
             |  def total(counters: List[$counters]): Int = counters.map(_.compareTo(Zero())).sum
             |}""".stripMargin,
        "Counters.scala" ->
          """trait Counter extends Comparable[Counter]
            |class Zero extends Counter { def compareTo(other: Counter): Int = 0 }
            |class One extends Counter { def compareTo(other: Counter): Int = 1 }
            |class Two extends Counter { def compareTo(other: Counter): Int = 2 }""".stripMargin
      )
    )
    assertEquals(total("Comparable[Counter]"), Vector.empty)
    assertFinding(
      total("Counter"),
      "megamorphic-call",
      "Main.total calls Comparable.compareTo in a loop",
      "3 receiver classes"
    )
  }

  test("test doubles do not count as implementations at calls in main code") {
    val result = megamorphic(
      Seq(
        "Main.scala" ->
          """trait Store { def get: Int }
            |class Memory extends Store { def get = 1 }
            |class Disk extends Store { def get = 2 }
            |object Main {
            |  def main(args: Array[String]): Unit = println(total(List(Memory(), Disk())))
            |  def total(stores: List[Store]): Int = stores.map(_.get).sum
            |}""".stripMargin
      ),
      Seq(
        "Fakes.scala" ->
          """class FakeStore extends Store { def get = 3 }
            |class FailingStore extends Store { def get = throw IllegalStateException() }
            |class StoreCheck { val total = Main.total(List(FakeStore(), FailingStore())) }""".stripMargin
      )
    )
    assertNoFinding(result)(_.detail.startsWith("Main.total"))
  }

  private def hosts(members: String*) = Seq(
    "Main.scala" ->
      """object Main {
        |  def main(args: Array[String]): Unit = println(total(List(Alpha(), Beta(), Gamma(), Delta(), Epsilon())))
        |  def total(hosts: List[Host]): Int = hosts.map(_.host.length).sum
        |}""".stripMargin,
    "Hosts.scala" ->
      (Seq("Alpha", "Beta", "Gamma", "Delta", "Epsilon").zip(members).map((name, member) =>
        s"class $name extends Host { $member }"
      ).prepended("trait Host { def host: String }").mkString("\n"))
  )

  test("members implemented by vals only are not megamorphic") {
    val names = Seq("alpha", "beta", "gamma", "delta", "epsilon")
    assertEquals(megamorphic(hosts(names.map(name => s"val host: String = \"$name\"")*)), Vector.empty)
  }

  test("members implemented by case-class fields are not megamorphic") {
    val result = megamorphic(
      Seq(
        "Main.scala" ->
          """object Main {
            |  def main(args: Array[String]): Unit =
            |    println(total(List(Alpha("a"), Beta("b"), Gamma("c"), Delta("d"), Epsilon("e"))))
            |  def total(hosts: List[Host]): Int = hosts.map(_.host.length).sum
            |}""".stripMargin,
        "Hosts.scala" ->
          """trait Host { def host: String }
            |case class Alpha(host: String) extends Host
            |case class Beta(host: String) extends Host
            |case class Gamma(host: String) extends Host
            |case class Delta(host: String) extends Host
            |case class Epsilon(host: String) extends Host""".stripMargin
      )
    )
    assertEquals(result, Vector.empty)
  }

  test("members implemented by methods with bodies are megamorphic") {
    val names = Seq("alpha", "beta", "gamma", "delta", "epsilon")
    assertFinding(
      megamorphic(hosts(names.map(name => s"def host = \"$name\".toUpperCase")*)),
      "megamorphic-call",
      "Main.total calls Host.host in a loop",
      "5 receiver classes"
    )
  }

  test("receiver classes are listed by simple name unless two collide") {
    val result = megamorphic(
      Seq(
        "Main.scala" ->
          """package shapes
            |trait Named { def name: String }
            |object A { class Impl extends Named { def name = "a" } }
            |object B { class Impl extends Named { def name = "b" } }
            |class Other extends Named { def name = "c" }
            |trait Figure { def area: Int }
            |class Circle extends Figure { def area = 1 }
            |class Square extends Figure { def area = 2 }
            |class Triangle extends Figure { def area = 3 }
            |object Main {
            |  def main(args: Array[String]): Unit = {
            |    println((names(List(A.Impl(), B.Impl(), Other())), areas(List(Circle(), Square(), Triangle()))))
            |  }
            |  def names(values: List[Named]): Int = values.map(_.name.length).sum
            |  def areas(shapes: List[Figure]): Int = shapes.map(_.area).sum
            |}""".stripMargin
      ),
      hot = Vector("**")
    )
    assertFinding(result, "megamorphic-call", "Main.names calls Named.name", "A.Impl, B.Impl, shapes.Other")
    assertFinding(result, "megamorphic-call", "Main.areas calls Figure.area", "Circle, Square, Triangle")
  }

  test("a suppression on the method silences the finding") {
    val loop                        = "    while (index < shapes.length) { total += shapes(index).area; index += 1 }"
    def looping(annotation: String) =
      s"""  $annotation def run(shapes: Array[Shape]): Int = {
         |    var total = 0
         |    var index = 0
         |$loop
         |    total
         |  }""".stripMargin
    val result = extract(
      Seq(
        "Main.scala" ->
          s"""object Main {
             |  def main(args: Array[String]): Unit = {
             |    val all = Array[Shape](Circle(), Square(), Triangle())
             |    println((Looped.run(all), Quiet.run(all)))
             |  }
             |}
             |object Looped {
             |${looping("")}
             |}
             |object Quiet {
             |${looping("@SuppressWarnings(Array(\"bonsai:megamorphic-call\"))")}
             |}""".stripMargin,
        "Shapes.scala" -> shapes
      ),
      hot = Vector("**")
    )
    val options  = Report.Options(Set.empty, Set.empty, Set.empty, info = true)
    val reported = Report(Megamorphic.findings(result.megamorphic, result.graph.nodes), result.graph.nodes, options)
      .map(_.finding.detail)
    assert(reported.exists(_.startsWith("Looped.run calls")), reported.mkString("\n"))
    assert(!reported.exists(_.startsWith("Quiet.run calls")), reported.mkString("\n"))
  }

  test("a call in a monadic loop is reported, and one outside any loop is not") {
    val result = megamorphic(
      Seq(
        "Main.scala" ->
          """object Main {
            |  def main(args: Array[String]): Unit = {
            |    val all = List[Shape](Circle(), Square(), Triangle())
            |    println((forever(all.head), once(all.head), unrelated(all.head)))
            |  }
            |  def forever(shape: Shape): Eff[Int] = Eff(() => shape.area).flatMap(_ => forever(shape))
            |  def once(shape: Shape): Eff[Int] = Eff(() => shape.area)
            |  def unrelated(shape: Shape): Eff[Int] = Eff(() => shape.area).flatMap(_ => once(shape))
            |}""".stripMargin,
        "Effects.scala" -> effects,
        "Shapes.scala"  -> shapes
      )
    )
    assertEquals(detail(result), Vector("Main.forever calls Shape.area in a loop"))
  }

  test("a call in a loop of tagless effects is reported") {
    val result = megamorphic(
      Seq(
        "Main.scala" ->
          """object Main {
            |  def main(args: Array[String]): Unit = {
            |    val all = List[Shape](Circle(), Square(), Triangle())
            |    println(forever[Option](all.head))
            |  }
            |  def forever[F[_]](shape: Shape)(using m: Monad[F]): F[Int] =
            |    m.flatMap(m.pure(shape.area))(_ => forever[F](shape))
            |  given Monad[Option] with {
            |    def pure[A](a: A): Option[A] = Some(a)
            |    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
            |  }
            |}
            |trait Monad[F[_]] {
            |  def pure[A](a: A): F[A]
            |  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
            |}""".stripMargin,
        "Shapes.scala" -> shapes
      )
    )
    assertEquals(detail(result), Vector("Main.forever calls Shape.area in a loop"))
  }

  test("objects are named without a trailing dot, and the doubles of tests are not receivers") {
    val result = megamorphic(
      Seq(
        "Main.scala" ->
          """object Main {
            |  def main(args: Array[String]): Unit = println(total(List[Shape](Circle(), Square())))
            |  def total(shapes: List[Shape]): Int = {
            |    var sum = 0
            |    var index = 0
            |    while (index < shapes.length) { sum += shapes(index).area; index += 1 }
            |    sum
            |  }
            |}""".stripMargin,
        "Shapes.scala" -> shapes
      ),
      Seq(
        "Fakes.scala" ->
          """object Fakes {
            |  def fake: Shape = new Shape { def area = 0 }
            |  def other: Shape = new Shape { def area = 1 }
            |  def check: Int = Main.total(List(fake, other))
            |}""".stripMargin
      )
    )
    assertEquals(result, Vector.empty)
    val named = megamorphic(
      Seq(
        "Main.scala" ->
          """object Main {
            |  def main(args: Array[String]): Unit = println(total(List[Shape](One, Two, Three)))
            |  def total(shapes: List[Shape]): Int = {
            |    var sum = 0
            |    var index = 0
            |    while (index < shapes.length) { sum += shapes(index).area; index += 1 }
            |    sum
            |  }
            |}
            |object One extends Shape { def area = 1 }
            |object Two extends Shape { def area = 2 }
            |object Three extends Shape { def area = 3 }""".stripMargin,
        "Shapes.scala" -> shapes
      )
    )
    assertFinding(named, "megamorphic-call", "Main.total calls Shape.area in a loop", "One, Three, Two")
  }

  private val handlers = Seq(
    "fixture/Handler.java" -> "package fixture;\n\npublic interface Handler { int handle(); }\n",
    "fixture/Alpha.java"   ->
      "package fixture;\n\npublic class Alpha implements Handler { public int handle() { return 1; } }\n",
    "fixture/Beta.java" ->
      "package fixture;\n\npublic class Beta implements Handler { public int handle() { return 2; } }\n",
    "fixture/Gamma.java" ->
      "package fixture;\n\npublic class Gamma implements Handler { public int handle() { return 3; } }\n",
    "fixture/Mod$.java" ->
      "package fixture;\n\npublic class Mod$ implements Handler { public int handle() { return 4; } }\n",
    "fixture/Outer.java" ->
      """package fixture;
        |
        |public class Outer {
        |  public static class One implements Handler { public int handle() { return 5; } }
        |  public static class Two implements Handler { public int handle() { return 6; } }
        |  public static class Three implements Handler { public int handle() { return 7; } }
        |}
        |""".stripMargin,
    "fixture/Wire.java" ->
      """package fixture;
        |
        |public class Wire {
        |  public static int run(Handler h) { return h.handle(); }
        |  public static void trio() { new Alpha(); new Beta(); new Gamma(); }
        |  public static void modules() { new Alpha(); new Beta(); new Mod$(); }
        |  public static void lambdas() { Handler h = () -> 9; new Alpha(); new Beta(); }
        |  public static void nested() { new Outer.One(); new Outer.Two(); new Outer.Three(); }
        |}
        |""".stripMargin
  )

  private def node(name: String, scope: Output.Scope = Output.Scope.Main): (String, Node) =
    name -> Node(name, "m", scope, "", 0, generated = false, exported = false, outer = None)

  private def site(
      at: String = "fixture.Handler",
      stable: Boolean = false,
      unreaching: Set[String] = Set.empty
  ): Megamorphic.Site = Megamorphic.Site(
    "fixture.Wire.run",
    at,
    "Handler.handle",
    "fixture.Handler.handle()I",
    unreaching,
    stable,
    Set.empty
  )

  private def resolved(root: String, nodes: Map[String, Node], at: Megamorphic.Site): Vector[Megamorphic.Found] =
    Scratch.using("bonsai-megamorphic-") { directory =>
      val classes = Files.createDirectories(directory.resolve("classes"))
      write(directory.resolve("src"), handlers)
      javac(handlers.map((name, _) => directory.resolve("src").resolve(name)), classes, classpath): Unit
      ClassSource.of(Vector(classes), classpath, None).fold(
        error => fail(error),
        source =>
          Using.resource(source) { source =>
            val world = ClassWorld(source.bytes(_))
            val roots =
              Set(Reference("fixture/Wire", root, "()V"), Reference("fixture/Wire", "run", "(Lfixture/Handler;)I"))
            Megamorphic.resolve(Vector(at), nodes, CallGraph.build(world, roots), world)
          }
      )
    }

  test("receivers outside the build count, and a nested class is the build's by its enclosing class") {
    val handler = Map(node("fixture.Handler"))
    assertEquals(resolved("trio", handler, site()).map(_.count), Vector(3))
    val nested = handler + node("fixture.Outer", Output.Scope.Test)
    assertEquals(resolved("nested", nested, site()), Vector.empty)
    assertEquals(resolved("nested", handler, site()).map(_.count), Vector(3))
  }

  test("a call at a site the build does not know is main code") {
    val tests = Seq("Alpha", "Beta", "Gamma").map(name => node(s"fixture.$name", Output.Scope.Test))
    val nodes = Map(node("fixture.Handler")) ++ tests
    assertEquals(resolved("trio", nodes, site("fixture.Main")), Vector.empty)
    assertEquals(resolved("trio", nodes, site()), Vector.empty)
    val inTests = tests.toMap + node("fixture.Handler", Output.Scope.Test)
    assertEquals(resolved("trio", inTests, site()).map(_.count), Vector(3))
  }

  test("a member every receiver implements by reading a field is exempt only when the build holds every receiver") {
    val some = Map(node("fixture.Handler"), node("fixture.Alpha"), node("fixture.Beta"))
    assertEquals(resolved("trio", some, site(stable = true)).map(_.count), Vector(3))
    val all = some + node("fixture.Gamma")
    assertEquals(resolved("trio", all, site(stable = true)), Vector.empty)
    assertEquals(resolved("trio", all, site()).map(_.count), Vector(3))
  }

  test("a lambda is never a member that the build holds") {
    val nodes = Map(node("fixture.Handler"), node("fixture.Alpha"), node("fixture.Beta"))
    assertEquals(resolved("lambdas", nodes, site(stable = true)).map(_.count), Vector(3))
  }

  test("a module is named without the dollar or the dot") {
    val nodes = Map(node("fixture.Handler"))
    assertEquals(resolved("modules", nodes, site()).map(_.names), Vector(Vector("Alpha", "Beta", "Mod")))
    assertEquals(resolved("modules", nodes, site(unreaching = Set("fixture.Mod"))), Vector.empty)
  }
}
