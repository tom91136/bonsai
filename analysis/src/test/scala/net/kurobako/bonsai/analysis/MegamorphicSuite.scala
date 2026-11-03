package net.kurobako.bonsai.analysis

class MegamorphicSuite extends AnalysisSuite {
  private val shapes = """trait Shape { def area: Int }
                         |class Circle extends Shape { def area = 1 }
                         |class Square extends Shape { def area = 2 }
                         |class Triangle extends Shape { def area = 3 }""".stripMargin

  private lazy val found = {
    val result = extract(
      Seq(
        "Main.scala" ->
          """object Main {
                        |  def main(args: Array[String]): Unit =
                        |    println((total(Nil), once(Circle()), pairs(Nil), sizes(Nil), solos(Nil), looped(Array.empty), shown(Nil), local(Nil)))
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
        "Shapes.scala" -> """trait Shape { def area: Int }
                          |class Circle extends Shape { def area = 1 }
                          |class Square extends Shape { def area = 2 }
                          |class Triangle extends Shape { def area = 3 }
                          |trait Pair { def value: Int }
                          |class One extends Pair { def value = 1 }
                          |class Two extends Pair { def value = 2 }
                          |abstract class Base { def size: Int = 0 }
                          |class Small extends Base
                          |class Medium extends Base { override def size = 1 }
                          |class Large extends Base { override def size = 2 }
                          |final class Solo { def weight: Int = 1 }""".stripMargin
      ),
      hot = Vector("Main")
    )
    Megamorphic.findings(result.megamorphic, result.graph.nodes)
  }

  test("inline bodies, receivers the loop does not change, and implementations of other type arguments do not count") {
    val result = extract(
      Seq(
        "Main.scala" ->
          """object Main {
            |  def main(args: Array[String]): Unit =
            |    println((total(Nil), fixed(Nil, Circle()), longs(Nil), mixed(Nil), held(Nil)))
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
      ),
      hot = Vector("Main")
    )
    assertEquals(
      Megamorphic.findings(result.megamorphic, result.graph.nodes).map(_.detail.takeWhile(_ != ',')).sorted,
      Vector("Main.mixed calls Operands.first in a loop", "Main.total calls Shape.area in a loop")
    )
  }

  test("calls in loops are megamorphic only in the classes the build marks hot, and never in a platform twin's code") {
    val main = "Main.scala" ->
      """object Main {
        |  def main(args: Array[String]): Unit = println(total(Nil))
        |  def total(shapes: List[Shape]): Int = shapes.map(_.area).sum
        |}""".stripMargin
    def megamorphic(result: Extraction.Result) = Megamorphic.findings(result.megamorphic, result.graph.nodes)
    assertEquals(megamorphic(extract(Seq(main, "Shapes.scala" -> shapes))), Vector.empty)
    assertEquals(megamorphic(extract(Seq(main, "Shapes.scala" -> shapes), hot = Vector("**", "!Main"))), Vector.empty)
    assertEquals(megamorphic(extract(Seq(main, "Shapes.scala" -> shapes), hot = Vector("**"))).size, 1)
    val twin = extract(
      Seq("Shapes.scala" -> shapes),
      twins = Seq(Seq("Explorer.scala" ->
        """object Explorer {
          |  def main(args: Array[String]): Unit = println(total(Nil))
          |  def total(shapes: List[Shape]): Int = shapes.map(_.area).sum
          |}""".stripMargin)),
      hot = Vector("**")
    )
    assertEquals(megamorphic(twin), Vector.empty)
  }

  test("calls in loops to methods with three or more implementations are megamorphic") {
    assertFinding(found, "megamorphic-call", "Main.total calls Shape.area in a loop", "3 implementations")
    assertFinding(found, "megamorphic-call", "Main.looped calls Shape.area in a loop")
    assertFinding(found, "megamorphic-call", "Main.sizes calls Base.size in a loop", "3 implementations")
    assertFinding(found, "megamorphic-call", "Main.local calls Shape.area in a loop")
  }

  test("calls outside loops, bimorphic and final calls, and members every class has are left alone") {
    assertNoFinding(found)(finding =>
      Vector("Main.once", "Main.pairs", "Main.solos", "Main.shown").exists(finding.detail.startsWith(_))
    )
  }

  test("test doubles do not count as implementations at calls in main code") {
    val result = extract(
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
    assertNoFinding(Megamorphic.findings(result.megamorphic, result.graph.nodes))(_.detail.startsWith("Main.total"))
  }

  test("members implemented by vals only are not megamorphic") {
    val result = extract(
      Seq(
        "Main.scala" ->
          """object Main {
            |  def main(args: Array[String]): Unit = println(total(Nil))
            |  def total(hosts: List[Host]): Int = hosts.map(_.host.length).sum
            |}""".stripMargin,
        "Hosts.scala" ->
          """trait Host { def host: String }
            |class Alpha extends Host { val host: String = "alpha" }
            |class Beta extends Host { val host: String = "beta" }
            |class Gamma extends Host { val host: String = "gamma" }
            |class Delta extends Host { val host: String = "delta" }
            |class Epsilon extends Host { val host: String = "epsilon" }""".stripMargin
      ),
      hot = Vector("Main")
    )
    assertEquals(Megamorphic.findings(result.megamorphic, result.graph.nodes), Vector.empty)
  }

  test("members implemented by case-class fields are not megamorphic") {
    val result = extract(
      Seq(
        "Main.scala" ->
          """object Main {
            |  def main(args: Array[String]): Unit = println(total(Nil))
            |  def total(hosts: List[Host]): Int = hosts.map(_.host.length).sum
            |}""".stripMargin,
        "Hosts.scala" ->
          """trait Host { def host: String }
            |case class Alpha(host: String) extends Host
            |case class Beta(host: String) extends Host
            |case class Gamma(host: String) extends Host
            |case class Delta(host: String) extends Host
            |case class Epsilon(host: String) extends Host""".stripMargin
      ),
      hot = Vector("Main")
    )
    assertEquals(Megamorphic.findings(result.megamorphic, result.graph.nodes), Vector.empty)
  }

  test("members implemented by methods with bodies are megamorphic") {
    val result = extract(
      Seq(
        "Main.scala" ->
          """object Main {
            |  def main(args: Array[String]): Unit = println(total(Nil))
            |  def total(hosts: List[Host]): Int = hosts.map(_.host.length).sum
            |}""".stripMargin,
        "Hosts.scala" ->
          """trait Host { def host: String }
            |class Alpha extends Host { def host = "alpha".toUpperCase }
            |class Beta extends Host { def host = "beta".toUpperCase }
            |class Gamma extends Host { def host = "gamma".toUpperCase }
            |class Delta extends Host { def host = "delta".toUpperCase }
            |class Epsilon extends Host { def host = "epsilon".toUpperCase }""".stripMargin
      ),
      hot = Vector("Main")
    )
    assertFinding(
      Megamorphic.findings(result.megamorphic, result.graph.nodes),
      "megamorphic-call",
      "Main.total calls Host.host in a loop",
      "5 implementations"
    )
  }
}
