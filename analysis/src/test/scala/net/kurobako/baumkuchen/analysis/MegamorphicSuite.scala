package net.kurobako.baumkuchen.analysis

class MegamorphicSuite extends AnalysisSuite {
  private lazy val found = {
    val result = extract(Seq(
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
    ))
    Megamorphic.findings(result.megamorphic, result.graph.nodes)
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
}
