package net.kurobako.bonsai.analysis

class MissingClassesSuite extends AnalysisSuite {
  private val Library = Seq(
    "lib/Lib.scala" ->
      """package lib
        |class Box(val items: List[Int]) {
        |  def size: Int = items.size
        |  def each(f: Int => Unit): Unit = items.foreach(f)
        |}
        |object Pair { def unapply(value: Int): Option[(Int, Int)] = Some((value, value)) }
        |object Cons { def unapply(value: List[Int]): Option[(Int, List[Int])] = value.headOption.map(_ -> value.tail) }
        |abstract class Base { def run(): Int; def twice(): Int = run() * 2 }
        |trait Named { def name: String = "named" }
        |object Interpolators { extension (context: StringContext) def tag(args: Any*): String = context.s(args*) }
        |object Defaults { def make(size: Int = 3, names: String*): Box = new Box(List.fill(size)(names.size)) }
        |object Syntax { implicit class Wrapped(val value: Int) extends AnyVal { def doubled: Int = value * 2 } }
        |""".stripMargin
  )

  private def read(source: String) = {
    val result = extract(program("Use", source), absent = Library)
    Report.findings(result)
  }

  test("calls into classes the classpath lacks are decided as unknown callees") {
    read("""import lib.*
           |object Use {
           |  def sizes(boxes: List[Box]): Int = boxes.map(box => box.size + new Box(Nil).size).sum
           |  def loop(box: Box): Unit = for (_ <- 1 to 3) box.each(println)
           |}""".stripMargin)
  }

  test("patterns on extractors from missing classes are decided as unknown") {
    read("""import lib.*
           |object Use {
           |  def pair(value: Int): Int = value match { case Pair(a, _) => a; case _ => 0 }
           |  def head(values: List[Int]): Int = values match { case Cons(head, tail) => head + tail.size; case _ => 0 }
           |}""".stripMargin)
  }

  test("values of types from missing classes are passed on and converted as unknown") {
    read("""import lib.*
           |object Use {
           |  def call(base: Base, named: Named): Int = base.twice() + base.run() + named.name.length
           |  def doubled(value: Int): Int = { import Syntax.*; value.doubled }
           |}""".stripMargin)
  }

  test("richer uses of missing classes are decided as unknown") {
    read("""import lib.*
           |import lib.Interpolators.*
           |object Use {
           |  def interpolated(box: Box): String = tag"size ${box.size}"
           |  def defaults(): Int = Defaults.make().size + Defaults.make(2, "a", "b").size
           |  def loops(boxes: Vector[Box]): Int = {
           |    var total = 0
           |    for (box <- boxes; item <- box.items) total += item + boxes.count(_.size == box.size)
           |    var index = 0
           |    while (index < boxes.size) { total += boxes(index).size; index += 1 }
           |    total
           |  }
           |  def typed(value: Any): Int = value match { case box: Box => box.size; case Pair(a, _) => a; case _ => 0 }
           |  def chained(box: Box): Int = box.items.map(_ + box.size).filter(_ > box.size).sum
           |  def lazily(box: Box): Int = { lazy val size = box.size; size + size }
           |}""".stripMargin)
  }
}
