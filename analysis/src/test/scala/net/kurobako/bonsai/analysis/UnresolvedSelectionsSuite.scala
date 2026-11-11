package net.kurobako.bonsai.analysis

class UnresolvedSelectionsSuite extends AnalysisSuite {
  private val Current = Seq(
    "lib/Defaults.scala" ->
      """package lib
        |class Box(val size: Int = 3)
        |object Defaults { def make(size: Int = 3, name: String = "a"): String = name * size }
        |object Cache { val table = scala.collection.mutable.Map.empty[Int, Int] }
        |""".stripMargin
  )

  private val Outdated = Seq(
    "lib/Defaults.scala" ->
      """package lib
        |class Box(val size: Int)
        |object Defaults { def make(size: Int, name: String): String = name * size }
        |object Cache
        |""".stripMargin
  )

  private def read(source: String) =
    Report.findings(extract(program("Use", source), absent = Current, outdated = Outdated))

  test("a default getter the classpath lacks is read") {
    read("""import lib.*
           |object Use {
           |  def made: String = Defaults.make()
           |}""".stripMargin)
  }

  test("a default getter of a constructor the classpath lacks is read") {
    read("""import lib.*
           |object Use {
           |  def sized: Int = new Box().size
           |}""".stripMargin)
  }

  test("a memo the classpath lacks is read") {
    read("""import lib.*
           |object Use {
           |  def depth(node: Int): Int = Cache.table.getOrElseUpdate(node, depth(node - 1) + 1)
           |  def depths(nodes: Vector[Int]): Vector[Int] = nodes.map(depth)
           |}""".stripMargin)
  }
}
