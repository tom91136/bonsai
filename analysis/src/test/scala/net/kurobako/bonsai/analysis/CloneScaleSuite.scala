package net.kurobako.bonsai.analysis

import net.kurobako.bonsai.collections.CloneCost
import scala.concurrent.duration.*

/** Equivalent Scala and Java code has the same cost, and so is a copy in both languages or in neither. */
class CloneScaleSuite extends AnalysisSuite {

  override val munitTimeout: Duration = 3.minutes

  private case class Equivalent(name: String, scala: String, java: String, cost: Int)

  private val ScalaUtil =
    """object Util {
      |  def g(x: Int): Int = x
      |  def h(x: Int, y: Int): Int = x
      |  def each(xs: List[Int], f: Int => Int): Unit = ()
      |}
      |class Base { var count = 0; def bump(x: Int): Int = x }""".stripMargin

  private val JavaUtil =
    """package demo;
      |public class Util {
      |  public static int g(int x) { return x; }
      |  public static int h(int x, int y) { return x; }
      |  public static void each(java.util.List<Integer> xs, java.util.function.IntUnaryOperator f) {}
      |}""".stripMargin

  private val JavaParent =
    "package demo;\npublic class Base { public int count = 0; public int bump(int x) { return x; } }"

  private def indented(text: String): String = text.linesIterator.map("    " + _).mkString("\n")

  /** The groups of copies among methods with the given bodies, each of which returns an int. */
  private def scala(bodies: String*): Vector[Clones.Group] = {
    val methods = bodies.zipWithIndex.map((body, index) =>
      s"  def f$index(xs: List[Int], s: String): Int = {\n${indented(body)}\n  }"
    )
    val source = s"import Util.*\nclass First extends Base {\n${methods.mkString("\n")}\n}"
    extract(Seq("First.scala" -> source, "Util.scala" -> ScalaUtil)).clones
  }

  private def java(bodies: String*): Vector[Clones.Group] = {
    val methods = bodies.zipWithIndex.map((body, index) =>
      s"  int f$index(List<Integer> xs, String s) {\n${indented(body)}\n  }"
    )
    val source =
      s"package demo;\nimport java.util.List;\nimport static demo.Util.*;\npublic class First extends Base {\n" +
        methods.mkString("\n") + "\n}"
    javaFacts(
      Set.empty,
      "demo/First.java" -> source,
      "demo/Util.java"  -> JavaUtil,
      "demo/Base.java"  -> JavaParent
    ).clones
  }

  private def largest(groups: Vector[Clones.Group]): Int = groups.map(_.cost).maxOption.getOrElse(0)

  private val ScalaPad = (1 to 10).map(i => s"val p$i = h($i, $i)").mkString("\n")
  private val JavaPad  = (1 to 10).map(i => s"int p$i = h($i, $i);").mkString("\n")

  private lazy val ScalaBaseCost = largest(scala(ScalaPad + "\n0", ScalaPad + "\n0"))
  private lazy val JavaBaseCost  = largest(java(JavaPad + "\nreturn 0;", JavaPad + "\nreturn 0;"))

  private val equivalents = Seq(
    Equivalent("a declaration", "val a = g(1)", "int a = g(1);", 4),
    Equivalent(
      "a loop over a list with a call and an assignment",
      "var total = 0\nfor x <- xs do {\n  g(x)\n  total = x\n}",
      "int total = 0;\nfor (int x : xs) {\n  g(x);\n  total = x;\n}",
      14
    ),
    Equivalent(
      "a chain of three calls",
      "val n = s.trim().toLowerCase().length()",
      "int n = s.trim().toLowerCase().length();",
      8
    ),
    Equivalent(
      "five statements",
      "val a = g(1)\nval b = g(a)\nval c = h(a, b)\ng(c)\nh(c, a)",
      "int a = g(1);\nint b = g(a);\nint c = h(a, b);\ng(c);\nh(c, a);",
      20
    ),
    Equivalent("arithmetic", "val a = g(1) + 2 * g(3)", "int a = g(1) + 2 * g(3);", 10),
    Equivalent(
      "a conditional with branches",
      "if g(1) > 0 then {\n  g(2)\n  g(4)\n} else {\n  g(3)\n  g(5)\n}",
      "if (g(1) > 0) {\n  g(2);\n  g(4);\n} else {\n  g(3);\n  g(5);\n}",
      20
    ),
    Equivalent("a lambda", "each(xs, x => g(x))", "each(xs, x -> g(x));", 8),
    Equivalent(
      "a lambda with a block",
      "each(xs, x => {\n  g(x)\n  g(2)\n})",
      "each(xs, x -> {\n  g(x);\n  return g(2);\n});",
      12
    ),
    Equivalent("a construction", "val b = new StringBuilder(s)", "StringBuilder b = new StringBuilder(s);", 3),
    Equivalent("an assignment to a field", "count = g(1)", "count = g(1);", 5),
    Equivalent("a call of an inherited method", "bump(2)", "bump(2);", 3),
    Equivalent(
      "a loop with a counter",
      "var i = 0\nwhile i < 3 do {\n  i = i + g(1)\n}",
      "int i = 0;\nwhile (i < 3) {\n  i = i + g(1);\n}",
      13
    ),
    Equivalent("an update in place", "count += 1", "count += 1;", 5),
    Equivalent("a cast", "val o = s.asInstanceOf[Object]", "Object o = (Object) s;", 3),
    Equivalent(
      "a try with a handler",
      "try g(1) catch case e: Exception => g(2)",
      "try { g(1); } catch (Exception e) { g(2); }",
      9
    )
  )

  equivalents.foreach { equivalent =>
    test(s"${equivalent.name} costs ${equivalent.cost} in Scala and in Java") {
      val scalaCost = largest(scala(Seq.fill(2)(ScalaPad + "\n" + equivalent.scala + "\n0")*)) - ScalaBaseCost
      val javaCost  = largest(java(Seq.fill(2)(JavaPad + "\n" + equivalent.java + "\nreturn 0;")*)) - JavaBaseCost
      assertEquals(scalaCost, equivalent.cost, "Scala")
      assertEquals(javaCost, equivalent.cost, "Java")
    }
  }

  private def scalaStatements(count: Int): String = (0 until count).map(i => s"val c$i = g($i)").mkString("\n")

  private def javaStatements(count: Int): String = (0 until count).map(i => s"int c$i = g($i);").mkString("\n")

  test("a body is a copy in both languages once it reaches the cost of an expression, and in neither before") {
    val perStatement = 4
    val enough       = (CloneCost.Expression - 2 + perStatement - 1) / perStatement
    Seq(enough - 1, enough).foreach { count =>
      val scalaCopy = scala(Seq.fill(2)(scalaStatements(count) + "\n0")*).nonEmpty
      val javaCopy  = java(Seq.fill(2)(javaStatements(count) + "\nreturn 0;")*).nonEmpty
      assertEquals(scalaCopy, count == enough, s"Scala with $count statements")
      assertEquals(javaCopy, count == enough, s"Java with $count statements")
    }
  }

  test("a run of statements shared by different methods is a copy in both languages once it reaches the window cost") {
    val perStatement = 4
    val enough       = (CloneCost.Window + perStatement - 1) / perStatement
    Seq(enough - 1, enough).foreach { count =>
      def scalaMethod(prefix: String, suffix: String) =
        s"$prefix\n${scalaStatements(count)}\n$suffix"
      def javaMethod(prefix: String, suffix: String) =
        s"$prefix\n${javaStatements(count)}\n$suffix"
      val scalaCopy = scala(scalaMethod("val x = g(1)", "x"), scalaMethod("val x = h(1, 2)", "x + 1")).nonEmpty
      val javaCopy  =
        java(javaMethod("int x = g(1);", "return x;"), javaMethod("int x = h(1, 2);", "return x + 1;")).nonEmpty
      assertEquals(scalaCopy, count == enough, s"Scala with $count statements")
      assertEquals(javaCopy, count == enough, s"Java with $count statements")
    }
  }
}
