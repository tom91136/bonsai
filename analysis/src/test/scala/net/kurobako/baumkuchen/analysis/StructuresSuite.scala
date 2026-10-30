package net.kurobako.baumkuchen.analysis

class StructuresSuite extends AnalysisSuite {
  private def findings(
      main: Seq[(String, String)],
      test: Seq[(String, String)] = Nil,
      twins: Seq[Seq[(String, String)]] = Nil
  ): Vector[Finding] = {
    val result = extract(main, test, twins = twins)
    Structures.findings(result.structures, result.graph)
  }

  private def of(found: Vector[Finding], rule: Finding.Rule): Vector[String] =
    found.filter(_.rule == rule).map(_.detail.takeWhile(_ != ';')).sorted

  test("a field named like its class reads twice wherever a value is named for the type") {
    val found = findings(Seq(
      "Model.scala" ->
        """case class Graph(graph: Map[String, Int])
          |class Nodes(val nodes: Vector[Int])
          |class Holder(holder: Int) { def value: Int = holder }
          |case class Edge(from: String)
          |object Use { def size(graph: Graph): Int = graph.graph.size + graph.graph.size }""".stripMargin
    ))
    assertEquals(
      of(found, Finding.Rule.FieldStutter),
      Vector(
        "Graph.graph is named like Graph, so code reads graph.graph, as 2 places do",
        "Nodes.nodes is named like Nodes, so code reads nodes.nodes"
      )
    )
  }

  test("a string naming a member its type no longer has is left from a rename") {
    val found = findings(
      Seq(
        "Tools.scala" ->
          """enum Color { case Red, Green }
            |object Tools {
            |  def renamed: Int = 1
            |  val notes = Vector(
            |    "Tools.original", "Tools.renamed", "Tools.renamed(1)", "Tools.original is gone", "Other.thing",
            |    "Color.Red", "Tools.scala", "Tools.toString", "Color.Blue("
            |  )
            |}""".stripMargin
      ),
      Seq("ToolsTest.scala" -> """object ToolsTest { val name = "Tools.removed" }""")
    )
    assertEquals(
      of(found, Finding.Rule.StaleNameString),
      Vector(
        "\"Color.Blue(\" names Color.Blue, which no Color of the build has",
        "\"Tools.original\" names Tools.original, which no Tools of the build has",
        "\"Tools.removed\" names Tools.removed, which no Tools of the build has"
      )
    )
    assertFinding(found, "stale-name-string", "Tools.removed")(using Finding.Level.Info)
    assertFinding(found, "stale-name-string", "Tools.original")
  }

  test("a literal completing a name that the splice before it begins is not a stale name") {
    val found = findings(Seq(
      "Images.scala" ->
        """object Images {
          |  class ImageFormat
          |  class IntegerImageFormat { object Layout }
          |  def render(family: String, args: String): String = s"Some(${family}ImageFormat.Layout($args))"
          |  def bare(args: String): String = "ImageFormat.Layout(" + args
          |}""".stripMargin
    ))
    assertEquals(
      of(found, Finding.Rule.StaleNameString),
      Vector("\"ImageFormat.Layout(\" names ImageFormat.Layout, which no ImageFormat of the build has")
    )
  }

  test("aliases filling one constructor with literals give each type a second spelling") {
    val found = findings(Seq(
      "Widths.scala" ->
        """class Bits[N <: Int]
          |object Widths {
          |  type Bits8  = Bits[8]
          |  type Bits16 = Bits[16]
          |  type Bits32 = Bits[32]
          |  type Pair1 = Bits[1]
          |  type Code = Bits[4]
          |  object Code { def zero: Int = 0 }
          |}
          |object Others { type Pair2 = Bits[2]; type Pair3 = Bits[3]; type Plain = Bits[Int] }""".stripMargin
    ))
    assertEquals(
      of(found, Finding.Rule.ParameterAliasFamily),
      Vector("4 aliases fill Bits with a literal (Bits16, Bits32, Bits8, Pair1), giving each type a second spelling")
    )
  }

  test("files of a package that depend on the rest, never the other way, can be a subpackage") {
    val core = Seq(
      "A.scala" -> "package app\nclass A { def b: B = B() }",
      "B.scala" -> "package app\nclass B { def c: C = C() }",
      "C.scala" -> "package app\nclass C { def a: A = A() }"
    )
    val ui = Seq(
      "X.scala" -> "package app\nclass X { def y: Y = Y(); def a: A = A() }",
      "Y.scala" -> "package app\nclass Y { def z: Z = Z() }",
      "Z.scala" -> "package app\nclass Z { def x: X = X() }"
    )
    assertEquals(
      of(findings(core ++ ui), Finding.Rule.PackageSplitCandidate),
      Vector(
        "in app, 3 files (X.scala, Y.scala, Z.scala) depend on 3 others through 1 reference, which never depend back"
      )
    )
    val tangled = core.tail :+ ("A.scala" -> "package app\nclass A { def b: B = B(); def x: X = X() }")
    assertEquals(of(findings(tangled ++ ui), Finding.Rule.PackageSplitCandidate), Vector.empty)
    val leaning = ui.map((name, source) => name -> source.replace(" }", "; def all = (A(), B(), C()) }"))
    assertEquals(of(findings(core ++ leaning), Finding.Rule.PackageSplitCandidate), Vector.empty)
  }

  test("a concrete class only its one subclass extends and nothing constructs is that subclass") {
    val found = findings(
      Seq(
        "Classes.scala" ->
          """class Base { def f: Int = 1 }
            |class Only extends Base { def g: Int = 2 }
            |class Built { def f: Int = 1 }
            |class Child extends Built
            |class Shared { def f: Int = 1 }
            |class Lhs extends Shared
            |class Rhs extends Shared
            |class Hooked { def f: Int = 1 }
            |abstract class Shape { def area: Int }
            |class Square extends Shape { def area: Int = 4 }
            |class Doubled { def f: Int = 1 }
            |object Main {
            |  def main(args: Array[String]): Unit =
            |    println(Seq(Only(), Built(), Child(), Lhs(), Rhs(), new Hooked { override def f = 2 }, Square()))
            |}""".stripMargin
      ),
      Seq("Doubles.scala" -> "class FakeDoubled extends Doubled")
    )
    assertEquals(
      of(found, Finding.Rule.BaseClassSingleSubclass),
      Vector("Base is never constructed itself and only Only extends it")
    )
  }

  test("an object whose members fall into groups that never meet holds two designs") {
    val found = findings(Seq(
      "Kinds.scala" -> "case class Token(text: String)\ncase class Page(lines: Vector[String])",
      "Util.scala"  ->
        """object Util {
          |  def parse(text: String): Token        = Token(text)
          |  def parseAll(texts: Vector[String])  = texts.map(parse)
          |  def render(page: Page): String       = page.lines.mkString
          |  def renderAll(pages: Vector[Page])   = pages.map(render)
          |  def lone: Int                        = 1
          |}
          |object Joined {
          |  def parse(text: String): Token  = Token(text)
          |  def parseAll(texts: Vector[String]) = texts.map(parse)
          |  def paged(token: Token): Page   = Page(Vector(token.text))
          |  def pagedAll(tokens: Vector[Token]) = tokens.map(paged)
          |}""".stripMargin
    ))
    assertEquals(
      of(found, Finding.Rule.DisjointObject),
      Vector(
        "object Util holds 2 groups of members that never use each other and use different types " +
          "({parse, parseAll}, {render, renderAll})"
      )
    )
  }

  test("a small type alone in its file that one type uses belongs beside it, unless a platform has its own") {
    val found = findings(
      Seq(
        "Small.scala"  -> "case class Small(value: Int)",
        "Big.scala"    -> "class Big { def small: Small = Small(1); def shared: Common = Common(1) }",
        "Common.scala" -> "case class Common(value: Int)",
        "Others.scala" -> "class First { def c: Common = Common(2) }\nclass Second { def c: Common = Common(3) }",
        "Wide.scala"   ->
          ("case class Wide(value: Int) {\n" + (1 to 10).map(n => s"  def f$n: Int = $n").mkString("\n") + "\n}"),
        "Uses.scala"  -> "class Uses { def wide: Wide = Wide(1) }",
        "Fence.scala" -> "object Fence { def fence(): Int = 1 }",
        "Heap.scala"  -> "class Heap { def allocate: Int = Fence.fence() }"
      ),
      // One platform shares the source another replaces.
      twins = Seq(Seq("Fence.scala" -> "object Fence { def fence(): Int = 2 }"), Seq.empty)
    )
    assertEquals(
      of(found, Finding.Rule.SatelliteType),
      Vector("Small is 1 line in a file of its own, used only by Big")
    )
  }

  test("functions matching over an enum beside it are methods of the enum") {
    val found = findings(Seq(
      "Color.scala" ->
        """enum Color { case Red, Green, Blue }
          |object Palette {
          |  def label(color: Color): String = color match {
          |    case Color.Red   => "r"
          |    case Color.Green => "g"
          |    case Color.Blue  => "b"
          |  }
          |  def warm(color: Color): Boolean = color match {
          |    case Color.Red => true
          |    case _         => false
          |  }
          |}""".stripMargin,
      "Shade.scala" ->
        """enum Shade { case Light, Dark }
          |object Shades {
          |  def code(shade: Shade): Int = shade match { case Shade.Light => 0; case Shade.Dark => 1 }
          |  def name(shade: Shade): String = shade match { case Shade.Light => "l"; case Shade.Dark => "d" }
          |  val offset: Int                = 1
          |  def shifted(shade: Shade): Int = shade match { case Shade.Light => offset; case Shade.Dark => offset + 1 }
          |}
          |class Stage {
          |  private enum Phase { case Early, Late }
          |  def step: Int                    = order(Phase.Early)
          |  private def order(phase: Phase): Int = phase match { case Phase.Early => 0; case Phase.Late => 1 }
          |}""".stripMargin,
      "Render.scala" ->
        """object Render {
          |  def show(color: Color): String = color match { case Color.Red => "R"; case _ => "?" }
          |  def full(color: Color): Int    = color match { case Color.Red => 1; case Color.Green => 2; case Color.Blue => 3 }
          |}""".stripMargin
    ))
    val dispatched = found.filter(_.rule == Finding.Rule.EnumDispatchFunction)
    assertEquals(
      dispatched.map(finding => (finding.level, finding.detail.takeWhile(_ != ';'))).sortBy(_._2),
      Vector(
        Finding.Level.Warning -> "code matches its Shade parameter shade over its cases",
        Finding.Level.Info    -> "label matches its Color parameter color over its cases",
        Finding.Level.Warning -> "name matches its Shade parameter shade over its cases",
        Finding.Level.Info    -> "order matches its Phase parameter phase over its cases"
      )
    )
  }
}
