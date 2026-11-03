package net.kurobako.bonsai.analysis

class LivenessSuite extends AnalysisSuite {
  private def findings(
      main: Seq[(String, String)],
      test: Seq[(String, String)] = Nil,
      resources: Seq[(String, String)] = Nil
  ): Set[(String, String)] =
    Liveness.findings(extract(main, test, resources).graph).map(finding => finding.rule.id -> finding.node.name).toSet

  private val entry = "src/main/scala/Main.scala" -> "object Main { def main(args: Array[String]): Unit = Used.run() }"

  test("definitions unreachable from an entry point are unused") {
    assertEquals(
      findings(Seq(
        entry,
        "src/main/scala/Used.scala" -> "object Used { def run(): Unit = () }",
        "src/main/scala/Dead.scala" -> "class Dead"
      )),
      Set("unused" -> "Dead")
    )
  }

  test("top-level aliases nothing refers to are unused") {
    assertEquals(
      findings(Seq(
        entry,
        "src/main/scala/Used.scala"    -> "object Used { def run(): Unit = () }",
        "src/main/scala/Aliases.scala" ->
          """package recovery
            |type Spare = scala.collection.immutable.List[Int]
            |val Idle: scala.collection.immutable.List.type = scala.collection.immutable.List""".stripMargin
      )),
      Set("unused" -> "recovery.Spare", "unused" -> "recovery.Idle")
    )
  }

  test("dependencies of unused definitions are unused, including dead cycles") {
    assertEquals(
      findings(
        Seq(
          entry,
          "src/main/scala/Used.scala" -> "object Used { def run(): Unit = () }",
          "src/main/scala/A.scala"    -> "class A(val b: B)",
          "src/main/scala/B.scala"    -> "class B(val a: Option[A], val c: C)",
          "src/main/scala/C.scala"    -> "class C"
        )
      ),
      Set("unused" -> "A", "unused" -> "B", "unused" -> "C")
    )
  }

  test("main definitions reachable only from tests are test-only") {
    assertEquals(
      findings(
        Seq(
          entry,
          "src/main/scala/Used.scala"   -> "object Used { def run(): Unit = () }",
          "src/main/scala/Helper.scala" -> "object Helper { def value = 1 }"
        ),
        Seq("src/test/scala/Check.scala" -> "class Check { def check = Helper.value }")
      ),
      Set("test-only" -> "Helper")
    )
  }

  test("companions are one definition") {
    assertEquals(
      findings(
        Seq(
          "src/main/scala/Main.scala" -> "object Main { def main(args: Array[String]): Unit = { val _ = Pair.of(1) } }",
          "src/main/scala/Pair.scala" ->
            "case class Pair(value: Int); object Pair { def of(value: Int) = new Pair(value) }"
        )
      ),
      Set.empty
    )
  }

  test("an implementation never constructed is unused even when its interface is used") {
    assertEquals(
      findings(
        Seq(
          "src/main/scala/Main.scala" ->
            "object Main { def main(args: Array[String]): Unit = Main.use(Live()) ; def use(shape: Shape) = shape.area }",
          "src/main/scala/Shape.scala"   -> "trait Shape { def area: Int }",
          "src/main/scala/Live.scala"    -> "class Live extends Shape { def area = 1 }",
          "src/main/scala/Unbuilt.scala" -> "class Unbuilt extends Shape { def area = 2 }"
        )
      ),
      Set("unused" -> "Unbuilt")
    )
  }

  test("nested definitions are reported at their outermost unused enclosure") {
    assertEquals(
      findings(
        Seq(
          entry,
          "src/main/scala/Used.scala"  -> "object Used { def run(): Unit = (); class Spare }",
          "src/main/scala/Outer.scala" -> "object Outer { class Inner; def inner = Inner() }"
        )
      ),
      Set("unused" -> "Used.Spare", "unused" -> "Outer")
    )
  }

  test("top-level definitions are reported individually") {
    assertEquals(
      findings(
        Seq(
          "src/main/scala/Main.scala"  -> "@main def run(): Unit = println(used)",
          "src/main/scala/tools.scala" -> "def used = 1\ndef spare = 2"
        )
      ),
      Set("unused" -> "spare")
    )
  }

  test("enum cases belong to their enum") {
    assertEquals(
      findings(
        Seq(
          "src/main/scala/Main.scala"   -> "object Main { def main(args: Array[String]): Unit = println(Colour.Red) }",
          "src/main/scala/Colour.scala" -> "enum Colour { case Red, Green; case Mixed(level: Int) }"
        )
      ),
      Set.empty
    )
  }

  test("givens resolved through imports keep their owners alive") {
    assertEquals(
      findings(
        Seq(
          "src/main/scala/Main.scala" ->
            "object Main { import Instances.given; def main(args: Array[String]): Unit = println(summon[Ordering[Main.type]]) }",
          "src/main/scala/Instances.scala" -> "object Instances { given Ordering[Main.type] = Ordering.by(_ => 0) }"
        )
      ),
      Set.empty
    )
  }

  test("givens summoned during inline expansion are kept alive by their imports") {
    assertEquals(
      findings(
        Seq(
          "src/main/scala/Main.scala" ->
            "import Instances.given\nobject Main { def main(args: Array[String]): Unit = println(Codec.derive[Main.type]) }",
          "src/main/scala/Codec.scala" ->
            "object Codec { inline def derive[A]: Ordering[A] = scala.compiletime.summonInline[Ordering[A]] }",
          "src/main/scala/Instances.scala" -> "object Instances { given Ordering[Main.type] = Ordering.by(_ => 0) }"
        )
      ),
      Set.empty
    )
  }

  test("given instances summoned by inline expansion live with their owner") {
    assertEquals(
      findings(
        Seq(
          "src/main/scala/Main.scala" ->
            "object Main { def main(args: Array[String]): Unit = println(Checked.check(1)) }",
          "src/main/scala/Checked.scala" ->
            """trait Check[A] { def ok(value: A): Boolean }
              |object Check { given Positive: Check[Int] with { def ok(value: Int) = value > 0 } }
              |object Checked {
              |  inline def check(value: Int): Boolean = scala.compiletime.summonInline[Check[Int]].ok(value)
              |}""".stripMargin
        )
      ),
      Set.empty
    )
  }

  test("service, compiler plugin and sbt plugin registrations are entry points") {
    assertEquals(
      findings(
        Seq(
          "src/main/scala/Service.scala" -> "package p; class Service { def helper = Helper }",
          "src/main/scala/Helper.scala"  -> "package p; object Helper",
          "src/main/scala/Plugin.scala"  -> "package p; class Plugin",
          "src/main/scala/Build.scala"   -> "package p; object Build"
        ),
        resources = Seq(
          "META-INF/services/java.lang.Runnable" -> "p.Service\n",
          "plugin.properties"                    -> "pluginClass=p.Plugin\n",
          "sbt/sbt.autoplugins"                  -> "p.Build\n"
        )
      ),
      Set.empty
    )
  }

  test("the public types of a library are entry points, while its hidden types still need a user") {
    val result = extract(
      Seq("Client.scala" ->
        """package lib
          |object Client { def fetch: Int = Helper.value }
          |object Helper { def value: Int = 1 }
          |private object Hidden
          |private[lib] object Internal
          |object Outer { private object Inner }""".stripMargin),
      Seq("ClientCheck.scala" -> "class ClientCheck { val value = lib.Client.fetch }"),
      libraries = Set("m")
    )
    val found = Liveness.findings(result.graph).map(finding => s"${finding.rule.id} ${finding.node.name}")
    assertEquals(found.sorted, Vector("unused lib.Hidden", "unused lib.Internal", "unused lib.Outer.Inner"))
  }

  test("generated sources are categorised by path") {
    val graph =
      extract(
        Seq(
          "src/generated/main/scala/Table.scala" -> "object Table",
          "src/main/scala/Code.scala"            -> "object Code"
        ),
        generated = GeneratedSources
      ).graph
    assertEquals(
      graph.nodes.values.map(node => node.name -> node.generated).toMap,
      Map("Table" -> true, "Code" -> false)
    )
  }

  test("generated definitions and members nothing uses are not reported, their handwritten users still count") {
    val result = extract(
      Seq(
        entry,
        "src/main/scala/Used.scala"            -> "object Used { def run(): Unit = println(Keys.Read) }",
        "src/generated/main/scala/Keys.scala"  -> "object Keys { val Read = \"READ\"; val Spare = \"SPARE\" }",
        "src/generated/main/scala/Table.scala" -> "object Table { val Rows = Vector(Helper.row) }",
        "src/main/scala/Helper.scala"          -> "object Helper { def row = 1 }"
      ),
      generated = GeneratedSources
    )
    val found = Liveness.findings(result.graph) ++
      Members.findings(result.members, result.graph.nodes, Liveness.live(result.graph))
    assertEquals(found.filter(_.node.generated), Vector.empty)
    assertFinding(found, "unused", "Helper is unreachable from every entry point")
  }
}
