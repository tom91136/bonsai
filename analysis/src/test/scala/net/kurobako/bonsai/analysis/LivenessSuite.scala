package net.kurobako.bonsai.analysis

class LivenessSuite extends AnalysisSuite {
  private def findings(
      main: Seq[(String, String)],
      test: Seq[(String, String)] = Nil,
      resources: Seq[(String, String)] = Nil,
      resourced: Seq[(String, String)] = Nil
  ): Set[(String, String)] = {
    val result = extract(main, test, resources, resourced = resourced)
    Liveness.findings(result.graph, result.reach).map(finding => finding.rule.id -> finding.node.name).toSet
  }

  private val entry = "src/main/scala/Main.scala" -> "object Main { def main(args: Array[String]): Unit = Used.run() }"

  test("a Java type the build names only in a string is an entry point") {
    val result = javaFactsReflective(
      Set.empty,
      Vector("demo.Controller"),
      "demo/Controller.java" -> """
        |package demo;
        |
        |public class Controller {
        |  public void initialize() {}
        |}
        |""".stripMargin
    )

    assertEquals(
      Liveness.findings(result.graph, result.reach).map(finding => finding.rule.id -> finding.node.name),
      Vector.empty
    )
  }

  test("a Java type named only in a string keeps the types inside it") {
    val result = javaFactsReflective(
      Set.empty,
      Vector("demo.Outer"),
      "demo/Outer.java" -> """
        |package demo;
        |
        |public class Outer {
        |  public static class Inner {
        |    public void initialize() {}
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(
      Liveness.findings(result.graph, result.reach).map(finding => finding.rule.id -> finding.node.name),
      Vector.empty
    )
  }

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

  test("a type only a dead method of a live type refers to is unused, however it refers to it") {
    assertEquals(
      findings(
        Seq(
          entry,
          "src/main/scala/Used.scala" ->
            "object Used { def run(): Unit = (); def built(): Orphan = new Orphan; def typed(shed: Shed): Int = 1 }",
          "src/main/scala/Orphan.scala" -> "class Orphan",
          "src/main/scala/Shed.scala"   -> "class Shed"
        )
      ),
      Set("unused" -> "Orphan", "unused" -> "Shed")
    )
  }

  test("a type that live code names only in a type position stays live") {
    assertEquals(
      findings(
        Seq(
          entry,
          "src/main/scala/Used.scala"    -> "object Used { def run(): Unit = { val _ = List.empty[Phantom] } }",
          "src/main/scala/Phantom.scala" -> "class Phantom"
        )
      ),
      Set.empty
    )
  }

  test("a type only the macro implementation of a reachable inline method names is live") {
    assertEquals(
      findings(
        Seq(
          entry,
          "src/main/scala/Used.scala" ->
            "object Used { inline def text: String = ${ Expansion.text }; def run(): Unit = println(text) }",
          "src/main/scala/Expansion.scala" ->
            "import scala.quoted.*\nobject Expansion { def text(using Quotes): Expr[String] = Expr(Layers.describe) }",
          "src/main/scala/Layers.scala" -> "object Layers { def describe: String = \"layers\" }"
        )
      ),
      Set.empty
    )
  }

  test("a type only the macro implementation of an unreachable inline method names is unused") {
    assertEquals(
      findings(
        Seq(
          entry,
          "src/main/scala/Used.scala" ->
            "object Used { inline def text: String = ${ Expansion.text }; def run(): Unit = () }",
          "src/main/scala/Expansion.scala" ->
            "import scala.quoted.*\nobject Expansion { def text(using Quotes): Expr[String] = Expr(Layers.describe) }",
          "src/main/scala/Layers.scala" -> "object Layers { def describe: String = \"layers\" }"
        )
      ),
      Set("unused" -> "Layers")
    )
  }

  private def node(name: String, module: String, test: Boolean = false): (String, Node) = name ->
    Node(
      name,
      module,
      if (test) Output.Scope.Test else Output.Scope.Main,
      s"$name.scala",
      1,
      generated = false,
      exported = false,
      None
    )

  private def testOnly(nodes: Map[String, Node], declared: Map[String, Set[String]], tested: Set[String]): Set[String] =
    Liveness.findings(Graph(nodes, declared, Set.empty), Reach(Set.empty, tested, Set.empty, Set.empty))
      .collect { case finding if finding.rule == Finding.Rule.TestOnly => finding.node.name }.toSet

  private val kit = Map(node("kit.Fake", "kit"), node("app.Main", "app"), node("app.Spec", "app", test = true))

  test("a module only other modules' tests use serves tests, so what they reach in it is not test-only") {
    assertEquals(testOnly(kit, Map("app.Spec" -> Set("kit.Fake")), Set("kit.Fake")), Set.empty[String])
  }

  test("a module no root but a test reaches serves tests, so what they reach in it is not test-only") {
    val fixtures = Map(node("tests.Fixture", "tests"), node("tests.Spec", "tests", test = true))
    assertEquals(testOnly(fixtures, Map("tests.Spec" -> Set("tests.Fixture")), Set("tests.Fixture")), Set.empty[String])
  }

  test("a module another module's main code uses is not test support, so test-only code in it is reported") {
    val used  = kit + node("kit.Used", "kit")
    val edges = Map("app.Spec" -> Set("kit.Fake"), "app.Main" -> Set("kit.Used"))
    assertEquals(testOnly(used, edges, Set("kit.Fake")), Set("kit.Fake"))
  }

  test("a single module with its own entry point is not test support, so test-only code in it is reported") {
    val single = Map(node("m.Main", "m"), node("m.Staged", "m"), node("m.Spec", "m", test = true))
    assertEquals(testOnly(single, Map("m.Spec" -> Set("m.Staged")), Set("m.Staged")), Set("m.Staged"))
  }

  test("a type only a method reached from tests alone refers to is test-only") {
    assertEquals(
      findings(
        Seq(
          entry,
          "src/main/scala/Used.scala"   -> "object Used { def run(): Unit = (); def staged(): Staged = new Staged }",
          "src/main/scala/Staged.scala" -> "class Staged"
        ),
        Seq("src/test/scala/Check.scala" -> "class Check { def check = Used.staged() }")
      ),
      Set("test-only" -> "Staged")
    )
  }

  test("a Java type only a dead Java method refers to is unused, and one a reachable method refers to is live") {
    val result = javaFacts(
      Set.empty,
      "demo/App.java" ->
        """package demo;
          |
          |public class App {
          |  public static void main(String[] args) { Used.run(); }
          |  void dead() { new Orphan(); }
          |}
          |""".stripMargin,
      "demo/Used.java"   -> "package demo;\n\nclass Used {\n  static void run() {}\n}\n",
      "demo/Orphan.java" -> "package demo;\n\nclass Orphan {}\n"
    )
    assertEquals(
      Liveness.findings(result.graph, result.reach).map(finding => finding.rule.id -> finding.node.name).toSet,
      Set("unused" -> "demo.Orphan")
    )
  }

  test("the producers say how each root is entered") {
    val result = extract(
      Seq(
        entry,
        "src/main/scala/Used.scala"    -> "object Used { def run(): Unit = () }",
        "src/main/scala/Service.scala" -> "package p; class Service extends Runnable { def run(): Unit = () }",
        "src/main/scala/Named.scala"   -> "package p; object Named",
        "src/main/scala/Api.scala"     -> "package lib; object Api"
      ),
      resources = Seq("META-INF/services/java.lang.Runnable" -> "p.Service\n"),
      reflective = Vector("p.Named"),
      libraries = Set("m")
    )
    val kinds = result.graph.roots.groupMap(_.name)(_.kind)
    assertEquals(kinds("Main"), Set(Root.Kind.Main, Root.Kind.Exported))
    assertEquals(kinds("p.Service"), Set(Root.Kind.Provided, Root.Kind.Exported))
    assertEquals(kinds("p.Named"), Set(Root.Kind.Reflective))
    assertEquals(kinds("lib.Api"), Set(Root.Kind.Exported))
  }

  test("Java types are rooted as main, exported and reflective entries") {
    val result = javaFactsReflective(
      Set("j"),
      Vector("demo.Reflected"),
      "demo/App.java" ->
        "package demo;\n\npublic class App {\n  public static void main(String[] args) {}\n}\n",
      "demo/Reflected.java" -> "package demo;\n\nclass Reflected {}\n"
    )
    val kinds = result.graph.roots.groupMap(_.name)(_.kind)
    assertEquals(kinds("demo.App"), Set(Root.Kind.Main, Root.Kind.Exported))
    assertEquals(kinds("demo.Reflected"), Set(Root.Kind.Reflective))
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
          "src/main/scala/Service.scala" ->
            "package p; class Service extends Runnable { def run(): Unit = { val _ = Helper } }",
          "src/main/scala/Helper.scala" -> "package p; object Helper",
          "src/main/scala/Plugin.scala" -> "package p; class Plugin",
          "src/main/scala/Build.scala"  -> "package p; object Build"
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

  test("registrations in a resource directory of the build are entry points") {
    assertEquals(
      findings(
        Seq(
          "src/main/scala/Service.scala" ->
            "package p; class Service extends Runnable { def run(): Unit = { val _ = Helper } }",
          "src/main/scala/Helper.scala" -> "package p; object Helper",
          "src/main/scala/Plugin.scala" -> "package p; class Plugin",
          "src/main/scala/Build.scala"  -> "package p; object Build"
        ),
        resourced = Seq(
          "META-INF/services/java.lang.Runnable" -> "p.Service\n",
          "plugin.properties"                    -> "pluginClass=p.Plugin\n",
          "sbt/sbt.autoplugins"                  -> "p.Build\n"
        )
      ),
      Set.empty
    )
  }

  test("a registration in both the classes and a resource directory is read once") {
    val sources = Seq(
      "src/main/scala/Service.scala" -> "package p; class Service extends Runnable { def run(): Unit = () }"
    )
    val registration = "META-INF/services/java.lang.Runnable" -> "p.Service\n"
    val both         = extract(sources, resources = Seq(registration), resourced = Seq(registration))
    val classes      = extract(sources, resources = Seq(registration))
    assertEquals(both.graph.roots, classes.graph.roots)
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
    val found = Liveness.findings(result.graph, result.reach).map(finding => s"${finding.rule.id} ${finding.node.name}")
    assertEquals(found.sorted, Vector("unused lib.Hidden", "unused lib.Internal", "unused lib.Outer.Inner"))
  }

  test("the overrides of a library's public abstract method run when the program instantiates them") {
    val result = extract(
      Seq(
        "Gate.scala" ->
          """package lib
            |trait Gate { def wrap(n: Int): Gate }
            |object Gate {
            |  def apply: Gate = new Plain
            |  private final class Plain extends Gate { def wrap(n: Int): Gate = new Wrapped }
            |  private final class Wrapped extends Gate { def wrap(n: Int): Gate = this }
            |}""".stripMargin
      ),
      Seq("GateCheck.scala" -> "class GateCheck { def run(gate: lib.Gate): lib.Gate = gate.wrap(1) }"),
      libraries = Set("m")
    )
    val found = Liveness.findings(result.graph, result.reach).map(finding => s"${finding.rule.id} ${finding.node.name}")
    assertEquals(found.sorted, Vector.empty[String])
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
    val found = Liveness.findings(result.graph, result.reach) ++
      Members.findings(result.members, result.graph.nodes, result.reach)
    assertEquals(found.filter(_.node.generated), Vector.empty)
    assertFinding(found, "unused", "Helper is unreachable from every entry point")
  }
}
