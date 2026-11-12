package net.kurobako.bonsai.analysis

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import net.kurobako.bonsai.testing.*

/** Builds where one module has an object and an unrelated one the package of its name, which the analysis reads in
  * several runs: every finding is the one a single run over all the modules gives.
  */
class DividedSuite extends AnalysisSuite {

  private def manifest(
      directory: Path,
      padded: String,
      shared: Seq[(String, String)],
      objects: String,
      packaged: String,
      top: String,
      twins: Set[String] = Set.empty,
      own: String = "",
      replacing: String = ""
  ): Manifest = {
    def padding(module: String) =
      Option.when(module == padded)("Padding.scala" -> s"package padding.$module\nclass One\nclass Two\nclass Three")
    val base           = compile(directory.resolve("base"), shared, Vector.empty)
    val objectSources  = Seq("B.scala" -> s"package a\nobject b {\n$objects\n}") ++ padding("objects")
    val packageSources =
      Seq("Side.scala" -> s"package a.b\nobject Side {\n$packaged\n}\n$top") ++ padding("packaged")
    val onObjects = compile(directory.resolve("objects"), objectSources, Vector(base))
    val onPackage = compile(directory.resolve("packaged"), packageSources, Vector(base))
    val visible   = classpath :+ base
    val replaced  = Option.when(replacing.nonEmpty)(
      packageSources.map((name, code) =>
        if (name == "Side.scala") name -> s"package a.b\nobject Side {\n$packaged\n}\n$replacing" else name -> code
      )
    )
    val twinned = Vector("objects" -> objectSources, "packaged" -> replaced.getOrElse(packageSources)).collect {
      case (module, sources) if twins(module) =>
        val classes =
          compile(
            directory.resolve(s"${module}Js"),
            sources :+ ("Js.scala" -> s"package twin\nobject ${module}Js {\n$own\n}"),
            Vector(base)
          )
        Output(s"${module}Js", Output.Scope.Main, classes)
    }
    Manifest(
      twinned ++ Vector(
        Output("base", Output.Scope.Main, base),
        Output("objects", Output.Scope.Main, onObjects),
        Output("packaged", Output.Scope.Main, onPackage)
      ),
      scoped(Map("base" -> classpath, "objects" -> visible, "packaged" -> visible) ++ twinned.map(_.module -> visible)),
      Set.empty,
      platforms = twinned.map(_.module).toSet,
      jdk = targetJdk
    )
  }

  private def eachSide(
      shared: Seq[(String, String)],
      objects: String,
      packaged: String,
      top: String = "",
      bounded: Vector[Complexity.Bounded] = Vector.empty
  )(check: Extraction.Result => Unit): Unit = Seq("objects", "packaged").foreach { padded =>
    Scratch.using("bonsai-") { directory =>
      val result = Extraction.read(manifest(directory, padded, shared, objects, packaged, top), bounded = bounded)
        .fold(errors => fail(errors.mkString("\n")), identity)
      assertEquals(result.problems, Vector.empty)
      check(result)
    }
  }

  private def smells(result: Extraction.Result): Vector[Finding] =
    Smells.findings(result.smells, result.algebra.products, result.graph.nodes)

  private def base(code: String): Seq[(String, String)] = Seq("Base.scala" -> s"package base\n$code")

  test("calls of a method from both sides of a clash are counted together") {
    eachSide(
      base("object Util { def scale(x: Int, factor: Int): Int = x * factor }"),
      "def one: Int = base.Util.scale(1, 5)",
      "def two: Int = base.Util.scale(2, 5)"
    )(result => assertFinding(smells(result), "constant-argument", "Util.scale(x, factor).factor is always 5 across 2"))
  }

  test("a result used on one side of a clash is not never used") {
    eachSide(
      base("object Util { def compute(x: Int): Int = x + 1 }"),
      "def drop(): Unit = { base.Util.compute(1); () }",
      "def keep: Int = base.Util.compute(2)"
    )(result =>
      assertEquals(
        smells(result).filter(_.rule == Finding.Rule.UnusedReturnValue).map(_.detail),
        Vector.empty
      )
    )
  }

  test("a result dropped at a call on each side of a clash is dropped at all calls") {
    eachSide(
      base("object Util { def compute(x: Int): Int = x + 1 }"),
      "def drop(): Unit = { base.Util.compute(1); () }",
      "def drop(): Unit = { base.Util.compute(2); () }"
    )(result => assertFinding(smells(result), "ignored-result", "its Int result is discarded at all 2 calls"))
  }

  test("type arguments given on both sides of a clash are not one type") {
    eachSide(
      base("class Box[T](val value: T)"),
      "def first: Any = (base.Box[Int](1), base.Box[Int](2))",
      "def second: Any = (base.Box[String](\"a\"), base.Box[String](\"b\"))"
    )(result =>
      assertEquals(smells(result).filter(_.rule == Finding.Rule.FixedTypeParameter).map(_.detail), Vector.empty)
    )
  }

  test("a default repeated on both sides of a clash is passed explicitly at all the calls") {
    eachSide(
      base("object Util { def pad(text: String, width: Int = 8): String = text.take(width) }"),
      "def one: String = base.Util.pad(\"a\", 8) + base.Util.pad(\"c\", 4)",
      "def two: String = base.Util.pad(\"b\", 8)"
    )(result => assertFinding(smells(result), "redundant-default", "passed its default 8 explicitly at 2 calls"))
  }

  test("a default that one side of a clash relies on is used") {
    eachSide(
      base("object Util { def pad(text: String, width: Int = 8): String = text.take(width) }"),
      "def one: String = base.Util.pad(\"a\", 4)",
      "def two: String = base.Util.pad(\"b\")"
    )(result =>
      assertEquals(smells(result).filter(_.rule == Finding.Rule.UnusedDefault).map(_.detail), Vector.empty)
    )
  }

  test("a default that neither side of a clash sets is fixed at all the calls") {
    eachSide(
      base("object Util { def pad(text: String, width: Int = 8): String = text.take(width) }"),
      "def one: String = base.Util.pad(\"a\")",
      "def two: String = base.Util.pad(\"b\")"
    )(result =>
      assertFinding(smells(result), "fixed-default", "is never set, as its default is the value at all 2 calls")(
        using Finding.Level.Info
      )
    )
  }

  test("an abstraction implemented in each run of a clash has more than one implementation") {
    eachSide(
      base("trait Plugin { def run(): Int }\nclass Default extends Plugin { def run(): Int = 1 }"),
      "class Custom extends base.Plugin { def run(): Int = 2 }",
      "def other: Int = 3"
    )(result =>
      assertEquals(smells(result).filter(_.rule == Finding.Rule.SingleImplementation).map(_.detail), Vector.empty)
    )
  }

  test("code copied from one side of a clash to the other is grouped") {
    val body = "def stats(xs: List[Int]): Int = {\n  val ys = xs.map(_ * 2).filter(_ > 3)\n" +
      "  val zs = ys.zip(ys.drop(1)).map((a, b) => a * b + a)\n  zs.sum + ys.size + xs.length + zs.max\n}"
    val nodes = Set("a.b", "a.b.Side")
    eachSide(base("object Util { def one: Int = 1 }"), body, body) { result =>
      assert(
        result.clones.exists(group => group.fragments.map(_.node).toSet == nodes),
        result.clones.toString
      )
    }
  }

  test("a class constructed on the other side of a clash is constructed") {
    eachSide(
      base("class Widget\nclass Fancy extends Widget"),
      "def make: Any = new base.Widget",
      "def other: Int = 3"
    ) { result =>
      assertEquals(result.structures.concretes.find(_.name == "base.Widget").map(_.unconstructed), Some(false))
    }
  }

  test("a field read off a value named like it on the other side of a clash is read there") {
    eachSide(
      base("case class Graph(graph: Int)"),
      "def other: Int = 3",
      "def read: Int = { val graph = base.Graph(1); graph.graph }"
    ) { result =>
      assertEquals(result.structures.stutters.find(_.owner == "base.Graph").map(_.accesses), Some(1))
    }
  }

  test("a string on one side of a clash naming a member the other side's type lacks is a stale name") {
    eachSide(
      base("class Tools { def original: Int = 1 }"),
      "val note: String = \"Tools.removed\"",
      "def other: Int = 3"
    ) { result =>
      assertEquals(result.structures.mentions.map(_.text), Vector("Tools.removed"))
    }
  }

  test("a string naming a member that a type of the same name on the other side of a clash has is not stale") {
    eachSide(
      base("class Tools { def original: Int = 1 }"),
      "val note: String = \"Tools.removed\"",
      "def other: Int = 3",
      "class Tools { def removed: Int = 2 }"
    ) { result =>
      assertEquals(result.structures.mentions, Vector.empty)
    }
  }

  test("a given named on the other side of a clash is named") {
    eachSide(
      base("trait Show[A]\nobject Shows { given intShow: Show[Int] = new Show[Int] {} }"),
      "def other: Int = 3",
      "def shown: Any = base.Shows.intShow"
    ) { result =>
      assertEquals(result.members.givens.find(_.name == "base.Shows.intShow").map(_.named), Some(true))
    }
  }

  test("a class mirrored on the other side of a clash is derived") {
    eachSide(
      base("case class Cfg(a: Int, b: String)"),
      "def other: Int = 3",
      "def mirror: Any = summon[scala.deriving.Mirror.Of[base.Cfg]]"
    ) { result =>
      assertEquals(result.algebra.products.find(_.name == "base.Cfg").map(_.origin), Some(Algebra.Origin.Derived))
    }
  }

  test("a dictionary parameter given different instances on each side of a clash is not fixed") {
    eachSide(
      base(
        "trait Codec[F[_]]\nobject Codecs {\n  given listCodec: Codec[List] = new Codec[List] {}\n  given optCodec: Codec[Option] = new Codec[Option] {}\n}\n" +
          "object Util { def encode[F[_]](x: Int)(using c: Codec[F]): Int = x }"
      ),
      "def one: Int = base.Util.encode[List](1)(using base.Codecs.listCodec)",
      "def two: Int = base.Util.encode[Option](2)(using base.Codecs.optCodec)"
    )(result => assertEquals(Dictionaries.findings(result.dictionaries, result.graph.nodes), Vector.empty))
  }

  test("a call that an implementation on the other side of a clash makes effectful is not pure") {
    eachSide(
      base("trait Task { def run(): Int }\nobject Util { def exec(t: Task): Int = t.run() }"),
      "class A extends base.Task { def run(): Int = 1 }\ndef drop(t: base.Task): Unit = { base.Util.exec(t); () }",
      "class B extends base.Task { def run(): Int = { println(\"x\"); 2 } }"
    )(result => assertEquals(result.purity.discards, Vector.empty))
  }

  test("a bound that a member on one side of a clash matches is used") {
    eachSide(
      base("object Util { def one: Int = 1 }"),
      "def pairs(xs: Vector[Int], ys: Vector[Int]): Vector[Vector[Int]] = xs.map(x => ys.map(y => y + x))",
      "def two: Int = 2",
      bounded = Vector(Complexity.Bounded("a.b.pairs.ys", 4, "four at most"))
    )(result => assertEquals(result.unusedBounds, Vector.empty))
  }

  private val Twinned = base(
    "object Util {\n  def scale(x: Int, factor: Int): Int = x * factor\n  def compute(x: Int): Int = x + 1\n" +
      "  def spare: Int = 1\n  def untouched: Int = 2\n}"
  )

  private val Stats = "def stats(xs: List[Int]): Int = {\n  val ys = xs.map(_ * 2).filter(_ > 3)\n" +
    "  val zs = ys.zip(ys.drop(1)).map((a, b) => a * b + a)\n  zs.sum + ys.size + xs.length + zs.max\n}"

  private def weight(side: String, heavy: String): String =
    if (side == heavy) (1 to 12).map(index => s"class Pad$index").mkString("\n", "\n", "") else ""

  private def twinned(heavy: String, twins: Set[String], own: String = ""): Extraction.Result =
    Scratch.using("bonsai-") { directory =>
      reads(
        manifest(
          directory,
          "",
          Twinned,
          s"def one: Int = base.Util.scale(1, 5)\ndef drop(): Unit = { base.Util.compute(1); () }\n$Stats" +
            weight("objects", heavy),
          s"def two: Int = base.Util.scale(2, 5)\ndef drop(): Unit = { base.Util.compute(2); () }\n$Stats" +
            weight("packaged", heavy),
          "class Lonely { def lonely: Int = 3 }",
          twins,
          own
        )
      )
    }

  private def facts(result: Extraction.Result): Vector[String] =
    ((smells(result) ++ Members.findings(result.members, result.graph.nodes, result.reach))
      .map(finding => s"${finding.rule.id} ${finding.detail}").filterNot(_.contains("twin")) ++
      result.clones.map(group => s"clones ${group.fragments.map(_.node).sorted}")).sorted

  private def sidesWith(twins: Set[String], own: String = "")(check: (String, Extraction.Result) => Unit): Unit =
    Seq("objects", "packaged").foreach(heavy => check(heavy, twinned(heavy, twins, own)))

  test("a platform twin read in the run of its peer has its own classes and counts the shared ones once") {
    Seq("objects", "packaged").foreach { side =>
      sidesWith(Set(side)) { (heavy, result) =>
        assert(result.graph.nodes.contains(s"twin.${side}Js"), result.graph.nodes.keySet.toString)
        assertEquals(facts(result), facts(twinned(heavy, Set.empty)))
        assertFinding(smells(result), "constant-argument", "Util.scale(x, factor).factor is always 5 across 2")
        assertFinding(smells(result), "ignored-result", "its Int result is discarded at all 2 calls")
        assert(result.clones.nonEmpty)
      }
    }
  }

  test("a source a platform twin replaces leaves the findings of its peer's version in a run apart") {
    Scratch.using("bonsai-") { directory =>
      val result = reads(
        manifest(
          directory,
          "",
          Twinned,
          s"def one: Int = base.Util.scale(1, 5)${weight("objects", "objects")}",
          "def drops(): Unit = { new Lonely().produce(); new Lonely().produce(); () }",
          "class Lonely { def produce(): Int = 3 }",
          Set("packaged"),
          replacing = "class Lonely { def produce(): Int = 4; def extra: Int = 5 }"
        )
      )
      assertFinding(smells(result), "ignored-result", "Lonely.produce", "discarded at all 2 calls")
    }
  }

  test("a platform twin on each side of a clash is read in the run of its peer") {
    sidesWith(Set("objects", "packaged")) { (heavy, result) =>
      assert(Seq("objectsJs", "packagedJs").forall(side => result.graph.nodes.contains(s"twin.$side")))
      assertEquals(facts(result), facts(twinned(heavy, Set.empty)))
      assertFinding(smells(result), "constant-argument", "Util.scale(x, factor).factor is always 5 across 2")
    }
  }

  test("a call from a platform twin into a module in another run is a use of its callee") {
    Seq(Set("objects"), Set("packaged"), Set("objects", "packaged")).foreach { twins =>
      sidesWith(twins, "def viaTwin: Int = base.Util.spare") { (_, result) =>
        val unused = Members.findings(result.members, result.graph.nodes, result.reach)
          .filter(_.rule == Finding.Rule.UnusedMember).map(_.detail)
        assert(unused.contains("Util.untouched is never referenced"), unused.toString)
        assert(!unused.contains("Util.spare is never referenced"), unused.toString)
      }
    }
  }

  test("a platform twin without a peer is read in a run of its own") {
    Seq("objects", "packaged").foreach { padded =>
      Scratch.using("bonsai-") { directory =>
        val whole  = manifest(directory, padded, Twinned, "def a: Int = 1", "def b: Int = 2", "")
        val result = reads(whole.copy(platforms = Set("packaged")))
        assert(result.graph.nodes.contains("a.b.Side"), result.graph.nodes.keySet.toString)
      }
    }
  }

  private def unusedMembers(result: Extraction.Result): Vector[String] =
    Members.findings(result.members, result.graph.nodes, result.reach).filter(_.rule == Finding.Rule.UnusedMember)
      .map(_.detail)

  private def reaches(result: Extraction.Result, from: String, to: String): Boolean = {
    val graph = result.graph
    graph.edges.exists((source, targets) => source.startsWith(from) && targets.exists(_.startsWith(to))) ||
    graph.bodies.exists((use, targets) => use.node.startsWith(from) && targets.exists(_.startsWith(to)))
  }

  test("a platform twin without a peer keeps the calls it makes into the other runs") {
    Scratch.using("bonsai-") { directory =>
      val whole  = manifest(directory, "objects", Twinned, "def a: Int = 1", "def viaTwin: Int = base.Util.spare", "")
      val result = reads(whole.copy(platforms = Set("packaged")))
      assert(reaches(result, "a.b.Side", "base.Util"), result.graph.bodies.toString)
      assert(result.reach.live("base.Util"))
      val unused = unusedMembers(result)
      assert(!unused.contains("Util.spare is never referenced"), unused.toString)
      assert(unused.contains("Util.untouched is never referenced"), unused.toString)
    }
  }

  private def sole(padded: String)(check: Extraction.Result => Unit): Unit = Scratch.using("bonsai-") { directory =>
    def padding(module: String) =
      Option.when(module == padded)("Padding.scala" -> s"package padding.$module\nclass One\nclass Two\nclass Three")
    val jvm = compile(
      directory.resolve("core"),
      Seq(
        "B.scala"    -> "package a\nobject b",
        "Main.scala" ->
          ("package core\nobject Main { def main(args: Array[String]): Unit = { Helper.used(); () } }\n" +
            "object Helper { def used(): Int = 1\n  def spare(): Int = 2 }")
      ) ++ padding("core"),
      Vector.empty
    )
    val twin = compile(
      directory.resolve("coreJs"),
      Seq(
        "Side.scala" ->
          ("package a.b\nobject Side { def main(args: Array[String]): Unit = { Tool.go(); () } }\n" +
            "object Tool { def go(): Int = 1\n  def idle(): Int = 2 }")
      ) ++ padding("coreJs"),
      Vector.empty
    )
    val whole = Manifest(
      Vector(Output("coreJs", Output.Scope.Main, twin), Output("core", Output.Scope.Main, jvm)),
      scoped(Map("coreJs" -> classpath, "core" -> classpath)),
      Set.empty,
      platforms = Set("coreJs"),
      jdk = targetJdk
    )
    check(reads(whole))
  }

  private def soleChecks(result: Extraction.Result): Unit = {
    assert(reaches(result, "core.Main", "core.Helper"), result.graph.bodies.toString)
    assert(reaches(result, "a.b.Side", "a.b.Tool"), result.graph.bodies.toString)
    assert(result.reach.live("core.Helper") && result.reach.live("a.b.Tool"))
    assertEquals(
      unusedMembers(result).sorted,
      Vector("Helper.spare is never referenced", "Tool.idle is never referenced")
    )
  }

  test("a run of platform twins alone keeps the edges of every run") {
    sole("core")(soleChecks(_))
    sole("coreJs")(soleChecks(_))
  }

  private val Pairs =
    "def pairs(xs: Vector[Int], ys: Vector[Int]): Vector[Vector[Int]] = xs.map(x => ys.map(y => y + x))"

  private def boundsOf(bounded: Vector[Complexity.Bounded], own: String): Vector[Complexity.Bounded] =
    Scratch.using("bonsai-") { directory =>
      val shared = Seq("Work.scala" -> s"package app\nobject Work {\n$Pairs\n}")
      val jvm    = compile(directory.resolve("core"), shared, Vector.empty)
      val twin   = compile(
        directory.resolve("coreJs"),
        shared :+ ("Js.scala" -> s"package twin\nobject Js {\n$own\n}"),
        Vector.empty
      )
      val whole = Manifest(
        Vector(Output("coreJs", Output.Scope.Main, twin), Output("core", Output.Scope.Main, jvm)),
        scoped(Map("coreJs" -> classpath, "core" -> classpath)),
        Set.empty,
        platforms = Set("coreJs"),
        jdk = targetJdk
      )
      val result = Extraction.read(whole, bounded = bounded).fold(errors => fail(errors.mkString("\n")), identity)
      assertEquals(result.problems, Vector.empty)
      result.unusedBounds
    }

  private val OnJvm   = Complexity.Bounded("app.Work.pairs.ys", 4, "four at most")
  private val OnTwin  = Complexity.Bounded("twin.Js.rows.zs", 4, "four at most")
  private val Nowhere = Complexity.Bounded("app.Nowhere.rows", 4, "four at most")
  private val Clashed = Complexity.Bounded("a.b.pairs.ys", 4, "four at most")
  private val Rows    = "def rows(zs: Vector[Int]): Vector[Vector[Int]] = zs.map(z => zs.map(_ + z))"

  test("a bound that only the JVM modules match is used in a build with a platform twin") {
    assertEquals(boundsOf(Vector(OnJvm), Rows), Vector.empty)
  }

  test("a bound that only a platform twin matches is used") {
    assertEquals(boundsOf(Vector(OnTwin), Rows), Vector.empty)
  }

  test("a bound that nothing matches is reported once in a build with a platform twin") {
    assertEquals(boundsOf(Vector(OnJvm, Nowhere, OnTwin), Rows), Vector(Nowhere))
  }

  test("a bound that only the JVM side of a clash matches is used in a build with a platform twin") {
    Seq("objects", "packaged").foreach { twin =>
      Scratch.using("bonsai-") { directory =>
        val whole  = manifest(directory, "objects", Twinned, Pairs, "def two: Int = 2", "", twins = Set(twin))
        val result = Extraction.read(whole, bounded = Vector(Clashed))
          .fold(errors => fail(errors.mkString("\n")), identity)
        assertEquals(result.unusedBounds, Vector.empty)
      }
    }
  }

  private def barred(redeclared: Boolean): Unit =
    Scratch.using("bonsai-") { directory =>
      val padding = "Padding.scala" -> "package padding\nclass One\nclass Two\nclass Three"
      val jvm     = compile(
        directory.resolve("core"),
        Seq("Foo.scala" -> "package a\nobject Foo { def x: Int = 1 }", padding),
        Vector.empty
      )
      val peer = compile(
        directory.resolve("coreJs"),
        Seq("Foo.scala" -> "package a\nobject Foo { def x: Int = 2 }"),
        Vector.empty
      )
      val lone = compile(
        directory.resolve("appJs"),
        Seq(
          "Bar.scala"    -> "package a.Foo\nclass Bar { def produce: Int = 3 }",
          "Caller.scala" ->
            "package other\nobject Caller { def go(): Unit = { new a.Foo.Bar().produce; new a.Foo.Bar().produce; () } }"
        ),
        Vector.empty
      )
      val whole = Manifest(
        Vector(Output("appJs", Output.Scope.Main, lone), Output("core", Output.Scope.Main, jvm)) ++
          Option.when(redeclared)(Output("coreJs", Output.Scope.Main, peer)),
        scoped(Map("appJs" -> classpath, "core" -> classpath) ++ Option.when(redeclared)("coreJs" -> classpath)),
        Set.empty,
        platforms = Set("appJs") ++ Option.when(redeclared)("coreJs"),
        jdk = targetJdk
      )
      assertFinding(smells(reads(whole)), "ignored-result", "Bar.produce", "discarded at all 2 calls")
    }

  test("a type of a twin in a run apart is analysed") {
    barred(redeclared = false)
  }

  test("a type a twin in one run redeclares leaves the findings about a type of another twin in a run apart") {
    barred(redeclared = true)
  }

  private def apart(
      core: Seq[(String, String)],
      lib: Seq[(String, String)],
      libraries: Set[String]
  )(check: (Extraction.Result, Extraction.Result) => Unit): Unit = Seq(true, false).foreach { coreFirst =>
    Scratch.using("bonsai-") { directory =>
      val coreClasses = compile(directory.resolve("core"), core, Vector.empty)
      val libClasses  = compile(directory.resolve("lib"), lib, Vector(coreClasses))
      val outputs     = Vector(
        Output("core", Output.Scope.Main, coreClasses),
        Output("lib", Output.Scope.Main, libClasses)
      )
      val whole = Manifest(
        outputs,
        scoped(Map("core" -> classpath, "lib" -> (classpath :+ coreClasses))),
        libraries,
        jdk = targetJdk
      )
      val settings =
        Extraction.Settings(Vector.empty, Set.empty, Vector.empty, Vector.empty, Vector.empty, Parallel.sequential)
      def read(division: Clashes.Division) = Extraction.readDivided(whole, settings, division)
        .fold(errors => fail(errors.mkString("\n")), identity)
      val together = read((_, _, _) => Right(Clashes.Divided(Vector(outputs.toSet), Vector.empty)))
      val groups   = if (coreFirst) outputs else outputs.reverse
      val divided  = read((_, _, _) => Right(Clashes.Divided(groups.map(Set(_)), Vector.empty)))
      assertEquals(divided.problems, Vector.empty)
      check(together, divided)
    }
  }

  test("a base class of an exported class in another run keeps its members exempt") {
    apart(
      Seq("Base.scala" -> "package core\nclass Base { def hook: Int = 1 }"),
      Seq("Impl.scala" -> "package lib\nclass Impl extends core.Base"),
      Set("lib")
    ) { (together, divided) =>
      def unused(result: Extraction.Result) =
        Members.findings(result.members, result.graph.nodes, result.reach).filter(_.rule == Finding.Rule.UnusedMember)
          .map(_.detail)
      assertEquals(unused(together), Vector.empty)
      assertEquals(unused(divided), Vector.empty)
    }
  }

  test("a platform twin holding one side of a clash and its peer the other fails naming them") {
    Scratch.using("bonsai-") { directory =>
      val shared  = "Peer.scala" -> "package twinned\nclass Peer { def one: Int = 1 }"
      val objects = compile(directory.resolve("objects"), Seq("B.scala" -> "package a\nobject b", shared), Vector.empty)
      val twin    =
        compile(directory.resolve("twin"), Seq("Other.scala" -> "package a.b\nobject Other", shared), Vector.empty)
      val packaged = compile(
        directory.resolve("packaged"),
        Seq(
          "Side.scala"    -> "package a.b\nobject Side",
          "Padding.scala" -> "package padding\nclass One\nclass Two\nclass Three\nclass Four"
        ),
        Vector.empty
      )
      val whole = Manifest(
        Vector(
          Output("objectsJs", Output.Scope.Main, twin),
          Output("objects", Output.Scope.Main, objects),
          Output("packaged", Output.Scope.Main, packaged)
        ),
        scoped(Map("objectsJs" -> classpath, "objects" -> classpath, "packaged" -> classpath)),
        Set.empty,
        platforms = Set("objectsJs"),
        jdk = targetJdk
      )
      val errors = Extraction.read(whole).fold(identity, _ => fail("read"))
      assertEquals(errors.size, 1)
      assert(errors.head.startsWith("a.b is a class in module objects and a package in module objectsJs"), errors.head)
      assert(errors.head.endsWith("and objects, objectsJs need both"), errors.head)
    }
  }

  private def library(directory: Path, name: String, version: String, source: String): Path =
    archive(
      compile(directory.resolve(s"$name-$version"), Seq(s"$name.scala" -> source), Vector.empty),
      directory.resolve(s"$name-$version.jar"),
      name,
      version
    )

  private val Scale = "object Util { def scale(x: Int, factor: Int): Int = x * factor }"
  private val Users = "package user\nobject Users { def one: Int = base.Util.scale(lib.implicits.one, 5) }"
  private val Tools = "package tool\nobject Tools { def two: Int = base.Util.scale(new lib.implicits.Tool().two, 5) }"

  private def libraries(
      directory: Path,
      padded: String,
      onObjects: Path,
      onPackage: Path,
      tools: String = Tools,
      twinJars: Option[(Path, Path)] = None,
      userCode: String = Users,
      core: String = Scale
  ): Manifest = {
    def padding(module: String) =
      Option.when(module == padded)("Padding.scala" -> s"package padding.$module\nclass One\nclass Two\nclass Three")
    val shared = compile(
      directory.resolve("base"),
      base(core),
      Vector.empty
    )
    val userSources = Seq("Users.scala" -> userCode) ++ padding("users")
    val toolSources = Seq("Tools.scala" -> tools) ++ padding("tools")
    val users       = compile(directory.resolve("users"), userSources, Vector(shared, onObjects))
    val using       = compile(directory.resolve("tools"), toolSources, Vector(shared, onPackage))
    val twinned     = twinJars.toVector.flatMap { (objectsJar, packageJar) =>
      Vector(
        ("usersNative", userSources, objectsJar),
        ("toolsJs", toolSources, packageJar)
      ).map { (module, sources, jar) =>
        val own = s"Own.scala" -> s"package twin\nobject $module { def go: Int = 3 }"
        (
          Output(module, Output.Scope.Main, compile(directory.resolve(module), sources :+ own, Vector(shared, jar))),
          jar
        )
      }
    }
    Manifest(
      twinned.map(_._1) ++ Vector(
        Output("base", Output.Scope.Main, shared),
        Output("users", Output.Scope.Main, users),
        Output("tools", Output.Scope.Main, using)
      ),
      scoped(Map(
        "base"  -> classpath,
        "users" -> (classpath ++ Vector(shared, onObjects)),
        "tools" -> (classpath ++ Vector(shared, onPackage))
      ) ++ twinned.map((output, jar) => output.module -> (classpath ++ Vector(shared, jar)))),
      Set.empty,
      platforms = twinned.map(_._1.module).toSet,
      jdk = targetJdk
    )
  }

  private val Cache = "trait Cache { def get: Int }\n" + Scale

  private def memo(owner: String): String =
    s"class $owner extends base.Cache {\n  def get: Int = 1\n  $Stats\n}"

  private def alongside(whole: Manifest, directory: Path, twinned: Boolean): Manifest = {
    val shared  = whole.outputs.find(_.module == "base").map(_.path).toVector
    val visible = classpath ++ shared
    val sources = Seq("Safe.scala" -> s"package safe\n${memo("Safe")}")
    val modules = Vector("safe" -> sources) ++
      Option.when(twinned)("safeJs" -> (sources :+ ("Own.scala" -> "package twin\nobject safeJs")))
    val outputs =
      modules.map((module, code) => Output(module, Output.Scope.Main, compile(directory.resolve(module), code, shared)))
    whole.copy(
      outputs = whole.outputs ++ outputs,
      classpath = whole.classpath ++ scoped(outputs.map(_.module -> visible).toMap),
      platforms = whole.platforms ++ Option.when(twinned)("safeJs")
    )
  }

  private def exported(whole: Manifest, directory: Path): Manifest = {
    val classes = whole.outputs.find(_.module == "base").map(_.path)
    val jar     = archive(classes.get, directory.resolve("base.jar"), "base", "1.0")
    whole.copy(classpath =
      whole.classpath.view.mapValues(_.map(entry => if (classes.contains(entry)) jar else entry)).toMap
    )
  }

  test("copies in modules read in different runs are one finding, whether or not their modules have twins") {
    val flags   = Seq(false, true)
    val layouts = for {
      users <- flags
      safe  <- flags
      jar   <- flags
    } yield (users, safe, jar)
    layouts.foreach { (users, safe, jar) =>
      Scratch.using("bonsai-") { directory =>
        val pair = Option.when(users)(
          (
            library(directory, "objectsNative", "1.0", objectLibrary),
            library(directory, "packagedJs", "1.0", packageLibrary)
          )
        )
        val built = alongside(
          libraries(
            directory,
            "tools",
            library(directory, "objects", "1.0", objectLibrary),
            library(directory, "packaged", "1.0", packageLibrary),
            twinJars = pair,
            userCode = s"$Users\n${memo("Memo")}",
            core = Cache
          ),
          directory,
          safe
        )
        val whole = if (jar) exported(built, directory) else built
        assertEquals(groups(whole, Set("lib.implicits")), 2)
        val result = reads(whole)
        val found  = Clones.findings(result.clones, result.graph.nodes, Intended(), result.graph.edges)
        assert(
          found.exists(finding => finding.rule == Finding.Rule.DuplicateCode && finding.detail.contains("Safe.scala")),
          s"users twin $users, safe twin $safe, jar $jar: ${result.clones}\n${found.mkString("\n")}"
        )
      }
    }
  }

  test("a call into a module of another run, found in its jar, is a use of its callee") {
    Seq("users", "tools").foreach { padded =>
      Scratch.using("bonsai-") { directory =>
        val whole = exported(
          libraries(
            directory,
            padded,
            library(directory, "objects", "1.0", objectLibrary),
            library(directory, "packaged", "1.0", packageLibrary)
          ),
          directory
        )
        assertEquals(groups(whole, Set("lib.implicits")), 2)
        val result = reads(whole)
        assertFinding(smells(result), "constant-argument", "Util.scale(x, factor).factor is always 5 across 2")
      }
    }
  }

  private val objectLibrary  = "package lib\nobject implicits { def one: Int = 1 }"
  private val packageLibrary = "package lib.implicits\nclass Tool { def two: Int = 2 }"

  private def reads(whole: Manifest): Extraction.Result = {
    val result = Extraction.read(whole).fold(errors => fail(errors.mkString("\n")), identity)
    assertEquals(result.problems, Vector.empty)
    assertEquals(result.notes, Vector.empty)
    result
  }

  private def fails(whole: Manifest): String = {
    val errors = Extraction.read(whole).fold(identity, result => fail(s"read: ${result.notes}"))
    assertEquals(errors.size, 1)
    errors.head
  }

  private def groups(whole: Manifest, known: Set[String] = Set.empty): Int =
    Clashes.runs(whole, whole.outputs.map(output => output -> Outputs.files(output.path)), known)
      .fold(errors => fail(errors.mkString("\n")), _.runs.size)

  private def single(
      module: String,
      directory: Path,
      sources: Seq[(String, String)],
      jars: Vector[Path] = Vector.empty
  ): Manifest = Manifest(
    Vector(Output(module, Output.Scope.Main, compile(directory.resolve(module), sources, jars))),
    scoped(Map(module -> (classpath ++ jars))),
    Set.empty,
    jdk = targetJdk
  )

  private def withJars(whole: Manifest, module: String, jars: Path*): Manifest =
    whole.copy(classpath =
      whole.classpath.updated(module -> Output.Scope.Main, whole.classpath(module -> Output.Scope.Main) ++ jars)
    )

  test("an object in one library and a package of its name in another, each on an unrelated module, are read apart") {
    Seq("users", "tools").foreach { padded =>
      Scratch.using("bonsai-") { directory =>
        val whole = libraries(
          directory,
          padded,
          library(directory, "objects", "1.0", objectLibrary),
          library(directory, "packaged", "1.0", packageLibrary)
        )
        val result = reads(whole)
        assert(Seq("user.Users", "tool.Tools").forall(result.graph.nodes.contains), result.graph.nodes.keySet.toString)
        assertFinding(smells(result), "constant-argument", "Util.scale(x, factor).factor is always 5 across 2")
      }
    }
  }

  test("platform twins using the libraries of each side of a clash are read with their peers") {
    Seq("users", "tools").foreach { padded =>
      Scratch.using("bonsai-") { directory =>
        val whole = libraries(
          directory,
          padded,
          library(directory, "objects", "1.0", objectLibrary),
          library(directory, "packaged", "1.0", packageLibrary),
          twinJars = Some(
            (
              library(directory, "objectsNative", "1.0", objectLibrary),
              library(directory, "packagedJs", "1.0", packageLibrary)
            )
          )
        )
        val result = reads(whole)
        assert(
          Seq("user.Users", "tool.Tools", "twin.usersNative", "twin.toolsJs").forall(result.graph.nodes.contains),
          result.graph.nodes.keySet.toString
        )
        assertFinding(smells(result), "constant-argument", "Util.scale(x, factor).factor is always 5 across 2")
      }
    }
  }

  test("unrelated modules on two versions of one artifact are read apart") {
    Scratch.using("bonsai-") { directory =>
      val whole = libraries(
        directory,
        "tools",
        library(directory, "lib", "3.0", objectLibrary),
        library(directory, "lib", "2.0", packageLibrary),
        "package tool\nobject Tools { def two: Int = base.Util.scale(2, 5) }"
      )
      val result = reads(whole)
      assert(Seq("user.Users", "tool.Tools").forall(result.graph.nodes.contains), result.graph.nodes.keySet.toString)
      assertEquals(groups(whole, Set("lib.implicits")), 2)
    }
  }

  test("two versions of one artifact on a module are read as the first alone") {
    Scratch.using("bonsai-") { directory =>
      val newer = library(directory, "lib", "3.0", objectLibrary)
      val older = library(directory, "lib", "2.0", packageLibrary)
      val whole = withJars(
        single(
          "users",
          directory,
          Seq("Users.scala" -> "package user\nobject Users { def one: Int = lib.implicits.one }"),
          Vector(newer)
        ),
        "users",
        older
      )
      assert(reads(whole).graph.nodes.contains("user.Users"))
    }
  }

  private def versioned(directory: Path, module: String, scope: Output.Scope, code: String, jar: Path): Output =
    Output(
      module,
      scope,
      compile(directory.resolve(s"$module-${scope.id}"), Seq(s"$module.scala" -> code), Vector(jar))
    )

  test("modules on two versions of a library with no clash are read apart, each against its own") {
    Scratch.using("bonsai-") { directory =>
      val one   = library(directory, "dep", "1.0", "package dep\nobject Dep { def old: Int = 1 }")
      val two   = library(directory, "dep", "2.0", "package dep\nobject Dep { def fresh: Int = 2 }")
      val first =
        versioned(directory, "first", Output.Scope.Main, "package a\nobject First { def go: Int = dep.Dep.old }", one)
      val other =
        versioned(directory, "other", Output.Scope.Main, "package b\nobject Other { def go: Int = dep.Dep.fresh }", two)
      val whole = Manifest(
        Vector(first, other),
        scoped(Map("first" -> (classpath :+ one), "other" -> (classpath :+ two))),
        Set.empty,
        jdk = targetJdk
      )
      assertEquals(groups(whole), 2)
      val result = reads(whole)
      assert(Seq("a.First", "b.Other").forall(result.graph.nodes.contains), result.graph.nodes.keySet.toString)
    }
  }

  test("the main and test scopes of a module resolving different versions of a library are read apart") {
    Scratch.using("bonsai-") { directory =>
      val one  = library(directory, "dep", "1.0", "package dep\nobject Dep { def old: Int = 1 }")
      val two  = library(directory, "dep", "2.0", "package dep\nobject Dep { def fresh: Int = 2 }")
      val main =
        versioned(directory, "m", Output.Scope.Main, "package a\nobject Main { def go: Int = dep.Dep.old }", one)
      val test =
        versioned(directory, "m", Output.Scope.Test, "package b\nobject Spec { def go: Int = dep.Dep.fresh }", two)
      val whole = Manifest(
        Vector(main, test),
        Map(("m", Output.Scope.Main) -> (classpath :+ one), ("m", Output.Scope.Test) -> (classpath :+ two)),
        Set.empty,
        jdk = targetJdk
      )
      assertEquals(groups(whole), 2)
      val result = reads(whole)
      assert(Seq("a.Main", "b.Spec").forall(result.graph.nodes.contains), result.graph.nodes.keySet.toString)
    }
  }

  test("modules on one version of a library are read in one run") {
    Scratch.using("bonsai-") { directory =>
      val one   = library(directory, "dep", "1.0", "package dep\nobject Dep { def old: Int = 1 }")
      val first =
        versioned(directory, "first", Output.Scope.Main, "package a\nobject First { def go: Int = dep.Dep.old }", one)
      val other =
        versioned(directory, "other", Output.Scope.Main, "package b\nobject Other { def go: Int = dep.Dep.old }", one)
      val whole = Manifest(
        Vector(first, other),
        scoped(Map("first" -> (classpath :+ one), "other" -> (classpath :+ one))),
        Set.empty,
        jdk = targetJdk
      )
      assertEquals(groups(whole), 1)
    }
  }

  test("platform twins of a class and of a package that share their sources and no read holds both are read together") {
    Scratch.using("bonsai-") { directory =>
      val shared = Seq("Peer.scala" -> "package porcupine\nobject Peer { def go: Int = 1 }")
      val native = compile(
        directory.resolve("coreNative"),
        shared :+ ("Binding.scala" -> "package porcupine\nobject sqlite3 { def open: Int = 2 }"),
        Vector.empty
      )
      val js = compile(
        directory.resolve("coreJs"),
        shared :+ ("Facade.scala" -> "package porcupine.sqlite3\nclass Facade { def open: Int = 3 }"),
        Vector.empty
      )
      val core  = compile(directory.resolve("core"), shared, Vector.empty)
      val whole = Manifest(
        Vector(
          Output("core", Output.Scope.Main, core),
          Output("coreJs", Output.Scope.Main, js),
          Output("coreNative", Output.Scope.Main, native)
        ),
        scoped(Map("core" -> classpath, "coreJs" -> classpath, "coreNative" -> classpath)),
        Set.empty,
        platforms = Set("coreJs", "coreNative"),
        jdk = targetJdk
      )
      assertEquals(groups(whole), 1)
    }
  }

  test("a platform twin on another version of a library than its peers reads against its own") {
    Scratch.using("bonsai-") { directory =>
      val objects  = library(directory, "lib", "3.0", objectLibrary)
      val packaged = library(directory, "lib", "2.0", packageLibrary)
      val peer     = "Peer.scala" -> "package peer\nobject Peer { def go: Int = 1 }"
      val app      = compile(
        directory.resolve("app"),
        Seq(peer, "App.scala" -> "package user\nobject App { def one: Int = lib.implicits.one }"),
        Vector(objects)
      )
      val appJs = compile(
        directory.resolve("appJs"),
        Seq(peer, "Js.scala" -> "package user\nobject Js { def two: Int = new lib.implicits.Tool().two }"),
        Vector(packaged)
      )
      val other = versioned(
        directory,
        "other",
        Output.Scope.Main,
        "package o\nobject Other { def one: Int = lib.implicits.one }",
        objects
      )
      val whole = Manifest(
        Vector(Output("app", Output.Scope.Main, app), Output("appJs", Output.Scope.Main, appJs), other),
        scoped(Map(
          "app"   -> (classpath :+ objects),
          "appJs" -> (classpath :+ packaged),
          "other" -> (classpath :+ objects)
        )),
        Set.empty,
        platforms = Set("appJs"),
        jdk = targetJdk
      )
      val result = reads(whole)
      assert(Seq("user.App", "o.Other").forall(result.graph.nodes.contains), result.graph.nodes.keySet.toString)
    }
  }

  test("a module whose classpath holds an object and a package of one name in two libraries fails naming them") {
    Scratch.using("bonsai-") { directory =>
      val objects  = library(directory, "objects", "1.0", objectLibrary)
      val packaged = library(directory, "packaged", "1.0", packageLibrary)
      val whole    = libraries(directory, "tools", objects, packaged)
      val both     = whole.classpath("users" -> Output.Scope.Main) ++
        whole.classpath("tools" -> Output.Scope.Main).takeRight(1)
      val error = fails(whole.copy(classpath = whole.classpath.updated("users" -> Output.Scope.Main, both)))
      assert(
        error.startsWith(s"lib.implicits is a class in library $objects and a package in library $packaged"),
        error
      )
      assert(error.endsWith("users need both"), error)
    }
  }

  test("two jars of one file name are told apart by their paths") {
    Scratch.using("bonsai-") { directory =>
      def jar(folder: String, title: String, source: String): Path = {
        val classes = compile(directory.resolve(title), Seq(s"$title.scala" -> source), Vector.empty)
        archive(classes, Files.createDirectories(directory.resolve(folder)).resolve("lib.jar"), title, "1.0")
      }
      val objects  = jar("one", "objects", objectLibrary)
      val packaged = jar("two", "packaged", packageLibrary)
      val whole    = libraries(directory, "tools", objects, packaged)
      val both     = whole.classpath("users" -> Output.Scope.Main) ++
        whole.classpath("tools" -> Output.Scope.Main).takeRight(1)
      val error = fails(whole.copy(classpath = whole.classpath.updated("users" -> Output.Scope.Main, both)))
      assert(
        error.startsWith(s"lib.implicits is a class in library $objects and a package in library $packaged"),
        error
      )
    }
  }

  test("a class in the default package does not clash with a package of its name") {
    Scratch.using("bonsai-") { directory =>
      val whole = single("m", directory, Seq("A.scala" -> "class a", "C.scala" -> "package a.b\nclass C"))
      assert(reads(whole).graph.nodes.contains("a.b.C"))
    }
  }

  test("a class in the default package of one module and a package of its name in another are read together") {
    Scratch.using("bonsai-") { directory =>
      val root  = compile(directory.resolve("root"), Seq("A.scala" -> "class a"), Vector.empty)
      val deep  = compile(directory.resolve("deep"), Seq("C.scala" -> "package a.b\nclass C"), Vector.empty)
      val whole = Manifest(
        Vector(Output("root", Output.Scope.Main, root), Output("deep", Output.Scope.Main, deep)),
        scoped(Map("root" -> classpath, "deep" -> classpath)),
        Set.empty,
        jdk = targetJdk
      )
      assertEquals(groups(whole), 1)
    }
  }

  test("a jar with a class in the default package and a package of its name is no clash") {
    Scratch.using("bonsai-") { directory =>
      val classes = compile(
        directory.resolve("obf"),
        Seq("A.scala" -> "class a", "C.scala" -> "package a.b\nclass C"),
        Vector.empty
      )
      val whole = withJars(
        single("users", directory, Seq("Users.scala" -> "package user\nobject Users")),
        "users",
        archive(classes, directory.resolve("obf.jar"), "obf", "1.0")
      )
      assert(reads(whole).graph.nodes.contains("user.Users"))
    }
  }

  private def resourced(directory: Path, name: String, resource: String): Path = {
    val classes = compile(directory.resolve(name), Seq("MyTest.scala" -> "package com.foo\nclass MyTest"), Vector.empty)
    write(classes, Seq(resource -> "{}"))
    classes
  }

  test("a resource directory without subdirectories named like a class of its module is no package") {
    Scratch.using("bonsai-") { directory =>
      val classes = resourced(directory, "m", "com/foo/MyTest/input.json")
      val whole   = Manifest(
        Vector(Output("m", Output.Scope.Main, classes)),
        scoped(Map("m" -> classpath)),
        Set.empty,
        jdk = targetJdk
      )
      assert(reads(whole).graph.nodes.contains("com.foo.MyTest"))
    }
  }

  test("a resource directory with subdirectories named like a class of its module fails naming the clash") {
    Scratch.using("bonsai-") { directory =>
      val classes = resourced(directory, "m", "com/foo/MyTest/data/input.json")
      val whole   = Manifest(
        Vector(Output("m", Output.Scope.Main, classes)),
        scoped(Map("m" -> classpath)),
        Set.empty,
        jdk = targetJdk
      )
      val error = fails(whole)
      assert(error.startsWith("com.foo.MyTest is a class in module m and a package in module m"), error)
    }
  }

  test("a resource directory with subdirectories named like a class of another module is read apart") {
    Scratch.using("bonsai-") { directory =>
      val classes = compile(
        directory.resolve("classes"),
        Seq("MyTest.scala" -> "package com.foo\nclass MyTest", "Use.scala" -> "package com.foo\nclass Use"),
        Vector.empty
      )
      val data = Files.createDirectories(directory.resolve("data"))
      write(data, Seq("com/foo/MyTest/data/input.json" -> "{}"))
      val padding = compile(
        directory.resolve("padding"),
        Seq("P.scala" -> "package padding\nclass One\nclass Two\nclass Three"),
        Vector.empty
      )
      val whole = Manifest(
        Vector(
          Output("classes", Output.Scope.Main, classes),
          Output("data", Output.Scope.Main, data),
          Output("padding", Output.Scope.Main, padding)
        ),
        scoped(Map("classes" -> classpath, "data" -> classpath, "padding" -> classpath)),
        Set.empty,
        jdk = targetJdk
      )
      assertEquals(groups(whole, Set("lib.implicits")), 2)
      assert(reads(whole).graph.nodes.contains("com.foo.Use"))
    }
  }

  private def resources(directory: Path, name: String, resource: String): Path = {
    val classes = Files.createDirectories(directory.resolve(name))
    write(classes, Seq(resource -> "data"))
    archive(classes, directory.resolve(s"$name.jar"), name, "1.0")
  }

  test("a library holding only resource directories named like an object in another is read apart") {
    Scratch.using("bonsai-") { directory =>
      val whole = libraries(
        directory,
        "tools",
        library(directory, "objects", "1.0", objectLibrary),
        resources(directory, "data", "lib/implicits/data/data.txt"),
        "package tool\nobject Tools { def two: Int = base.Util.scale(2, 5) }"
      )
      val result = reads(whole)
      assert(Seq("user.Users", "tool.Tools").forall(result.graph.nodes.contains), result.graph.nodes.keySet.toString)
    }
  }

  test("libraries that the analysed code never completes the package of are no clash") {
    Scratch.using("bonsai-") { directory =>
      val classes = library(directory, "foo", "1.0", "package org.x\nclass Foo")
      val nested  = library(directory, "bar", "1.0", "package org.x.Foo\nclass Bar")
      val whole   = withJars(
        single("users", directory, Seq("Users.scala" -> "package user\nobject Users")),
        "users",
        classes,
        nested
      )
      assert(reads(whole).graph.nodes.contains("user.Users"))
    }
  }

  test("a library only the classpath of a module without classes holds is kept in the first run") {
    Scratch.using("bonsai-") { directory =>
      val whole = libraries(
        directory,
        "tools",
        library(directory, "objects", "1.0", objectLibrary),
        library(directory, "packaged", "1.0", packageLibrary)
      )
      val objects = whole.classpath("users" -> Output.Scope.Main).last
      val only    = whole.copy(
        outputs = whole.outputs.filterNot(_.module == "users"),
        classpath = whole.classpath.removed("users" -> Output.Scope.Main).updated(
          "ghost" -> Output.Scope.Main,
          classpath :+ objects
        )
      )
      val result = Extraction.read(only).fold(errors => fail(errors.mkString("\n")), identity)
      assertEquals(result.problems, Vector.empty)
      assertEquals(result.notes, Vector.empty)
      assert(result.graph.nodes.contains("tool.Tools"), result.graph.nodes.keySet.toString)
    }
  }

  private def nested(directory: Path, jar: Path, qualified: String, module: String): Manifest = {
    val classes =
      compile(directory.resolve(module), Seq("Extra.scala" -> s"package $qualified\nclass Extra"), Vector.empty)
    Manifest(
      Vector(Output(module, Output.Scope.Main, classes)),
      scoped(Map(module -> (classpath :+ jar))),
      Set.empty,
      jdk = targetJdk
    )
  }

  test("a package of a module and an object of its name in a jar the compiler tolerates is read as one run") {
    Scratch.using("bonsai-") { directory =>
      val object_ = compile(directory.resolve("caps"), Seq("c.scala" -> "package q\nobject caps"), Vector.empty)
      val content = Files.createDirectories(directory.resolve("content"))
      val caps    = Files.createDirectories(content.resolve("scala"))
      Files.copy(object_.resolve("q/caps.class"), caps.resolve("caps.class"))
      Files.copy(object_.resolve("q/caps$.class"), caps.resolve("caps$.class"))
      val jar   = archive(content, directory.resolve("caps.jar"), "caps", "1.0")
      val whole = nested(directory, jar, "scala.caps", "m")
      assert(reads(whole).graph.nodes.contains("scala.caps.Extra"))
      assertEquals(groups(whole), 1)
    }
  }

  test("a package of a module and an object of its name in a jar the code completes fails naming the clash") {
    Scratch.using("bonsai-") { directory =>
      val jar   = library(directory, "objects", "1.0", "package p\nobject q")
      val whole = nested(directory, jar, "p.q", "m")
      val error = fails(whole)
      assert(error.startsWith(s"p.q is a class in library $jar and a package in module m"), error)
      assert(error.endsWith("m need both"), error)
    }
  }

  test("libraries that only modules without classes use fail as they cannot be read apart") {
    Scratch.using("bonsai-") { directory =>
      val whole = libraries(
        directory,
        "tools",
        library(directory, "objects", "1.0", objectLibrary),
        library(directory, "packaged", "1.0", packageLibrary)
      )
      val only = whole.copy(
        outputs = whole.outputs.filterNot(_.module == "users"),
        classpath = whole.classpath.removed("users" -> Output.Scope.Main).updated(
          "ghost" -> Output.Scope.Main,
          classpath :+ whole.classpath("users" -> Output.Scope.Main).last
        )
          .updated("shadow" -> Output.Scope.Main, classpath :+ whole.classpath("tools" -> Output.Scope.Main).last)
      )
      val error = fails(only)
      assert(error.startsWith("lib.implicits is a class in library"), error)
      assert(error.endsWith("ghost, shadow, tools cannot be read apart"), error)
    }
  }

  test("a directory on a classpath holding an object and a library holding a package of its name are read apart") {
    Scratch.using("bonsai-") { directory =>
      val unpacked = compile(directory.resolve("unpacked"), Seq("o.scala" -> objectLibrary), Vector.empty)
      val whole    = libraries(directory, "tools", unpacked, library(directory, "packaged", "1.0", packageLibrary))
      val result   = reads(whole)
      assert(Seq("user.Users", "tool.Tools").forall(result.graph.nodes.contains), result.graph.nodes.keySet.toString)
    }
  }

  test("entries below META-INF are no packages or classes") {
    Scratch.using("bonsai-") { directory =>
      val classes = Files.createDirectories(directory.resolve("versions"))
      write(classes, Seq("META-INF/versions/9/a/B.class" -> "x", "META-INF/versions/9/a/B/X.class" -> "x"))
      val whole = withJars(
        single("users", directory, Seq("Users.scala" -> "package user\nobject Users")),
        "users",
        archive(classes, directory.resolve("versions.jar"), "versions", "1.0")
      )
      assert(reads(whole).graph.nodes.contains("user.Users"))
    }
  }

  test("a clash the compiler reports that no division resolves is never a successful read") {
    Scratch.using("bonsai-") { directory =>
      val whole = libraries(
        directory,
        "tools",
        library(directory, "objects", "1.0", objectLibrary),
        library(directory, "packaged", "1.0", packageLibrary)
      )
      val settings =
        Extraction.Settings(Vector.empty, Set.empty, Vector.empty, Vector.empty, Vector.empty, Parallel.sequential)
      val errors = Extraction.readDivided(
        whole,
        settings,
        (_, _, _) => Right(Clashes.Divided(Vector(whole.outputs.toSet), Vector.empty))
      )
        .fold(identity, result => fail(s"read: ${result.notes}"))
      assert(errors.exists(_.contains("lib.implicits")), errors.toString)
    }
  }

  private case class Ran(status: Int, out: String, err: String) {
    def all: String                        = out + err
    def lines(prefix: String): Seq[String] = err.linesIterator.filter(_.startsWith(prefix)).toSeq
  }

  private val Traces = Seq("exception caught", "AssertionError", "contains object and package", "Extraction$")

  // scalafix:off ForbiddenAPIs.forbidden-api
  private def captured[A](body: => A): (A, String, String) = {
    val out    = ByteArrayOutputStream()
    val err    = ByteArrayOutputStream()
    val before = (System.out, System.err)
    System.setOut(PrintStream(out, true, UTF_8))
    System.setErr(PrintStream(err, true, UTF_8))
    try {
      val value = Console.withOut(out)(Console.withErr(err)(body))
      (value, out.toString(UTF_8), err.toString(UTF_8))
    } finally {
      System.setOut(before._1)
      System.setErr(before._2)
    }
  }

  // scalafix:on ForbiddenAPIs.forbidden-api

  private def ran(whole: Manifest, directory: Path, mode: String = "relaxed"): Ran = {
    val file  = Files.createDirectories(directory.resolve("target/bonsai")).resolve("manifest.tsv")
    val lines = whole.outputs.map(o => s"output\t${o.module}\t${o.scope.id}\t${o.path}") ++
      whole.classpath.toVector.sortBy((key, _) => (key._1, key._2.ordinal)).flatMap { case ((module, scope), entries) =>
        entries.map(entry => s"classpath\t$module\t${scope.id}\t$entry")
      } ++
      whole.libraries.toVector.sorted.map(module => s"library\t$module") ++
      whole.platforms.toVector.sorted.map(module => s"platform\t$module") ++
      whole.jdk.map(home => s"jdk\t$home") :+ s"root\t$directory"
    Files.writeString(file, lines.mkString("", "\n", "\n"))
    val (status, out, err) = captured(CLI.run(List("--manifest", file.toString, "--parallelism", "4", "--mode", mode)))
    Ran(status, out, err)
  }

  private def clashing(directory: Path, padded: String = "tools"): Manifest = libraries(
    directory,
    padded,
    library(directory, "objects", "1.0", objectLibrary),
    library(directory, "packaged", "1.0", packageLibrary)
  )

  test("a divided read of a library clash prints nothing of the attempt that met the clash") {
    Scratch.using("bonsai-") { directory =>
      val (result, out, err) = captured(Extraction.read(clashing(directory)))
      assert(result.isRight, result.toString)
      Traces.foreach(trace => assert(!(out + err).contains(trace), s"$trace in:\n$out\n$err"))
    }
  }

  test("a divided read prints one line naming the runs, the clash and its sides") {
    Seq("users", "tools").foreach { padded =>
      Scratch.using("bonsai-") { directory =>
        val whole = clashing(directory, padded)
        val run   = ran(whole, directory)
        assertEquals(run.status, 0)
        Traces.foreach(trace => assert(!run.all.contains(trace), s"$trace in:\n${run.out}\n${run.err}"))
        val objects  = directory.resolve("objects-1.0.jar")
        val packaged = directory.resolve("packaged-1.0.jar")
        assertEquals(
          run.lines("read in"),
          Seq(
            s"read in 2 groups: lib.implicits is a class in library $objects and a package in library $packaged"
          )
        )
      }
    }
  }

  test("the line of a divided read is kept when the report is reused") {
    Scratch.using("bonsai-") { directory =>
      val whole = clashing(directory)
      val first = ran(whole, directory)
      val again = ran(whole, directory)
      assert(again.err.linesIterator.contains(Reuse.Marker), again.err)
      assertEquals(again.lines("read in"), first.lines("read in"))
      assertEquals(first.lines("read in").size, 1)
    }
  }

  test("the line counts the groups of modules read apart, whatever the platform twins add to the reads") {
    Scratch.using("bonsai-") { directory =>
      val whole = libraries(
        directory,
        "tools",
        library(directory, "objects", "1.0", objectLibrary),
        library(directory, "packaged", "1.0", packageLibrary),
        twinJars = Some(
          (
            library(directory, "objectsNative", "1.0", objectLibrary),
            library(directory, "packagedJs", "1.0", packageLibrary)
          )
        )
      )
      val line = ran(whole, directory).lines("read in")
      assertEquals(line.size, 1)
      assert(line.head.startsWith("read in 2 groups: lib.implicits is a class in library "), line.head)
    }
  }

  test("the line names the reasons the division gives") {
    Scratch.using("bonsai-") { directory =>
      val whole    = clashing(directory)
      val settings =
        Extraction.Settings(Vector.empty, Set.empty, Vector.empty, Vector.empty, Vector.empty, Parallel.sequential)
      val division: Clashes.Division = (manifest, outputs, known) =>
        Clashes.runs(manifest, outputs, known).map(_.copy(because = Vector("one reason", "another")))
      val result = Extraction.readDivided(whole, settings, division).fold(errors => fail(errors.mkString), identity)
      assertEquals(result.status, Vector("read in 2 groups: one reason; another"))
    }
  }

  test("a build read in one run prints no line about runs") {
    Scratch.using("bonsai-") { directory =>
      val whole = single("users", directory, Seq("Users.scala" -> "package user\nobject Users"))
      assertEquals(ran(whole, directory).lines("read in"), Seq.empty)
    }
  }

  test("the problems of the divided read still print and fail the strict mode") {
    Scratch.using("bonsai-") { directory =>
      val whole = clashing(directory)
      val base  = whole.outputs.find(_.module == "base").get.path
      val bytes = Files.readAllBytes(Outputs.files(base).find(_.toString.endsWith(".class")).get)
      bytes(7) = 99
      Files.write(base.resolve("Broken.class"), bytes): Unit
      val relaxed = ran(whole, directory)
      assertEquals(relaxed.status, 0)
      assert(relaxed.lines("problem: ").exists(_.contains("Broken could not be read")), relaxed.err)
      assertEquals(relaxed.lines("read in").size, 1)
      Traces.foreach(trace => assert(!relaxed.all.contains(trace), s"$trace in:\n${relaxed.err}"))
      val strict = ran(whole, directory, "strict")
      assertEquals(strict.status, 1)
      assert(strict.lines("problem: ").exists(_.contains("Broken could not be read")), strict.err)
    }
  }

  test("the compiler output of the read of a clash no division resolves reaches the user once") {
    Scratch.using("bonsai-") { directory =>
      val settings =
        Extraction.Settings(Vector.empty, Set.empty, Vector.empty, Vector.empty, Vector.empty, Parallel.sequential)
      val whole              = clashing(directory)
      val (result, out, err) =
        captured(Extraction.readDivided(
          whole,
          settings,
          (_, _, _) => Right(Clashes.Divided(Vector(whole.outputs.toSet), Vector.empty))
        ))
      assert(result.isLeft, result.toString)
      assertEquals("exception caught when loading".r.findAllIn(out + err).size, 1)
    }
  }

  // scalafix:off ForbiddenAPIs.forbidden-api
  test("the output of an attempt that is not discarded reaches the user as it was printed") {
    val (kept, keptOut, keptErr) = captured(Quiet.attempt {
      println("one")
      Console.err.println("two")
      System.err.println("three")
      System.out.println("four")
      7
    }(_ => false))
    assertEquals(kept, 7)
    assertEquals(keptOut.linesIterator.toSeq, Seq("one", "four"))
    assertEquals(keptErr.linesIterator.toSeq, Seq("two", "three"))
  }

  test("the output of a discarded attempt reaches nobody, from any thread it starts") {
    val (dropped, out, err) = captured(Quiet.attempt {
      println("one")
      Console.err.println("two")
      Throwable("trace").printStackTrace()
      val thread = Thread(() => System.err.println("three"))
      thread.start()
      thread.join()
      7
    }(_ => true))
    assertEquals(dropped, 7)
    assertEquals((out, err), ("", ""))
  }

  test("an attempt hides its output only from its own thread") {
    val (_, out, err) = captured {
      val started = CountDownLatch(1)
      val release = CountDownLatch(1)
      val inside  = Thread(() =>
        Quiet.attempt { started.countDown(); release.await(1, TimeUnit.MINUTES); println("inside") }(_ => true)
      )
      inside.start()
      started.await(1, TimeUnit.MINUTES)
      System.err.println("beside")
      release.countDown()
      inside.join()
    }
    assertEquals(out, "")
    assertEquals(err.linesIterator.toSeq, Seq("beside"))
  }
  // scalafix:on ForbiddenAPIs.forbidden-api
}
