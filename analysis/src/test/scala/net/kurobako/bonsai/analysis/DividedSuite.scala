package net.kurobako.bonsai.analysis

import java.nio.file.Path
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
      top: String
  ): Manifest = {
    def padding(module: String) =
      Option.when(module == padded)("Padding.scala" -> s"package padding.$module\nclass One\nclass Two\nclass Three")
    val base      = compile(directory.resolve("base"), shared, Vector.empty)
    val onObjects = compile(
      directory.resolve("objects"),
      Seq("B.scala" -> s"package a\nobject b {\n$objects\n}") ++ padding("objects"),
      Vector(base)
    )
    val onPackage = compile(
      directory.resolve("packaged"),
      Seq("Side.scala" -> s"package a.b\nobject Side {\n$packaged\n}\n$top") ++ padding("packaged"),
      Vector(base)
    )
    Manifest(
      Vector(
        Output("base", Output.Scope.Main, base),
        Output("objects", Output.Scope.Main, onObjects),
        Output("packaged", Output.Scope.Main, onPackage)
      ),
      Map("base" -> classpath, "objects" -> (classpath :+ base), "packaged" -> (classpath :+ base)),
      Set.empty,
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

  test("a platform twin that a clash would read apart fails naming the clash") {
    Scratch.using("bonsai-") { directory =>
      val whole =
        manifest(directory, "objects", base("object Util { def one: Int = 1 }"), "def a: Int = 1", "def b: Int = 2", "")
      val errors = Extraction.read(whole.copy(platforms = Set("packaged"))).fold(identity, _ => fail("read"))
      assertEquals(errors.size, 1)
      assert(errors.head.startsWith("a.b is a class in module objects and a package in module packaged"), errors.head)
      assert(errors.head.contains("packaged compile for another platform"), errors.head)
    }
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
        Map("core" -> classpath, "lib" -> (classpath :+ coreClasses)),
        libraries,
        jdk = targetJdk
      )
      val settings =
        Extraction.Settings(Vector.empty, Set.empty, Vector.empty, Vector.empty, Vector.empty, Parallel.sequential)
      def read(division: Clashes.Division) = Extraction.readDivided(whole, settings, division)
        .fold(errors => fail(errors.mkString("\n")), identity)
      val together = read((_, _) => Right(Vector(outputs.toSet)))
      val groups   = if (coreFirst) outputs else outputs.reverse
      val divided  = read((_, _) => Right(groups.map(Set(_))))
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

  test("a platform twin whose peer a clash would read apart fails naming the clash") {
    Scratch.using("bonsai-") { directory =>
      val shared  = "Peer.scala" -> "package twinned\nclass Peer { def one: Int = 1 }"
      val objects = compile(directory.resolve("objects"), Seq("B.scala" -> "package a\nobject b", shared), Vector.empty)
      val twin    = compile(directory.resolve("twin"), Seq(shared), Vector.empty)
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
        Map("objectsJs" -> classpath, "objects" -> classpath, "packaged" -> classpath),
        Set.empty,
        platforms = Set("objectsJs"),
        jdk = targetJdk
      )
      val errors = Extraction.read(whole).fold(identity, _ => fail("read"))
      assertEquals(errors.size, 1)
      assert(errors.head.startsWith("a.b is a class in module objects and a package in module packaged"), errors.head)
      assert(errors.head.contains("objectsJs compile for another platform"), errors.head)
    }
  }
}
