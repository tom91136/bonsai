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

  private def eachSide(shared: Seq[(String, String)], objects: String, packaged: String, top: String = "")(
      check: Extraction.Result => Unit
  ): Unit = Seq("objects", "packaged").foreach { padded =>
    Scratch.using("bonsai-") { directory =>
      val result = Extraction.read(manifest(directory, padded, shared, objects, packaged, top))
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
}
