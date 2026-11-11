package net.kurobako.bonsai.analysis

import java.nio.file.Path
import net.kurobako.bonsai.testing.*

/** Builds where two modules that are not platform twins compile a class of one name, which the compiler reads once. */
class DuplicatedSuite extends AnalysisSuite {

  private val Core    = "package core\ntrait Base { def one: Int = 1 }"
  private val Flat    = "package p\nobject Syn { def f(a: Int, b: Int, c: Int): Int = a + b + c }"
  private val Curried =
    "package p\nobject Syn extends core.Base { def f(a: Int, b: Int)(implicit c: Int): Int = a + b + c + one }"
  private val Use = "package q\nobject Use { def g: Int = p.Syn.f(1, 2, 3) + p.Syn.f(4, 5, 3) }"

  private def build(directory: Path, named: Boolean = false): Manifest = {
    val core    = compile(directory.resolve("core"), Seq("Base.scala" -> Core), Vector.empty)
    val flat    = compile(directory.resolve("flat"), Seq("Syn.scala" -> Flat), Vector.empty)
    val curried = compile(directory.resolve("curried"), Seq("Syn.scala" -> Curried), Vector(core))
    val caller  = compile(directory.resolve("caller"), Seq("Use.scala" -> Use), Vector(flat))
    val modules = Vector("core" -> core, "curried" -> curried, "flat" -> flat, "caller" -> caller)
    val ahead   = Map("curried" -> core, "caller" -> flat)
    Manifest(
      modules.map((module, path) => Output(module, Output.Scope.Main, path)),
      modules.map((module, _) => module -> (classpath ++ Option.when(!named)(ahead.get(module)).flatten)).toMap,
      Set.empty,
      jdk = targetJdk,
      dependencies = if (named) Map("curried" -> Set("core"), "caller" -> Set("flat")) else Map.empty
    )
  }

  private def groups(whole: Manifest): Either[Vector[String], Clashes.Divided] =
    Clashes.runs(whole, whole.outputs.map(output => output -> Outputs.files(output.path)), Set.empty)

  private def assertCalled(result: Extraction.Result): Unit =
    assertFinding(
      Smells.findings(result.smells, result.algebra.products, result.graph.nodes),
      "constant-argument",
      "Syn.f(a, b, c).c is always 3 across 2"
    )

  private def dividedByCopies(named: Boolean): Unit =
    Scratch.using("bonsai-") { directory =>
      val whole  = build(directory, named)
      val result = Extraction.read(whole).fold(errors => fail(errors.mkString("\n")), identity)
      assertEquals(result.problems, Vector.empty)
      assertEquals(result.notes, Vector.empty)
      val found   = groups(whole).fold(errors => fail(errors.mkString("\n")), identity)
      val modules = found.runs.map(_.map(_.module)).sortBy(_.size)
      assertEquals(modules, Vector(Set("curried"), Set("core", "flat", "caller")))
      assertEquals(found.because, Vector("p.Syn is a class in module curried and in module flat"))
      assertEquals(result.status, Vector("read in 2 groups: p.Syn is a class in module curried and in module flat"))
      assertCalled(result)
    }

  test("a class two modules compile is read in a run for each, the caller with the module it uses") {
    dividedByCopies(named = false)
  }

  test("a caller whose manifest names the module it is built on is read with it") {
    dividedByCopies(named = true)
  }

  test("a class one module compiles is read in one run") {
    Scratch.using("bonsai-") { directory =>
      val whole = build(directory)
      val alone = whole.copy(
        outputs = whole.outputs.filterNot(_.module == "curried"),
        classpath = whole.classpath - "curried"
      )
      assertEquals(groups(alone).map(_.runs.size), Right(1))
      val result = Extraction.read(alone).fold(errors => fail(errors.mkString("\n")), identity)
      assertEquals(result.status, Vector.empty)
      assertCalled(result)
    }
  }

  test("a class a platform twin compiles again is read in one run with its peer") {
    Scratch.using("bonsai-") { directory =>
      val whole = build(directory)
      val twins = whole.copy(
        outputs = whole.outputs.map(output =>
          if (output.module == "curried") output.copy(module = "flatJs") else output
        ),
        classpath = whole.classpath - "curried" + ("flatJs" -> classpath),
        platforms = Set("flatJs")
      )
      assertEquals(groups(twins).map(_.runs.size), Right(1))
    }
  }

  test("a module that needs both copies of a class is read in a single run") {
    Scratch.using("bonsai-") { directory =>
      val whole = build(directory)
      val both  = whole.copy(classpath =
        whole.classpath.updated(
          "caller",
          whole.classpath("caller") ++ Vector(
            whole.outputs.find(_.module == "curried").get.path
          )
        )
      )
      assertEquals(groups(both).map(_.runs.size), Right(1))
    }
  }

  private val Shared =
    """package shared
      |object Machine {
      |  case class Created(id: Int, name: String, size: Long)
      |  case class Failed(code: Int, reason: String, trace: Vector[String])
      |  case class Done(count: Int, total: Long)
      |}
      |object Left { case class Point(x: Int, y: Int, z: Int, w: Int) }
      |object Right { case class Point(x: Int, y: Int, z: Int, w: Int) }
      |object Owner {
      |  def summary(items: List[Int], label: String): String = {
      |    val kept   = items.filter(_ > 0)
      |    val scaled = kept.map(value => value * 2 + 1)
      |    val total  = scaled.foldLeft(0)(_ + _)
      |    val count  = scaled.size
      |    if (count == 0) label else s"$label: ${total / count}"
      |  }
      |}""".stripMargin

  private def sharing(directory: Path, samePath: Boolean): Vector[Finding] = {
    val one = compile(directory.resolve("one"), Seq("Shared.scala" -> Shared), Vector.empty)
    val two =
      if (samePath) compile(directory.resolve("one"), Seq("Shared.scala" -> Shared), Vector.empty, classes = "again")
      else compile(directory.resolve("two"), Seq("Other.scala" -> Shared), Vector.empty)
    val modules = Vector("one" -> one, "two" -> two)
    val whole   = Manifest(
      modules.map((module, path) => Output(module, Output.Scope.Main, path)),
      modules.map((module, _) => module -> classpath).toMap,
      Set.empty,
      jdk = targetJdk
    )
    val result = Extraction.read(whole).fold(errors => fail(errors.mkString("\n")), identity)
    assertEquals(result.problems, Vector.empty)
    assertEquals(groups(whole).map(_.runs.size), Right(2))
    Report.findings(result)
  }

  private def named(found: Vector[Finding], rule: String): Vector[String] =
    found.filter(_.rule.id == rule).map(_.detail)

  test("a source two modules compile is one definition to the rules that compare definitions") {
    Scratch.using("bonsai-") { directory =>
      val found = sharing(directory, samePath = true)
      assertEquals(named(found, "parallel-family"), Vector.empty)
      assertEquals(named(found, "duplicate-code"), Vector.empty)
      assertEquals(named(found, "duplicate-shape").map(_.takeWhile(_ != ';')).size, 1, found.mkString("\n"))
      assert(named(found, "duplicate-shape").head.endsWith("Left.Point, Right.Point"), found.mkString("\n"))
      assertEquals(
        found.groupBy(finding => (finding.rule, finding.node.name, finding.detail)).filter(_._2.size > 1),
        Map.empty
      )
    }
  }

  test("a source two modules compile once each, as different files, is still repeated code") {
    Scratch.using("bonsai-") { directory =>
      val found = sharing(directory, samePath = false)
      assertEquals(named(found, "duplicate-code").size, 1, found.mkString("\n"))
    }
  }
}
