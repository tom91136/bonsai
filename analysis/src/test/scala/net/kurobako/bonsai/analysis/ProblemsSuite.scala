package net.kurobako.bonsai.analysis

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import net.kurobako.bonsai.testing.*
import scala.jdk.CollectionConverters.*
import scala.util.Using

class ProblemsSuite extends AnalysisSuite {
  override def munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(2, "min")

  private case class Ran(status: Int, err: String) {
    def problems: Vector[String] = err.linesIterator.filter(_.startsWith("problem: ")).toVector
  }

  private val compiled: Path = compile(
    Scratch.directory("bonsai-problems-"),
    Seq("Main.scala" -> "object Main { def main(args: Array[String]): Unit = println(1) }"),
    Vector.empty
  )

  private def copy(from: Path, to: Path): Unit =
    Using.resource(Files.walk(from))(_.toList.asScala.foreach { path =>
      val target = to.resolve(from.relativize(path).toString)
      if (Files.isDirectory(path)) Files.createDirectories(target) else Files.copy(path, target): Unit
    })

  private class Fixture(extra: Path => Seq[String]) {
    val dir: Path      = Scratch.directory("bonsai-problems-run-")
    val classes: Path  = Files.createDirectories(dir.resolve("classes"))
    val manifest: Path = Files.createDirectories(dir.resolve("target/bonsai")).resolve("manifest.tsv")

    copy(compiled, classes)
    Files.writeString(
      manifest,
      (Seq(s"output\tm\tmain\t$classes", s"jdk\t${targetJdk.get}", s"root\t$dir") ++
        classpath.filter(Files.isRegularFile(_)).map(jar => s"classpath\tm\tmain\t$jar") ++ extra(dir)).mkString(
        "",
        "\n",
        "\n"
      )
    )

    def run(mode: String, more: String*): Ran = {
      val err    = ByteArrayOutputStream()
      val status = Console.withOut(ByteArrayOutputStream())(Console.withErr(err)(CLI.run(
        List("--manifest", manifest.toString, "--parallelism", "4", "--mode", mode) ++ more
      )))
      Ran(status, err.toString(UTF_8))
    }
  }

  private def unreadableModel = Fixture(dir => Seq(s"model\tm\tmain\t${dir.resolve("missing.tsv")}"))

  private def brokenClass = Fixture { dir =>
    val bytes = Files.readAllBytes(compiled.resolve("Main.class"))
    bytes(7) = 99
    Files.write(dir.resolve("classes/Broken.class"), bytes): Unit
    Nil
  }

  test("a build with nothing wrong passes in both modes") {
    val f = Fixture(_ => Nil)
    assertEquals(f.run("relaxed").status, 0)
    assertEquals(f.run("strict", "--parallelism", "3").status, 0)
  }

  test("the strict mode fails on a Java model that cannot be read, and the relaxed mode prints it and passes") {
    val f       = unreadableModel
    val relaxed = f.run("relaxed")
    assertEquals(relaxed.status, 0)
    assert(relaxed.problems.exists(_.contains("missing.tsv could not be read")), relaxed.err)
    val strict = f.run("strict")
    assertEquals(strict.status, 1)
    assert(strict.problems.exists(_.contains("missing.tsv could not be read")), strict.err)
  }

  test("the strict mode fails on a class file of the build that cannot be read") {
    val f = brokenClass
    assertEquals(f.run("relaxed").status, 0)
    val strict = f.run("strict")
    assertEquals(strict.status, 1)
    assert(strict.problems.exists(_.startsWith("problem: class file Broken could not be read")), strict.err)
  }

  test("a build read in one run prints its banner and its problems and nothing else") {
    val f      = brokenClass
    val rules  = Finding.Rule.values.size
    val banner = s"bonsai: relaxed mode, ${rules - 1} of $rules analysis rules active"
    val run    = f.run("relaxed", "--nesting", "--disable", "unused-member")
    assertEquals(run.problems.size, 1)
    assertEquals(run.err.linesIterator.toSeq, banner +: run.problems)
    assert(run.problems.head.startsWith("problem: class file Broken could not be read"), run.err)
  }

  test("the run names its mode and the analysis rules it applies") {
    val f       = Fixture(_ => Nil)
    val rules   = Finding.Rule.values.size
    val default = s"bonsai: strict mode, ${rules - 1} of $rules analysis rules active"
    val plain   = f.run("strict")
    assert(plain.err.linesIterator.contains(default), plain.err)
    val nesting = s"bonsai: relaxed mode, ${rules - 1} of $rules analysis rules active"
    val opted   = f.run("relaxed", "--nesting", "--disable", "unused-member")
    assert(opted.err.linesIterator.contains(nesting), opted.err)
  }

  test("a report reused for the strict mode still fails on the problems it held") {
    val f     = unreadableModel
    val first = f.run("strict")
    val again = f.run("strict")
    assert(again.err.linesIterator.contains(Reuse.Marker), again.err)
    assertEquals((first.status, again.status), (1, 1))
    assertEquals(again.problems, first.problems)
  }

  test("notes do not fail the strict mode, and records that hold no problem are not problems") {
    assert(Reuse.Record(Vector("note: could not read library bodies of a.B in c.jar"), Vector.empty).problems.isEmpty)
    assert(Reuse.Record(Vector("purity: 3 pure"), Vector.empty).problems.isEmpty)
    assertEquals(Reuse.Record(Vector("note: x", "problem: y"), Vector.empty).problems, Vector("problem: y"))
  }
}
