package net.kurobako.bonsai.plugin

import java.nio.file.Files
import munit.FunSuite
import net.kurobako.bonsai.collections.*

class RecordSuite extends FunSuite {
  private val source = """object Lints {
                         |  private trait Shape
                         |  def check(left: Array[Int], right: Array[Int]) = left == right
                         |  def kind(value: Any): Int = value match { case _: Shape => 1; case _ => 0 }
                         |  def apply(value: Long): Long = apply(value)
                         |}""".stripMargin

  private val clean = "object Clean { def one = 1 }"

  private case class Compiled(errors: List[String], warnings: List[String], records: Vector[Recorded.Record])

  private def compile(sources: Seq[(String, String)], options: String*): Compiled =
    Compilations.run(sources, options) { (result, directory) =>
      Compiled(
        result.allErrors.map(_.message).toList,
        result.allWarnings.map(_.message).toList,
        Recorded.all(directory.resolve("classes")).flatMap(_._2).sortBy(_.source)
      )
    }

  private def entries(compiled: Compiled, name: String): List[(String, Recorded.Level, Int)] =
    compiled.records.filter(_.source.endsWith(name)).flatMap(_.entries).sortBy(e => (e.line, e.rule)).map(e =>
      (e.rule, e.level, e.line)
    ).toList

  test("audit mode compiles what ci mode fails, with each finding a warning") {
    val ci = compile(Seq("Lints.scala" -> source), "mode=ci")
    assert(ci.errors.exists(_.endsWith("[bonsai:array-equality]")), ci.errors.mkString("\n"))
    val report = compile(Seq("Lints.scala" -> source), "mode=audit")
    assertEquals(report.errors, Nil)
    val ids = report.warnings.filter(_.contains("[bonsai:")).map(_.split("\\[bonsai:").last.stripSuffix("]")).sorted
    assertEquals(ids, List("accidental-self-call", "array-equality", "open-type-test"))
  }

  test("each finding is recorded with the level ci mode gives it") {
    val report = compile(Seq("Lints.scala" -> source), "mode=audit")
    assertEquals(
      entries(report, "Lints.scala"),
      List(
        ("array-equality", Recorded.Level.Error, 3),
        ("open-type-test", Recorded.Level.Error, 4),
        ("accidental-self-call", Recorded.Level.Error, 5)
      )
    )
  }

  test("a check kept a warning is recorded as one, and a disabled check is not recorded") {
    val kept = compile(Seq("Lints.scala" -> source), "mode=audit", "warn=array-equality", "disable=open-type-test")
    assertEquals(
      entries(kept, "Lints.scala"),
      List(("array-equality", Recorded.Level.Warning, 3), ("accidental-self-call", Recorded.Level.Error, 5))
    )
  }

  test("a check made an error is recorded as an error and does not fail the compile") {
    val made = compile(Seq("Lints.scala" -> source), "mode=audit", "error=open-type-test")
    assertEquals(made.errors, Nil)
    assert(entries(made, "Lints.scala").contains(("open-type-test", Recorded.Level.Error, 4)))
  }

  test("a source with no findings is recorded with none, so a missing record means the source was not compiled") {
    val both = compile(Seq("Lints.scala" -> source, "Clean.scala" -> clean), "mode=audit")
    assertEquals(both.records.map(_.source.split('/').last), Vector("Clean.scala", "Lints.scala"))
    assertEquals(entries(both, "Clean.scala"), Nil)
  }

  test("a record holds the digest of the source it was made from") {
    Compilations.run(Seq("Lints.scala" -> source), Seq("mode=audit")) { (_, directory) =>
      val record = Recorded.all(directory.resolve("classes")).flatMap(_._2).head
      assertEquals(record.digest, Recorded.digest(Files.readAllBytes(directory.resolve("Lints.scala"))))
    }
  }

  test("the other modes record nothing") {
    for (mode <- Seq("mode=dev", "mode=ci"))
      assertEquals(compile(Seq("Lints.scala" -> source), mode).records, Vector.empty, mode)
  }

  test("the options of the mode name the modes the plugin knows") {
    val bad = compile(Seq("Clean.scala" -> clean), "mode=release")
    assertEquals(bad.errors, List("bonsai option mode must be one of dev, ci, audit, not release"))
  }
}
