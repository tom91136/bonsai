package net.kurobako.bonsai.analysis

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import java.io.File
import java.nio.file.Files
import net.kurobako.bonsai.java.JavaLint
import net.kurobako.bonsai.java.Model as JavaModel
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** The lints the javac plugin finds in the Java code of a build, which the analysis reports as findings of their own.
  */
class JavaLintsSuite extends AnalysisSuite {

  /** A record of the model of the Java code, with the fields separated by tabs. */
  private def record(kind: String, fields: String*): String = (kind +: fields).mkString("\t")

  /** The model of the Java code of a build, read as the analysis reads it. */
  private def read(model: Vector[String], scope: Output.Scope = Output.Scope.Main): Extraction.Result = {
    val directory = Files.createTempDirectory("bonsai-")
    try {
      val file = directory.resolve("java-model.tsv")
      Files.writeString(file, model.mkString("", "\n", "\n")): Unit
      val manifest = Manifest(
        Vector.empty,
        classpath,
        Set.empty,
        models = Vector(Output("j", scope, file))
      )
      Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
    } finally Using.resource(Files.walk(directory))(_.toList.asScala.reverse.foreach(Files.delete(_)))
  }

  /** The finding of the lint of the given id, which the analysis reports. */
  private def reported(result: Extraction.Result, id: String): Finding =
    Report.findings(result).find(finding => finding.rule.id == id)
      .getOrElse(fail(s"the lint $id was not reported"))

  test("a lint in a method of a type is reported on the node of the type") {
    val result = read(Vector(
      record("node", "demo.Counter", "demo/Counter.java", "4", "true"),
      record(
        "lint",
        "redundant-cast",
        "demo.Counter",
        "demo.Counter.increment",
        "demo/Counter.java",
        "12",
        "the cast to java.lang.String is redundant"
      )
    ))

    val finding = reported(result, "redundant-cast")
    assertEquals(finding.node.name, "demo.Counter")
    assertEquals(finding.node.source, "demo/Counter.java")
    assertEquals(finding.node.line, 12)
    assertEquals(finding.detail, "Counter.increment: the cast to java.lang.String is redundant")
    assertEquals(finding.level, Finding.Level.Warning)
  }

  test("a lint of the file itself is reported on a node named by the path of the file") {
    val result = read(Vector(
      record(
        "lint",
        "unused-import",
        "",
        "",
        "demo/Counter.java",
        "3",
        "the import of java.util.List is never used"
      )
    ))

    val finding = reported(result, "unused-import")
    assertEquals(finding.node.name, "demo/Counter.java")
    assertEquals(finding.node.source, "demo/Counter.java")
    assertEquals(finding.node.line, 3)
    assertEquals(finding.detail, "the import of java.util.List is never used")
  }

  test("a lint of a type the code suppresses is not reported") {
    val result = read(Vector(
      record("node", "demo.Counter", "demo/Counter.java", "4", "true", "", "redundant-cast"),
      record(
        "lint",
        "redundant-cast",
        "demo.Counter",
        "demo.Counter.increment",
        "demo/Counter.java",
        "12",
        "the cast to java.lang.String is redundant"
      )
    ))

    // The lint is found, and the suppression of the type keeps it out of the report.
    assertEquals(reported(result, "redundant-cast").node.name, "demo.Counter")
    val lines = Report(
      Report.findings(result),
      result.graph.nodes,
      Report.Options(Set.empty, Set.empty, Set.empty, info = false)
    )
    assert(!lines.exists(_.finding.rule.id == "redundant-cast"), lines.mkString("\n"))
  }

  test("a lint of the test code is reported as information") {
    val result = read(
      Vector(
        record("node", "demo.Counter", "demo/Counter.java", "4", "true"),
        record(
          "lint",
          "redundant-cast",
          "demo.Counter",
          "demo.Counter.increment",
          "demo/Counter.java",
          "12",
          "the cast to java.lang.String is redundant"
        )
      ),
      Output.Scope.Test
    )

    assertEquals(reported(result, "redundant-cast").level, Finding.Level.Info)
  }

  test("a lint of the Java code of a build of Scala code is reported") {
    val directory = Files.createTempDirectory("bonsai-")
    try {
      val classes = Files.createDirectories(directory.resolve("classes"))
      val source  = directory.resolve("A.scala")
      Files.writeString(source, "class A"): Unit
      val reporter = Driver().process(
        Array("-classpath", classpath.mkString(File.pathSeparator), "-d", classes.toString, source.toString),
        StoreReporter()
      )
      assert(!reporter.hasErrors, reporter.allErrors.map(_.message).mkString("\n"))

      val file = directory.resolve("java-model.tsv")
      Files.writeString(
        file,
        Vector(
          record("node", "demo.Counter", "demo/Counter.java", "4", "true"),
          record(
            "lint",
            "redundant-cast",
            "demo.Counter",
            "demo.Counter.increment",
            "demo/Counter.java",
            "12",
            "the cast to java.lang.String is redundant"
          )
        ).mkString("", "\n", "\n")
      ): Unit
      val manifest = Manifest(
        Vector(Output("m", Output.Scope.Main, classes)),
        classpath,
        Set.empty,
        models = Vector(Output("j", Output.Scope.Main, file))
      )

      val result = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)

      val finding = reported(result, "redundant-cast")
      assertEquals(finding.node.name, "demo.Counter")
      assertEquals(finding.node.line, 12)
    } finally Using.resource(Files.walk(directory))(_.toList.asScala.reverse.foreach(Files.delete(_)))
  }

  test("a lint no rule carries is not reported") {
    val result = read(Vector(
      record("node", "demo.Counter", "demo/Counter.java", "4", "true"),
      record(
        "lint",
        "a-lint-of-the-future",
        "demo.Counter",
        "demo.Counter.increment",
        "demo/Counter.java",
        "12",
        "the lint the analysis does not know yet"
      )
    ))

    assert(!Report.findings(result).exists(_.rule.id == "a-lint-of-the-future"))
  }

  test("no id of a lint is the id of a rule of the Scala code") {
    val lints = JavaLint.values.map(_.id).toSet
    val rules = Finding.Rule.values.map(_.id)
    // Every lint has a rule of its own, and no other rule carries the id of a lint.
    assertEquals(rules.count(lints.contains), lints.size)
    assertEquals(rules.distinct.size, rules.size)
  }
  test("a lint of generated code is reported, and labelled as generated") {
    val node = Node(
      "demo.Generated",
      "demo",
      Output.Scope.Main,
      "demo/Generated.java",
      1,
      generated = true,
      exported = false,
      outer = None
    )
    val lint = JavaModel.Lint(
      "redundant-cast",
      "demo.Generated",
      "demo.Generated.run",
      "demo/Generated.java",
      12,
      "the cast to java.lang.String is redundant"
    )

    val findings = JavaLints.findings(
      Vector(JavaLints.Found(lint, "demo", Output.Scope.Main, generated = false)),
      Map(node.name -> node)
    )

    assertEquals(findings.map(finding => (finding.rule.id, finding.node.generated)), Vector("redundant-cast" -> true))
  }

  test("a file-level lint of generated code is labelled as generated") {
    val lint = JavaModel.Lint("redundant-cast", "", "", "demo/Generated.java", 3, "the cast is redundant")

    val findings = JavaLints.findings(
      Vector(JavaLints.Found(lint, "demo", Output.Scope.Main, generated = true)),
      Map.empty
    )

    assertEquals(findings.map(_.node.generated), Vector(true))
  }

}
