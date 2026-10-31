package net.kurobako.bonsai.analysis

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import java.io.File
import java.nio.file.Files
import net.kurobako.bonsai.testing.Scratch

/** The Java code of a build, which the rules that need every call site read alike with the Scala code. */
class JavaParitySuite extends AnalysisSuite {

  private enum Inheritance {
    case Declared, Overriding
  }

  private enum Use {
    case Inverted, Direct
  }

  /** A record of the model of the Java code, with the fields separated by tabs. */
  private def record(kind: String, fields: String*): String = (kind +: fields).mkString("\t")

  /** The model of a Java type with a method whose result every call of it drops. */
  private def service(
      source: String = "demo/Service.java",
      returns: String = "String",
      inheritance: Inheritance = Inheritance.Declared
  ): Vector[String] = Vector(
    record("node", "demo.Service", source, "4", "true"),
    record(
      "method",
      "demo.Service",
      "demo.Service.helper",
      "false",
      "",
      "",
      s"$source:8",
      returns,
      (inheritance == Inheritance.Overriding).toString
    ),
    record("site", "demo.Service.one", "demo.Service.helper", "", s"$source:12", "true"),
    record("site", "demo.Service.two", "demo.Service.helper", "", s"$source:16", "true")
  )

  /** The finding of the rule of the given id, which the analysis reports. */
  private def reported(result: Extraction.Result, id: String): Finding =
    Report.findings(result).find(finding => finding.rule.id == id).getOrElse(fail(s"the rule $id was not reported"))
  test("the result of a Java method every call drops is reported as ignored") {
    val finding = reported(readJavaModel(service()), "ignored-result")

    assertEquals(finding.node.name, "demo.Service")
    assertEquals(finding.node.source, "demo/Service.java")
    assertEquals(finding.detail, "Service.helper: its String result is discarded at all 2 calls")
    assertEquals(finding.level, Finding.Level.Warning)
  }

  test("a Java method that overrides a method of a supertype is not reported, as its result is meant to be dropped") {
    val result   = readJavaModel(service(inheritance = Inheritance.Overriding))
    val findings = Report.findings(result)

    assertNoFinding(findings)(finding => finding.rule.id == "ignored-result")
  }

  test("a Java method of test code is reported as information") {
    val finding = reported(readJavaModel(service(), Output.Scope.Test), "ignored-result")

    assertEquals(finding.detail, "Service.helper: its String result is discarded at all 2 calls (all in tests)")
    assertEquals(finding.level, Finding.Level.Info)
  }

  test("a Java method of generated code is reported, and labelled as generated") {
    val result = readJavaModel(
      service(source = "/build/src/generated/Service.java"),
      generated = Vector("glob:**/src/generated/**")
    )
    val found = Report.findings(result).filter(_.rule.id == "ignored-result")

    assert(found.nonEmpty && found.forall(_.node.generated), found.mkString("\n"))
  }

  test("a Java method that returns nothing is not reported") {
    val result = readJavaModel(service(returns = "void"))

    assertNoFinding(Report.findings(result))(finding => finding.rule.id == "ignored-result")
  }

  test("a Java method that returns the type that holds it is not reported, as its result is meant to be dropped") {
    val result = readJavaModel(service(returns = "Service"))

    assertNoFinding(Report.findings(result))(finding => finding.rule.id == "ignored-result")
  }

  test("the methods of the test code of Java are not the ones a data clump is made of") {
    val parameters = Vector("x", "y", "z").map(name => s"$name|other|java.lang.String||false").mkString(";")
    def method(owner: String, name: String, source: String, line: Int) =
      record("method", owner, s"$owner.$name", "false", "", parameters, s"$source:$line", "String", "false")
    val model = Vector(
      record("node", "demo.First", "demo/First.java", "4", "true"),
      record("node", "demo.Second", "demo/Second.java", "4", "true"),
      method("demo.First", "one", "demo/First.java", 8),
      method("demo.First", "two", "demo/First.java", 12),
      method("demo.Second", "three", "demo/Second.java", 8)
    )

    assertFinding(Report.findings(readJavaModel(model)), "data-clump", "recur in 3 methods across 2 types")
    assertNoFinding(Report.findings(readJavaModel(model, Output.Scope.Test)))(finding =>
      finding.rule.id == "data-clump"
    )
  }

  test("a build of Scala and Java code reports the findings of both alike") {
    Scratch.using("bonsai-") { directory =>
      val classes = Files.createDirectories(directory.resolve("classes"))
      val source  = directory.resolve("Tools.scala")
      Files.writeString(
        source,
        """object Tools {
          |  def compute(): Int = 1
          |}
          |object Main {
          |  def use(): Int = {
          |    Tools.compute()
          |    Tools.compute()
          |    0
          |  }
          |}""".stripMargin
      ): Unit
      val reporter = Driver().process(
        Array("-classpath", classpath.mkString(File.pathSeparator), "-d", classes.toString, source.toString),
        StoreReporter()
      )
      assert(!reporter.hasErrors, reporter.allErrors.map(_.message).mkString("\n"))

      val file = directory.resolve("java-model.tsv")
      Files.writeString(file, service().mkString("", "\n", "\n")): Unit
      val manifest = Manifest(
        Vector(Output("m", Output.Scope.Main, classes)),
        classpath,
        Set.empty,
        models = Vector(Output("j", Output.Scope.Main, file))
      )

      val result = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)

      val findings = Report.findings(result)
      assertFinding(findings, "ignored-result", "Tools.compute(): its Int result is discarded at all 2 calls")
      assertFinding(findings, "ignored-result", "Service.helper: its String result is discarded at all 2 calls")
    }
  }

  test(
    "a call that passes fewer arguments than the method takes is read without failing"
  ) { // The model of the Java code names a method by its name, so a call to an overload of it passes the arguments of
    // that one, which are not the parameters of the one the model holds.
    val result = readJavaModel(Vector(
      record("node", "demo.Service", "demo/Service.java", "4", "true"),
      record(
        "method",
        "demo.Service",
        "demo.Service.helper",
        "false",
        "",
        "name|other|||false;exact|flag|||false",
        "demo/Service.java:8"
      ),
      record("site", "demo.Service.one", "demo.Service.helper", "present:name", "demo/Service.java:12", "true"),
      record("site", "demo.Service.two", "demo.Service.helper", "present:name", "demo/Service.java:16", "true")
    ))

    // The call passes one argument to a method that takes two, so the rule cannot read it and says nothing.
    assert(!Report.findings(result).exists(_.rule.id == "flag-argument"))
  }

  test("a parameter every caller passes a value to is not a constant one") {
    // The value of an argument the model does not know is not a constant, so the rule says nothing of it.
    val unknown = readJavaModel(Vector(
      record("node", "demo.Service", "demo/Service.java", "4", "true"),
      record(
        "method",
        "demo.Service",
        "demo.Service.helper",
        "false",
        "",
        "exact|flag|||false;limit|number|||false",
        "demo/Service.java:8"
      ),
      record(
        "site",
        "demo.Service.one",
        "demo.Service.helper",
        "present:flag,number:3",
        "demo/Service.java:12",
        "true"
      ),
      record("site", "demo.Service.two", "demo.Service.helper", "present:flag,number:3", "demo/Service.java:16", "true")
    ))

    val findings  = Report.findings(unknown)
    val constants =
      findings.filter(finding => finding.rule.id == "constant-argument" || finding.rule.id == "flag-argument")
    assert(!constants.exists(_.detail.contains("exact")), constants.mkString("\n"))
    // The value the callers pass is known, so the rule reports it.
    assert(constants.exists(_.detail.contains("limit is always 3")), constants.mkString("\n"))
  }

  /** The model of a Java type with a boolean method whose every call the model says inverts. */
  private def deadline(use: Use = Use.Inverted, inheritance: Inheritance = Inheritance.Declared): Vector[String] = {
    val inverted = (use == Use.Inverted).toString
    Vector(
      record("node", "demo.Deadline", "demo/Deadline.java", "4", "true"),
      record(
        "method",
        "demo.Deadline",
        "demo.Deadline.isExpired",
        "false",
        "",
        "",
        "demo/Deadline.java:8",
        "boolean",
        (inheritance == Inheritance.Overriding).toString
      ),
      record("site", "demo.Deadline.first", "demo.Deadline.isExpired", "", "demo/Deadline.java:12", "false", inverted),
      record("site", "demo.Deadline.second", "demo.Deadline.isExpired", "", "demo/Deadline.java:16", "false", inverted)
    )
  }

  test("a Java boolean method every caller inverts is reported as inverted") {
    val finding = reported(readJavaModel(deadline()), "inverted-boolean")

    assertEquals(finding.node.name, "demo.Deadline")
    assertEquals(finding.node.source, "demo/Deadline.java")
    assertEquals(finding.detail, "Deadline.isExpired: its boolean result is inverted at all 2 calls")
    assertEquals(finding.level, Finding.Level.Warning)
  }

  test("a Java boolean method the code also reads directly is not inverted") {
    assertNoFinding(Report.findings(readJavaModel(deadline(Use.Direct))))(finding =>
      finding.rule.id == "inverted-boolean"
    )
  }

  test("a Java method that overrides a method of a supertype is not reported as inverted") {
    assertNoFinding(Report.findings(readJavaModel(deadline(inheritance = Inheritance.Overriding))))(finding =>
      finding.rule.id == "inverted-boolean"
    )
  }

  test("a Java boolean method of test code every caller inverts is reported as information") {
    val finding = reported(readJavaModel(deadline(), Output.Scope.Test), "inverted-boolean")

    assertEquals(finding.detail, "Deadline.isExpired: its boolean result is inverted at all 2 calls (all in tests)")
    assertEquals(finding.level, Finding.Level.Info)
  }
}
