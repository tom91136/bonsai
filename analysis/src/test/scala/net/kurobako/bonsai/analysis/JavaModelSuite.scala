package net.kurobako.bonsai.analysis

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import java.io.File
import java.nio.file.Files
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** The model the javac plugin collected for the Java code of a build, which the analysis reads. */
class JavaModelSuite extends AnalysisSuite {

  /** The model of a Java class the plugin collected, with a type and a method that refer to another type. */
  private val model =
    """node	demo.Service	demo/Service.java	6	true
      |node	demo.Service.first	demo/Service.java	14	true	demo.Service
      |edge	demo.Service.first	java.util.List
      |""".stripMargin

  test("the model the javac plugin collected joins the graph of the build") {
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
      Files.writeString(file, model): Unit
      val manifest = Manifest(
        Vector(Output("m", Output.Scope.Main, classes)),
        classpath,
        Set.empty,
        models = Vector(Output("j", Output.Scope.Main, file))
      )

      val result = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)

      assert(result.graph.nodes.contains("demo.Service"), result.graph.nodes.keys.toVector.mkString("\n"))
      assert(result.graph.nodes.contains("demo.Service.first"), result.graph.nodes.keys.toVector.mkString("\n"))
      assert(result.graph.edges.get("demo.Service.first").exists(_.contains("java.util.List")))
    } finally Using.resource(Files.walk(directory))(_.toList.asScala.reverse.foreach(Files.delete(_)))
  }

  test("the public API of a Java library is used, as the public API of Scala code is") {
    val directory = Files.createTempDirectory("bonsai-")
    try {
      val file = directory.resolve("java-model.tsv")
      Files.writeString(file, model): Unit

      def graph(library: Boolean) = {
        val manifest = Manifest(
          Vector.empty,
          classpath,
          if (library) Set("j") else Set.empty,
          models = Vector(Output("j", Output.Scope.Main, file))
        )
        val result = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
        result.graph
      }

      assert(graph(library = true).roots.contains("demo.Service"))
      assert(!graph(library = false).roots.contains("demo.Service"))
    } finally Using.resource(Files.walk(directory))(_.toList.asScala.reverse.foreach(Files.delete(_)))
  }

  test("a Java type the native code names is used, as a reflective type is") {
    val directory = Files.createTempDirectory("bonsai-")
    try {
      val file = directory.resolve("java-model.tsv")
      Files.writeString(
        file,
        """node	demo.Service	demo/Service.java	6	true			true
                                |node	demo.Other	demo/Other.java	6	true
                                |""".stripMargin
      ): Unit
      val manifest = Manifest(
        Vector.empty,
        classpath,
        Set.empty,
        models = Vector(Output("j", Output.Scope.Main, file))
      )
      val result = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)

      assert(result.graph.roots.contains("demo.Service"), "the type the native code names is used")
      assert(!result.graph.roots.contains("demo.Other"), "the other type is not used from outside")
      val findings = Liveness.findings(result.graph)
      assert(
        findings.exists(finding => finding.rule.id == "unused" && finding.node.name == "demo.Other"),
        findings.mkString("\n")
      )
      assert(
        !findings.exists(finding => finding.rule.id == "unused" && finding.node.name == "demo.Service"),
        findings.mkString("\n")
      )
    } finally Using.resource(Files.walk(directory))(_.toList.asScala.reverse.foreach(Files.delete(_)))
  }

  test("a flag every caller passes as a literal is reported on the Java code") {
    val directory = Files.createTempDirectory("bonsai-")
    try {
      val file = directory.resolve("java-model.tsv")
      Files.writeString(
        file,
        """node	demo.Service	demo/Service.java	4	false
          |method	demo.Service	demo.Service.find	false		name|other|java.lang.String||false;exact|flag|boolean||false	demo/Service.java:8
          |site	demo.Service.one	demo.Service.find	present:name,true	demo/Service.java:12
          |site	demo.Service.two	demo.Service.find	present:name,false	demo/Service.java:16
          |""".stripMargin
      ): Unit
      val manifest = Manifest(
        Vector.empty,
        classpath,
        Set.empty,
        models = Vector(Output("j", Output.Scope.Main, file))
      )

      val result   = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      val findings = Smells.findings(result.smells, result.algebra.products, result.graph.nodes)

      assertFinding(findings, "flag-argument", "exact is a literal at all 2 calls")
    } finally Using.resource(Files.walk(directory))(_.toList.asScala.reverse.foreach(Files.delete(_)))
  }

  test("the code that recurs in the Java code is reported") {
    val directory = Files.createTempDirectory("bonsai-")
    try {
      val file = directory.resolve("java-model.tsv")
      Files.writeString(
        file,
        """node	demo.First	demo/First.java	4	true
          |node	demo.Second	demo/Second.java	4	true
          |clone	demo.First	demo/First.java	8	14	100	140	30	123456	123456	
          |clone	demo.Second	demo/Second.java	8	14	200	240	30	123456	123456	
          |""".stripMargin
      ): Unit
      val manifest = Manifest(
        Vector.empty,
        classpath,
        Set.empty,
        models = Vector(Output("j", Output.Scope.Main, file))
      )

      val result   = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      val findings = Clones.findings(result.clones, result.graph.nodes)

      assert(
        findings.exists(finding => finding.rule.id == "duplicate-code" && finding.node.name == "demo.First"),
        findings.mkString("\n")
      )
    } finally Using.resource(Files.walk(directory))(_.toList.asScala.reverse.foreach(Files.delete(_)))
  }

  test("the code the generator writes is reported as a copy, labelled as generated") {
    val directory = Files.createTempDirectory("bonsai-")
    try {
      val file = directory.resolve("java-model.tsv")
      Files.writeString(
        file,
        """node	demo.First	demo/First.java	4	true
          |node	demo.Second	demo/Second.java	4	true
          |clone	demo.First	demo/First.java	8	14	100	140	30	123456	123456	
          |clone	demo.Second	demo/Second.java	8	14	200	240	30	123456	123456	
          |""".stripMargin
      ): Unit
      val manifest = Manifest(
        Vector.empty,
        classpath,
        Set.empty,
        models = Vector(Output("j", Output.Scope.Main, file))
      )

      val result = Extraction.read(manifest, generated = Vector("glob:**/First.java")).fold(
        errors => fail(errors.mkString("\n")),
        identity
      )
      val findings = Clones.findings(result.clones, result.graph.nodes)

      assert(
        findings.exists(finding => finding.rule.id == "duplicate-code" && finding.node.generated),
        findings.mkString("\n")
      )
    } finally Using.resource(Files.walk(directory))(_.toList.asScala.reverse.foreach(Files.delete(_)))
  }

  test("the code that recurs with the literals ignored is reported as a near duplicate") {
    val directory = Files.createTempDirectory("bonsai-")
    try {
      val file = directory.resolve("java-model.tsv")
      Files.writeString(
        file,
        """node	demo.First	demo/First.java	4	true
          |node	demo.Second	demo/Second.java	4	true
          |clone	demo.First	demo/First.java	8	14	100	140	30	111	222	one,two
          |clone	demo.Second	demo/Second.java	8	14	200	240	30	333	222	three,two
          |""".stripMargin
      ): Unit
      val manifest = Manifest(
        Vector.empty,
        classpath,
        Set.empty,
        models = Vector(Output("j", Output.Scope.Main, file))
      )

      val result   = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      val findings = Clones.findings(result.clones, result.graph.nodes)

      assert(
        findings.exists(finding => finding.rule.id == "near-duplicate-code" && finding.node.name == "demo.First"),
        findings.mkString("\n")
      )
    } finally Using.resource(Files.walk(directory))(_.toList.asScala.reverse.foreach(Files.delete(_)))
  }

  test("a record of the Java code is a product, which the rules read alike with a case class") {
    val directory = Files.createTempDirectory("bonsai-")
    try {
      val file = directory.resolve("java-model.tsv")
      Files.writeString(
        file,
        """node	demo.Point	demo/Point.java	4	true
          |product	demo.Point	demo.Point	x|number|||false;y|number|||false		false
          |""".stripMargin
      ): Unit
      val manifest = Manifest(
        Vector.empty,
        classpath,
        Set.empty,
        models = Vector(Output("j", Output.Scope.Main, file))
      )

      val result  = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      val product = result.algebra.products.find(_.name == "demo.Point").get
      assertEquals(product.fields.map(_.name), Vector("x", "y"))
    } finally Using.resource(Files.walk(directory))(_.toList.asScala.reverse.foreach(Files.delete(_)))
  }

  test("a record of the Java code is the product a data clump would name") {
    val directory = Files.createTempDirectory("bonsai-")
    try {
      val file = directory.resolve("java-model.tsv")
      Files.writeString(
        file,
        """node	demo.Point	demo/Point.java	4	true
          |product	demo.Point	demo.Point	x|number|||false;y|number|||false;z|number|||false	-	false
          |""".stripMargin
      ): Unit
      val manifest = Manifest(
        Vector.empty,
        classpath,
        Set.empty,
        models = Vector(Output("j", Output.Scope.Main, file))
      )

      val result  = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      val product = result.algebra.products.find(_.name == "demo.Point").get
      assertEquals(product.fields.map(_.name), Vector("x", "y", "z"))
      // The shape of the product is what the clump compares the parameters of the methods by; the three numbers are
      // alike, so they make one term of the shape.
      assertEquals(product.shape.terms.size, 1, product.shape.toString)
    } finally Using.resource(Files.walk(directory))(_.toList.asScala.reverse.foreach(Files.delete(_)))
  }

  test("a sealed type of the Java code is a sum, which the rules read alike with a sealed trait") {
    val directory = Files.createTempDirectory("bonsai-")
    try {
      val file = directory.resolve("java-model.tsv")
      Files.writeString(
        file,
        """node	demo.Shape	demo/Shape.java	4	true
          |node	demo.Circle	demo/Circle.java	4	true
          |sum	demo.Shape	demo.Shape	demo.Circle
          |""".stripMargin
      ): Unit
      val manifest = Manifest(
        Vector.empty,
        classpath,
        Set.empty,
        models = Vector(Output("j", Output.Scope.Main, file))
      )

      val result = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      val sum    = result.algebra.sums.find(_.name == "demo.Shape").get
      assertEquals(sum.variants.map(_.name), Vector("demo.Circle"))
    } finally Using.resource(Files.walk(directory))(_.toList.asScala.reverse.foreach(Files.delete(_)))
  }

  test("a model the analysis cannot read fails the build rather than losing the code") {
    val directory = Files.createTempDirectory("bonsai-")
    try {
      val file = directory.resolve("java-model.tsv")
      Files.writeString(file, "node\tonly-one-field\n"): Unit
      val manifest = Manifest(
        Vector.empty,
        classpath,
        Set.empty,
        models = Vector(Output("j", Output.Scope.Main, file))
      )
      val failure = Extraction.read(manifest).swap.getOrElse(fail("the malformed model was not reported"))
      assert(failure.exists(_.contains("a node holds")), failure.mkString("\n"))
    } finally Using.resource(Files.walk(directory))(_.toList.asScala.reverse.foreach(Files.delete(_)))
  }
}
