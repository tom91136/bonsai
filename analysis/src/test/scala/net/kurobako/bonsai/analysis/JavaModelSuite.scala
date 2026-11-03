package net.kurobako.bonsai.analysis

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import java.io.File
import java.nio.file.Files
import net.kurobako.bonsai.testing.*

/** The model the javac plugin collected for the Java code of a build, which the analysis reads. */
class JavaModelSuite extends AnalysisSuite {

  /** The model of a Java class the plugin collected, with a type and a method that refer to another type. */
  private val model =
    """node	demo.Service	demo/Service.java	6	true
      |node	demo.Service.first	demo/Service.java	14	true	demo.Service
      |edge	demo.Service.first	java.util.List
      |""".stripMargin

  private def read(
      text: String,
      libraries: Set[String] = Set.empty,
      generated: Vector[String] = Vector.empty
  ): Extraction.Result =
    Scratch.using("bonsai-") { directory =>
      val file = directory.resolve("java-model.tsv")
      Files.writeString(file, text): Unit
      val manifest = Manifest(
        Vector.empty,
        classpaths,
        libraries,
        models = Vector(Output("j", Output.Scope.Main, file))
      )
      Extraction.read(manifest, generated = generated).fold(errors => fail(errors.mkString("\n")), identity)
    }

  test("the model the javac plugin collected joins the graph of the build") {
    Scratch.using("bonsai-") { directory =>
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
        classpaths,
        Set.empty,
        models = Vector(Output("j", Output.Scope.Main, file))
      )

      val result = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)

      assert(result.graph.nodes.contains("demo.Service"), result.graph.nodes.keys.toVector.mkString("\n"))
      assert(result.graph.nodes.contains("demo.Service.first"), result.graph.nodes.keys.toVector.mkString("\n"))
      assert(result.graph.edges.get("demo.Service.first").exists(_.contains("java.util.List")))
    }
  }

  test("the public API of a Java library is used, as the public API of Scala code is") {
    def graph(libraries: Set[String]) = read(model, libraries).graph
    assert(graph(Set("j")).roots.contains("demo.Service"))
    assert(!graph(Set.empty).roots.contains("demo.Service"))
  }

  test("a Java type the native code names is used, as a reflective type is") {
    val result = read(
      """node	demo.Service	demo/Service.java	6	true			true
                                |node	demo.Other	demo/Other.java	6	true
                                |""".stripMargin
    )

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
  }

  test("a flag every caller passes as a literal is reported on the Java code") {
    val result = read(
      """node	demo.Service	demo/Service.java	4	false
          |method	demo.Service	demo.Service.find	demo.Service.find(Ljava/lang/String;Z)V	false		name|other|java.lang.String||false;exact|flag|boolean||false	demo/Service.java:8
          |site	demo.Service.one	demo.Service.find(Ljava/lang/String;Z)V	present:name,true	demo/Service.java:12
          |site	demo.Service.two	demo.Service.find(Ljava/lang/String;Z)V	present:name,false	demo/Service.java:16
          |""".stripMargin
    )

    val findings = Smells.findings(result.smells, result.algebra.products, result.graph.nodes)

    assertFinding(findings, "flag-argument", "exact is a literal at all 2 calls")
  }

  test("the code that recurs in the Java code is reported") {
    val result = read(
      """node	demo.First	demo/First.java	4	true
          |node	demo.Second	demo/Second.java	4	true
          |clone	demo.First	demo/First.java	8	14	100	140	30	123456	123456	
          |clone	demo.Second	demo/Second.java	8	14	200	240	30	123456	123456	
          |""".stripMargin
    )

    val findings = Clones.findings(result.clones, result.graph.nodes)

    assert(
      findings.exists(finding => finding.rule.id == "duplicate-code" && finding.node.name == "demo.First"),
      findings.mkString("\n")
    )
  }

  test("the code the generator writes is reported as a copy, labelled as generated") {
    val result = read(
      """node	demo.First	demo/First.java	4	true
          |node	demo.Second	demo/Second.java	4	true
          |clone	demo.First	demo/First.java	8	14	100	140	30	123456	123456	
          |clone	demo.Second	demo/Second.java	8	14	200	240	30	123456	123456	
          |""".stripMargin,
      generated = Vector("glob:**/First.java")
    )

    val findings = Clones.findings(result.clones, result.graph.nodes)

    assert(
      findings.exists(finding => finding.rule.id == "duplicate-code" && finding.node.generated),
      findings.mkString("\n")
    )
  }

  test("the code that recurs with the literals ignored is reported as a near duplicate") {
    val result = read(
      """node	demo.First	demo/First.java	4	true
          |node	demo.Second	demo/Second.java	4	true
          |clone	demo.First	demo/First.java	8	14	100	140	30	111	222	one,two
          |clone	demo.Second	demo/Second.java	8	14	200	240	30	333	222	three,two
          |""".stripMargin
    )

    val findings = Clones.findings(result.clones, result.graph.nodes)

    assert(
      findings.exists(finding => finding.rule.id == "near-duplicate-code" && finding.node.name == "demo.First"),
      findings.mkString("\n")
    )
  }

  private def fragment(
      owner: String,
      lines: (Int, Int),
      offsets: (Int, Int),
      cost: Int,
      exact: Long,
      loose: Long,
      literals: String = ""
  ) = {
    val file = s"demo/${owner.stripPrefix("demo.")}.java"
    s"clone\t$owner\t$file\t${lines._1}\t${lines._2}\t${offsets._1}\t${offsets._2}\t$cost\t$exact\t$loose\t$literals"
  }

  private def scoped(models: (Output.Scope, Seq[String])*): Vector[Finding] = {
    val nodes = Seq("First", "Second", "Third").map(name => s"node\tdemo.$name\tdemo/$name.java\t4\ttrue")
    Scratch.using("bonsai-") { directory =>
      val outputs = models.zipWithIndex.map { case ((scope, _), index) =>
        Output(s"j$index", scope, directory.resolve(s"java-model-$index.tsv"))
      }
      outputs.zip(models).foreach((output, model) =>
        Files.writeString(output.path, (nodes ++ model._2).mkString("\n") + "\n")
      )
      val manifest = Manifest(Vector.empty, classpaths, Set.empty, models = outputs.toVector)
      val result   = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      Clones.findings(result.clones, result.graph.nodes)
    }
  }

  private def cloned(rows: String*): Vector[Finding] = scoped(Output.Scope.Main -> rows)

  test("Java copies of fewer than four lines are not reported") {
    val found = cloned(
      fragment("demo.First", (8, 10), (100, 140), 30, exact = 1L, loose = 1L),
      fragment("demo.Second", (8, 10), (200, 240), 30, exact = 1L, loose = 1L)
    )
    assert(found.isEmpty, found.mkString("\n"))
  }

  test("Java copies that overlap within a file are one copy") {
    val found = cloned(
      fragment("demo.First", (8, 14), (100, 140), 30, exact = 1L, loose = 1L),
      fragment("demo.First", (10, 16), (120, 160), 30, exact = 1L, loose = 1L),
      fragment("demo.Second", (8, 14), (200, 240), 30, exact = 1L, loose = 1L)
    )
    assert(
      found.map(_.detail) == Vector(
        "2 copies of 7 lines (30 nodes): demo/First.java:8-14, demo/Second.java:8-14"
      ),
      found.mkString("\n")
    )
  }

  test("Java copies that overlap are no group without a copy elsewhere") {
    val found = cloned(
      fragment("demo.First", (8, 14), (100, 140), 30, exact = 1L, loose = 1L),
      fragment("demo.First", (10, 16), (120, 160), 30, exact = 1L, loose = 1L)
    )
    assert(found.isEmpty, found.mkString("\n"))
  }

  test("Java copies within larger copies are not reported again") {
    val found = cloned(
      fragment("demo.First", (8, 30), (100, 300), 60, exact = 1L, loose = 1L),
      fragment("demo.Second", (8, 30), (500, 700), 60, exact = 1L, loose = 1L),
      fragment("demo.First", (10, 16), (120, 180), 30, exact = 2L, loose = 2L),
      fragment("demo.Second", (10, 16), (520, 580), 30, exact = 2L, loose = 2L)
    )
    assert(
      found.map(_.detail) == Vector(
        "2 copies of 23 lines (60 nodes): demo/First.java:8-30, demo/Second.java:8-30"
      ),
      found.mkString("\n")
    )
  }

  test("Java copies that differ in more literals than their size carries are not near duplicates") {
    val found = cloned(
      fragment("demo.First", (8, 14), (100, 140), 30, exact = 1L, loose = 9L, literals = "a,b,c,d"),
      fragment("demo.Second", (8, 14), (200, 240), 30, exact = 2L, loose = 9L, literals = "e,f,g,h")
    )
    assert(found.isEmpty, found.mkString("\n"))
  }

  test("larger Java copies may differ in one literal for every sixteen nodes") {
    val found = cloned(
      fragment("demo.First", (8, 14), (100, 140), 64, exact = 1L, loose = 9L, literals = "a,b,c,d"),
      fragment("demo.Second", (8, 14), (200, 240), 64, exact = 2L, loose = 9L, literals = "e,b,g,h")
    )
    assert(found.map(_.rule.id) == Vector("near-duplicate-code"), found.mkString("\n"))
    assert(found.head.detail.contains("literals {a, e}, {c, g}, {d, h}"), found.mkString("\n"))
  }

  test("Java copies with literals of different counts are not near duplicates") {
    val found = cloned(
      fragment("demo.First", (8, 14), (100, 140), 30, exact = 1L, loose = 9L, literals = "a,b"),
      fragment("demo.Second", (8, 14), (200, 240), 30, exact = 2L, loose = 9L, literals = "c")
    )
    assert(found.isEmpty, found.mkString("\n"))
  }

  test("Java copies split between main code and tests are information, and in tests alone too") {
    val rows = Seq(fragment("demo.First", (8, 14), (100, 140), 30, exact = 1L, loose = 1L))
    val also = Seq(fragment("demo.Second", (8, 14), (200, 240), 30, exact = 1L, loose = 1L))
    val both = scoped(Output.Scope.Main -> rows, Output.Scope.Test -> also)
    assert(both.map(_.level) == Vector(Finding.Level.Info), both.mkString("\n"))
    assert(both.head.detail.endsWith("(copies in main code and tests)"), both.mkString("\n"))
    val tests = scoped(Output.Scope.Test -> rows, Output.Scope.Test -> also)
    assert(tests.map(_.level) == Vector(Finding.Level.Info), tests.mkString("\n"))
    val main = scoped(Output.Scope.Main -> rows, Output.Scope.Main -> also)
    assert(main.map(_.level) == Vector(Finding.Level.Warning), main.mkString("\n"))
  }

  test("a record of the Java code is a product, which the rules read alike with a case class") {
    val result = read(
      """node	demo.Point	demo/Point.java	4	true
          |product	demo.Point	demo.Point	x|number|||false;y|number|||false		false
          |""".stripMargin
    )

    val product = result.algebra.products.find(_.name == "demo.Point").get
    assertEquals(product.fields.map(_.name), Vector("x", "y"))
  }

  test("a record of the Java code is the product a data clump would name") {
    val result = read(
      """node	demo.Point	demo/Point.java	4	true
          |product	demo.Point	demo.Point	x|number|||false;y|number|||false;z|number|||false	-	false
          |""".stripMargin
    )

    val product = result.algebra.products.find(_.name == "demo.Point").get
    assertEquals(product.fields.map(_.name), Vector("x", "y", "z"))
    // The shape of the product is what the clump compares the parameters of the methods by; the three numbers are
    // alike, so they make one term of the shape.
    assertEquals(product.shape.terms.size, 1, product.shape.toString)
  }

  test("a sealed type of the Java code is a sum, which the rules read alike with a sealed trait") {
    val result = read(
      """node	demo.Shape	demo/Shape.java	4	true
          |node	demo.Circle	demo/Circle.java	4	true
          |sum	demo.Shape	demo.Shape	demo.Circle
          |""".stripMargin
    )

    val sum = result.algebra.sums.find(_.name == "demo.Shape").get
    assertEquals(sum.variants.map(_.name), Vector("demo.Circle"))
  }

  test("a model the analysis cannot read fails the build rather than losing the code") {
    Scratch.using("bonsai-") { directory =>
      val file = directory.resolve("java-model.tsv")
      Files.writeString(file, "node\tonly-one-field\n"): Unit
      val manifest = Manifest(
        Vector.empty,
        classpaths,
        Set.empty,
        models = Vector(Output("j", Output.Scope.Main, file))
      )
      val failure = Extraction.read(manifest).swap.getOrElse(fail("the malformed model was not reported"))
      assert(failure.exists(_.contains("a node holds")), failure.mkString("\n"))
    }
  }

  private def reference(text: String, parameter: Int = -1): String =
    Seq("R", text, text, parameter.toString, "").mkString("|")

  private def expression(
      owner: String,
      line: Int,
      offsets: (Int, Int),
      cost: Int,
      exact: Long,
      roles: Int,
      member: String = "run",
      variables: Seq[String] = Seq.empty,
      reach: String = ""
  ) = {
    val file = s"demo/${owner.stripPrefix("demo.")}.java"
    Seq(
      "clone",
      owner,
      file,
      line.toString,
      line.toString,
      offsets._1.toString,
      offsets._2.toString,
      cost.toString,
      exact.toString,
      exact.toString,
      "",
      member,
      Broad.toString,
      variables.mkString(","),
      roles.toString,
      reach
    ).mkString("\t")
  }

  private val Broad  = 5L
  private val Small  = 1
  private val Tiny   = 2
  private val Entire = 4
  private val Helper = 16

  private def chainAt(index: Int, cost: Int, member: String) =
    expression("demo.First", index, (index * 100, index * 100 + 40), cost, exact = 1L, roles = Small, member = member)

  private def sites(count: Int, cost: Int) = (1 to count).map(chainAt(_, cost, "run"))

  private def sitesApart(count: Int) = (1 to count).map(index => chainAt(index, 14, s"run$index"))

  test("a chain of calls that Java code repeats at many places is a repeated expression") {
    val found = cloned(sitesApart(11)*)
    assert(found.map(_.rule.id) == Vector("duplicate-code"), found.mkString("\n"))
    assert(found.head.detail.startsWith("11 copies of 1 line (14 nodes)"), found.mkString("\n"))
  }

  test("a chain of calls that Java code repeats four times in a method is a repeated expression") {
    val found = cloned(sites(count = 4, cost = 10)*)
    assert(found.map(_.rule.id) == Vector("duplicate-code"), found.mkString("\n"))
  }

  test("a chain of calls that Java code repeats too few times for its size is not reported") {
    assert(cloned(sitesApart(10)*).isEmpty)
    val owners = Seq("First", "Second", "Third", "First")
    val apart  = owners.zipWithIndex.map { (owner, index) =>
      expression(
        s"demo.$owner",
        index,
        (index * 100, index * 100 + 40),
        10,
        exact = 1L,
        roles = Small,
        member = s"run$index"
      )
    }
    assert(cloned(apart*).isEmpty)
  }

  test("Java methods with the same small body that takes a parameter are duplicates") {
    val parameter = Seq(reference("free#1", 0))
    val found     = cloned(
      expression("demo.First", 8, (100, 140), 12, exact = 1L, Entire | Tiny, "label", parameter),
      expression("demo.Second", 9, (200, 240), 12, exact = 1L, Entire | Tiny, "title", parameter)
    )
    assert(found.map(_.rule.id) == Vector("duplicate-code"), found.mkString("\n"))
  }

  test("two Java constants with the same small body are no duplicates, three are") {
    val rows = Seq("First", "Second", "Third").zipWithIndex.map { (owner, index) =>
      expression(
        s"demo.$owner",
        8,
        (100 * index, 100 * index + 40),
        12,
        exact = 1L,
        Entire | Tiny,
        "PATTERN"
      )
    }
    assert(cloned(rows.take(2)*).isEmpty)
    assert(cloned(rows*).map(_.rule.id) == Vector("duplicate-code"))
  }

  test("a Java expression that is the body of a method with other values for its parameters is an inlined helper") {
    val helper = expression(
      "demo.First",
      8,
      (100, 140),
      12,
      exact = 1L,
      roles = Entire | Tiny | Helper,
      member = "last",
      variables = Seq(reference("free#1", 0), reference("demo.Names.SEP"))
    )
    val site = expression(
      "demo.Second",
      20,
      (400, 440),
      12,
      exact = 2L,
      roles = 0,
      variables = Seq(reference("demo.Second.name"), reference("demo.Names.SEP"))
    )
    val found = cloned(helper, site)
    assert(found.map(_.rule.id) == Vector("inlined-helper"), found.mkString("\n"))
    assert(found.head.node.name == "demo.Second" && found.head.detail.contains("First.last"), found.mkString("\n"))
  }

  test("a Java expression that differs from a helper where the helper holds no parameter is no inlined helper") {
    val helper = expression(
      "demo.First",
      8,
      (100, 140),
      12,
      exact = 1L,
      roles = Entire | Tiny | Helper,
      member = "last",
      variables = Seq(reference("free#1", 0), reference("demo.Names.SEP"))
    )
    val site = expression(
      "demo.Second",
      20,
      (400, 440),
      12,
      exact = 2L,
      roles = 0,
      variables = Seq(reference("demo.Second.name"), reference("demo.Names.OTHER"))
    )
    val found = cloned(helper, site)
    assert(found.isEmpty, found.mkString("\n"))
  }

  test("a Java helper that is visible to its type only is no helper for code elsewhere") {
    val helper = expression(
      "demo.First",
      8,
      (100, 140),
      12,
      exact = 1L,
      roles = Entire | Tiny | Helper,
      member = "last",
      variables = Seq(reference("free#1", 0)),
      reach = "demo.First"
    )
    val site = expression(
      "demo.Second",
      20,
      (400, 440),
      12,
      exact = 2L,
      roles = 0,
      variables = Seq(reference("demo.Second.name"))
    )
    assert(cloned(helper, site).isEmpty)
    val inside = expression(
      "demo.First",
      30,
      (500, 540),
      12,
      exact = 2L,
      roles = 0,
      member = "other",
      variables = Seq(reference("demo.First.name"))
    )
    assert(cloned(helper, inside).map(_.rule.id) == Vector("inlined-helper"))
  }

  test("a Java helper is no helper for code in another module") {
    val helper = expression(
      "demo.First",
      8,
      (100, 140),
      12,
      exact = 1L,
      roles = Entire | Tiny | Helper,
      member = "last",
      variables = Seq(reference("free#1", 0))
    )
    val site = expression(
      "demo.Second",
      20,
      (400, 440),
      12,
      exact = 2L,
      roles = 0,
      variables = Seq(reference("demo.Second.name"))
    )
    Scratch.using("bonsai-") { directory =>
      val rows    = Seq("First" -> helper, "Second" -> site)
      val outputs = rows.map((name, _) => Output(name, Output.Scope.Main, directory.resolve(s"$name.tsv")))
      outputs.zip(rows).foreach { case (output, (name, row)) =>
        Files.writeString(output.path, s"node\tdemo.$name\tdemo/$name.java\t4\ttrue\n$row\n"): Unit
      }
      val manifest = Manifest(Vector.empty, classpaths, Set.empty, models = outputs.toVector)
      val result   = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      val found    = Clones.findings(result.clones, result.graph.nodes)
      assert(found.isEmpty, found.mkString("\n"))
    }
  }
}
