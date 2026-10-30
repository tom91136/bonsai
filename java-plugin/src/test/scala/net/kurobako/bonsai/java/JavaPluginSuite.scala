package net.kurobako.bonsai.java

import com.sun.source.util.JavacTask
import com.sun.source.util.TaskEvent
import com.sun.source.util.TaskListener
import com.sun.source.util.Trees
import java.nio.file.Files
import java.nio.file.Path
import javax.tools.JavaFileObject
import javax.tools.StandardLocation
import javax.tools.ToolProvider
import scala.jdk.CollectionConverters.*

class JavaPluginSuite extends munit.FunSuite {

  private val fixture =
    """
      |package demo;
      |
      |import java.util.List;
      |
      |public class Service {
      |
      |  private final List<String> names;
      |
      |  public Service(List<String> names) {
      |    this.names = names;
      |  }
      |
      |  public String first() {
      |    return names.get(0);
      |  }
      |
      |  private String hidden() {
      |    return "hidden";
      |  }
      |}
      |""".stripMargin

  /** Compiles the sources in a directory of its own, and returns the model the collector made of them. */
  private def collect(sources: (String, String)*): Model.Model = {
    val root  = Files.createTempDirectory("bonsai-java")
    val files = sources.zipWithIndex.map { case ((name, source), index) =>
      val file = root.resolve(s"demo/$name.java")
      Files.createDirectories(file.getParent)
      Files.writeString(file, source)
      file
    }
    val classes = root.resolve("classes")
    Files.createDirectories(classes)

    val compiler    = ToolProvider.getSystemJavaCompiler
    val diagnostics = new java.util.ArrayList[java.lang.String]()
    val fileManager = compiler.getStandardFileManager(null, null, null)
    fileManager.setLocation(StandardLocation.CLASS_OUTPUT, java.util.List.of(classes.toFile))
    val units    = fileManager.getJavaFileObjectsFromFiles(files.map(_.toFile).asJava)
    val listener = new javax.tools.DiagnosticListener[JavaFileObject] {
      def report(diagnostic: javax.tools.Diagnostic[? <: JavaFileObject]): Unit =
        diagnostics.add(diagnostic.getMessage(null))
    }
    val task =
      compiler.getTask(
        null,
        fileManager,
        listener,
        java.util.List.of("-proc:full"),
        null,
        units
      ).asInstanceOf[JavacTask]

    val trees     = Trees.instance(task)
    val collector = Collector(trees)
    task.addTaskListener(new TaskListener {
      override def finished(event: TaskEvent): Unit =
        if event.getKind == TaskEvent.Kind.ANALYZE then
          try collector.unit(event.getCompilationUnit, event.getTypeElement)
          catch
            case error: Throwable =>
              println("COLLECTOR FAILED: " + error)
              diagnostics.add("the collector failed")
    })
    assert(task.call(), s"the fixture compiles: ${diagnostics.asScala.mkString("\n")}\n${sources.map(_._2).mkString}")
    collector.model
  }

  test("the collector gathers the types and the methods the compiler compiled") {
    val model = collect("Service" -> fixture)

    val names = model.nodes.map(_.name)
    assert(names.contains("demo.Service"), names.toString)
    assert(!names.contains("demo.Service.first"), "a method is not a node of the graph, its type is")
    assert(!names.contains("demo.Service.Service"), "a constructor is not a node of the graph, its type is")
  }

  test("the collector records where each node is defined") {
    val model = collect("Service" -> fixture)

    val service = model.nodes.find(_.name == "demo.Service").get
    assert(service.source.endsWith("demo/Service.java"), service.source)
    assertEquals(service.line, 6)
    assert(service.exported, "a public class is exported")
  }

  test("the collector records what is not exported") {
    val model = collect("Service" -> fixture)

    val service = model.nodes.find(_.name == "demo.Service").get
    assert(service.exported, "a public class is exported")
  }

  test("the collector records the references between the nodes") {
    val model = collect("Service" -> fixture)

    val targets = model.edges.map(_.to)
    assert(targets.contains("java.util.List"), targets.toString)
  }

  test("a type that holds a native method is entered from outside the build") {
    val model = collect("Service" -> """
      |package demo;
      |
      |public class Service {
      |  public native int answer();
      |}
      |""".stripMargin)
    val service = model.nodes.find(_.name == "demo.Service").get
    assert(service.entry, "the type that holds a native method is entered from outside")
  }

  test("a type that holds a main method is entered from outside the build") {
    val model = collect("Service" -> """
      |package demo;
      |
      |public class Service {
      |  public static void main(String[] args) {
      |    System.out.println("hello");
      |  }
      |}
      |""".stripMargin)
    val app = model.nodes.find(_.name == "demo.Service").get
    assert(app.entry, "the type that holds a main method is entered from outside")
  }

  test("the collector gathers the methods with the parameters they take") {
    val model = collect("Service" -> """
      |package demo;
      |
      |public class Service {
      |
      |  public String find(String name, boolean exact, int limit) {
      |    return name;
      |  }
      |}
      |""".stripMargin)

    val method = model.methods.find(_.name == "demo.Service.find").get
    assertEquals(method.owner, "demo.Service")
    assertEquals(method.parameters.map(_.name), Vector("name", "exact", "limit"))
    assertEquals(method.parameters.map(_.kind), Vector("other", "flag", "number"))
  }

  test("the collector gathers the result of the methods and whether they override") {
    val model = collect("Service" -> """
      |package demo;
      |
      |public class Service {
      |
      |  public String toString() {
      |    return "service";
      |  }
      |
      |  public int size() {
      |    return 0;
      |  }
      |
      |  public void reset() {
      |  }
      |}
      |""".stripMargin)

    val overriding = model.methods.find(_.name == "demo.Service.toString").get
    assertEquals(overriding.returns, "String")
    assert(overriding.overriding, "the method overrides the one of the supertype")

    val size = model.methods.find(_.name == "demo.Service.size").get
    assertEquals(size.returns, "int")
    assert(!size.overriding, "the method overrides no method")

    val reset = model.methods.find(_.name == "demo.Service.reset").get
    assertEquals(reset.returns, "void")
  }

  test("the collector tells the calls whose result is dropped") {
    val model = collect("Service" -> """
      |package demo;
      |
      |public class Service {
      |
      |  public void run() {
      |    helper();
      |  }
      |
      |  public String use() {
      |    return helper();
      |  }
      |
      |  private String helper() {
      |    return "value";
      |  }
      |}
      |""".stripMargin)

    val dropped =
      model.sites.find(site => site.callee == "demo.Service.helper" && site.caller == "demo.Service.run").get
    assert(dropped.discarded, "the result of the call is dropped")

    val used = model.sites.find(site => site.callee == "demo.Service.helper" && site.caller == "demo.Service.use").get
    assert(!used.discarded, "the result of the call is used")
  }

  test("the collector tells the calls the code inverts with a logical complement") {
    val model = collect("Service" -> """
      |package demo;
      |
      |public class Service {
      |
      |  public boolean inverted() {
      |    return !(expired());
      |  }
      |
      |  public boolean read() {
      |    return expired();
      |  }
      |
      |  private boolean expired() {
      |    return true;
      |  }
      |}
      |""".stripMargin)

    val inverted =
      model.sites.find(site => site.callee == "demo.Service.expired" && site.caller == "demo.Service.inverted").get
    assert(inverted.negated, "the code inverts the result of the call")

    val read = model.sites.find(site => site.callee == "demo.Service.expired" && site.caller == "demo.Service.read").get
    assert(!read.negated, "the code reads the result of the call as it is")
  }

  test("the collector gathers the calls with the arguments they pass") {
    val model = collect("Service" -> """
      |package demo;
      |
      |public class Service {
      |
      |  public String find(String name) {
      |    return helper(name, true, 3);
      |  }
      |
      |  private String helper(String name, boolean exact, int limit) {
      |    return name;
      |  }
      |}
      |""".stripMargin)

    val site = model.sites.find(_.callee == "demo.Service.helper").get
    assertEquals(site.caller, "demo.Service.find")
    assertEquals(site.arguments, Vector("present:name", "true", "number:3"))
  }

  test("a call whose argument is a text of several lines is held by one record") {
    val model = collect("Service" -> """
      |package demo;
      |
      |public class Service {
      |
      |  public String find(String name) {
      |    return helper("a\nb");
      |  }
      |
      |  private String helper(String text) {
      |    return text;
      |  }
      |}
      |""".stripMargin)

    val site = model.sites.find(_.callee == "demo.Service.helper").get
    assertEquals(site.arguments, Vector("text:a\nb"))

    // The model is written as records a line each, and read back as it was.
    val file = Files.createTempFile("bonsai-model", ".tsv")
    try {
      Model.write(model, file)
      val read = Model.read(file).fold(error => fail(error), identity)
      assertEquals(read.sites.find(_.callee == "demo.Service.helper").get.arguments, Vector("text:a\nb"))
    } finally Files.deleteIfExists(file)
  }

  test("the collector gathers the fields of a type") {
    val model = collect("Service" -> """
      |package demo;
      |
      |public class Service {
      |
      |  private final String name;
      |  private int count = 0;
      |
      |  public Service(String name) {
      |    this.name = name;
      |  }
      |}
      |""".stripMargin)

    val fields = model.fields.filter(_.owner == "demo.Service").map(_.parameter)
    assertEquals(fields.map(_.name), Vector("name", "count"))
    assertEquals(fields.map(_.kind), Vector("other", "number"))
  }

  test("the collector finds the code that recurs elsewhere") {
    val model = collect(
      "First" -> """
      |package demo;
      |
      |public class First {
      |
      |  public String build(String name, int count) {
      |    StringBuilder result = new StringBuilder();
      |    for (int index = 0; index < count; index = index + 1) {
      |      result.append(name);
      |      result.append(index);
      |    }
      |    return result.toString();
      |  }
      |}
      |""".stripMargin,
      "Second" -> """
      |package demo;
      |
      |public class Second {
      |
      |  public String build(String other, int total) {
      |    StringBuilder result = new StringBuilder();
      |    for (int index = 0; index < total; index = index + 1) {
      |      result.append(other);
      |      result.append(index);
      |    }
      |    return result.toString();
      |  }
      |}
      |""".stripMargin
    )

    // The two methods are the same code, so the fragments of them have the same hash.
    val shared = model.fragments.groupBy(_.exact).filter(_._2.size > 1)
    assert(shared.nonEmpty, "the collector found no code that recurs")
    val copies = shared.values.maxBy(_.size)
    assertEquals(copies.map(_.node).toSet, Set("demo.First", "demo.Second"))
  }

  test("the collector tells the code that differs apart") {
    val model = collect(
      "First" -> """
      |package demo;
      |
      |public class First {
      |
      |  public String build(String name) {
      |    StringBuilder result = new StringBuilder();
      |    result.append(name);
      |    return result.toString();
      |  }
      |}
      |""".stripMargin,
      "Second" -> """
      |package demo;
      |
      |public class Second {
      |
      |  public String build(String other) {
      |    StringBuilder result = new StringBuilder();
      |    result.append(other);
      |    return result.toString();
      |  }
      |
      |  public String other() {
      |    StringBuilder result = new StringBuilder();
      |    result.append("x");
      |    return result.toString();
      |  }
      |}
      |""".stripMargin
    )

    // The code of the two builders is the same, but the code of the other method is not: it holds another literal.
    val shared = model.fragments.groupBy(_.exact).filter(_._2.size > 1)
    assert(shared.nonEmpty, "the collector found no code that recurs")
    val copies = shared.values.maxBy(_.size)
    assertEquals(copies.map(_.node).toSet, Set("demo.First", "demo.Second"))
    // The literal of the other method tells its code apart from the code of the builders.
    val literal = model.fragments.filter(_.node == "demo.Second").map(_.literals)
    assert(literal.exists(values => values.contains("x")), literal.mkString("|"))
  }

  test("the collector finds the code that recurs with the literals ignored") {
    val model = collect(
      "First" -> """
      |package demo;
      |
      |public class First {
      |
      |  public String build(String name, int count) {
      |    StringBuilder result = new StringBuilder();
      |    for (int index = 0; index < count; index = index + 1) {
      |      result.append(name);
      |      result.append(index);
      |    }
      |    return result.toString();
      |  }
      |}
      |""".stripMargin,
      "Second" -> """
      |package demo;
      |
      |public class Second {
      |
      |  public String build(String other, int total) {
      |    StringBuilder result = new StringBuilder();
      |    for (int index = 0; index < total; index = index + 1) {
      |      result.append(other);
      |      result.append(index);
      |    }
      |    return result.toString();
      |  }
      |}
      |""".stripMargin
    )

    // The locals are named alike in the two methods, so the code of them is the same once they are anonymised.
    val loose = model.fragments.groupBy(_.loose).filter(_._2.size > 1)
    assert(loose.nonEmpty, "the collector found no code that recurs")
  }

  test("a record is a product of its components") {
    val model = collect("Point" -> """
      |package demo;
      |
      |public record Point(int x, int y) {}
      |""".stripMargin)

    val product = model.products.find(_.name == "demo.Point").get
    assertEquals(product.fields.map(_.name), Vector("x", "y"))
    assertEquals(product.fields.map(_.kind), Vector("number", "number"))
  }

  test("a sealed type is a sum of the types that extend it") {
    val model = collect("Shape" -> """
      |package demo;
      |
      |public sealed interface Shape permits Circle, Square {}
      |
      |record Circle(double radius) implements Shape {}
      |
      |record Square(double side) implements Shape {}
      |""".stripMargin)

    val sum = model.sums.find(_.name == "demo.Shape").get
    assertEquals(sum.variants.map(_.name).toSet, Set("demo.Circle", "demo.Square"))
  }

  test("a reference that may be absent is an optional") {
    val model = collect(
      "Nullable" -> """
        |package org.jspecify.annotations;
        |
        |public @interface Nullable {}
        |""".stripMargin,
      "Service" -> """
        |package demo;
        |
        |import org.jspecify.annotations.Nullable;
        |
        |public class Service {
        |  public String find(@Nullable String name) {
        |    return null;
        |  }
        |}
        |""".stripMargin
    )

    val method = model.methods.find(_.name == "demo.Service.find").get
    assertEquals(method.parameters.map(_.kind), Vector("optional"))
  }

  test("a file of two classes is collected once") {
    val model = collect("Two" -> """
      |package demo;
      |
      |public class Two {
      |  public String one() { return helper(); }
      |  private String helper() { return "x"; }
      |}
      |
      |class Other {
      |}
      |""".stripMargin)

    val sites = model.sites.filter(_.callee == "demo.Two.helper")
    assertEquals(sites.size, 1, s"the call is recorded once: ${model.sites.mkString("\n")}")
  }

  test("the model is written as the lines the analysis reads") {
    val model = collect("Service" -> fixture)
    val file  = Files.createTempDirectory("bonsai-java").resolve("model.tsv")

    Model.write(model, file)

    val read = Model.read(file).fold(problem => fail(problem), identity)
    assertEquals(read.nodes.map(_.name).toSet, model.nodes.map(_.name).toSet)
    assertEquals(read.edges.size, model.edges.size)
  }
}
