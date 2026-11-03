package net.kurobako.bonsai.java

import java.nio.file.Files
import javax.tools.StandardLocation
import javax.tools.ToolProvider
import net.kurobako.bonsai.testing.*
import scala.jdk.CollectionConverters.*

/** The suite of the lints the plugin finds in the code it compiles, which the model of the compilation holds.
  *
  * The plugin runs inside the compiler, so the suite compiles the code with the compiler of the JDK and reads the model
  * the plugin wrote, as the build does.
  */
class LintSuite extends munit.FunSuite {

  /** The processor path of the compiler, which holds the plugin and the library it runs on. */
  private def processorPath: String =
    getClass.getClassLoader match {
      case loader: java.net.URLClassLoader =>
        loader.getURLs.view
          .filter(url => url.getProtocol == "file")
          .map(url => url.getPath)
          .mkString(java.io.File.pathSeparator)
      case _ => sys.props("java.class.path")
    }

  /** Compiles the sources with the plugin, and returns the model the plugin wrote of them. */
  private def compile(sources: (String, String)*): Model.Model = compileEnabled(Vector.empty, sources*)

  /** Compiles the sources with the given opt-in lints enabled, and returns the model. */
  private def compileEnabled(enabled: Vector[String], sources: (String, String)*): Model.Model =
    compileWith(Nil, enabled, sources*)

  /** Compiles the sources with the extra options of the compiler, such as the release, and the lints enabled. */
  private def compileWith(options: Seq[String], enabled: Vector[String], sources: (String, String)*): Model.Model = {
    val root    = Scratch.directory("bonsai-lint")
    val files   = Fixtures.write(root, sources)
    val classes = root.resolve("classes")
    Files.createDirectories(classes)
    val output = root.resolve("model.tsv")

    val compiler    = ToolProvider.getSystemJavaCompiler
    val diagnostics = java.util.ArrayList[java.lang.String]()
    val fileManager = Fixtures.fileManager(classes)
    // The annotations of the test code, such as JSpecify, are on the classpath of the suite.
    val classpath = processorPath.split(java.io.File.pathSeparator).view.filter(_.nonEmpty).map(path =>
      java.io.File(path)
    ).toVector
    fileManager.setLocation(StandardLocation.CLASS_PATH, classpath.asJava)
    val units = fileManager.getJavaFileObjectsFromFiles(files.map(_.toFile).asJava)
    val task  =
      compiler.getTask(
        Fixtures.omitted,
        fileManager,
        Fixtures.listener(diagnostics),
        ((Seq("-proc:full", "-processorpath", processorPath) ++ options) :+
          ("-Xplugin:bonsai " + output + (if (enabled.nonEmpty) " " + enabled.mkString(",") else ""))).asJava,
        Fixtures.omitted,
        units
      )

    assert(task.call(), s"the fixture compiles: ${diagnostics.asScala.mkString("\n")}")

    Model.read(output).fold(problem => fail(problem), identity)
  }

  /** The lint of the id the model holds, which the suite asserts on. */
  private def lint(model: Model.Model, id: String): Model.Lint =
    model.lints.find(_.id == id).getOrElse(fail(s"the model holds no $id lint: ${model.lints.mkString(", ")}"))

  test("a cast of a value to the type it has is redundant") {
    val model = compile("Casts" -> """
      |package demo;
      |
      |public class Casts {
      |  public String cast(String value) {
      |    return (String) value;
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "redundant-cast")
    assertEquals(found.detail, "the cast to java.lang.String is redundant")
    assertEquals(found.owner, "demo.Casts")
    assertEquals(found.method, "demo.Casts.cast")
  }

  test("a cast that gives a call of a signature polymorphic method its type is not redundant") {
    val model = compile("Casts" -> """
      |package demo;
      |
      |import java.lang.invoke.MethodHandles;
      |import java.lang.invoke.VarHandle;
      |
      |public class Casts {
      |  private static final VarHandle VALUE;
      |
      |  static {
      |    try {
      |      VALUE = MethodHandles.lookup().findStaticVarHandle(Casts.class, "value", int.class);
      |    } catch (ReflectiveOperationException failure) {
      |      throw new ExceptionInInitializerError(failure);
      |    }
      |  }
      |
      |  private static int value;
      |
      |  public int read() {
      |    return (int) VALUE.getAcquire();
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id == "redundant-cast"), Vector.empty)
  }

  test("a cast of a lambda that names the interface it is to be read as is not redundant") {
    val model = compile("Casts" -> """
      |package demo;
      |
      |public class Casts {
      |  public interface Action {
      |    String run();
      |  }
      |
      |  public interface CheckedAction {
      |    String run() throws Exception;
      |  }
      |
      |  public static String run(Action action) {
      |    return action.run();
      |  }
      |
      |  public static String run(CheckedAction action) throws Exception {
      |    return action.run();
      |  }
      |
      |  public String cast() {
      |    return run((Action) () -> "value");
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id == "redundant-cast"), Vector.empty)
  }

  test("a cast of a call of a signature polymorphic method with arguments is not redundant") {
    val model = compile("Casts" -> """
      |package demo;
      |
      |import java.lang.invoke.MethodHandles;
      |import java.lang.invoke.VarHandle;
      |import java.nio.ByteBuffer;
      |import java.nio.ByteOrder;
      |
      |public class Casts {
      |  private static final VarHandle SEQUENCE =
      |      MethodHandles.byteBufferViewVarHandle(long[].class, ByteOrder.LITTLE_ENDIAN);
      |
      |  public long read(ByteBuffer mapping) {
      |    return (long) SEQUENCE.getAcquire(mapping, 0);
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id == "redundant-cast"), Vector.empty)
  }

  test("a value that is assigned to itself keeps the value it has") {
    val model = compile("Assignments" -> """
      |package demo;
      |
      |public class Assignments {
      |  public int run() {
      |    int value = 1;
      |    value = value;
      |    return value;
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "self-assignment")
    assertEquals(found.detail, "the value is assigned to itself")
    assertEquals(found.owner, "demo.Assignments")
    assertEquals(found.method, "demo.Assignments.run")
  }

  test("an exception that is caught and never looked at is swallowed") {
    val model = compile("Catches" -> """
      |package demo;
      |
      |public class Catches {
      |
      |  public void run() {
      |    try {
      |      helper();
      |    } catch (IllegalStateException failure) {
      |    }
      |  }
      |
      |  private void helper() {
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "swallowed-exception")
    assertEquals(found.detail, "the exception is caught and never looked at")
    assertEquals(found.owner, "demo.Catches")
    assertEquals(found.method, "demo.Catches.run")
  }

  test("an exception that is caught as ignored is let go on purpose") {
    val model = compile("Catches" -> """
      |package demo;
      |
      |public class Catches {
      |
      |  public void run() {
      |    try {
      |      helper();
      |    } catch (IllegalStateException ignored) {
      |    }
      |  }
      |
      |  private void helper() {
      |  }
      |}
      |""".stripMargin)

    assert(!model.lints.exists(_.id == "swallowed-exception"), model.lints.mkString(", "))
  }

  test("a suppression of the code is held by the node of the type") {
    val model = compile("Quiet" -> """
      |package demo;
      |
      |@SuppressWarnings("bonsai:unused-member")
      |public class Quiet {
      |  private int spare = 1;
      |}
      |""".stripMargin)

    assertEquals(model.nodes.find(_.name == "demo.Quiet").map(_.suppressed), Some(Set("unused-member")))
  }

  test("the file of the model is the argument of the plugin, not a property the modules of a build share") {
    val stale = Scratch.directory("bonsai-stale").resolve("model.tsv")
    sys.props.update(BonsaiPlugin.Output, stale.toString)
    try {
      val model = compile("Quiet" -> """
        |package demo;
        |
        |public class Quiet {
        |  private int spare = 1;
        |}
        |""".stripMargin)

      assert(model.nodes.exists(_.name == "demo.Quiet"))
      assert(!Files.exists(stale), s"the model was written to the property $stale")
    } finally sys.props.remove(BonsaiPlugin.Output): Unit
  }

  test("a modifier the code around it implies is of no use") {
    val model = compile("Shape" -> """
      |package demo;
      |
      |public interface Shape {
      |  public int sides();
      |
      |  int corners();
      |}
      |""".stripMargin)

    val found = lint(model, "redundant-modifier")
    assertEquals(found.detail, "the modifier is already implied by the code around it")
    assertEquals(found.owner, "demo.Shape")
    assertEquals(found.method, "demo.Shape.sides")
    // The method that writes no modifier has none the code around it implies.
    assertEquals(model.lints.count(lint => lint.id == "redundant-modifier"), 1, model.lints.mkString(", "))
  }

  test("two strings that are compared by reference are not compared by their text") {
    val model = compile("Texts" -> """
      |package demo;
      |
      |public class Texts {
      |  public boolean same(String left, String right) {
      |    return left == right;
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "string-reference-equality")
    assertEquals(found.detail, "the strings are compared by reference and not by their text")
    assertEquals(found.owner, "demo.Texts")
    assertEquals(found.method, "demo.Texts.same")
  }

  test("a type that compares its values and does not give them a hash code is not found in a collection") {
    val model = compile("Values" -> """
      |package demo;
      |
      |public class Values {
      |
      |  private final int value;
      |
      |  public Values(int value) {
      |    this.value = value;
      |  }
      |
      |  @Override
      |  public boolean equals(Object other) {
      |    if (!(other instanceof Values)) return false;
      |    return this.value == ((Values) other).value;
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "equals-without-hashcode")
    assertEquals(found.detail, "the type compares its values but does not give them a hash code")
    assertEquals(found.owner, "demo.Values")
    assertEquals(found.method, "", "the lint is of the type, not of a method of it")
  }

  test("a field of the state of the type is given the value it starts with") {
    val model = compile("Fields" -> """
      |package demo;
      |
      |public class Fields {
      |  private static boolean loaded;
      |  private static volatile int count;
      |  private volatile boolean started;
      |  private int plain;
      |  private static String name;
      |  private final int fixed = 0;
      |}
      |""".stripMargin)

    val lints = model.lints.filter(_.id == "missing-field-initializer")
    assertEquals(lints.size, 3, s"the lints of the fields: ${model.lints.mkString(", ")}")
    assert(
      lints.forall(_.detail == "the field is not given the value it starts with"),
      lints.mkString(", ")
    )
    assertEquals(lints.map(_.owner).toSet, Set("demo.Fields"))
  }

  test("a field that is given a value of its own is given it for a reason") {
    val model = compile("Fields" -> """
      |package demo;
      |
      |public class Fields {
      |
      |  private int count = 1;
      |  private String name = "demo";
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "missing-field-initializer"),
      s"the fields are given the values they need: ${model.lints.mkString(", ")}"
    )
  }

  test("an import the file never uses is of no use") {
    val model = compile("Imports" -> """
      |package demo;
      |
      |import java.util.List;
      |
      |public class Imports {
      |  public String name() {
      |    return "demo";
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "unused-import")
    assertEquals(found.detail, "the import of List is never used")
    assertEquals(found.owner, "", "the lint is of the file, not of a type of it")
    assertEquals(found.method, "", "the lint is of the file, not of a method of it")
  }

  test("a method that overrides the one of a supertype and does not say so") {
    val model = compile(
      "Shape" -> """
        |package demo;
        |
        |public interface Shape {
        |  int sides();
        |}
        |""".stripMargin,
      "Square" -> """
        |package demo;
        |
        |public class Square implements Shape {
        |  public int sides() {
        |    return 4;
        |  }
        |}
        |""".stripMargin
    )

    val found = lint(model, "missing-override")
    assertEquals(found.detail, "the method overrides the one of the supertype and does not say so")
    assertEquals(found.owner, "demo.Square")
    assertEquals(found.method, "demo.Square.sides")
    // The method of the interface overrides no method of a supertype, so only the method of the class is reported.
    assertEquals(
      model.lints.count(_.id == "missing-override"),
      1,
      s"the lints of the methods: ${model.lints.mkString(", ")}"
    )
  }

  test("a method that overrides the one of a supertype and says so keeps the annotation") {
    val model = compile(
      "Shape" -> """
        |package demo;
        |
        |public interface Shape {
        |  int sides();
        |}
        |""".stripMargin,
      "Square" -> """
        |package demo;
        |
        |public class Square implements Shape {
        |  @Override
        |  public int sides() {
        |    return 4;
        |  }
        |}
        |""".stripMargin
    )

    assert(
      !model.lints.exists(_.id == "missing-override"),
      s"the method says that it overrides: ${model.lints.mkString(", ")}"
    )
  }

  test("a method that overrides no method of a supertype needs no annotation") {
    val model = compile("Circle" -> """
      |package demo;
      |
      |public class Circle {
      |  public int sides() {
      |    return 0;
      |  }
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "missing-override"),
      s"the method overrides no method of a supertype: ${model.lints.mkString(", ")}"
    )
  }

  test("a method that overrides the one of a supertype stays silent when suppressed by the error prone name") {
    val model = compile(
      "Shape" -> """
        |package demo;
        |
        |public interface Shape {
        |  int sides();
        |}
        |""".stripMargin,
      "Square" -> """
        |package demo;
        |
        |public class Square implements Shape {
        |  @SuppressWarnings("MissingOverride")
        |  public int sides() {
        |    return 4;
        |  }
        |}
        |""".stripMargin,
      "Triangle" -> """
        |package demo;
        |
        |public class Triangle implements Shape {
        |  public int sides() {
        |    return 3;
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(
      model.lints.filter(_.id == "missing-override").map(_.owner),
      Vector("demo.Triangle"),
      s"the lints of the methods: ${model.lints.mkString(", ")}"
    )
  }

  test("a method that hides the one of a supertype and overrides it not needs no annotation") {
    val model = compile(
      "Base" -> """
        |package demo;
        |
        |public class Base {
        |  public static int count() {
        |    return 0;
        |  }
        |}
        |""".stripMargin,
      "Counters" -> """
        |package demo;
        |
        |public class Counters extends Base {
        |  public static int count() {
        |    return 1;
        |  }
        |}
        |""".stripMargin
    )

    assert(
      !model.lints.exists(_.id == "missing-override"),
      s"the method hides the one of the supertype: ${model.lints.mkString(", ")}"
    )
  }

  test("a stack trace that is written to the output hides the failure of the program") {
    val model = compile("Traces" -> """
      |package demo;
      |
      |public class Traces extends Exception {
      |  public void report() {
      |    printStackTrace();
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "print-stack-trace")
    assertEquals(found.detail, "the stack trace is written to the output of the program instead of being reported")
    assertEquals(found.owner, "demo.Traces")
    assertEquals(found.method, "demo.Traces.report")
  }

  test("a switch of an enumeration that handles every case of it needs no default") {
    val model = compile(
      "Colour" -> """
        |package demo;
        |
        |public enum Colour {
        |  RED,
        |  GREEN
        |}
        |""".stripMargin,
      "Switches" -> """
        |package demo;
        |
        |public class Switches {
        |  public void count(Colour colour) {
        |    switch (colour) {
        |      case RED:
        |        report("red");
        |        break;
        |      case GREEN:
        |        report("green");
        |        break;
        |    }
        |  }
        |
        |  private void report(String colour) {
        |    System.out.println(colour);
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(model.lints.filter(_.id == "missing-default-in-switch"), Vector.empty)
  }

  test("a switch of an enumeration that does not handle every case of it misses the cases it gains later") {
    val model = compile(
      "Colour" -> """
        |package demo;
        |
        |public enum Colour {
        |  RED,
        |  GREEN
        |}
        |""".stripMargin,
      "Switches" -> """
        |package demo;
        |
        |public class Switches {
        |  public void count(Colour colour) {
        |    switch (colour) {
        |      case RED:
        |        report("red");
        |        break;
        |    }
        |  }
        |
        |  private void report(String colour) {
        |    System.out.println(colour);
        |  }
        |}
        |""".stripMargin
    )

    val found = lint(model, "missing-default-in-switch")
    assertEquals(found.detail, "the switch does not handle every case of the enumeration")
    assertEquals(found.owner, "demo.Switches")
    assertEquals(found.method, "demo.Switches.count")
  }

  test("a switch whose default is beside null handles every case of the enumeration") {
    val model = compile(
      "Colour" -> """
        |package demo;
        |
        |public enum Colour {
        |  RED,
        |  GREEN
        |}
        |""".stripMargin,
      "Switches" -> """
        |package demo;
        |
        |public class Switches {
        |  public void count(Colour colour) {
        |    switch (colour) {
        |      case RED:
        |        report("red");
        |        break;
        |      case GREEN:
        |        report("green");
        |        break;
        |      case null, default:
        |        report("none");
        |        break;
        |    }
        |  }
        |
        |  private void report(String colour) {
        |    System.out.println(colour);
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(model.lints.filter(_.id == "missing-default-in-switch"), Vector.empty)
  }

  test("an exception that is caught and never looked at") {
    val model = compile("Errors" -> """
      |package demo;
      |
      |public class Errors {
      |  public void run() {
      |    try {
      |      work();
      |    } catch (Exception error) {
      |    }
      |  }
      |
      |  private void work() {
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "swallowed-exception")
    assertEquals(found.detail, "the exception is caught and never looked at")
    assertEquals(found.owner, "demo.Errors")
    assertEquals(found.method, "demo.Errors.run")
  }

  test("an exception that is caught so generally that the failures of the program are hidden") {
    val model = compile("Errors" -> """
      |package demo;
      |
      |public class Errors {
      |  public void run() {
      |    try {
      |      work();
      |    } catch (Throwable error) {
      |      report(error.getMessage());
      |    }
      |  }
      |
      |  private void work() {
      |  }
      |
      |  private void report(String message) {
      |    System.out.println(message);
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "catch-throwable")
    assertEquals(found.detail, "the exception is caught so generally that the failures of the program are hidden")
    assertEquals(found.owner, "demo.Errors")
    assertEquals(found.method, "demo.Errors.run")
  }

  test("an exception looked at only through a same-named field of an inner class is reported") {
    val model = compile("Shadowed" -> """
      |package demo;
      |
      |public class Shadowed {
      |  public void run() {
      |    try {
      |      work();
      |    } catch (RuntimeException failure) {
      |      Runnable inner = new Runnable() {
      |        private final String failure = "unused";
      |
      |        public void run() {
      |          System.out.println(failure);
      |        }
      |      };
      |      inner.run();
      |    }
      |  }
      |
      |  private void work() {
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "swallowed-exception")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
    assertEquals(found.head.detail, "the exception is caught and never looked at")
  }

  test("an exception used inside an inner class is not reported") {
    val model = compile("Captured" -> """
      |package demo;
      |
      |public class Captured {
      |  public void run() {
      |    try {
      |      work();
      |    } catch (RuntimeException failure) {
      |      Runnable inner = new Runnable() {
      |        public void run() {
      |          System.out.println(failure);
      |        }
      |      };
      |      inner.run();
      |    }
      |  }
      |
      |  private void work() {
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id == "swallowed-exception").size, 0, model.lints.mkString("\n"))
  }

  test("a same-named lambda parameter does not count as a derived throwable") {
    val model = compile("Lambdas" -> """
      |package demo;
      |
      |import java.util.List;
      |
      |public class Lambdas {
      |  public void run(List<String> names) {
      |    try {
      |      names.size();
      |    } catch (Throwable failure) {
      |      if (names.isEmpty()) {
      |        Throwable cause = failure.getCause();
      |        cause.hashCode();
      |      }
      |      names.forEach(cause -> log(cause));
      |    }
      |  }
      |
      |  public void handed(List<String> names) {
      |    try {
      |      names.size();
      |    } catch (Throwable failure) {
      |      names.forEach(item -> log(failure));
      |    }
      |  }
      |
      |  private void log(Object value) {
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "catch-throwable")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
    assertEquals(found.head.method, "demo.Lambdas.run")
  }

  test("a catch of throwable that only logs a message is reported") {
    val model = compile("Catches" -> """
      |package demo;
      |
      |public class Catches {
      |  public void run(Runnable task) {
      |    try {
      |      task.run();
      |    } catch (Throwable t) {
      |      log("x");
      |    }
      |  }
      |
      |  public void copy(Runnable task) {
      |    try {
      |      task.run();
      |    } catch (Throwable t) {
      |      String message = t.getMessage();
      |      log(message);
      |    }
      |  }
      |
      |  public void failing(Runnable task) {
      |    try {
      |      task.run();
      |    } catch (Error error) {
      |      log(error.getMessage());
      |    }
      |  }
      |
      |  private void log(String message) {
      |    System.out.println(message);
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      model.lints.filter(_.id == "catch-throwable").map(_.method),
      Vector("demo.Catches.run", "demo.Catches.copy", "demo.Catches.failing")
    )
  }

  test("a catch of throwable that passes the throwable on is not reported") {
    val model = compile("Catches" -> """
      |package demo;
      |
      |import java.util.concurrent.CompletableFuture;
      |import java.util.concurrent.atomic.AtomicReference;
      |
      |public class Catches {
      |  private final AtomicReference<Throwable> failure = new AtomicReference<>();
      |  private final CompletableFuture<String> future = new CompletableFuture<>();
      |  private Throwable last;
      |
      |  public void argument(Runnable task) {
      |    try {
      |      task.run();
      |    } catch (Throwable t) {
      |      failure.set(t);
      |    }
      |  }
      |
      |  public void wrapped(Runnable task) {
      |    try {
      |      task.run();
      |    } catch (Throwable t) {
      |      throw new IllegalStateException(t);
      |    }
      |  }
      |
      |  public void rethrown(Runnable task) {
      |    try {
      |      task.run();
      |    } catch (Throwable t) {
      |      throw t;
      |    }
      |  }
      |
      |  public void completed(Runnable task) {
      |    try {
      |      task.run();
      |    } catch (Throwable t) {
      |      future.completeExceptionally(t);
      |    }
      |  }
      |
      |  public void assigned(Runnable task) {
      |    try {
      |      task.run();
      |    } catch (Throwable t) {
      |      last = t;
      |    }
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      model.lints.filter(_.id == "catch-throwable").map(_.method),
      Vector.empty,
      s"the catches pass the throwable on: ${model.lints.mkString(", ")}"
    )
  }

  test("a catch of throwable that returns the throwable or a cause of it is not reported") {
    val model = compile("Catches" -> """
      |package demo;
      |
      |import java.util.concurrent.atomic.AtomicReference;
      |
      |public class Catches {
      |  private final AtomicReference<Throwable> failure = new AtomicReference<>();
      |
      |  public Throwable returned(Runnable task) {
      |    try {
      |      task.run();
      |      return null;
      |    } catch (Throwable t) {
      |      return t;
      |    }
      |  }
      |
      |  public void derived(Runnable task) {
      |    try {
      |      task.run();
      |    } catch (Throwable t) {
      |      Throwable cause = t.getCause();
      |      failure.set(cause);
      |    }
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      model.lints.filter(_.id == "catch-throwable").map(_.method),
      Vector.empty,
      s"the catches return or pass on the throwable: ${model.lints.mkString(", ")}"
    )
  }

  test("a catch of throwable that stores it in an array is reported unless the array is handed on") {
    val model = compile("Catches" -> """
      |package demo;
      |
      |public class Catches {
      |  private void report(Object value) {}
      |
      |  public void handed(Runnable task) {
      |    try {
      |      task.run();
      |    } catch (Throwable t) {
      |      Throwable[] held = { t };
      |      report(held);
      |    }
      |  }
      |
      |  public void stored(Runnable task) {
      |    try {
      |      task.run();
      |    } catch (Throwable t) {
      |      Throwable[] held = { t };
      |      System.out.println(held.length);
      |    }
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      model.lints.filter(_.id == "catch-throwable").map(_.method),
      Vector("demo.Catches.stored")
    )
  }

  test("an exception that is caught and looked at is not swallowed") {
    val model = compile("Errors" -> """
      |package demo;
      |
      |public class Errors {
      |  public void run() {
      |    try {
      |      work();
      |    } catch (Exception error) {
      |      report(error);
      |    }
      |  }
      |
      |  private void work() {
      |  }
      |
      |  private void report(Exception error) {
      |    System.out.println(error);
      |  }
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "swallowed-exception"),
      s"the exception is looked at: ${model.lints.mkString(", ")}"
    )
  }

  test("a method that returns from its finally block hides the exception it was to report") {
    val model = compile("Cleaners" -> """
      |package demo;
      |
      |public class Cleaners {
      |  public int run() {
      |    try {
      |      work();
      |    } finally {
      |      return 1;
      |    }
      |  }
      |
      |  private void work() {
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "return-in-finally")
    assertEquals(found.detail, "the return hides the exception the method was to report")
    assertEquals(found.owner, "demo.Cleaners")
    assertEquals(found.method, "demo.Cleaners.run")
  }

  test("a return outside the finally block hides no exception") {
    val model = compile("Cleaners" -> """
      |package demo;
      |
      |public class Cleaners {
      |  public int run() {
      |    try {
      |      work();
      |    } catch (RuntimeException error) {
      |      report(error);
      |      return 0;
      |    }
      |    return 1;
      |  }
      |
      |  private void work() {
      |  }
      |
      |  private void report(RuntimeException error) {
      |    System.out.println(error);
      |  }
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "return-in-finally"),
      s"the returns are of the code of the method, not of its finally block: ${model.lints.mkString(", ")}"
    )
  }

  test("a resource that a finally block closes is closed by the try") {
    val model = compile("Resources" -> """
      |package demo;
      |
      |import java.io.ByteArrayInputStream;
      |import java.io.IOException;
      |
      |public class Resources {
      |  public void run() throws IOException {
      |    ByteArrayInputStream input = new ByteArrayInputStream(new byte[0]);
      |    try {
      |      input.read();
      |    } finally {
      |      work();
      |      input.close();
      |    }
      |  }
      |
      |  private void work() {
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "close-in-finally")
    assertEquals(found.detail, "the resource input is closed in a finally block; let the try statement close it")
  }

  test("a resource that the try statement closes is not closed by its finally block") {
    val model = compile("Resources" -> """
      |package demo;
      |
      |import java.io.ByteArrayInputStream;
      |import java.io.IOException;
      |
      |public class Resources {
      |  public void run() throws IOException {
      |    try (ByteArrayInputStream input = new ByteArrayInputStream(new byte[0])) {
      |      input.read();
      |    }
      |  }
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "close-in-finally"),
      s"the try statement closes the resource: ${model.lints.mkString(", ")}"
    )
  }

  test("a type that is not closeable is not a resource") {
    val model = compile("Resources" -> """
      |package demo;
      |
      |public class Resources {
      |  private static final class Handle {
      |    void close() {
      |    }
      |  }
      |
      |  public void run() {
      |    Handle handle = new Handle();
      |    try {
      |      work();
      |    } finally {
      |      handle.close();
      |    }
      |  }
      |
      |  private void work() {
      |  }
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "close-in-finally"),
      s"the handle is not a resource: ${model.lints.mkString(", ")}"
    )
  }

  test("a resource that the try statement gives a value is not closed by it") {
    val model = compile("Resources" -> """
      |package demo;
      |
      |import java.io.ByteArrayInputStream;
      |import java.io.IOException;
      |
      |public class Resources {
      |  public void run() throws IOException {
      |    ByteArrayInputStream input = new ByteArrayInputStream(new byte[0]);
      |    try {
      |      input = new ByteArrayInputStream(new byte[] {1});
      |      input.read();
      |    } finally {
      |      input.close();
      |    }
      |  }
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "close-in-finally"),
      s"the try gives the resource another value: ${model.lints.mkString(", ")}"
    )
  }

  test("a condition that is always the same is always true or always false") {
    val model = compile("Conditions" -> """
      |package demo;
      |
      |public class Conditions {
      |  public void run() {
      |    if (true) {
      |      work();
      |    }
      |  }
      |
      |  public void never() {
      |    do {
      |      work();
      |    } while (false);
      |  }
      |
      |  public void forever() {
      |    while (true) {
      |      work();
      |      break;
      |    }
      |  }
      |
      |  public void again() {
      |    do {
      |      work();
      |    } while (true);
      |  }
      |
      |  private void work() {
      |  }
      |}
      |""".stripMargin)

    val lints = model.lints.filter(_.id == "constant-condition")
    assertEquals(lints.size, 2, s"the lints of the conditions: ${model.lints.mkString(", ")}")
    assertEquals(lints.map(_.detail).toSet, Set("the condition is always true", "the condition is always false"))
    assertEquals(lints.map(_.owner).toSet, Set("demo.Conditions"))
    assertEquals(lints.map(_.method).toSet, Set("demo.Conditions.run", "demo.Conditions.never"))
  }

  test("a value that is boxed although the code does not need it") {
    val model = compile("Boxes" -> """
      |package demo;
      |
      |public class Boxes {
      |  public int run() {
      |    int value = Integer.valueOf(42);
      |    return Integer.valueOf(42);
      |  }
      |}
      |""".stripMargin)

    val lints = model.lints.filter(_.id == "unnecessary-boxing")
    assertEquals(lints.size, 2, s"the lints of the boxes: ${model.lints.mkString(", ")}")
    assert(lints.forall(_.detail == "the value is boxed although the code does not need it"), lints.mkString(", "))
    assertEquals(lints.map(_.owner).toSet, Set("demo.Boxes"))
    assertEquals(lints.map(_.method).toSet, Set("demo.Boxes.run"))
  }

  test("a value that is kept as a box is a box the code needs") {
    val model = compile("Boxes" -> """
      |package demo;
      |
      |public class Boxes {
      |  public Integer run() {
      |    Integer value = Integer.valueOf(42);
      |    return value;
      |  }
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "unnecessary-boxing"),
      s"the code keeps the value as a box: ${model.lints.mkString(", ")}"
    )
  }

  test("a modifier the code around it already implies is of no use") {
    val model = compile("Contract" -> """
      |package demo;
      |
      |public interface Contract {
      |
      |  public int value();
      |
      |  public static final int LIMIT = 10;
      |}
      |""".stripMargin)

    val lints = model.lints.filter(_.id == "redundant-modifier")
    assertEquals(lints.size, 2, s"the lints of the members: ${model.lints.mkString(", ")}")
    assert(lints.forall(_.detail == "the modifier is already implied by the code around it"), lints.mkString(", "))
    assertEquals(lints.map(_.owner).toSet, Set("demo.Contract"))
    // A member of an interface is public and abstract already, so the lint of the field is of the type, not of a method.
    assertEquals(lints.map(_.method).toSet, Set("demo.Contract.value", ""))
  }

  test("a case of a switch that falls through to the case after it") {
    val model = compile("Switches" -> """
      |package demo;
      |
      |public class Switches {
      |  public void run(int value) {
      |    switch (value) {
      |      case 1:
      |        work();
      |      case 2:
      |        work();
      |        break;
      |      default:
      |        break;
      |    }
      |  }
      |
      |  private void work() {
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "fallthrough-in-switch")
    assertEquals(found.detail, "1 case(s) of the switch fall through to the case after them")
    assertEquals(found.owner, "demo.Switches")
    assertEquals(found.method, "demo.Switches.run")
  }

  test("a lock of a field that may be replaced while the code waits for it") {
    val model = compile("Locks" -> """
      |package demo;
      |
      |public class Locks {
      |
      |  private Object lock = new Object();
      |
      |  public void run() {
      |    synchronized (lock) {
      |      work();
      |    }
      |  }
      |
      |  private void work() {
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "synchronized-on-non-final")
    assertEquals(found.detail, "the lock is a field that may be replaced while the code waits for it")
    assertEquals(found.owner, "demo.Locks")
    assertEquals(found.method, "demo.Locks.run")
  }

  test("a lock of a field that is never replaced is a lock of one thing") {
    val model = compile("Locks" -> """
      |package demo;
      |
      |public class Locks {
      |
      |  private final Object lock = new Object();
      |
      |  public void run() {
      |    synchronized (lock) {
      |      work();
      |    }
      |  }
      |
      |  private void work() {
      |  }
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "synchronized-on-non-final"),
      s"the lock is never replaced: ${model.lints.mkString(", ")}"
    )
  }

  test("a wait that is not in a loop can miss the notification that wakes the thread") {
    val model = compile("Waiters" -> """
      |package demo;
      |
      |public class Waiters {
      |
      |  private final Object lock = new Object();
      |
      |  public void run() throws InterruptedException {
      |    synchronized (lock) {
      |      lock.wait();
      |    }
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "wait-not-in-loop")
    assertEquals(found.detail, "the wait is not in a loop, so it can miss the notification that wakes the thread")
    assertEquals(found.owner, "demo.Waiters")
    assertEquals(found.method, "demo.Waiters.run")
  }

  test("a wait in a loop cannot miss the notification that wakes the thread") {
    val model = compile("Waiters" -> """
      |package demo;
      |
      |public class Waiters {
      |
      |  private final Object lock = new Object();
      |
      |  private boolean ready;
      |
      |  public void run() throws InterruptedException {
      |    synchronized (lock) {
      |      while (!ready) {
      |        lock.wait();
      |      }
      |    }
      |  }
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "wait-not-in-loop"),
      s"the wait is in a loop: ${model.lints.mkString(", ")}"
    )
  }

  test("a text that is built anew on every turn of a loop") {
    val model = compile("Texts" -> """
      |package demo;
      |
      |import java.util.List;
      |
      |public class Texts {
      |  public String run(List<String> values) {
      |    String text = "";
      |    for (String value : values) {
      |      text = text + value;
      |    }
      |    return text;
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "string-concatenation-in-loop")
    assertEquals(found.detail, "the text is built anew on every turn of the loop")
    assertEquals(found.owner, "demo.Texts")
    assertEquals(found.method, "demo.Texts.run")
  }

  test("a text that is added to on every turn of a loop is built anew on every turn of it") {
    val model = compile("Texts" -> """
      |package demo;
      |
      |import java.util.List;
      |
      |public class Texts {
      |  public String run(List<String> values) {
      |    String text = "";
      |    for (String value : values) {
      |      text += value;
      |    }
      |    return text;
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "string-concatenation-in-loop")
    assertEquals(found.detail, "the text is built anew on every turn of the loop")
    assertEquals(found.owner, "demo.Texts")
    assertEquals(found.method, "demo.Texts.run")
  }

  test("a text that is built outside a loop is built once") {
    val model = compile("Texts" -> """
      |package demo;
      |
      |public class Texts {
      |  public String run(String value) {
      |    String text = "";
      |    text = text + value;
      |    return text;
      |  }
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "string-concatenation-in-loop"),
      s"the text is built once: ${model.lints.mkString(", ")}"
    )
  }

  test("the size of a collection that is asked for on every turn of a loop") {
    val model = compile("Sizes" -> """
      |package demo;
      |
      |import java.util.List;
      |
      |public class Sizes {
      |  public void run(List<String> values) {
      |    for (String value : values) {
      |      if (values.size() > 0) {
      |        work();
      |      }
      |    }
      |  }
      |
      |  private void work() {
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "collection-size-in-loop")
    assertEquals(found.detail, "the size of the collection is asked for on every turn of the loop")
    assertEquals(found.owner, "demo.Sizes")
    assertEquals(found.method, "demo.Sizes.run")
  }

  test("a value that is boxed on every turn of a loop") {
    val model = compile("Boxes" -> """
      |package demo;
      |
      |public class Boxes {
      |  public void run(int count) {
      |    for (int index = 0; index < count; index = index + 1) {
      |      Integer value = Integer.valueOf(index);
      |      work(value);
      |    }
      |  }
      |
      |  private void work(Integer value) {
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "boxing-in-loop")
    assertEquals(found.detail, "the value is boxed on every turn of the loop")
    assertEquals(found.owner, "demo.Boxes")
    assertEquals(found.method, "demo.Boxes.run")
  }

  test("a text that is built in a way that another thread may see half of") {
    val model = compile("Buffers" -> """
      |package demo;
      |
      |public class Buffers {
      |  public String run() {
      |    StringBuffer text = new StringBuffer();
      |    return text.toString();
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "string-buffer-instead-of-builder")
    assertEquals(found.detail, "the text is built in a way that another thread may see half of")
    assertEquals(found.owner, "demo.Buffers")
    assertEquals(found.method, "demo.Buffers.run")
  }

  test("a type that is serialized and does not name the version of its form") {
    val model = compile("Payloads" -> """
      |package demo;
      |
      |import java.io.Serializable;
      |
      |public class Payloads implements Serializable {
      |
      |  private String name;
      |}
      |""".stripMargin)

    val found = lint(model, "missing-serial-version-uid")
    assertEquals(found.detail, "the type is serialized and does not name the version of its form")
    assertEquals(found.owner, "demo.Payloads")
    assertEquals(found.method, "", "the lint is of the type, not of a method of it")
  }

  test("a type that is serialized and names the version of its form keeps it") {
    val model = compile("Payloads" -> """
      |package demo;
      |
      |import java.io.Serializable;
      |
      |public class Payloads implements Serializable {
      |
      |  private static final long serialVersionUID = 1L;
      |
      |  private String name;
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "missing-serial-version-uid"),
      s"the type names the version of its form: ${model.lints.mkString(", ")}"
    )
  }

  test("a qualifier of a member that the code around it already holds is of no use") {
    val model = compile("Widgets" -> """
      |package demo;
      |
      |public class Widgets {
      |
      |  private int count;
      |
      |  public int count() {
      |    return this.count;
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "redundant-this")
    assertEquals(found.detail, "the qualifier of count is not needed")
    assertEquals(found.owner, "demo.Widgets")
    assertEquals(found.method, "demo.Widgets.count")
  }

  test("a qualifier of a member that the code around it hides is needed") {
    val model = compile("Widgets" -> """
      |package demo;
      |
      |public class Widgets {
      |
      |  private int count;
      |
      |  public int count(int count) {
      |    return this.count + count;
      |  }
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "redundant-this"),
      s"the parameter hides the member: ${model.lints.mkString(", ")}"
    )
  }

  test("a qualifier the block writes around it is kept") {
    val model = compile("Widgets" -> """
      |package demo;
      |
      |public class Widgets {
      |
      |  private int count;
      |  private int other;
      |
      |  public Widgets(int count) {
      |    this.count = count;
      |    this.other = 1;
      |  }
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "redundant-this"),
      s"the block qualifies its members: ${model.lints.mkString(", ")}"
    )
  }

  test("a qualifier the block mostly does without is redundant") {
    val model = compile("Widgets" -> """
      |package demo;
      |
      |public class Widgets {
      |
      |  private int count;
      |  private int other;
      |  private int more;
      |
      |  public void run() {
      |    this.count = 1;
      |    other = 2;
      |    more = 3;
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "redundant-this")
    assertEquals(found.detail, "the qualifier of count is not needed")
  }

  test("a qualifier is compared with the members of its kind") {
    val model = compile("Widgets" -> """
      |package demo;
      |
      |public class Widgets {
      |  private int count;
      |  private int other;
      |
      |  public void run() {
      |    this.count = 1;
      |    other = 2;
      |    work();
      |    more();
      |    again();
      |  }
      |
      |  private void work() {}
      |  private void more() {}
      |  private void again() {}
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "redundant-this"),
      s"the fields of the block are split evenly: ${model.lints.mkString(", ")}"
    )
  }

  test("a qualifier of a block that is split almost evenly is kept") {
    val model = compile("Widgets" -> """
      |package demo;
      |
      |public class Widgets {
      |  private int a, b, c, d, e, f, g;
      |
      |  public void run() {
      |    this.a = 1;
      |    this.b = 2;
      |    this.c = 3;
      |    d = 4;
      |    e = 5;
      |    f = 6;
      |    g = 7;
      |  }
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "redundant-this"),
      s"the block is split almost evenly: ${model.lints.mkString(", ")}"
    )
  }

  test("a qualifier of a member of a block that names this elsewhere is kept") {
    val model = compile("Widgets" -> """
      |package demo;
      |
      |public class Widgets {
      |  private int count;
      |
      |  public int count(Object other) {
      |    if (this == other) return 0;
      |    return this.count;
      |  }
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "redundant-this"),
      s"the block names this elsewhere: ${model.lints.mkString(", ")}"
    )
  }

  test("a qualifier of a block that is split evenly is kept") {
    val model = compile("Widgets" -> """
      |package demo;
      |
      |public class Widgets {
      |
      |  private int count;
      |  private int other;
      |
      |  public void run() {
      |    this.count = 1;
      |    other = 2;
      |  }
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "redundant-this"),
      s"the block is split evenly: ${model.lints.mkString(", ")}"
    )
  }

  test("a file of two types holds each lint of them once") {
    val model = compile("Two" -> """
      |package demo;
      |
      |import java.util.List;
      |
      |public class Two {
      |  public String cast(String value) {
      |    return (String) value;
      |  }
      |}
      |
      |class Other {
      |  public String text(String value) {
      |    return value;
      |  }
      |}
      |""".stripMargin)

    val lints = model.lints.filter(_.id == "redundant-cast")
    assertEquals(lints.size, 1, s"the lint is held once: ${model.lints.mkString(", ")}")
    assertEquals(lints.head.owner, "demo.Two")
    // The lints of the file as a whole are asked once, whichever type the compiler attributes.
    val imports = model.lints.filter(_.id == "unused-import")
    assertEquals(imports.size, 1, s"the lint of the file is held once: ${model.lints.mkString(", ")}")
  }

  test("an if chain that compares one text with constants can be a switch") {
    val model = compile("Routes" -> """
      |package demo;
      |
      |public class Routes {
      |  public String route(String path) {
      |    if (path.equals("/a")) return "a";
      |    else if (path.equals("/b")) return "b";
      |    else if (path.equals("/c")) return "c";
      |    else return "other";
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "if-can-be-switch")
    assertEquals(found.detail, "the if chain on 'path' can be replaced with a switch statement")
    assertEquals(found.owner, "demo.Routes")
  }

  test("an if chain of two branches, or of different values, is not a switch") {
    val model = compile("Routes" -> """
      |package demo;
      |
      |public class Routes {
      |  public String route(String path, String other) {
      |    if (path.equals("/a")) return "a";
      |    else if (path.equals("/b")) return "b";
      |    else return "other";
      |  }
      |
      |  public String mixed(String path, String other) {
      |    if (path.equals("/a")) return "a";
      |    else if (other.equals("/b")) return "b";
      |    else if (path.equals("/c")) return "c";
      |    else return "other";
      |  }
      |}
      |""".stripMargin)

    assert(!model.lints.exists(_.id == "if-can-be-switch"), model.lints.mkString("\n"))
  }

  test("the branches of an if that hold the same code are reported") {
    val model = compile("Branches" -> """
      |package demo;
      |
      |public class Branches {
      |  private void work() {}
      |
      |  public void run(boolean flag) {
      |    if (flag) {
      |      work();
      |    } else {
      |      work();
      |    }
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "if-identical-branches")
    assertEquals(found.detail, "the branches of the if statement are identical")
  }

  test("the branches of an if that end with the same statement are reported") {
    val model = compile("Branches" -> """
      |package demo;
      |
      |public class Branches {
      |  private void work() {}
      |  private void other() {}
      |  private void common() {}
      |
      |  public void run(boolean flag) {
      |    if (flag) {
      |      work();
      |      common();
      |    } else {
      |      other();
      |      common();
      |    }
      |  }
      |}
      |""".stripMargin)

    val found = lint(model, "if-identical-branches")
    assertEquals(found.detail, "the branches of the if statement end with the same statement")
  }

  test("a field initializer that gives the value the field starts with is redundant") {
    val model = compile("Fields" -> """
      |package demo;
      |
      |public class Fields {
      |  private int width = 0;
      |  private boolean ready = false;
      |  private Object value = null;
      |  private final int fixed = 0;
      |  private String text = "x";
      |
      |  public int read() {
      |    int local = 0;
      |    return width + local + fixed + text.length();
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "unused-assignment").map(_.detail).sorted
    assertEquals(
      found,
      Vector(
        "the initializer 0 of the field width is redundant",
        "the initializer false of the field ready is redundant",
        "the initializer null of the field value is redundant"
      )
    )
  }

  test("the initializer of a field of the state of the type is not redundant, as it is required") {
    val model = compile("Fields" -> """
      |package demo;
      |
      |public class Fields {
      |  private static int counter = 0;
      |  private volatile boolean started = false;
      |  private static Object cache = null;
      |  private int width = 0;
      |  private static long total;
      |}
      |""".stripMargin)

    assertEquals(
      model.lints.filter(_.id == "unused-assignment").map(_.detail).sorted,
      Vector(
        "the initializer 0 of the field width is redundant",
        "the initializer null of the field cache is redundant"
      ).sorted
    )
    assertEquals(model.lints.filter(_.id == "missing-field-initializer").size, 1, model.lints.mkString(", "))
  }

  test("an if that returns one boolean and then the other is trivial") {
    val model = compile("Checks" -> """
      |package demo;
      |
      |public class Checks {
      |  public boolean ok(boolean flag) {
      |    if (flag) return false;
      |    return true;
      |  }
      |
      |  public boolean same(boolean flag) {
      |    if (flag) return true;
      |    else return false;
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "trivial-if")
    assertEquals(found.size, 2, model.lints.mkString("\n"))
  }

  test("a return at the end of a void method is unnecessary") {
    val model = compile("Returns" -> """
      |package demo;
      |
      |public class Returns {
      |  private void work() {}
      |
      |  public void last() {
      |    work();
      |    return;
      |  }
      |
      |  public void notLast(boolean flag) {
      |    if (flag) return;
      |    work();
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "unnecessary-return")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
    assertEquals(found.head.detail, "the return is unnecessary as the last statement of a void method")
    assertEquals(found.head.method, "demo.Returns.last")
  }

  test("an arithmetic expression with the identity of its operator is pointless") {
    val model = compile("Arithmetic" -> """
      |package demo;
      |
      |public class Arithmetic {
      |  public int size(int value) {
      |    return 0 + value;
      |  }
      |
      |  public int scale(int value) {
      |    return value * 1;
      |  }
      |
      |  public int same(int value) {
      |    return value - 0;
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "pointless-arithmetic").map(_.detail)
    assertEquals(found.size, 3, model.lints.mkString("\n"))
    assert(found.exists(_.contains("0 + value")), found.mkString("\n"))
    assert(found.exists(_.contains("value * 1")), found.mkString("\n"))
    assert(found.exists(_.contains("value - 0")), found.mkString("\n"))
  }

  test("an arithmetic expression that keeps a text or an operand is not pointless") {
    val model = compile("Arithmetic" -> """
      |package demo;
      |
      |public class Arithmetic {
      |  public String text(int value) {
      |    return 0 + "" + value;
      |  }
      |
      |  public int value(int value) {
      |    return value + 1;
      |  }
      |}
      |""".stripMargin)

    assert(!model.lints.exists(_.id == "pointless-arithmetic"), model.lints.mkString("\n"))
  }

  test("a boxed boolean compared with true is pointless") {
    val model = compile("Booleans" -> """
      |package demo;
      |
      |@org.jspecify.annotations.NullMarked
      |public class Booleans {
      |  public boolean ok(Boolean value) {
      |    return Boolean.TRUE.equals(value);
      |  }
      |
      |  public boolean off(Boolean value) {
      |    return Boolean.FALSE.equals(value);
      |  }
      |
      |  public boolean primitive(boolean value) {
      |    return Boolean.TRUE.equals(value);
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "pointless-boolean").map(_.detail)
    assertEquals(found.size, 3, model.lints.mkString("\n"))
    assert(found.exists(_.contains("can be simplified to value")), found.mkString("\n"))
    assert(found.exists(_.contains("can be simplified to !value")), found.mkString("\n"))
  }

  test("boxed boolean equals on nullable or unknown value is not pointless") {
    val model = compile("Booleans" -> """
      |package demo;
      |
      |import java.util.concurrent.atomic.AtomicReference;
      |import org.jspecify.annotations.Nullable;
      |
      |public class Booleans {
      |  public boolean checkNullable(@Nullable Boolean value) {
      |    return Boolean.TRUE.equals(value);
      |  }
      |
      |  public boolean checkUnknown(AtomicReference<Boolean> ref) {
      |    return Boolean.TRUE.equals(ref.get());
      |  }
      |
      |  public boolean checkUnannotated(Boolean value) {
      |    return Boolean.TRUE.equals(value);
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "pointless-boolean")
    assertEquals(found.size, 0, model.lints.mkString("\n"))
  }

  test("a boolean expression that says something is not pointless") {
    val model = compile("Booleans" -> """
      |package demo;
      |
      |public class Booleans {
      |  public boolean both(boolean left, boolean right) {
      |    return left && right;
      |  }
      |
      |  public boolean value(boolean value) {
      |    return value == true;
      |  }
      |}
      |""".stripMargin)

    // `value == true` is pointless, but `left && right` is not.
    val found = model.lints.filter(_.id == "pointless-boolean")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
  }

  test("a field that is only used in one method can be a local variable") {
    val model = compile("Fields" -> """
      |package demo;
      |
      |public class Fields {
      |  private int local;
      |  private int shared;
      |
      |  public void first() {
      |    local = 1;
      |    System.out.println(local);
      |    shared = 1;
      |  }
      |
      |  public void second() {
      |    System.out.println(shared);
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "field-can-be-local")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
    assert(found.head.detail.contains("field local"), found.head.detail)
  }

  test("a field read in one method but assigned in constructor or read before write cannot be local") {
    val model = compile(
      "Delegating" -> """
        |package demo;
        |
        |public class Delegating {
        |  private String delegates;
        |  public Delegating(String delegates) {
        |    this.delegates = delegates;
        |  }
        |  public void onComplete() {
        |    System.out.println(delegates);
        |  }
        |}
        |""".stripMargin,
      "OnlyRead" -> """
        |package demo;
        |
        |public class OnlyRead {
        |  private String state;
        |  public void read() {
        |    System.out.println(state);
        |  }
        |}
        |""".stripMargin,
      "Conditional" -> """
        |package demo;
        |
        |public class Conditional {
        |  private int state;
        |  public void run(boolean flag) {
        |    if (flag) state = 1;
        |    System.out.println(state);
        |  }
        |}
        |""".stripMargin
    )

    val found = model.lints.filter(_.id == "field-can-be-local")
    assertEquals(found.size, 0, model.lints.mkString("\n"))
  }

  test("a catch parameter named as ignored that the block reads is reported") {
    val model = compile("Caught" -> """
      |package demo;
      |
      |public class Caught {
      |  public void run() {
      |    try {
      |      work();
      |    } catch (RuntimeException ignored) {
      |      ignored.toString();
      |    }
      |    try {
      |      work();
      |    } catch (RuntimeException ignored) {
      |    }
      |  }
      |
      |  private void work() {}
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "catch-may-ignore-exception")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
  }

  test("a catch parameter named as ignored is not read when only a same-named field is") {
    val model = compile("Ignored" -> """
      |package demo;
      |
      |public class Ignored {
      |  public void run(String value) {
      |    try {
      |      Integer.parseInt(value);
      |    } catch (RuntimeException ignored) {
      |      Runnable inner = new Runnable() {
      |        private final String ignored = "unused";
      |
      |        public void run() {
      |          System.out.println(ignored);
      |        }
      |      };
      |      inner.run();
      |    }
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id == "catch-may-ignore-exception").size, 0, model.lints.mkString("\n"))
  }

  test("an Optional.get without a presence check is reported, and a guarded one is not") {
    val model = compile("Optionals" -> """
      |package demo;
      |
      |import java.util.Optional;
      |
      |public class Optionals {
      |  public String unguarded(Optional<String> value) {
      |    return value.get();
      |  }
      |
      |  public String guarded(Optional<String> value) {
      |    if (value.isPresent()) return value.get();
      |    return "none";
      |  }
      |
      |  public String early(Optional<String> value) {
      |    if (!value.isPresent()) return "none";
      |    return value.get();
      |  }
      |}
      |""".stripMargin)

    val gets = model.lints.filter(_.id == "optional-get")
    assertEquals(gets.size, 1, model.lints.mkString("\n"))
    assert(gets.head.detail.contains("value.get()"), gets.head.detail)
  }

  test("an isPresent branch that calls get is reported as a functional expression") {
    val model = compile("Optionals" -> """
      |package demo;
      |
      |import java.util.Optional;
      |
      |public class Optionals {
      |  public String guarded(Optional<String> value) {
      |    if (value.isPresent()) return value.get();
      |    return "none";
      |  }
      |
      |  public String absent(Optional<String> value) {
      |    if (!value.isPresent()) return "none";
      |    return value.get();
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "optional-is-present")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
  }

  test("a nullable value used where a non-null one is required is reported unless a check proves it") {
    val model = compile(
      "NullableBoundary" -> "package demo; public @interface NullableBoundary { String value(); }",
      "package-info"     -> "@org.jspecify.annotations.NullMarked package demo;",
      "Nullness"         ->
        """package demo;
          |
          |import java.util.Objects;
          |import org.jspecify.annotations.Nullable;
          |
          |public class Nullness {
          |  @Nullable static String maybe() {
          |    return null;
          |  }
          |
          |  @NullableBoundary("futures may complete with null")
          |  static <T> T await() {
          |    return null;
          |  }
          |
          |  static String unquote(String value) {
          |    return value;
          |  }
          |
          |  static String direct() {
          |    return unquote(maybe());
          |  }
          |
          |  static String checked() {
          |    return unquote(Objects.requireNonNull(maybe()));
          |  }
          |
          |  static String guarded() {
          |    String value = maybe();
          |    if (value == null) return "";
          |    return unquote(value);
          |  }
          |
          |  static String branch() {
          |    String value = maybe();
          |    if (value != null) return unquote(value);
          |    return "";
          |  }
          |
          |  static String awaited() {
          |    return unquote(await());
          |  }
          |
          |  static int receiver() {
          |    return maybe().length();
          |  }
          |}
          |""".stripMargin
    )

    val found = model.lints.filter(_.id == "nullable-use")
    assertEquals(found.size, 3, model.lints.mkString("\n"))
    assert(found.exists(_.detail.contains("maybe()")), found.mkString("\n"))
    assert(found.exists(_.detail.contains("await()")), found.mkString("\n"))
  }

  test("an override that only calls the method it overrides is redundant") {
    val model = compile("Overrides" -> """
      |package demo;
      |
      |public class Overrides {
      |  static class Parent {
      |    protected boolean mayBlock() {
      |      return false;
      |    }
      |
      |    protected void work(int value) {}
      |  }
      |
      |  static class Child extends Parent {
      |    @Override
      |    protected boolean mayBlock() {
      |      return super.mayBlock();
      |    }
      |
      |    @Override
      |    protected void work(int value) {
      |      super.work(value);
      |    }
      |
      |    protected void other(int value) {
      |      super.work(value + 1);
      |    }
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "redundant-method-override")
    assertEquals(found.size, 2, model.lints.mkString("\n"))
  }

  test("a checked exception a method never throws is redundant") {
    val model = compile("Throws" -> """
      |package demo;
      |
      |import java.io.IOException;
      |
      |public class Throws {
      |  public void fine() throws IOException {
      |    throw new IOException("failure");
      |  }
      |
      |  private void redundant() throws IOException {
      |    System.out.println("no failure");
      |  }
      |
      |  public void unchecked() throws IllegalArgumentException {}
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "redundant-throws")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
    assert(found.head.detail.contains("IOException"), found.head.detail)
  }

  test("a checked exception that callers outside the compilation can rely on is not redundant") {
    val model = compile("Api" -> """
      |package demo;
      |
      |import java.io.IOException;
      |
      |public class Api {
      |  public void open() throws IOException {
      |    System.out.println("open");
      |  }
      |
      |  public final void sealed() throws IOException {
      |    System.out.println("sealed");
      |  }
      |
      |  public static void shared() throws IOException {
      |    System.out.println("shared");
      |  }
      |
      |  void local() throws IOException {
      |    System.out.println("local");
      |  }
      |
      |  private static final class Impl implements AutoCloseable {
      |    @Override
      |    public void close() throws Exception {
      |      System.out.println("close");
      |    }
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id == "redundant-throws"), Vector.empty, model.lints.mkString("\n"))
  }

  test("a checked exception that only the file can see is redundant when the method never throws it") {
    val model = compile("Inside" -> """
      |package demo;
      |
      |import java.io.IOException;
      |
      |public class Inside {
      |  private void hidden() throws IOException {
      |    System.out.println("hidden");
      |  }
      |
      |  private static void shared() throws IOException {
      |    System.out.println("shared");
      |  }
      |
      |  private static final class Impl {
      |    void run() throws IOException {
      |      System.out.println("run");
      |    }
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "redundant-throws").map(_.method).sorted
    assertEquals(
      found,
      Vector("demo.Inside.Impl.run", "demo.Inside.hidden", "demo.Inside.shared"),
      model.lints.mkString("\n")
    )
  }

  test("a Thread.sleep in a loop is a busy wait") {
    val model = compile("Waiting" -> """
      |package demo;
      |
      |public class Waiting {
      |  public void poll(boolean[] ready) throws Exception {
      |    while (!ready[0]) {
      |      Thread.sleep(5);
      |    }
      |  }
      |
      |  public void once() throws Exception {
      |    Thread.sleep(5);
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "busy-wait")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
    assertEquals(found.head.method, "demo.Waiting.poll")
  }

  test("a field or local that is never assigned again can be final") {
    val model = compile("Fields" -> """
      |package demo;
      |
      |public class Fields {
      |  private int kept = 1;
      |  private int changed = 1;
      |  private final int fixed = 1;
      |
      |  public void run() {
      |    int local = 1;
      |    int reassigned = 1;
      |    reassigned = 2;
      |    changed = 2;
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "can-be-final").map(_.detail).sorted
    assertEquals(
      found,
      Vector(
        "the field kept can be final",
        "the local variable local can be final"
      )
    )
  }

  test("a field written through a qualifier of the enclosing class or a lambda is not reported") {
    val model = compile("Tabs" -> """
      |package demo;
      |
      |public class Tabs {
      |  private static final class Entry {
      |    private String title = "New Tab";
      |    private int visits = 0;
      |    private int kept = 1;
      |  }
      |
      |  private final Entry entry = new Entry();
      |
      |  public void run(String value) {
      |    entry.title = value;
      |    final Runnable visit = () -> entry.visits++;
      |    visit.run();
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "can-be-final").map(_.detail).sorted
    assertEquals(found, Vector("the field kept can be final"), model.lints.mkString("\n"))
  }

  test("a field written by a compound assignment through a qualifier of a sibling or outer class is not reported") {
    val model = compile("Counters" -> """
      |package demo;
      |
      |public class Counters {
      |  private int total = 0;
      |  private int hits = 0;
      |  private int misses = 0;
      |  private int kept = 1;
      |
      |  static final class Sibling {
      |    void add(Counters counters) {
      |      counters.total += 2;
      |    }
      |  }
      |
      |  class Inner {
      |    void record() {
      |      Counters.this.misses++;
      |      Counters.this.hits = 1;
      |    }
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "can-be-final").map(_.detail).sorted
    assertEquals(found, Vector("the field kept can be final"), model.lints.mkString("\n"))
  }

  test("a field that only another file can write is not reported as able to be final") {
    val model = compile(
      "Shared" -> """
        |package demo;
        |
        |public class Shared {
        |  int count = 0;
        |  protected int level = 0;
        |  public int open = 0;
        |  private int kept = 1;
        |}
        |""".stripMargin,
      "Writer" -> """
        |package demo;
        |
        |class Writer {
        |  void run(Shared shared) {
        |    shared.count = 1;
        |    shared.level++;
        |    shared.open += 2;
        |  }
        |}
        |""".stripMargin
    )

    val found = model.lints.filter(_.id == "can-be-final").map(_.detail).sorted
    assertEquals(found, Vector("the field kept can be final"), model.lints.mkString("\n"))
  }

  private def finalisable(members: String, access: String): Vector[String] =
    compile("Case" -> Constructed.source(members, access)).lints.collect {
      case lint if lint.id == "can-be-final" && lint.detail.startsWith("the field") => lint.detail
    }

  for (name, members) <- Constructed.assigned :+ Constructed.annotated do
    test(s"a private field that every constructor assigns once can be final: $name") {
      assertEquals(finalisable(members, "private"), Vector("the field value can be final"))
    }

  for (name, members) <- Constructed.rejected ++ Constructed.repeated ++ Constructed.written ++ Constructed.excluded do
    test(s"a private field that some constructor leaves open or assigns again cannot be final: $name") {
      assertEquals(finalisable(members, "private"), Vector.empty)
    }

  test("a cast a narrower variable type makes unnecessary is reported") {
    val model = compile("Casts" -> """
      |package demo;
      |
      |public class Casts {
      |  interface Base {}
      |
      |  static class Impl implements Base {
      |    void work() {}
      |  }
      |
      |  public void run() {
      |    Base value = new Impl();
      |    ((Impl) value).work();
      |  }
      |
      |  public void other(Base value) {
      |    ((Impl) value).work();
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "cast-narrowing-variable")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
    assertEquals(
      found.head.detail,
      "the cast to demo.Casts.Impl can be removed by declaring value as demo.Casts.Impl (verified by the earlier tree check)"
    )
  }

  test("a standard charset is used instead of a name or Charset.forName") {
    val model = compile("Charsets" -> """
      |package demo;
      |
      |import java.io.ByteArrayOutputStream;
      |import java.nio.charset.Charset;
      |import java.nio.charset.StandardCharsets;
      |
      |public class Charsets {
      |  public Charset forName() {
      |    return Charset.forName("UTF-8");
      |  }
      |
      |  public String decode(ByteArrayOutputStream bytes) throws Exception {
      |    return bytes.toString(StandardCharsets.US_ASCII.name());
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "standard-charset")
    assertEquals(found.size, 2, model.lints.mkString("\n"))
  }
  test("explicit constructor type arguments that the target type gives can be a diamond") {
    val model = compile("Diamonds" -> """
      |package demo;
      |
      |public class Diamonds {
      |  static class Box<T> {
      |    Box() {}
      |  }
      |
      |  public Box<String> make() {
      |    Box<String> box = new Box<String>();
      |    return box;
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "convert-2-diamond")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
  }

  test("a system property a standard constant names is reported") {
    val model = compile("Properties" -> """
      |package demo;
      |
      |public class Properties {
      |  public String separator() {
      |    return System.getProperty("file.separator");
      |  }
      |
      |  public String version() {
      |    return System.getProperty("java.version");
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "system-get-property")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
  }

  test("a thread built without a task is reported") {
    val model = compile("Threads" -> """
      |package demo;
      |
      |public class Threads {
      |  public Thread taskless() {
      |    return new Thread("taskless");
      |  }
      |
      |  public Thread task() {
      |    return new Thread(() -> {}, "task");
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "thread-default-run")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
  }

  test("a loop that fills an array with one value is reported") {
    val model = compile("Filling" -> """
      |package demo;
      |
      |public class Filling {
      |  public int[] values() {
      |    int[] values = new int[3];
      |    for (int i = 0; i < values.length; i++) {
      |      values[i] = -1;
      |    }
      |    return values;
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "explicit-array-filling")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
  }

  test("an unmodifiable copy of a collection is reported") {
    val model = compile("Copies" -> """
      |package demo;
      |
      |import java.util.ArrayList;
      |import java.util.Collections;
      |import java.util.List;
      |
      |public class Copies {
      |  public List<String> copy(List<String> values) {
      |    return Collections.unmodifiableList(new ArrayList<>(values));
      |  }
      |
      |  public List<String> same(List<String> values) {
      |    return Collections.unmodifiableList(values);
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "java9-collection-factory")
    assertEquals(
      found.map(_.detail),
      Vector("use List.copyOf instead of unmodifiableList of a copy: List.copyOf(values)")
    )
  }

  test("an unmodifiable copy into an unordered set is reported with the factory of the set") {
    val model = compile("Copies" -> """
      |package demo;
      |
      |import java.util.Collections;
      |import java.util.HashSet;
      |import java.util.Set;
      |
      |public class Copies {
      |  public Set<String> copy(Set<String> values) {
      |    return Collections.unmodifiableSet(new HashSet<>(values));
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      model.lints.filter(_.id == "java9-collection-factory").map(_.detail),
      Vector("use Set.copyOf instead of unmodifiableSet of a copy: Set.copyOf(values)")
    )
  }

  test("an unmodifiable copy that keeps the order of its set or map is not reported") {
    val model = compile("Copies" -> """
      |package demo;
      |
      |import java.util.Collections;
      |import java.util.LinkedHashMap;
      |import java.util.LinkedHashSet;
      |import java.util.Map;
      |import java.util.Set;
      |import java.util.TreeMap;
      |import java.util.TreeSet;
      |
      |public class Copies {
      |  public Set<String> linked(Set<String> values) {
      |    return Collections.unmodifiableSet(new LinkedHashSet<>(values));
      |  }
      |
      |  public Set<String> sorted(Set<String> values) {
      |    return Collections.unmodifiableSet(new TreeSet<>(values));
      |  }
      |
      |  public Map<String, String> linkedMap(Map<String, String> values) {
      |    return Collections.unmodifiableMap(new LinkedHashMap<>(values));
      |  }
      |
      |  public Map<String, String> sortedMap(Map<String, String> values) {
      |    return Collections.unmodifiableMap(new TreeMap<>(values));
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      model.lints.filter(_.id == "java9-collection-factory").map(_.method),
      Vector.empty,
      s"the copies keep their order: ${model.lints.mkString(", ")}"
    )
  }

  test("a primitive array passed to a varargs method is reported") {
    val model = compile("Varargs" -> """
      |package demo;
      |
      |import java.lang.reflect.Method;
      |
      |public class Varargs {
      |  public Object call(Method method, byte[] value) throws Exception {
      |    return method.invoke(null, value);
      |  }
      |
      |  public Object explicit(Method method, byte[] value) throws Exception {
      |    return method.invoke(null, (Object) value);
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "primitive-array-varargs")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
  }

  test("a negated optional presence check is reported") {
    val model = compile("Presence" -> """
      |package demo;
      |
      |import java.util.Optional;
      |
      |public class Presence {
      |  public boolean absent(Optional<String> value) {
      |    return !value.isPresent();
      |  }
      |
      |  public boolean present(Optional<String> value) {
      |    return !value.isEmpty();
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "none-check")
    assertEquals(
      found.map(_.detail),
      Vector(
        "use isEmpty() instead of !isPresent(): value.isEmpty()",
        "use isPresent() instead of !isEmpty(): value.isPresent()"
      ),
      model.lints.mkString("\n")
    )
  }

  test("a call whose result says whether it worked is reported when the result is dropped") {
    val model = compile("Results" -> """
      |package demo;
      |
      |import java.io.InputStream;
      |import java.util.concurrent.CountDownLatch;
      |
      |public class Results {
      |  public void read(InputStream input) throws Exception {
      |    input.read();
      |  }
      |
      |  public void await(CountDownLatch latch) throws Exception {
      |    latch.await(1, java.util.concurrent.TimeUnit.SECONDS);
      |  }
      |
      |  public int used(InputStream input) throws Exception {
      |    return input.read();
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "ignored-call-result")
    assertEquals(found.size, 2, model.lints.mkString("\n"))
  }

  test("a serialVersionUID field can be annotated with @Serial") {
    val model = compile("Serialized" -> """
      |package demo;
      |
      |import java.io.Serial;
      |
      |public class Serialized implements java.io.Serializable {
      |  private static final long serialVersionUID = 1L;
      |
      |  @Serial
      |  private static final long other = 1L;
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "missing-serial-annotation")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
    assertEquals(found.head.detail, "the serialVersionUID field can be annotated with @Serial")
  }

  test("an empty method is reported") {
    val model = compile("Empties" -> """
      |package demo;
      |
      |public class Empties {
      |  private void empty() {}
      |
      |  public void used() {
      |    System.out.println("used");
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "empty-method")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
  }

  test("an empty method that other code can override is not reported") {
    val model = compile("Hooks" -> """
      |package demo;
      |
      |public class Hooks {
      |  public void open() {}
      |
      |  protected void hook() {}
      |
      |  void local() {}
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id == "empty-method"), Vector.empty, model.lints.mkString("\n"))
  }

  test("an empty method that nothing can override is reported") {
    val model = compile("Closed" -> """
      |package demo;
      |
      |public class Closed {
      |  public void open() {}
      |
      |  private void hidden() {}
      |
      |  public final void sealed() {}
      |
      |  public static void shared() {}
      |
      |  private static class Inner {
      |    void run() {}
      |  }
      |
      |  static final class Done {
      |    void run() {}
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "empty-method").map(_.method).sorted
    assertEquals(
      found,
      Vector(
        "demo.Closed.Done.run",
        "demo.Closed.Inner.run",
        "demo.Closed.hidden",
        "demo.Closed.sealed",
        "demo.Closed.shared"
      ),
      model.lints.mkString("\n")
    )
  }

  test("a statement with an empty body is reported") {
    val model = compile("Bodies" -> """
      |package demo;
      |
      |public class Bodies {
      |  public void waitFor(boolean ready) {
      |    while (ready) ;
      |    if (ready) ;
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "empty-statement-body")
    assertEquals(found.size, 2, model.lints.mkString("\n"))
  }

  test("a size compared with zero is reported") {
    val model = compile("Sizes" -> """
      |package demo;
      |
      |import java.util.List;
      |
      |public class Sizes {
      |  public boolean empty(List<String> values) {
      |    return values.size() == 0;
      |  }
      |
      |  public boolean present(List<String> values) {
      |    return values.size() > 0;
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      model.lints.filter(lint => lint.id.startsWith("size-to-")).map(lint => (lint.id, lint.method, lint.detail)),
      Vector(
        (
          "size-to-is-empty",
          "demo.Sizes.empty",
          "use isEmpty() instead of a size comparison with zero: values.isEmpty()"
        ),
        (
          "size-to-non-empty",
          "demo.Sizes.present",
          "use !isEmpty() instead of a size comparison with zero: !values.isEmpty()"
        )
      ),
      model.lints.mkString("\n")
    )
  }

  test("a size compared with zero is reported on maps and sets, and not on a count or a size that means more") {
    val model = compile("Sizes" -> """
      |package demo;
      |
      |import java.util.Map;
      |import java.util.Set;
      |
      |public class Sizes {
      |  public boolean map(Map<String, String> values) {
      |    return values.size() != 0;
      |  }
      |
      |  public boolean set(Set<String> values) {
      |    return 0 == values.size();
      |  }
      |
      |  public boolean two(Set<String> values) {
      |    return values.size() == 2;
      |  }
      |
      |  public boolean stream(java.util.stream.Stream<String> values) {
      |    return values.count() == 0;
      |  }
      |
      |  public boolean text(String value) {
      |    return value.length() == 0;
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      model.lints.filter(lint => lint.id.startsWith("size-to-")).map(lint => (lint.id, lint.method)),
      Vector(("size-to-non-empty", "demo.Sizes.map"), ("size-to-is-empty", "demo.Sizes.set")),
      model.lints.mkString("\n")
    )
  }

  test("a size compared with zero of a collection that overrides the size is not reported") {
    val model = compile("Sizes" -> """
      |package demo;
      |
      |import java.util.ArrayList;
      |
      |public class Sizes {
      |  static class Counted extends ArrayList<String> {
      |    @Override public int size() {
      |      return 0;
      |    }
      |  }
      |
      |  static class Plain extends ArrayList<String> {}
      |
      |  public boolean counted(Counted values) {
      |    return values.size() == 0;
      |  }
      |
      |  public boolean plain(Plain values) {
      |    return values.size() == 0;
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id == "size-to-is-empty").map(_.method), Vector("demo.Sizes.plain"))
  }

  test("a size compared with zero in the isEmpty of a collection is not reported") {
    val model = compile("Sizes" -> """
      |package demo;
      |
      |import java.util.ArrayList;
      |
      |public class Sizes extends ArrayList<String> {
      |  @Override public boolean isEmpty() {
      |    return super.size() == 0;
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id == "size-to-is-empty"), Vector.empty)
  }

  test("a size compared with zero in generated code is not reported") {
    val model = compile("Sizes" -> """
      |package demo;
      |
      |import java.util.List;
      |
      |@javax.annotation.processing.Generated("tool")
      |public class Sizes {
      |  public boolean empty(List<String> values) {
      |    return values.size() == 0;
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id == "size-to-is-empty"), Vector.empty)
  }

  test("the laws of a chain are suppressed by their ids") {
    val model = compile("Sizes" -> """
      |package demo;
      |
      |import java.util.List;
      |
      |public class Sizes {
      |  @SuppressWarnings("bonsai:size-to-is-empty")
      |  public boolean empty(List<String> values) {
      |    return values.size() == 0;
      |  }
      |
      |  @SuppressWarnings("SizeReplaceableByIsEmpty")
      |  public boolean named(List<String> values) {
      |    return values.size() == 0;
      |  }
      |
      |  public boolean loud(List<String> values) {
      |    return values.size() == 0;
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id == "size-to-is-empty").map(_.method), Vector("demo.Sizes.loud"))
  }

  test("the first match of a stream checked for presence is reported as a match") {
    val model = compile("Matches" -> """
      |package demo;
      |
      |import java.util.List;
      |
      |public class Matches {
      |  public boolean has(List<String> values) {
      |    return values.stream().filter(v -> v.isEmpty()).findFirst().isPresent();
      |  }
      |
      |  public boolean lacks(List<String> values) {
      |    return values.stream().filter(v -> v.isEmpty()).findAny().isEmpty();
      |  }
      |
      |  public boolean kept(List<String> values) {
      |    return values.stream().filter(v -> v.isEmpty()).findFirst().isPresent() && values.isEmpty();
      |  }
      |
      |  public String first(List<String> values) {
      |    return values.stream().filter(v -> v.isEmpty()).findFirst().orElse("");
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      model.lints.filter(_.id == "find-to-exists").map(lint => lint.method -> lint.detail),
      Vector(
        "demo.Matches.has" ->
          "use anyMatch for filter, findFirst and isPresent: values.stream().anyMatch((v)->v.isEmpty())",
        "demo.Matches.lacks" ->
          "use noneMatch for filter, findFirst and isEmpty: values.stream().noneMatch((v)->v.isEmpty())",
        "demo.Matches.kept" ->
          "use anyMatch for filter, findFirst and isPresent: values.stream().anyMatch((v)->v.isEmpty())"
      ),
      model.lints.mkString("\n")
    )
  }

  test("a filtered count compared with zero is reported unless the predicate has an effect") {
    val model = compile("Counts" -> """
      |package demo;
      |
      |import java.util.List;
      |
      |public class Counts {
      |  private int seen;
      |
      |  public boolean none(List<String> values) {
      |    return values.stream().filter(v -> v.length() > 3).count() == 0;
      |  }
      |
      |  public boolean pure(List<Integer> values) {
      |    return values.stream().filter(v -> v > 3).count() == 0;
      |  }
      |
      |  public boolean any(List<Integer> values) {
      |    return values.stream().filter(v -> v > 3).count() > 0;
      |  }
      |
      |  public boolean effect(List<Integer> values) {
      |    return values.stream().filter(v -> seen++ > 3).count() == 0;
      |  }
      |
      |  public boolean impure(List<String> values) {
      |    return values.stream().filter(v -> check(v)).count() == 0;
      |  }
      |
      |  private boolean check(String value) {
      |    return true;
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      model.lints.filter(lint => lint.id.startsWith("size-to-")).map(lint => (lint.id, lint.method)),
      Vector(
        ("size-to-is-empty", "demo.Counts.pure"),
        ("size-to-non-empty", "demo.Counts.any")
      ),
      model.lints.mkString("\n")
    )
  }

  test("a sort followed by the first element is reported as a minimum") {
    val model = compile("Minimums" -> """
      |package demo;
      |
      |import java.util.Comparator;
      |import java.util.List;
      |import java.util.Optional;
      |
      |public class Minimums {
      |  public Optional<String> ordered(List<String> values, Comparator<String> order) {
      |    return values.stream().sorted(order).findFirst();
      |  }
      |
      |  public Optional<String> natural(List<String> values) {
      |    return values.stream().sorted().findFirst();
      |  }
      |
      |  public List<String> all(List<String> values) {
      |    return values.stream().sorted().toList();
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      model.lints.filter(_.id == "sorted-min-max").map(lint => lint.method -> lint.detail),
      Vector(
        "demo.Minimums.ordered" ->
          "sorted then findFirst sorts every element; min compares in one pass: values.stream().min(order)",
        "demo.Minimums.natural" ->
          "sorted then findFirst sorts every element; min compares in one pass: values.stream().min(Comparator.naturalOrder())"
      ),
      model.lints.mkString("\n")
    )
  }

  test("the chains of types that only share the names of a collection are not reported") {
    val model = compile("Mine" -> """
      |package demo;
      |
      |import java.util.function.Predicate;
      |
      |public class Mine {
      |  static class Things {
      |    Things filter(Predicate<String> predicate) { return this; }
      |    long count() { return 0; }
      |    int size() { return 0; }
      |    boolean isPresent() { return true; }
      |    Things findFirst() { return this; }
      |  }
      |
      |  public boolean run(Things things) {
      |    return things.filter(v -> true).count() == 0 || things.size() == 0 || things.filter(v -> true).findFirst().isPresent();
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      model.lints.filter(lint => lint.id.startsWith("size-to-") || lint.id == "find-to-exists"),
      Vector.empty
    )
  }

  test("the ids the chain lint reports are the laws that have a Java form") {
    val model = compile("Many" -> """
      |package demo;
      |
      |import java.util.ArrayList;
      |import java.util.Arrays;
      |import java.util.Collections;
      |import java.util.List;
      |import java.util.Optional;
      |import java.util.stream.Collectors;
      |
      |public class Many {
      |  public Object run(List<String> values, Optional<String> value) {
      |    return List.of(
      |        values.size() == 0,
      |        Arrays.asList("a").stream().count(),
      |        values.stream().collect(Collectors.joining(",")),
      |        Collections.unmodifiableList(new ArrayList<>(values)),
      |        !value.isPresent(),
      |        values.stream().sorted().findFirst(),
      |        values.stream().filter(v -> v.isEmpty()).findFirst().isPresent());
      |  }
      |}
      |""".stripMargin)

    val ids = model.lints.map(_.id).toSet - "optional-signature"
    assertEquals(
      ids,
      Set(
        "size-to-is-empty",
        "stream-of",
        "stream-join",
        "java9-collection-factory",
        "none-check",
        "sorted-min-max",
        "find-to-exists"
      )
    )
    assert(ids.subsetOf(LintId.chains.toSet), ids.mkString(", "))
  }

  test("an unnecessary unboxing is reported") {
    val model = compile("Unboxing" -> """
      |package demo;
      |
      |public class Unboxing {
      |  public boolean nonZero(Integer value) {
      |    return value.intValue() != 0;
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "unnecessary-unboxing")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
  }

  test("a string read from a file can use Files.readString") {
    val model = compile("Reads" -> """
      |package demo;
      |
      |import java.io.IOException;
      |import java.nio.charset.StandardCharsets;
      |import java.nio.file.Files;
      |import java.nio.file.Path;
      |
      |public class Reads {
      |  public String read(Path path) throws IOException {
      |    return new String(Files.readAllBytes(path), StandardCharsets.UTF_8);
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "read-string")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
  }

  test("a boolean-to-string conditional is reported") {
    val model = compile("Conditionals" -> """
      |package demo;
      |
      |public class Conditionals {
      |  public String text(boolean value) {
      |    return value ? "true" : "false";
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "simplifiable-conditional")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
  }

  test("a method that always returns the same literal is reported") {
    val model = compile("Returns" -> """
      |package demo;
      |
      |public class Returns {
      |  private boolean always() {
      |    return false;
      |  }
      |
      |  public boolean varies(boolean value) {
      |    return value;
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "same-return-value")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
  }

  test("a method that returns the same literal and other code can override is not reported") {
    val model = compile("Defaults" -> """
      |package demo;
      |
      |public class Defaults {
      |  public int open() {
      |    return 1;
      |  }
      |
      |  protected boolean hook() {
      |    return false;
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id == "same-return-value"), Vector.empty, model.lints.mkString("\n"))
  }

  test("a method that returns the same literal and nothing can override is reported") {
    val model = compile("Fixed" -> """
      |package demo;
      |
      |public class Fixed {
      |  public int open() {
      |    return 1;
      |  }
      |
      |  private int hidden() {
      |    return 1;
      |  }
      |
      |  public final int sealed() {
      |    return 1;
      |  }
      |
      |  public static int shared() {
      |    return 1;
      |  }
      |
      |  private static class Inner {
      |    int run() {
      |      return 1;
      |    }
      |  }
      |
      |  static final class Done {
      |    int run() {
      |      return 1;
      |    }
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "same-return-value").map(_.method).sorted
    assertEquals(
      found,
      Vector(
        "demo.Fixed.Done.run",
        "demo.Fixed.Inner.run",
        "demo.Fixed.hidden",
        "demo.Fixed.sealed",
        "demo.Fixed.shared"
      ),
      model.lints.mkString("\n")
    )
  }

  test("a stream collected by joining is reported") {
    val model = compile("Joins" -> """
      |package demo;
      |
      |import java.util.List;
      |import java.util.stream.Collectors;
      |
      |public class Joins {
      |  public String join(List<String> values) {
      |    return values.stream().collect(Collectors.joining(" | "));
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "stream-join")
    assertEquals(
      found.map(_.detail),
      Vector("use String.join instead of collecting a stream with joining: String.join(\" | \", values)"),
      model.lints.mkString("\n")
    )
  }

  test("a Unicode escape that the source can hold is reported") {
    val model = compile("Escapes" -> """
      |package demo;
      |
      |public class Escapes {
      |  private final String arrow = "\u25C0";
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "unnecessary-unicode-escape")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
  }

  test("an invocation handler that always returns null is reported") {
    val model = compile("Handlers" -> """
      |package demo;
      |
      |import java.lang.reflect.InvocationHandler;
      |import java.lang.reflect.Method;
      |
      |public class Handlers {
      |  static final class Bad implements InvocationHandler {
      |    @Override
      |    public Object invoke(Object proxy, Method method, Object[] args) {
      |      return null;
      |    }
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "suspicious-invocation-handler")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
  }

  test("a large anonymous implementation is reported for extraction") {
    val model = compile("Extracts" -> """
      |package demo;
      |
      |public class Extracts {
      |  interface Work {
      |    void first();
      |    void second();
      |  }
      |
      |  public Work work() {
      |    Work work = new Work() {
      |      @Override
      |      public void first() {
      |        System.out.println("first");
      |      }
      |
      |      @Override
      |      public void second() {
      |        System.out.println("second");
      |      }
      |    };
      |    return work;
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.count(_.id == "extract-method"), 1, model.lints.mkString("\\n"))
  }

  test("a reflected member that does not exist and a reflective call are reported") {
    val model = compile("Reflections" -> """
      |package demo;
      |
      |import java.lang.reflect.Method;
      |
      |public class Reflections {
      |  static class Target {
      |    public void ok(String value) {}
      |  }
      |
      |  public Method missing() throws Exception {
      |    return Target.class.getMethod("missing");
      |  }
      |
      |  public Method found() throws Exception {
      |    return Target.class.getMethod("ok", String.class);
      |  }
      |
      |  public Object call() throws Exception {
      |    Method method = Target.class.getMethod("ok", String.class);
      |    return method.invoke(null);
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.count(_.id == "reflection-member"), 1, model.lints.mkString("\\n"))
    assertEquals(model.lints.count(_.id == "reflection-invocation"), 1, model.lints.mkString("\\n"))
  }

  test("a type of the unsupported JDK module is reported in its import and its uses") {
    val model = compile("Unsupported" -> """
      |package demo;
      |
      |import sun.misc.Signal;
      |
      |public class Unsupported {
      |  public Signal signal(String name) {
      |    return new Signal(name);
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "jdk-internal-api")
    assertEquals(found.size, 3, model.lints.mkString("\n"))
    assert(found.forall(_.detail.contains("in the unsupported module jdk.unsupported")), found.mkString("\n"))
  }

  test("a type of a package the JDK does not export to the code is reported") {
    val model = compileWith(
      Seq("--add-exports", "java.base/jdk.internal.misc=ALL-UNNAMED"),
      Vector.empty,
      "Internal" -> """
        |package demo;
        |
        |public class Internal {
        |  public Object vm() {
        |    jdk.internal.misc.VM vm = null;
        |    return vm;
        |  }
        |}
        |""".stripMargin
    )

    val found = model.lints.filter(_.id == "jdk-internal-api")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
    assert(found.head.detail.contains("not exported by java.base"), found.mkString("\n"))
  }

  test("reflection by name of an unsupported type and lookups of its members are reported") {
    val model = compile("Reflective" -> """
      |package demo;
      |
      |public class Reflective {
      |  public Object load() throws Exception {
      |    return Class.forName("sun.misc.Unsafe");
      |  }
      |
      |  public Object field() throws Exception {
      |    return sun.misc.Unsafe.class.getDeclaredField("theUnsafe");
      |  }
      |
      |  public Object loaded() throws Exception {
      |    return Class.forName("java.util.ArrayList").getDeclaredConstructor();
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "jdk-internal-api")
    assertEquals(found.size, 3, model.lints.mkString("\n"))
    assertEquals(found.count(_.detail.contains("theUnsafe")), 1, model.lints.mkString("\n"))
  }

  test("loading a class by name and looking up a member of an unsupported class are reported") {
    val model = compile("Loaders" ->
      """
      |package demo;
      |
      |import java.lang.invoke.MethodHandles;
      |import java.lang.invoke.MethodType;
      |
      |public class Loaders {
      |  public Class<?> load(ClassLoader loader) throws Exception {
      |    return loader.loadClass("sun.misc.Signal");
      |  }
      |
      |  public Object raise() throws Exception {
      |    return MethodHandles.lookup().findStatic(sun.misc.Signal.class, "raise", MethodType.methodType(void.class, int.class));
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "jdk-internal-api").map(_.detail)
    assertEquals(found.count(_.contains("named by java.lang.ClassLoader.loadClass")), 1, model.lints.mkString("\n"))
    assertEquals(
      found.count(_.contains("the reflective lookup of raise in sun.misc.Signal")),
      1,
      model.lints.mkString("\n")
    )
  }

  test("supported JDK APIs are not reported") {
    val model = compile("Supported" -> """
      |package demo;
      |
      |import com.sun.net.httpserver.HttpServer;
      |
      |public class Supported {
      |  public HttpServer server() {
      |    return null;
      |  }
      |
      |  public Object load() throws Exception {
      |    return Class.forName("java.util.ArrayList");
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.count(_.id == "jdk-internal-api"), 0, model.lints.mkString("\n"))
  }

  test("a suppression names the JDK internal API rule or its IntelliJ inspections") {
    val model = compile(
      "Suppressed" -> """
        |package demo;
        |
        |//noinspection jdk-internal-api
        |import sun.misc.Signal;
        |
        |@SuppressWarnings("jdk-internal-api")
        |public class Suppressed {
        |  public Signal signal(String name) {
        |    return new Signal(name);
        |  }
        |}
        |""".stripMargin,
      "Inspected" -> """
        |package demo;
        |
        |//noinspection Java9ReflectionClassVisibility
        |import sun.misc.Signal;
        |
        |@SuppressWarnings("Java9ReflectionClassVisibility")
        |public class Inspected {
        |  public Object load() throws Exception {
        |    return Class.forName("sun.misc.Signal");
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(model.lints.count(_.id == "jdk-internal-api"), 0, model.lints.mkString("\n"))
  }

  test("an import is suppressed by the annotation of the top-level type") {
    val model = compile("Imported" -> """
      |package demo;
      |
      |import sun.misc.Signal;
      |
      |@SuppressWarnings("bonsai:jdk-internal-api")
      |public class Imported {
      |  public int hash() {
      |    return 1;
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.count(_.id == "jdk-internal-api"), 0, model.lints.mkString("\n"))
  }

  test("an import is not suppressed by a top-level type that suppresses another rule") {
    val model = compile("Imported" -> """
      |package demo;
      |
      |import sun.misc.Signal;
      |
      |@SuppressWarnings("bonsai:unused-member")
      |public class Imported {
      |  public int hash() {
      |    return 1;
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "jdk-internal-api")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
    assertEquals(found.head.line, 4)
  }

  test("an import is suppressed only when every top-level type suppresses the rule") {
    val both = compile("Both" -> """
      |package demo;
      |
      |import sun.misc.Signal;
      |
      |@SuppressWarnings("bonsai:jdk-internal-api")
      |public class Both {
      |}
      |
      |@SuppressWarnings("bonsai:jdk-internal-api")
      |class Other {
      |}
      |""".stripMargin)
    assertEquals(both.lints.count(_.id == "jdk-internal-api"), 0, both.lints.mkString("\n"))

    val one = compile("One" -> """
      |package demo;
      |
      |import sun.misc.Signal;
      |
      |@SuppressWarnings("bonsai:jdk-internal-api")
      |public class One {
      |}
      |
      |class Other {
      |}
      |""".stripMargin)
    assertEquals(one.lints.count(_.id == "jdk-internal-api"), 1, one.lints.mkString("\n"))
  }

  test("similar log messages are reported") {
    val model = compile("Logs" -> """
      |package demo;
      |
      |import org.slf4j.Logger;
      |import org.slf4j.LoggerFactory;
      |
      |public class Logs {
      |  private static final Logger LOG = LoggerFactory.getLogger(Logs.class);
      |
      |  public void run() {
      |    LOG.error("failed to start the server");
      |    LOG.error("failed to start the server now");
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.count(_.id == "logging-similar"), 1, model.lints.mkString("\\n"))
  }

  test("a magic modifier argument is reported") {
    val model = compile("Events" -> """
      |package demo;
      |
      |import java.awt.Panel;
      |import java.awt.event.InputEvent;
      |import java.awt.event.MouseEvent;
      |
      |public class Events {
      |  public MouseEvent magic(Panel panel) {
      |    int modifiers = 0;
      |    return new MouseEvent(panel, MouseEvent.MOUSE_MOVED, 1L, modifiers, 1, 1, 1, false, 1);
      |  }
      |
      |  public MouseEvent named(Panel panel) {
      |    return new MouseEvent(
      |        panel, MouseEvent.MOUSE_MOVED, 1L, InputEvent.SHIFT_DOWN_MASK, 1, 1, 1, false, 1);
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.count(_.id == "magic-constant"), 1, model.lints.mkString("\\n"))
  }

  test("List.of(...).stream() is reported") {
    val model = compile("Streams" -> """
      |package demo;
      |
      |import java.util.List;
      |
      |public class Streams {
      |  public long count() {
      |    return List.of("a").stream().count();
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      model.lints.filter(_.id == "stream-of").map(_.detail),
      Vector("use Stream.of instead of List.of(...).stream(): Stream.of(\"a\")"),
      model.lints.mkString("\\n")
    )
  }

  test("Optional in a field or parameter is reported by default") {
    val source = """
      |package demo;
      |
      |import java.util.Optional;
      |
      |public class Optionals {
      |  private Optional<String> field;
      |
      |  public void set(Optional<String> value) {}
      |
      |  public Optional<String> get() {
      |    return Optional.empty();
      |  }
      |
      |  public void local() {
      |    Optional<String> value = Optional.empty();
      |  }
      |}
      |""".stripMargin

    assertEquals(compile("Optionals" -> source).lints.count(_.id == "optional-signature"), 2)
  }

  test("the nullness and resource scopes are checked") {
    val model = compile(
      "package-info" -> "@org.jspecify.annotations.NullMarked package demo;",
      "Scopes"       ->
        """package demo;
          |
          |import java.io.FileInputStream;
          |import org.jspecify.annotations.Nullable;
          |
          |public class Scopes {
          |  private String field;
          |
          |  public void consume(String value) {}
          |
          |  public void use(String value, @Nullable String maybe) throws Exception {
          |    if (field == null) {}
          |    if (maybe == null) {}
          |    consume(null);
          |  }
          |
          |  public String bad() {
          |    return null;
          |  }
          |
          |  public void resources() throws Exception {
          |    FileInputStream input = new FileInputStream("data");
          |    try {
          |      System.out.println(input.available());
          |    } finally {
          |      input.close();
          |    }
          |  }
          |
          |  public void leaked() throws Exception {
          |    FileInputStream input = new FileInputStream("data");
          |    System.out.println(input.available());
          |  }
          |
          |  public void safe() throws Exception {
          |    try (FileInputStream input = new FileInputStream("data")) {
          |      System.out.println(input.available());
          |    }
          |  }
          |}
          |""".stripMargin
    )

    assertEquals(model.lints.count(_.id == "not-null-field"), 1, model.lints.mkString("\n"))
    assertEquals(model.lints.count(_.id == "constant-value"), 1, model.lints.mkString("\n"))
    assertEquals(model.lints.count(_.id == "data-flow-null"), 1, model.lints.mkString("\n"))
    assertEquals(model.lints.count(_.id == "nullable-problem"), 1, model.lints.mkString("\n"))
    assertEquals(model.lints.count(_.id == "auto-closeable-resource"), 1, model.lints.mkString("\n"))
  }

  test("a not-null field assigned by a constructor is initialized") {
    val model = compile(
      "package-info" -> "@org.jspecify.annotations.NullMarked package demo;",
      "Fields"       -> """
        |package demo;
        |
        |public class Fields {
        |  private final String initialized;
        |  private String missing;
        |
        |  public Fields(String initialized) {
        |    this.initialized = initialized;
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(model.lints.count(_.id == "not-null-field"), 1, model.lints.mkString("\n"))
  }

  test("an AutoCloseable local used by a later try is a resource") {
    val model = compile("Resources" -> """
      |package demo;
      |
      |import java.io.ByteArrayInputStream;
      |
      |public class Resources {
      |  public void use() throws Exception {
      |    ByteArrayInputStream input = new ByteArrayInputStream(new byte[0]);
      |    try (input) {
      |      System.out.println(input.available());
      |    }
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.count(_.id == "auto-closeable-resource"), 0, model.lints.mkString("\n"))
  }

  test("a local copied from a nullable value is not a constant") {
    val model = compile(
      "package-info" -> "@org.jspecify.annotations.NullMarked package demo;",
      "Values"       -> """
        |package demo;
        |
        |import org.jspecify.annotations.Nullable;
        |
        |public class Values {
        |  private @Nullable String maybe;
        |
        |  public void use() {
        |    String local = maybe;
        |    if (local == null) return;
        |    String fixed = "value";
        |    if (fixed == null) return;
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(model.lints.count(_.id == "constant-value"), 1, model.lints.mkString("\n"))
  }

  test("a primitive field is initialized by its default") {
    val model = compile(
      "package-info" -> "@org.jspecify.annotations.NullMarked package demo;",
      "Counters"     -> """
        |package demo;
        |
        |public class Counters {
        |  private int count;
        |  private boolean ready;
        |}
        |""".stripMargin
    )

    assertEquals(model.lints.count(_.id == "not-null-field"), 0, model.lints.mkString("\\n"))
  }

  test("only an owned and unescaped resource is a leak") {
    val model = compile(
      "Leaks" -> """
        |package demo;
        |
        |public class Leaks {
        |  static final class Handle implements AutoCloseable {
        |    public void close() {}
        |  }
        |
        |  private static void consume(Handle handle) {}
        |
        |  public Handle make() {
        |    Handle handle = new Handle();
        |    return handle;
        |  }
        |
        |  public void pass() {
        |    Handle handle = new Handle();
        |    consume(handle);
        |  }
        |
        |  public void close() {
        |    Handle handle = new Handle();
        |    handle.close();
        |  }
        |
        |  public void leak() {
        |    Handle handle = new Handle();
        |    handle.hashCode();
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(model.lints.count(_.id == "auto-closeable-resource"), 1, model.lints.mkString("\n"))
  }

  test("a primitive class literal names a member with a primitive parameter") {
    val model = compile("PrimitiveMembers" -> """
      |package demo;
      |
      |public class PrimitiveMembers {
      |  static class Target {
      |    public Target(long value) {}
      |  }
      |
      |  public Object constructor() throws Exception {
      |    return Target.class.getConstructor(long.class);
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.count(_.id == "reflection-member"), 0, model.lints.mkString("\n"))
  }

  test("a return in a lambda body is not a return from the enclosing method") {
    val model = compile(
      "package-info" -> "@org.jspecify.annotations.NullMarked package demo;",
      "Lambdas"      -> """
        |package demo;
        |
        |import java.util.function.Function;
        |
        |public class Lambdas {
        |  public Function<String, Object> observer() {
        |    return value -> {
        |      return null;
        |    };
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(model.lints.count(_.id == "nullable-problem"), 0, model.lints.mkString("\n"))
  }

  test("a try with resources is not a resource that its finally block closes") {
    val model = compile("Resources" -> """
      |package demo;
      |
      |import java.io.ByteArrayInputStream;
      |
      |public class Resources {
      |  public void use() throws Exception {
      |    ByteArrayInputStream input = new ByteArrayInputStream(new byte[0]);
      |    try (input) {
      |      System.out.println(input.available());
      |    } finally {
      |      input.close();
      |    }
      |  }
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "close-in-finally"),
      s"the try statement closes the resource: ${model.lints.mkString(", ")}"
    )
  }

  test("a resolvable reflective call is checked for its arguments") {
    val model = compile("Calls" -> """
      |package demo;
      |
      |import java.lang.reflect.Method;
      |
      |public class Calls {
      |  static class Target {
      |    public void ok(String value) {}
      |  }
      |
      |  public Object missingArgument() throws Exception {
      |    Method method = Target.class.getMethod("ok", String.class);
      |    return method.invoke(null);
      |  }
      |
      |  public Object unknown(Method method) throws Exception {
      |    return method.invoke(null);
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.count(_.id == "reflection-invocation"), 1, model.lints.mkString("\n"))
  }

  test("a local assigned from a nullable value is not a constant") {
    val model = compile(
      "package-info" -> "@org.jspecify.annotations.NullMarked package demo;",
      "Reassigned"   -> """
        |package demo;
        |
        |import org.jspecify.annotations.Nullable;
        |
        |public class Reassigned {
        |  public void use(@Nullable String maybe) {
        |    String current = "value";
        |    current = maybe;
        |    if (current == null) return;
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(model.lints.count(_.id == "constant-value"), 0, model.lints.mkString("\n"))
  }

  test("a native method may return null") {
    val model = compile(
      "package-info" -> "@org.jspecify.annotations.NullMarked package demo;",
      "Natives"      -> """
        |package demo;
        |
        |public class Natives {
        |  private static native Object value();
        |
        |  public void use() {
        |    Object local = value();
        |    if (local == null) return;
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(model.lints.count(_.id == "constant-value"), 0, model.lints.mkString("\n"))
  }

  private val marking = "package-info" -> "@org.jspecify.annotations.NullMarked package demo;"

  private def flow(id: String, sources: (String, String)*): Vector[Model.Lint] =
    compile((marking +: sources)*).lints.filter(_.id == id).toVector

  test("a call whose type parameter has a nullable bound is nullable only where the type argument is") {
    val found = flow(
      "nullable-use",
      "Pumps" ->
        """package demo;
          |
          |import java.util.concurrent.CompletableFuture;
          |import org.jspecify.annotations.Nullable;
          |
          |public class Pumps {
          |  static <T extends @Nullable Object> T get(CompletableFuture<T> future) {
          |    return future.join();
          |  }
          |
          |  static int silent(CompletableFuture<String> future) {
          |    return get(future).length();
          |  }
          |
          |  static int unboxed(CompletableFuture<Integer> future) {
          |    int value = get(future);
          |    return value;
          |  }
          |
          |  static int nullable(CompletableFuture<@Nullable String> future) {
          |    return get(future).length();
          |  }
          |
          |  static int nullableUnboxed(CompletableFuture<@Nullable Integer> future) {
          |    int value = get(future);
          |    return value;
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line).sorted, Vector(21, 25), found.mkString("\n"))
  }

  test("unboxing a nullable value is reported unless a check proves it") {
    val found = flow(
      "nullable-use",
      "Boxes" ->
        """package demo;
          |
          |import org.jspecify.annotations.Nullable;
          |
          |public class Boxes {
          |  static @Nullable Integer maybe() {
          |    return null;
          |  }
          |
          |  static int assigned() {
          |    int value = maybe();
          |    return value;
          |  }
          |
          |  static int arithmetic() {
          |    return maybe() + 1;
          |  }
          |
          |  static int checked() {
          |    Integer value = maybe();
          |    if (value == null) return 0;
          |    int copy = value;
          |    return copy;
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line).sorted, Vector(11, 16), found.mkString("\n"))
  }

  test("a null returned from a lambda is not a null returned from the method that holds it") {
    val sources = Seq(
      "Lambdas" ->
        """package demo;
          |
          |import java.util.concurrent.CompletableFuture;
          |
          |public class Lambdas {
          |  static CompletableFuture<Void> clear(CompletableFuture<String> future) {
          |    return future.thenApply(ignored -> null);
          |  }
          |
          |  static CompletableFuture<Void> blocks(CompletableFuture<String> future) {
          |    return future.thenApply(ignored -> {
          |      if (ignored.isEmpty()) return null;
          |      return null;
          |    });
          |  }
          |
          |  static String direct() {
          |    return null;
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(flow("nullable-problem", sources*).map(_.line), Vector(18))
    assertEquals(flow("data-flow-null", sources*), Vector.empty)
  }

  test("a null passed to a callee that is not null-marked is not reported, and one passed to a marked callee is") {
    val found = flow(
      "data-flow-null",
      "Other"   -> "package other; public class Other { public static void take(String value) {} }",
      "Callers" ->
        """package demo;
          |
          |import java.util.List;
          |import java.util.concurrent.CompletableFuture;
          |
          |public class Callers {
          |  static void take(String value) {}
          |
          |  static void jdk(CompletableFuture<Void> done, List<String> list) {
          |    done.complete(null);
          |    list.contains(null);
          |  }
          |
          |  static void unmarked() {
          |    other.Other.take(null);
          |  }
          |
          |  static void marked() {
          |    take(null);
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(19), found.mkString("\n"))
  }

  test("requireNonNullElse of a value the scope says is not null returns its first argument") {
    val found = flow(
      "constant-value",
      "Defaults" ->
        """package demo;
          |
          |import java.util.Objects;
          |import org.jspecify.annotations.Nullable;
          |
          |public class Defaults {
          |  static String redundant(String location) {
          |    return Objects.requireNonNullElse(location, "");
          |  }
          |
          |  static String needed(@Nullable String location) {
          |    return Objects.requireNonNullElse(location, "");
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(8), found.mkString("\n"))
  }

  test("a null stored in an array of non-null elements is reported, and one in an array of nullable elements is not") {
    val found = flow(
      "data-flow-null",
      "Holders" ->
        """package demo;
          |
          |import org.jspecify.annotations.Nullable;
          |
          |public class Holders {
          |  static void fill() {
          |    String[] strict = {null};
          |    @Nullable String[] loose = {null};
          |    String[] sized = new String[1];
          |    sized[0] = null;
          |    loose[0] = null;
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line).sorted, Vector(7, 10), found.mkString("\n"))
  }

  test("a comparison with null is constant for a field, an element or a value an earlier check settles") {
    val found = flow(
      "constant-value",
      "Checks" ->
        """package demo;
          |
          |import java.util.List;
          |import org.jspecify.annotations.Nullable;
          |
          |public class Checks {
          |  private String field;
          |
          |  Checks(String field) {
          |    this.field = field;
          |  }
          |
          |  boolean declared(String value) {
          |    return value != null;
          |  }
          |
          |  boolean again(@Nullable String value) {
          |    if (value == null) return false;
          |    return value != null;
          |  }
          |
          |  boolean open(@Nullable String value) {
          |    return value != null;
          |  }
          |
          |  boolean element(String[] holder) {
          |    String entry = holder[0];
          |    return entry == null;
          |  }
          |
          |  boolean member() {
          |    return field != null;
          |  }
          |
          |  void lambda(List<String> values) {
          |    values.forEach(value -> {
          |      if (value == null) return;
          |    });
          |  }
          |
          |  boolean assigned(@Nullable String value) {
          |    String copy = value;
          |    copy = "x";
          |    return copy == null;
          |  }
          |
          |  boolean loop(@Nullable String value) {
          |    String copy = "x";
          |    for (int i = 0; i < 2; i++) {
          |      if (copy == null) return true;
          |      copy = value;
          |    }
          |    return false;
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line).sorted, Vector(19, 28, 32, 44), found.mkString("\n"))
  }

  test("an override must not narrow a nullable parameter or widen a non-null result of a null-marked supertype") {
    val found = flow(
      "nullable-problem",
      "Sources" ->
        """package demo;
          |
          |import java.util.function.Consumer;
          |import org.jspecify.annotations.Nullable;
          |
          |public class Sources {
          |  interface Source {
          |    void accept(@Nullable String value);
          |
          |    @Nullable String find();
          |
          |    String name();
          |  }
          |
          |  static class Narrowing implements Source {
          |    public void accept(String value) {}
          |
          |    public @Nullable String find() {
          |      return null;
          |    }
          |
          |    public @Nullable String name() {
          |      return null;
          |    }
          |  }
          |
          |  static class Widening implements Source {
          |    public void accept(@Nullable String value) {}
          |
          |    public String find() {
          |      return "";
          |    }
          |
          |    public String name() {
          |      return "";
          |    }
          |  }
          |
          |  static class FromJdk implements Consumer<String> {
          |    public void accept(String value) {}
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line).sorted, Vector(16, 22), found.mkString("\n"))
  }

  test("a nullable method that never returns null is reported unless it overrides one") {
    val found = flow(
      "nullable-problem",
      "Never" ->
        """package demo;
          |
          |import org.jspecify.annotations.Nullable;
          |
          |public class Never {
          |  interface Source {
          |    @Nullable String find();
          |  }
          |
          |  static @Nullable String never() {
          |    return "x";
          |  }
          |
          |  static @Nullable String sometimes(boolean flag) {
          |    if (flag) return null;
          |    return "x";
          |  }
          |
          |  static class Impl implements Source {
          |    public @Nullable String find() {
          |      return "x";
          |    }
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(10), found.mkString("\n"))
  }

  test("a nullable field that the code checks is not null where it is used") {
    val found = flow(
      "nullable-use",
      "Fields" ->
        """package demo;
          |
          |import org.jspecify.annotations.Nullable;
          |
          |public class Fields {
          |  private @Nullable String name;
          |
          |  int length() {
          |    if (name == null) return 0;
          |    return name.length();
          |  }
          |
          |  int viaThis() {
          |    if (this.name == null) return 0;
          |    return this.name.length();
          |  }
          |
          |  int shortCircuit() {
          |    return name == null || name.isEmpty() ? 0 : 1;
          |  }
          |
          |  int bare() {
          |    return name.length();
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(23), found.mkString("\n"))
  }

  test("a boundary annotation does not make a result nullable, and only a method nothing overrides states its result") {
    val found = flow(
      "nullable-problem",
      "NullableBoundary" -> "package demo; public @interface NullableBoundary { String value(); }",
      "Boundaries"       ->
        """package demo;
          |
          |import org.jspecify.annotations.Nullable;
          |
          |public class Boundaries {
          |  @NullableBoundary("external")
          |  static String boundary() {
          |    return "x";
          |  }
          |
          |  public @Nullable String open() {
          |    return "x";
          |  }
          |
          |  static final class Closed {
          |    public @Nullable String find() {
          |      return "x";
          |    }
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(16), found.mkString("\n"))
  }

  test("a null check of a parameter the scope says is not null is defensive, and one after a check is redundant") {
    val found = flow(
      "constant-value",
      "Defensive" ->
        """package demo;
          |
          |public class Defensive {
          |  static boolean check(String value) {
          |    return value != null;
          |  }
          |
          |  static boolean twice(String value) {
          |    if (value == null) return false;
          |    return value != null;
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(10), found.mkString("\n"))
  }

  test("a boundary annotation makes the result nullable only for a type variable") {
    val found = flow(
      "nullable-use",
      "NullableBoundary" -> "package demo; public @interface NullableBoundary { String value(); }",
      "Calls"            ->
        """package demo;
          |
          |import java.util.concurrent.CompletableFuture;
          |
          |public class Calls {
          |  @NullableBoundary("params may be null")
          |  static CompletableFuture<Void> call(String method) {
          |    return CompletableFuture.completedFuture(null);
          |  }
          |
          |  static void use() {
          |    call("a").thenApply(ignored -> 1);
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found, Vector.empty)
  }

  test("a null-marked package marks its own types and not those of its subpackages") {
    val sources = Seq(
      "Sub" ->
        """package demo.sub;
          |
          |public class Sub {
          |  static boolean check(String value) {
          |    String copy = value;
          |    return copy != null;
          |  }
          |
          |  static void caller() {
          |    take(null);
          |  }
          |
          |  static void take(String value) {}
          |}
          |""".stripMargin
    )
    assertEquals(flow("constant-value", sources*), Vector.empty)
    assertEquals(flow("data-flow-null", sources*), Vector.empty)
  }

  test("a call makes the code forget that a field is null but not that it is set, and a volatile field is never held") {
    val found = flow(
      "constant-value",
      "Mutable" ->
        """package demo;
          |
          |import org.jspecify.annotations.Nullable;
          |
          |public class Mutable {
          |  private @Nullable String name;
          |  private volatile @Nullable String shared;
          |
          |  int afterCall() {
          |    name = null;
          |    log();
          |    return name == null ? 0 : 1;
          |  }
          |
          |  int kept() {
          |    name = "x";
          |    log();
          |    return name == null ? 0 : 1;
          |  }
          |
          |  int volatileField() {
          |    shared = "x";
          |    return shared == null ? 0 : 1;
          |  }
          |
          |  int condition(java.util.Queue<String> queue) {
          |    String line;
          |    int count = 0;
          |    while ((line = queue.poll()) != null) count += line.length();
          |    return count;
          |  }
          |
          |  void log() {}
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(18), found.mkString("\n"))
  }

  private val lifecycleAnnotations = Seq(
    "org.junit.jupiter.api" -> Seq("BeforeAll", "BeforeEach", "AfterAll", "AfterEach", "Test"),
    "org.junit"             -> Seq("BeforeClass", "Before", "AfterClass", "After")
  ).flatMap { (pkg, names) =>
    names.map(name => name -> s"package $pkg; public @interface $name {}")
  }

  test("a field a setup method assigns may be null in a teardown method and in code created before the setup runs") {
    val found = flow(
      "constant-value",
      (lifecycleAnnotations :+
        ("Suite" ->
          """package demo;
          |
          |import java.util.function.Supplier;
          |import org.junit.jupiter.api.AfterAll;
          |import org.junit.jupiter.api.AfterEach;
          |import org.junit.jupiter.api.BeforeAll;
          |import org.junit.jupiter.api.BeforeEach;
          |
          |public class Suite {
          |  static Object browser;
          |  static Object engine;
          |  Object session;
          |  static Supplier<Object> lookup;
          |
          |  @BeforeAll
          |  static void setUp() {
          |    lookup = () -> browser != null ? browser : null;
          |    Runnable watcher = new Runnable() {
          |      public void run() {
          |        if (engine != null) engine.toString();
          |      }
          |    };
          |    engine = new Object();
          |    browser = new Object();
          |  }
          |
          |  @BeforeEach
          |  void open() {
          |    session = new Object();
          |  }
          |
          |  @AfterAll
          |  static void tearDown() {
          |    if (browser != null) browser.toString();
          |    if (engine == null) return;
          |  }
          |
          |  @AfterEach
          |  void close() {
          |    if (session != null) session.toString();
          |  }
          |}
          |""".stripMargin))*
    )
    assertEquals(found, Vector.empty, found.mkString("\n"))
  }

  test("a field a junit 4 setup method assigns may be null in a teardown method") {
    val found = flow(
      "constant-value",
      (lifecycleAnnotations :+
        ("Legacy" ->
          """package demo;
          |
          |import org.junit.After;
          |import org.junit.AfterClass;
          |import org.junit.Before;
          |import org.junit.BeforeClass;
          |
          |public class Legacy {
          |  static Object shared;
          |  Object local;
          |
          |  @BeforeClass
          |  public static void setUpClass() {
          |    shared = new Object();
          |  }
          |
          |  @Before
          |  public void setUp() {
          |    local = new Object();
          |  }
          |
          |  @AfterClass
          |  public static void tearDownClass() {
          |    if (shared != null) shared.toString();
          |  }
          |
          |  @After
          |  public void tearDown() {
          |    if (local != null) local.toString();
          |  }
          |}
          |""".stripMargin))*
    )
    assertEquals(found, Vector.empty, found.mkString("\n"))
  }

  private def withLifecycle(sources: (String, String)*): Seq[(String, String)] = lifecycleAnnotations ++ sources

  test("code created after the setup assigns a field sees it set") {
    val sources = withLifecycle(
      "Late" ->
        """package demo;
          |
          |import org.junit.jupiter.api.BeforeAll;
          |import org.junit.jupiter.api.Test;
          |
          |public class Late {
          |  static Object engine;
          |
          |  @BeforeAll
          |  static void setUp() {
          |    engine = new Object();
          |    Runnable made = new Runnable() {
          |      public void run() {
          |        if (engine != null) engine.toString();
          |      }
          |    };
          |  }
          |
          |  @Test
          |  void inTest() {
          |    Runnable made = new Runnable() {
          |      public void run() {
          |        if (engine != null) engine.toString();
          |      }
          |    };
          |    Runnable lambda = () -> engine.toString();
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(flow("constant-value", sources*).map(_.line), Vector(14, 23))
    assertEquals(flow("nullable-use", sources*), Vector.empty)
  }

  test("code created by an initialiser or a constructor may see a field the setup assigns before it is set") {
    val sources = withLifecycle(
      "Early" ->
        """package demo;
          |
          |import org.junit.jupiter.api.BeforeAll;
          |
          |public class Early {
          |  static Object engine;
          |  static final Runnable field = new Runnable() {
          |    public void run() {
          |      if (engine != null) engine.toString();
          |    }
          |  };
          |  final Runnable lambda;
          |
          |  Early() {
          |    lambda = () -> engine.toString();
          |  }
          |
          |  @BeforeAll
          |  static void setUp() {
          |    engine = new Object();
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(flow("constant-value", sources*), Vector.empty)
    assertEquals(flow("nullable-use", sources*).map(_.line), Vector(15))
  }

  test("a field is still not null where the setup has run or where no setup assigns it") {
    val found = flow(
      "constant-value",
      (lifecycleAnnotations :+
        ("Checked" ->
          """package demo;
          |
          |import org.junit.jupiter.api.AfterAll;
          |import org.junit.jupiter.api.AfterEach;
          |import org.junit.jupiter.api.BeforeAll;
          |import org.junit.jupiter.api.Test;
          |
          |public class Checked {
          |  static Object browser;
          |  static final Object fixed = new Object();
          |  Object built;
          |
          |  Checked() {
          |    built = new Object();
          |  }
          |
          |  @BeforeAll
          |  static void setUp() {
          |    browser = new Object();
          |  }
          |
          |  @Test
          |  void inTest() {
          |    if (browser != null) browser.toString();
          |  }
          |
          |  @Test
          |  void inLambda() {
          |    Runnable run = () -> {
          |      if (fixed != null) fixed.toString();
          |    };
          |  }
          |
          |  @AfterAll
          |  static void tearDown() {
          |    if (fixed != null) fixed.toString();
          |  }
          |
          |  @AfterEach
          |  void close() {
          |    if (built != null) built.toString();
          |  }
          |}
          |""".stripMargin))*
    )
    assertEquals(found.map(_.line), Vector(24, 30, 36, 41), found.mkString("\n"))
  }

  test("the elements of an array a method creates are not trusted to be non-null, those of a parameter are") {
    val found = flow(
      "constant-value",
      "Holders2" ->
        """package demo;
          |
          |public class Holders2 {
          |  static boolean fresh() {
          |    String[] holder = new String[1];
          |    return holder[0] == null;
          |  }
          |
          |  static boolean parameter(String[] holder) {
          |    return holder[0] == null;
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(10), found.mkString("\n"))
  }

  test("a nullable annotation on the use of a type variable makes the result of a call nullable") {
    val found = flow(
      "nullable-use",
      "Lookup" ->
        """package demo;
          |
          |import org.jspecify.annotations.Nullable;
          |
          |public class Lookup<T> {
          |  @Nullable T find(int id) {
          |    return null;
          |  }
          |
          |  static int use(Lookup<String> lookup) {
          |    return lookup.find(1).length();
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(11), found.mkString("\n"))
  }

  test("code after a loop over a collection is reached, so a null returned there counts") {
    val found = flow(
      "nullable-problem",
      "Threads" ->
        """package demo;
          |
          |import java.util.List;
          |import org.jspecify.annotations.Nullable;
          |
          |public class Threads {
          |  private static @Nullable String named(List<String> names) {
          |    for (String name : names) {
          |      if (name.isEmpty()) return name;
          |    }
          |    return null;
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found, Vector.empty)
  }

  test("a field of another object that the code checks is not null where it is used, a resource that it checks too") {
    val found = flow(
      "nullable-use",
      "Servers" ->
        """package demo;
          |
          |import java.util.concurrent.CountDownLatch;
          |import org.jspecify.annotations.Nullable;
          |
          |public class Servers {
          |  static class Spec {
          |    @Nullable CountDownLatch started;
          |  }
          |
          |  static void respond(Spec spec) {
          |    if (spec.started != null) spec.started.countDown();
          |  }
          |
          |  static void bare(Spec spec) {
          |    spec.started.countDown();
          |  }
          |
          |  static @Nullable AutoCloseable open() {
          |    return null;
          |  }
          |
          |  static void close() throws Exception {
          |    try (AutoCloseable host = open()) {
          |      if (host == null) return;
          |      host.toString();
          |    }
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(16), found.mkString("\n"))
  }

  test("a null passed for the elements of a varargs parameter is not reported") {
    val found = flow(
      "data-flow-null",
      "Varargs" ->
        """package demo;
          |
          |public class Varargs {
          |  static void invoke(String name, Object... args) {}
          |
          |  static void caller() {
          |    invoke("a", null, null);
          |    invoke(null, "b");
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(8), found.mkString("\n"))
  }

  test("a method that always returns one value is reported where it is declared, if every override returns it too") {
    val found = flow(
      "same-return-value",
      "Sizes" ->
        """package demo;
          |
          |public class Sizes {
          |  private interface Sized {
          |    int size();
          |  }
          |
          |  private abstract static class Base {
          |    int encoded() {
          |      return 8;
          |    }
          |
          |    boolean handled() {
          |      return false;
          |    }
          |
          |    abstract int width();
          |  }
          |
          |  private static class Leaf extends Base {
          |    @Override
          |    int encoded() {
          |      return 8;
          |    }
          |
          |    @Override
          |    boolean handled() {
          |      return true;
          |    }
          |
          |    @Override
          |    int width() {
          |      return 2;
          |    }
          |  }
          |
          |  private static class Other extends Base {
          |    @Override
          |    int width() {
          |      return 2;
          |    }
          |  }
          |
          |  private static class Fixed implements Sized {
          |    public int size() {
          |      return 4;
          |    }
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line).sorted, Vector(5, 9, 17), found.mkString("\n"))
  }

  test("Boolean.TRUE.equals of a value that may be null is not a pointless boolean expression") {
    val found = flow(
      "pointless-boolean",
      "Booleans" ->
        """package demo;
          |
          |import org.jspecify.annotations.Nullable;
          |
          |public class Booleans {
          |  static boolean reduce(@Nullable Boolean left, @Nullable Boolean right) {
          |    return Boolean.TRUE.equals(left) || Boolean.TRUE.equals(right);
          |  }
          |
          |  static boolean strict(Boolean value) {
          |    return Boolean.TRUE.equals(value);
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(11), found.mkString("\n"))
  }

  private def flowAt(
      release: Seq[String],
      enabled: Vector[String],
      id: String,
      sources: (String, String)*
  ): Vector[Model.Lint] =
    compileWith(release, enabled, (marking +: sources)*).lints.filter(_.id == id).toVector

  private val readsText =
    """package demo;
      |
      |import java.io.IOException;
      |import java.nio.charset.StandardCharsets;
      |import java.nio.file.Files;
      |import java.nio.file.Path;
      |
      |public class Reads {
      |  static String text(Path path) throws IOException {
      |    return new String(Files.readAllBytes(path), StandardCharsets.UTF_8);
      |  }
      |}
      |""".stripMargin

  test("a suggestion of an API that a newer release introduced is made only to code compiled for that release") {
    assertEquals(flowAt(Seq("--release", "8"), Vector.empty, "read-string", "Reads" -> readsText), Vector.empty)
    assertEquals(
      flowAt(Seq("--release", "11"), Vector.empty, "read-string", "Reads" -> readsText).map(_.line),
      Vector(10)
    )
    assertEquals(
      flowAt(Seq("--release", "17"), Vector.empty, "read-string", "Reads" -> readsText).map(_.line),
      Vector(10)
    )
  }

  test("a release the build names overrides the one the platform classes show") {
    assertEquals(flowAt(Nil, Vector("release=8"), "read-string", "Reads" -> readsText), Vector.empty)
    assertEquals(flowAt(Nil, Vector("release=17"), "read-string", "Reads" -> readsText).map(_.line), Vector(10))
  }

  private val copies =
    """package demo;
      |
      |import java.util.ArrayList;
      |import java.util.Collections;
      |import java.util.List;
      |import java.util.Optional;
      |
      |public class Copies {
      |  static List<String> frozen(List<String> source) {
      |    return Collections.unmodifiableList(new ArrayList<>(source));
      |  }
      |
      |  static boolean absent(Optional<String> value) {
      |    return !value.isPresent();
      |  }
      |
      |  static void anonymous() {
      |    Runnable run = new Runnable() {
      |      public void run() {}
      |    };
      |    java.util.Comparator<String> order = new java.util.Comparator<String>() {
      |      public int compare(String left, String right) {
      |        return 0;
      |      }
      |    };
      |  }
      |}
      |""".stripMargin

  test(
    "copyOf is suggested from Java 10, isEmpty of an optional from Java 11, and the diamond of an anonymous class from 9"
  ) {
    def lines(release: String, id: String) =
      flowAt(Seq("--release", release), Vector.empty, id, "Copies" -> copies).map(_.line)
    assertEquals(lines("8", "java9-collection-factory"), Vector.empty)
    assertEquals(lines("9", "java9-collection-factory"), Vector.empty)
    assertEquals(lines("11", "java9-collection-factory"), Vector(10))
    assertEquals(lines("8", "none-check"), Vector.empty)
    assertEquals(lines("11", "none-check"), Vector(14))
    assertEquals(lines("8", "convert-2-diamond"), Vector.empty)
    assertEquals(lines("17", "convert-2-diamond"), Vector(21))
  }

  private val closes =
    """package demo;
      |
      |import java.io.ByteArrayInputStream;
      |import java.io.IOException;
      |
      |public class Closes {
      |  static void parameter(ByteArrayInputStream input) throws IOException {
      |    try {
      |      System.out.println(input.available());
      |    } finally {
      |      input.close();
      |    }
      |  }
      |
      |  static void local() throws IOException {
      |    ByteArrayInputStream input = new ByteArrayInputStream(new byte[0]);
      |    try {
      |      System.out.println(input.available());
      |    } finally {
      |      input.close();
      |    }
      |  }
      |}
      |""".stripMargin

  test(
    "a resource declared outside the try is a resource of the try from Java 9, one declared in the method from Java 7"
  ) {
    def lines(release: String, id: String) =
      flowAt(Seq("--release", release), Vector.empty, id, "Closes" -> closes).map(_.line)
    assertEquals(lines("8", "close-in-finally"), Vector(20))
    assertEquals(lines("11", "close-in-finally").sorted, Vector(11, 20))
  }

  test(
    "a final field closed by a finally block is a resource of the try from Java 9, a field that is not final is not"
  ) {
    val fields =
      """package demo;
        |
        |import java.io.ByteArrayInputStream;
        |import java.io.IOException;
        |
        |public class Fields {
        |  private final ByteArrayInputStream input = new ByteArrayInputStream(new byte[0]);
        |  private ByteArrayInputStream loose = new ByteArrayInputStream(new byte[0]);
        |
        |  void named() throws IOException {
        |    try {
        |      System.out.println(input.available());
        |    } finally {
        |      input.close();
        |    }
        |  }
        |
        |  void qualified() throws IOException {
        |    try {
        |      System.out.println(this.input.available());
        |    } finally {
        |      this.input.close();
        |    }
        |  }
        |
        |  void unfinal() throws IOException {
        |    try {
        |      System.out.println(loose.available());
        |    } finally {
        |      loose.close();
        |    }
        |  }
        |}
        |""".stripMargin
    def lines(release: String) =
      flowAt(Seq("--release", release), Vector.empty, "close-in-finally", "Fields" -> fields).map(_.line).sorted
    assertEquals(lines("8"), Vector.empty)
    assertEquals(lines("11"), Vector(14, 22))
  }

  test("an empty method is reported where it is declared, unless an override of it does something") {
    val found = flow(
      "empty-method",
      "Handlers" ->
        """package demo;
          |
          |public class Handlers {
          |  private interface Handler {
          |    default void onStart() {}
          |    default void onStop() {}
          |  }
          |
          |  private static class Quiet implements Handler {
          |    @Override
          |    public void onStop() {}
          |  }
          |
          |  private static class Loud implements Handler {
          |    @Override
          |    public void onStop() {
          |      System.out.println("stop");
          |    }
          |  }
          |
          |  private void helper() {}
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line).sorted, Vector(5, 21), found.mkString("\n"))
  }

  test(
    "a resource from a helper or an optional factory that allocates it is reported, one the code only looks up is not"
  ) {
    val found = flow(
      "auto-closeable-resource",
      "Resources" ->
        """package demo;
          |
          |import java.util.Optional;
          |
          |public class Resources {
          |  static class Conn implements AutoCloseable {
          |    void read() {}
          |    public void close() {}
          |    static Optional<Conn> create() {
          |      return Optional.of(new Conn());
          |    }
          |  }
          |
          |  static class Holder {
          |    private final Conn conn = new Conn();
          |    Optional<Conn> current() {
          |      return Optional.ofNullable(conn);
          |    }
          |  }
          |
          |  static Conn open(String url) {
          |    return new Conn();
          |  }
          |
          |  static void helper() {
          |    Conn c = open("x");
          |    c.read();
          |  }
          |
          |  static void optional() {
          |    Conn c = Conn.create().orElseThrow();
          |    c.read();
          |  }
          |
          |  static void borrowed(Holder holder) {
          |    Conn c = holder.current().orElse(null);
          |    c.read();
          |  }
          |
          |  static void closed() {
          |    Conn c = open("y");
          |    c.read();
          |    c.close();
          |  }
          |
          |  static void bytes() {
          |    java.io.ByteArrayOutputStream out = new java.io.ByteArrayOutputStream();
          |    out.write(1);
          |  }
          |
          |  static void input() {
          |    java.io.ByteArrayInputStream in = new java.io.ByteArrayInputStream(new byte[0]);
          |    in.read();
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line).sorted, Vector(26, 31), found.mkString("\n"))
  }

  test("a reflected member is checked against signatures the code builds from arrays of class literals") {
    val found = flow(
      "reflection-member",
      "Arrays2" ->
        """package demo;
          |
          |public class Arrays2 {
          |  public static class Host {
          |    public static int create(String a, Integer b) {
          |      return 0;
          |    }
          |  }
          |
          |  static Class<?>[] append(Class<?>[] arr, Class<?> elem) {
          |    Class<?>[] result = new Class<?>[arr.length + 1];
          |    System.arraycopy(arr, 0, result, 0, arr.length);
          |    result[arr.length] = elem;
          |    return result;
          |  }
          |
          |  static void probe(Class<?>[] unknown) throws Exception {
          |    Class<?>[] base = {String.class};
          |    Host.class.getMethod("create", append(base, Integer.class));
          |    Host.class.getMethod("create", append(append(base, Integer.class), Long.class));
          |    Host.class.getMethod("create", base);
          |    Host.class.getMethod("create", unknown);
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line).sorted, Vector(20, 21), found.mkString("\n"))
  }

  test("a reflective call is checked against the member it resolves, with arguments typed as Object left alone") {
    val found = flow(
      "reflection-invocation",
      "Calls" ->
        """package demo;
          |
          |import java.lang.reflect.Method;
          |
          |public class Calls {
          |  public static class Signal {}
          |
          |  public static class Handler {}
          |
          |  public static class Host {
          |    public static void handle(Signal signal, Handler handler) {}
          |  }
          |
          |  static void erased(Object signal, Object handler) throws Exception {
          |    Method method = Host.class.getMethod("handle", Signal.class, Handler.class);
          |    method.invoke(null, signal, handler);
          |  }
          |
          |  static void exact() throws Exception {
          |    Method method = Host.class.getMethod("handle", Signal.class, Handler.class);
          |    method.invoke(null, new Signal(), new Handler());
          |  }
          |
          |  static void wrong() throws Exception {
          |    Method method = Host.class.getMethod("handle", Signal.class, Handler.class);
          |    method.invoke(null, "text", new Handler());
          |  }
          |
          |  static void arity() throws Exception {
          |    Method method = Host.class.getMethod("handle", Signal.class, Handler.class);
          |    method.invoke(null, new Signal());
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line).sorted, Vector(26, 31), found.mkString("\n"))
  }

  test("a loop with an empty block for a body is reported, one that does something is not") {
    val found = flow(
      "empty-statement-body",
      "Loops" ->
        """package demo;
          |
          |import java.util.Queue;
          |
          |public class Loops {
          |  static byte[] drain(Queue<byte[]> queue) {
          |    byte[] item;
          |    while ((item = queue.poll()) == null) {
          |      // waiting
          |    }
          |    return item;
          |  }
          |
          |  static int count(java.io.InputStream in) throws Exception {
          |    int n = 0;
          |    while (in.read() >= 0) {}
          |    return n;
          |  }
          |
          |  static void work(Queue<byte[]> queue) {
          |    while (!queue.isEmpty()) {
          |      queue.poll();
          |    }
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line).sorted, Vector(8, 16), found.mkString("\n"))
  }

  test("an invocation handler lambda that returns null on every path is reported, one that answers a method is not") {
    val found = flow(
      "suspicious-invocation-handler",
      "Handlers2" ->
        """package demo;
          |
          |import java.lang.reflect.Proxy;
          |
          |public class Handlers2 {
          |  static Object quiet(Class<?> type) {
          |    return Proxy.newProxyInstance(type.getClassLoader(), new Class<?>[] {type}, (proxy, method, args) -> {
          |      if ("run".equals(method.getName())) {
          |        System.out.println("run");
          |      }
          |      return null;
          |    });
          |  }
          |
          |  static Object named(Class<?> type) {
          |    return Proxy.newProxyInstance(type.getClassLoader(), new Class<?>[] {type}, (proxy, method, args) -> {
          |      if (method.getName().equals("toString")) return "handler";
          |      return null;
          |    });
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(11), found.mkString("\n"))
  }

  test("a unicode escape of a character that cannot be written in the source is necessary") {
    val found = flow(
      "unnecessary-unicode-escape",
      "Escapes3" ->
        """package demo;
          |
          |public class Escapes3 {
          |  String arrow = "\u25C0";
          |  String line = "a\u2028b";
          |  char icon = '\ue000';
          |  String times = "\u00d7";
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line).sorted, Vector(4, 7), found.mkString("\n"))
  }

  test("a zero for the modifiers of an awt event says there are none") {
    val found = flow(
      "magic-constant",
      "Mouse" ->
        """package demo;
          |
          |import java.awt.event.MouseEvent;
          |
          |public class Mouse {
          |  static void send(java.awt.Component c) {
          |    new MouseEvent(c, 501, 0L, 0, 1, 2, 0, false, 0);
          |    new MouseEvent(c, 501, 0L, 8, 1, 2, 0, false, 0);
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(8), found.mkString("\n"))
  }

  test("a declared exception that a callee declares is not redundant") {
    val found = flow(
      "redundant-throws",
      "Throws" ->
        """package demo;
          |
          |public class Throws {
          |  static void load() throws Exception { throw new Exception("x"); }
          |  void test() throws Exception { load(); }
          |  private void bad() throws Exception { int x = 1; }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(6), found.mkString("\n"))
  }

  test("a backslash that an odd number of backslashes precede starts no unicode escape") {
    val found = flow(
      "unnecessary-unicode-escape",
      "Escapes4" ->
        """package demo;
          |
          |public class Escapes4 {
          |  String script = "\\u00e9";
          |  String real = "\u00e9";
          |  String doubled = "\\\u00e9";
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line).sorted, Vector(5, 6), found.mkString("\n"))
  }

  test("the model reads an enum parameter by its cases, and a constant or an enum case a call passes by its symbol") {
    val model = compile(
      "Args" ->
        """package demo;
          |
          |public class Args {
          |  enum Mode { FAST, SLOW }
          |
          |  static final int LIMIT = 5;
          |
          |  static void run(String name, int limit, Mode mode, int plain) {}
          |
          |  static void go(int plain) {
          |    run("a", LIMIT, Mode.FAST, plain);
          |  }
          |}
          |""".stripMargin
    )
    val method = model.methods.find(_.name == "demo.Args.run").getOrElse(fail(model.methods.mkString("\n")))
    assertEquals(method.parameters.map(_.kind), Vector("other", "number", "tag", "number"))
    val site = model.sites.find(
      _.callee == "demo.Args.run(Ljava/lang/String;ILdemo/Args$Mode;I)V"
    ).getOrElse(fail(model.sites.mkString("\n")))
    assertEquals(
      site.arguments,
      Vector("text:a", "symbol:demo.Args.LIMIT", "symbol:demo.Args.Mode.FAST", "present:plain")
    )
  }

  test("the optionals of primitives are reported in a parameter as Optional is") {
    val found = flowAt(
      Nil,
      Vector("optional-signature"),
      "optional-signature",
      "Bounds" ->
        """package demo;
          |
          |import java.util.Optional;
          |import java.util.OptionalInt;
          |import java.util.OptionalLong;
          |
          |public class Bounds {
          |  Bounds left(OptionalLong left) { return this; }
          |  Bounds top(OptionalInt top) { return this; }
          |  Bounds name(Optional<String> name) { return this; }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(8, 9, 10), found.mkString("\n"))
  }

  test("code of a type marked as generated is linted like any other code") {
    val source =
      """package demo;
        |
        |import java.util.Optional;
        |import javax.annotation.processing.Generated;
        |
        |@Generated("tool") public class Built {
        |  Built name(Optional<String> name) { return this; }
        |
        |  boolean same(String value) { return value != null; }
        |
        |  private void nothing() {}
        |}
        |""".stripMargin
    val plain = source.replace("@Generated(\"tool\") ", "")
    assertEquals(
      flowAt(Nil, Vector("optional-signature"), "optional-signature", "Built" -> source).map(_.line),
      Vector(7)
    )
    assertEquals(flow("empty-method", "Built" -> source).map(_.line), Vector(11))
    assertEquals(flow("constant-value", "Built" -> source), flow("constant-value", "Built" -> plain))
  }

  test("an Optional field is told to be a nullable field, and an Optional parameter to be an overload") {
    val found = flow(
      "optional-signature",
      "Settings" ->
        """package demo;
          |
          |import java.util.Optional;
          |
          |public class Settings {
          |  private Optional<String> name = Optional.empty();
          |
          |  public void apply(Optional<String> label) {}
          |
          |  public void both(Optional<String> label, Optional<Integer> size) {}
          |
          |  void helper(Optional<String> label) {}
          |
          |  private void inner(Optional<String> label, Optional<Integer> size) {}
          |
          |  static class Child extends Settings {
          |    @Override
          |    public void apply(Optional<String> label) {}
          |  }
          |}
          |""".stripMargin
    )
    val field    = "an Optional field is a nullable field in JSpecify: annotate the field @Nullable instead"
    val overload = "replace the Optional parameter with an overload that omits the argument; only as a last " +
      "resort annotate the parameter @Nullable"
    val grouped = "several Optional parameters: annotate them @Nullable, or group them in a parameter object or builder"
    val nullable = "an Optional parameter of a helper is a @Nullable parameter: annotate it instead"
    assertEquals(
      found.map(lint => lint.line -> lint.detail).sorted,
      Vector(6 -> field, 8 -> overload, 10 -> grouped, 12 -> nullable, 14 -> nullable, 14 -> nullable)
    )
  }

  test("a resource released by the release method it closes through is closed") {
    val found = flow(
      "auto-closeable-resource",
      "Panels" ->
        """package demo;
          |
          |public class Panels {
          |  static class Panel implements AutoCloseable {
          |    void release() {}
          |    public void close() { release(); }
          |  }
          |
          |  static void released() {
          |    Panel panel = new Panel();
          |    panel.release();
          |  }
          |
          |  static void leaked() {
          |    Panel panel = new Panel();
          |    panel.toString();
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(15), found.mkString("\n"))
  }

  test("a loop over a collection inside another loop over the same collection takes quadratic time") {
    val model = compile("Pairs" -> """
      |package demo;
      |
      |import java.util.List;
      |
      |public class Pairs {
      |  private final List<String> names = null;
      |
      |  public int each(List<String> values) {
      |    int count = 0;
      |    for (String left : values) {
      |      for (String right : values) {
      |        if (left.equals(right)) count++;
      |      }
      |    }
      |    return count;
      |  }
      |
      |  public int indexed() {
      |    int count = 0;
      |    for (int i = 0; i < names.size(); i++) {
      |      for (int j = 0; j < this.names.size(); j++) {
      |        count++;
      |      }
      |    }
      |    return count;
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "nested-loop-over-same-collection")
    assertEquals(found.map(_.method).distinct, Vector("demo.Pairs.each", "demo.Pairs.indexed"))
    assertEquals(
      found.map(_.detail).distinct.head,
      "the loop runs over values inside another loop over it, which takes quadratic time"
    )
  }

  test("loops over different collections, or one after the other, are not nested over the same collection") {
    val model = compile("Pairs" -> """
      |package demo;
      |
      |import java.util.List;
      |
      |public class Pairs {
      |  public int product(List<String> left, List<String> right) {
      |    int count = 0;
      |    for (String a : left) {
      |      for (String b : right) count++;
      |    }
      |    for (String a : left) count++;
      |    for (String b : left) count++;
      |    return count;
      |  }
      |
      |  public int drain(String[] pending) {
      |    int count = 0;
      |    while (pending.length > 0) {
      |      for (String value : pending) count++;
      |      pending = new String[0];
      |    }
      |    return count;
      |  }
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "nested-loop-over-same-collection"),
      s"no loop is inside a loop over the same collection: ${model.lints.mkString(", ")}"
    )
  }

  test("a search of a list on every turn of a loop reads the whole list each time") {
    val model = compile("Searches" -> """
      |package demo;
      |
      |import java.util.ArrayList;
      |import java.util.List;
      |
      |public class Searches {
      |  private final List<String> allowed = new ArrayList<>();
      |
      |  public int run(List<String> values) {
      |    int count = 0;
      |    for (String value : values) {
      |      if (allowed.contains(value)) count++;
      |      if (allowed.indexOf(value) > 0) count++;
      |    }
      |    return count;
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "list-search-in-loop")
    assertEquals(
      found.map(_.detail),
      Vector(
        "contains reads the whole list on every turn of the loop",
        "indexOf reads the whole list on every turn of the loop"
      )
    )
    assertEquals(found.head.method, "demo.Searches.run")
  }

  test("searches of sets, searches outside loops and calls on results are not list searches in a loop") {
    val model = compile("Searches" -> """
      |package demo;
      |
      |import java.util.HashSet;
      |import java.util.List;
      |import java.util.Set;
      |
      |public class Searches {
      |  private final Set<String> allowed = new HashSet<>();
      |
      |  public int run(List<String> values, List<String> others) {
      |    int count = others.contains("a") ? 1 : 0;
      |    for (String value : values) {
      |      if (allowed.contains(value)) count++;
      |      if (List.of(value).contains("a")) count++;
      |    }
      |    return count;
      |  }
      |}
      |""".stripMargin)

    assert(
      !model.lints.exists(_.id == "list-search-in-loop"),
      s"no list is searched in a loop: ${model.lints.mkString(", ")}"
    )
  }

  test("an edit at the front of an array list on every turn of a loop moves every element after it") {
    val model = compile("Fronts" -> """
      |package demo;
      |
      |import java.util.ArrayList;
      |import java.util.LinkedList;
      |import java.util.List;
      |
      |public class Fronts {
      |  public int drain(ArrayList<String> queue) {
      |    int count = 0;
      |    while (!queue.isEmpty()) {
      |      queue.remove(0);
      |      count++;
      |    }
      |    return count;
      |  }
      |
      |  public void fill(List<String> values) {
      |    for (int i = 0; i < 10; i++) {
      |      values.add(0, "a");
      |    }
      |  }
      |
      |  public void last(ArrayList<String> queue, LinkedList<String> linked) {
      |    while (!queue.isEmpty()) {
      |      queue.remove(queue.size() - 1);
      |      linked.remove(0);
      |    }
      |    queue.remove(0);
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "list-front-edit-in-loop")
    assertEquals(found.map(_.method), Vector("demo.Fronts.drain", "demo.Fronts.fill"))
    assertEquals(
      found.map(_.detail),
      Vector(
        "remove at the front of the list moves every element on each turn of the loop",
        "add at the front of the list moves every element on each turn of the loop"
      )
    )
  }

  test("an Optional.get guarded by a short-circuit presence check is not reported") {
    val model = compile("Guards" -> """
      |package demo;
      |
      |import java.util.Optional;
      |
      |public class Guards {
      |  public boolean both(Optional<String> value) {
      |    return value.isPresent() && value.get().isEmpty();
      |  }
      |
      |  public boolean either(Optional<String> value) {
      |    return value.isEmpty() || value.get().isEmpty();
      |  }
      |
      |  public boolean conjunction(Optional<String> value, boolean other) {
      |    if (other && value.isPresent() && value.get().isEmpty()) return true;
      |    return false;
      |  }
      |
      |  public String early(Optional<String> value, boolean other) {
      |    if (!value.isPresent() || other) return "none";
      |    return value.get();
      |  }
      |
      |  public boolean unguarded(Optional<String> value, boolean other) {
      |    return other || value.get().isEmpty();
      |  }
      |
      |  public boolean wrongBranch(Optional<String> value) {
      |    return value.isPresent() || value.get().isEmpty();
      |  }
      |}
      |""".stripMargin)

    val gets = model.lints.filter(_.id == "optional-get")
    assertEquals(gets.size, 2, model.lints.mkString("\n"))
    assertEquals(gets.map(_.method.split('.').last).toSet, Set("unguarded", "wrongBranch"))
  }

  test("a latch await without a result is not an ignored result, and a timed one is") {
    val model = compile("Latches" -> """
      |package demo;
      |
      |import java.util.concurrent.CountDownLatch;
      |import java.util.concurrent.TimeUnit;
      |
      |public class Latches {
      |  public void untimed(CountDownLatch latch) throws InterruptedException {
      |    latch.await();
      |  }
      |
      |  public void timed(CountDownLatch latch) throws InterruptedException {
      |    latch.await(1, TimeUnit.SECONDS);
      |  }
      |}
      |""".stripMargin)

    val ignored = model.lints.filter(_.id == "ignored-call-result")
    assertEquals(ignored.size, 1, model.lints.mkString("\n"))
    assert(ignored.head.method.endsWith("timed"), ignored.head.method)
  }

  test("a field copied between two objects is not a self assignment") {
    val model = compile("Copies" -> """
      |package demo;
      |
      |public class Copies {
      |  int value;
      |
      |  void copy(Copies other) {
      |    this.value = other.value;
      |    other.value = this.value;
      |  }
      |
      |  void same() {
      |    this.value = value;
      |  }
      |
      |  void sameQualified(Copies other) {
      |    other.value = other.value;
      |  }
      |}
      |""".stripMargin)

    val assigned = model.lints.filter(_.id == "self-assignment")
    assertEquals(assigned.size, 2, model.lints.mkString("\n"))
    assertEquals(assigned.map(_.method.split('.').last).toSet, Set("same", "sameQualified"))
  }

  test("an interrupt that is restored is not a swallowed exception") {
    val model = compile("Interrupts" -> """
      |package demo;
      |
      |public class Interrupts {
      |  void restored() {
      |    try {
      |      Thread.sleep(1);
      |    } catch (InterruptedException interrupted) {
      |      Thread.currentThread().interrupt();
      |    }
      |  }
      |
      |  void dropped() {
      |    try {
      |      Thread.sleep(1);
      |    } catch (InterruptedException interrupted) {
      |      System.out.println("woken");
      |    }
      |  }
      |}
      |""".stripMargin)

    val swallowed = model.lints.filter(_.id == "swallowed-exception")
    assertEquals(swallowed.size, 1, model.lints.mkString("\n"))
    assert(swallowed.head.method.endsWith("dropped"), swallowed.head.method)
  }

  test("an exception thrown from a catch block that drops the caught exception as its cause is reported") {
    val model = compile("Rethrow" -> """
      |package demo;
      |
      |public class Rethrow {
      |  void dropped(String value) {
      |    try {
      |      Integer.parseInt(value);
      |    } catch (IllegalArgumentException failure) {
      |      throw new IllegalArgumentException("Invalid: " + value);
      |    }
      |  }
      |
      |  void preserved(String value) {
      |    try {
      |      Integer.parseInt(value);
      |    } catch (IllegalArgumentException failure) {
      |      throw new IllegalArgumentException("Invalid: " + value, failure);
      |    }
      |  }
      |
      |  void chained(String value) {
      |    try {
      |      Integer.parseInt(value);
      |    } catch (IllegalArgumentException failure) {
      |      IllegalArgumentException ex = new IllegalArgumentException("Invalid: " + value);
      |      ex.initCause(failure);
      |      throw ex;
      |    }
      |  }
      |
      |  void rethrown(String value) {
      |    try {
      |      Integer.parseInt(value);
      |    } catch (IllegalArgumentException failure) {
      |      throw failure;
      |    }
      |  }
      |}
      |""".stripMargin)

    val swallowed = model.lints.filter(_.id == "swallowed-exception")
    assertEquals(swallowed.size, 1, model.lints.mkString("\n"))
    assert(swallowed.head.method.endsWith("dropped"), swallowed.head.method)
    assertEquals(
      swallowed.head.detail,
      "the exception thrown from the catch block drops the caught exception as its cause"
    )
  }

  test("a same-named field passed as a cause does not hide a dropped exception") {
    val model = compile("Causes" -> """
      |package demo;
      |
      |public class Causes {
      |  private String cause;
      |
      |  public void dropped(String value) {
      |    try {
      |      Integer.parseInt(value);
      |    } catch (IllegalArgumentException failure) {
      |      if (value != null) {
      |        Exception cause = failure;
      |      }
      |      throw new IllegalStateException(cause);
      |    }
      |  }
      |
      |  public void derived(String value) {
      |    try {
      |      Integer.parseInt(value);
      |    } catch (IllegalArgumentException failure) {
      |      Exception cause = failure;
      |      throw new IllegalStateException(cause);
      |    }
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "swallowed-exception")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
    assert(found.head.method.endsWith("dropped"), found.head.method)
    assertEquals(
      found.head.detail,
      "the exception thrown from the catch block drops the caught exception as its cause"
    )
  }

  test("a message derived from the caught exception is not a cause") {
    val model = compile("Messages" -> """
      |package demo;
      |
      |public class Messages {
      |  public void dropped(String value) {
      |    try {
      |      Integer.parseInt(value);
      |    } catch (IllegalArgumentException failure) {
      |      String message = failure.getMessage();
      |      throw new IllegalStateException(message);
      |    }
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "swallowed-exception")
    assertEquals(found.size, 1, model.lints.mkString("\n"))
    assert(found.head.method.endsWith("dropped"), found.head.method)
    assertEquals(
      found.head.detail,
      "the exception thrown from the catch block drops the caught exception as its cause"
    )
  }

  test("a cause held in an Optional is recognised as the caught exception") {
    val model = compile("Holders" -> """
      |package demo;
      |
      |import java.util.Optional;
      |
      |public class Holders {
      |  public void run(String value) {
      |    try {
      |      Integer.parseInt(value);
      |    } catch (IllegalArgumentException failure) {
      |      Optional<Throwable> held = Optional.of(failure);
      |      throw new IllegalStateException("invalid", held.get());
      |    }
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id == "swallowed-exception").size, 0, model.lints.mkString("\n"))
  }

  test("a cause held in an array or a List is recognised as the caught exception") {
    val model = compile("Holders" -> """
      |package demo;
      |
      |import java.util.List;
      |
      |public class Holders {
      |  public void array(String value) {
      |    try {
      |      Integer.parseInt(value);
      |    } catch (IllegalArgumentException failure) {
      |      Throwable[] held = { failure };
      |      throw new IllegalStateException("invalid", held[0]);
      |    }
      |  }
      |
      |  public void list(String value) {
      |    try {
      |      Integer.parseInt(value);
      |    } catch (IllegalArgumentException failure) {
      |      List<Throwable> held = List.of(failure);
      |      throw new IllegalStateException("invalid", held.get(0));
      |    }
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id == "swallowed-exception").size, 0, model.lints.mkString("\n"))
  }

  test("an exception thrown by orElseThrow counts as thrown, and an unthrown one does not") {
    val model = compile("Throwing" -> """
      |package demo;
      |
      |import java.io.IOException;
      |import java.util.Optional;
      |
      |public class Throwing {
      |  String thrown(Optional<String> value) throws IOException {
      |    return value.orElseThrow(() -> new IOException("none"));
      |  }
      |
      |  private String unthrown(Optional<String> value) throws IOException {
      |    return value.orElse("none");
      |  }
      |}
      |""".stripMargin)

    val redundant = model.lints.filter(_.id == "redundant-throws")
    assertEquals(redundant.size, 1, model.lints.mkString("\n"))
    assert(redundant.head.method.endsWith("unthrown"), redundant.head.method)
  }

  test("a loop that fills an array with a value of the index is not an explicit fill") {
    val model = compile("Fills" -> """
      |package demo;
      |
      |public class Fills {
      |  void constant(int[] array) {
      |    for (int i = 0; i < array.length; i++) {
      |      array[i] = 7;
      |    }
      |  }
      |
      |  void varying(int[] array, int[] source) {
      |    for (int i = 0; i < array.length; i++) {
      |      array[i] = source[2 + i];
      |    }
      |  }
      |}
      |""".stripMargin)

    val fills = model.lints.filter(_.id == "explicit-array-filling")
    assertEquals(fills.size, 1, model.lints.mkString("\n"))
    assert(fills.head.method.endsWith("constant"), fills.head.method)
  }

  test("the size of a collection made inside the loop is not asked for again") {
    val model = compile("Sizes" -> """
      |package demo;
      |
      |import java.util.ArrayList;
      |import java.util.List;
      |
      |public class Sizes {
      |  int outside(List<String> items) {
      |    int total = 0;
      |    for (int i = 0; i < 3; i++) {
      |      total += items.size();
      |    }
      |    return total;
      |  }
      |
      |  int inside(int count) {
      |    int total = 0;
      |    for (int i = 0; i < count; i++) {
      |      List<String> made = new ArrayList<>();
      |      made.add("x");
      |      total += made.size();
      |    }
      |    return total;
      |  }
      |}
      |""".stripMargin)

    val sizes = model.lints.filter(_.id == "collection-size-in-loop")
    assertEquals(sizes.size, 1, model.lints.mkString("\n"))
    assert(sizes.head.method.endsWith("outside"), sizes.head.method)
  }

  test("an Optional parameter of a lambda is set by its interface and is not reported") {
    val model = compile("Lambdas" -> """
      |package demo;
      |
      |import java.util.Optional;
      |import java.util.function.BiConsumer;
      |
      |public class Lambdas {
      |  public void run() {
      |    BiConsumer<String, Optional<String>> sink = (name, value) -> {};
      |    sink.accept("a", Optional.empty());
      |  }
      |
      |  public void declared(Optional<String> value) {}
      |}
      |""".stripMargin)

    val signatures = model.lints.filter(_.id == "optional-signature")
    assertEquals(signatures.size, 1, model.lints.mkString("\n"))
    assert(signatures.head.method.endsWith("declared"), signatures.head.method)
  }

  test("a type annotated as generated is a generated node, with the types it holds") {
    val model = compile(
      "Made" -> """
      |package demo;
      |
      |@javax.annotation.processing.Generated("tool")
      |public class Made {
      |  public static class Inner {}
      |}
      |""".stripMargin,
      "Written" -> """
      |package demo;
      |
      |public class Written {}
      |""".stripMargin
    )

    assertEquals(model.nodes.filter(_.generated).map(_.name).toSet, Set("demo.Made", "demo.Made.Inner"))
  }

  test("a type named through its holder is used") {
    val model = compile(
      "Holder" -> """
      |package demo;
      |
      |@Marker(Holder.Named.class)
      |public class Holder {
      |  static final class Named {}
      |  static final class Lonely {}
      |}
      |""".stripMargin,
      "Marker" -> """
      |package demo;
      |
      |public @interface Marker {
      |  Class<?> value();
      |}
      |""".stripMargin
    )

    val used = model.edges.filter(_.from == "demo.Holder").map(_.to).toSet
    assert(used.contains("demo.Holder.Named"), used.toString)
    assert(!used.contains("demo.Holder.Lonely"), used.toString)
  }

  test(
    "an array element guarded by short-circuit null check is not reported as nullable-use, while unguarded access keeps firing"
  ) {
    val found = flow(
      "nullable-use",
      "ArrayGuard" ->
        """package demo;
          |import org.jspecify.annotations.Nullable;
          |public class ArrayGuard {
          |  static int test(@Nullable String[] args, int i) {
          |    if (args[i] == null || args[i].isEmpty()) {
          |      return 0;
          |    }
          |    return args[i].length();
          |  }
          |  static int dead(@Nullable String[] args, int i) {
          |    return args[i].length();
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(11))
  }

  test(
    "a captured variable guarded by a null check before capture in an anonymous class is not reported as nullable-use, while unguarded capture keeps firing"
  ) {
    val found = flow(
      "nullable-use",
      "CapturedGuard" ->
        """package demo;
          |import org.jspecify.annotations.Nullable;
          |public class CapturedGuard {
          |  interface Callback { void cancel(); }
          |  static void runGuarded(@Nullable Callback callback) {
          |    if (callback == null) return;
          |    Runnable r = new Runnable() {
          |      public void run() {
          |        callback.cancel();
          |      }
          |    };
          |  }
          |  static void runUnguarded(@Nullable Callback callback) {
          |    Runnable r = new Runnable() {
          |      public void run() {
          |        callback.cancel();
          |      }
          |    };
          |  }
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(16))
  }

  test(
    "branches ending with statements naming distinct-typed branch-local variables are not reported as if-identical-branches, while truly identical branch ends keep firing"
  ) {
    val model = compile("DistinctBranchLocals" -> """
      |package demo;
      |
      |public class DistinctBranchLocals {
      |  static class TypeA { int get() { return 1; } }
      |  static class TypeB { int get() { return 2; } }
      |
      |  public int testDistinct(boolean flag) {
      |    if (flag) {
      |      TypeA wi = new TypeA();
      |      return wi.get();
      |    } else {
      |      TypeB wi = new TypeB();
      |      return wi.get();
      |    }
      |  }
      |
      |  public int testIdentical(boolean flag) {
      |    if (flag) {
      |      int a = 1;
      |      return 42;
      |    } else {
      |      int b = 2;
      |      return 42;
      |    }
      |  }
      |}
      |""".stripMargin)
    val found = model.lints.filter(_.id == "if-identical-branches")
    assertEquals(found.size, 1, found.mkString("\n"))
  }

  test("method references are recorded as call sites with unknown arguments, while direct calls keep their arguments") {
    val model = compile("MethodRefs" -> """
      |package demo;
      |
      |import java.util.function.Consumer;
      |
      |public class MethodRefs {
      |  public void openNewTab(String url) {}
      |
      |  public void use() {
      |    Consumer<String> c = this::openNewTab;
      |    openNewTab("https://example.com");
      |  }
      |}
      |""".stripMargin)
    val sites = model.sites.filter(_.callee == "demo.MethodRefs.openNewTab(Ljava/lang/String;)V")
    assertEquals(sites.size, 2, model.sites.mkString("\n"))
    assertEquals(sites.map(_.arguments), Vector(Vector("unknown"), Vector("text:https://example.com")))
  }

  test(
    "a method with @SuppressWarnings using an IntelliJ inspection name stays silent while an unsuppressed sibling fires"
  ) {
    val model = compile("SuppressedMethods" -> """
      |package demo;
      |
      |import java.util.Optional;
      |
      |public class SuppressedMethods {
      |  @SuppressWarnings("EmptyMethod")
      |  private void quietEmpty() {}
      |
      |  private void noisyEmpty() {}
      |
      |  @SuppressWarnings("SameReturnValue")
      |  private int quietSame() {
      |    return 42;
      |  }
      |
      |  private int noisySame() {
      |    return 42;
      |  }
      |
      |  @SuppressWarnings("UnnecessaryUnboxing")
      |  public int quietUnbox(Integer boxed) {
      |    return boxed.intValue();
      |  }
      |
      |  public int noisyUnbox(Integer boxed) {
      |    return boxed.intValue();
      |  }
      |
      |  @SuppressWarnings("OptionalGetWithoutIsPresent")
      |  public String quietGet(Optional<String> opt) {
      |    return opt.get();
      |  }
      |
      |  public String noisyGet(Optional<String> opt) {
      |    return opt.get();
      |  }
      |}
      |""".stripMargin)

    val emptyMethods = model.lints.filter(_.id == "empty-method")
    assertEquals(emptyMethods.map(_.method), Vector("demo.SuppressedMethods.noisyEmpty"))

    val sameReturns = model.lints.filter(_.id == "same-return-value")
    assertEquals(sameReturns.map(_.method), Vector("demo.SuppressedMethods.noisySame"))

    val unboxings = model.lints.filter(_.id == "unnecessary-unboxing")
    assertEquals(unboxings.map(_.method), Vector("demo.SuppressedMethods.noisyUnbox"))

    val optionalGets = model.lints.filter(_.id == "optional-get")
    assertEquals(optionalGets.map(_.method), Vector("demo.SuppressedMethods.noisyGet"))
  }

  private def catches(suppression: String) = compile("Catches" -> s"""
      |package demo;
      |
      |public class Catches {
      |  $suppression
      |  public void run() {
      |    try {
      |      work();
      |    } catch (Exception e) {
      |    }
      |    try {
      |      work();
      |    } catch (Exception ignored) {
      |      System.out.println(ignored);
      |    }
      |  }
      |
      |  private void work() throws Exception {
      |    throw new Exception("failure");
      |  }
      |}
      |""".stripMargin).lints.collect { case lint if Caught(lint.id) => lint.id }

  test("the EmptyCatchBlock inspection names the swallowed exception and not the ignored one that is read") {
    assertEquals(catches("@SuppressWarnings(\"EmptyCatchBlock\")"), Vector("catch-may-ignore-exception"))
  }

  test("the CatchMayIgnoreException inspection names the ignored exception that is read and not the swallowed one") {
    assertEquals(catches("@SuppressWarnings(\"CatchMayIgnoreException\")"), Vector("swallowed-exception"))
  }

  test("an unsuppressed method reports both the swallowed and the ignored exception") {
    assertEquals(catches("").sorted, Vector("catch-may-ignore-exception", "swallowed-exception"))
  }

  private val Caught      = Set("swallowed-exception", "catch-may-ignore-exception")
  private val Conditional = Set("simplifiable-conditional", "trivial-if")

  private def conditionals(suppression: String) = compile("Conditionals" -> s"""
      |package demo;
      |
      |public class Conditionals {
      |  $suppression
      |  public boolean run(boolean value, int count) {
      |    String text = value ? "true" : "false";
      |    if (count > 1) {
      |      return true;
      |    } else {
      |      return false;
      |    }
      |  }
      |}
      |""".stripMargin).lints.collect { case lint if Conditional(lint.id) => lint.id }

  test("the SimplifiableConditionalExpression inspection names the conditional and not the trivial if") {
    assertEquals(conditionals("@SuppressWarnings(\"SimplifiableConditionalExpression\")"), Vector("trivial-if"))
  }

  test("the TrivialIf inspection names the trivial if and not the conditional") {
    assertEquals(conditionals("@SuppressWarnings(\"TrivialIf\")"), Vector("simplifiable-conditional"))
  }

  test("an unsuppressed method reports both the conditional and the trivial if") {
    assertEquals(conditionals("").sorted, Vector("simplifiable-conditional", "trivial-if"))
  }

  test("a method with //noinspection stays silent while an unsuppressed sibling fires") {
    val model = compile("NoInspectionMethods" -> """
      |package demo;
      |
      |public class NoInspectionMethods {
      |  //noinspection EmptyMethod
      |  private void quiet() {}
      |
      |  private void noisy() {}
      |
      |  //noinspection PointlessBooleanExpression
      |  public boolean quietBool(boolean x) {
      |    return x && true;
      |  }
      |
      |  public boolean noisyBool(boolean x) {
      |    return x && true;
      |  }
      |}
      |""".stripMargin)

    val emptyMethods = model.lints.filter(_.id == "empty-method")
    assertEquals(emptyMethods.map(_.method), Vector("demo.NoInspectionMethods.noisy"))

    val booleans = model.lints.filter(_.id == "pointless-boolean")
    assertEquals(booleans.map(_.method), Vector("demo.NoInspectionMethods.noisyBool"))
  }

  test("class-level and statement-level suppressions scope correctly") {
    val model = compile(
      "ClassLevel" -> """
        |package demo;
        |
        |@SuppressWarnings("EmptyMethod")
        |public class ClassLevel {
        |  private void first() {}
        |  private void second() {}
        |}
        |""".stripMargin,
      "SiblingClass" -> """
        |package demo;
        |
        |public class SiblingClass {
        |  private void unsuppressed() {}
        |}
        |""".stripMargin,
      "StatementLevel" -> """
        |package demo;
        |
        |public class StatementLevel {
        |  public boolean check(boolean x) {
        |    //noinspection PointlessBooleanExpression
        |    boolean a = x && true;
        |    boolean b = x && true;
        |    return a || b;
        |  }
        |}
        |""".stripMargin
    )

    val emptyMethods = model.lints.filter(_.id == "empty-method")
    assertEquals(emptyMethods.map(_.method), Vector("demo.SiblingClass.unsuppressed"))

    val booleans = model.lints.filter(_.id == "pointless-boolean")
    assertEquals(booleans.map(_.line), Vector(8))
  }

  test("a null comparison of an unassigned array slot names the unassigned slot") {
    val found = flow(
      "constant-value",
      marking,
      "ArraySlot" -> """
        |package demo;
        |
        |public class ArraySlot {
        |  static boolean check() {
        |    String[] items = new String[2];
        |    items[1] = null;
        |    return items[1] == null;
        |  }
        |}
        |""".stripMargin
    )
    assertEquals(found.map(_.detail), Vector("the condition is always true, as slot 1 is unassigned"))
  }

  test("an array access of a nullable array element is not constant") {
    val found = flow(
      "constant-value",
      marking,
      "NullableArray" -> """
        |package demo;
        |
        |import org.jspecify.annotations.Nullable;
        |
        |public class NullableArray {
        |  boolean checkParam(@Nullable String[] holder) {
        |    return holder[0] == null;
        |  }
        |  boolean checkLocal() {
        |    @Nullable String[] arr = new @Nullable String[1];
        |    return arr[0] == null;
        |  }
        |}
        |""".stripMargin
    )
    assertEquals(found, Vector.empty)
  }

  test("a call on a class with NullableBoundary returns unknown nullness") {
    val found = flow(
      "constant-value",
      marking,
      "NullableBoundary" -> "package demo; public @interface NullableBoundary { String value(); }",
      "Engine"           -> """
        |package demo;
        |
        |@NullableBoundary("JavaFX bean properties use null for unset values")
        |public class Engine {
        |  public String getTitle() { return ""; }
        |}
        |""".stripMargin,
      "Client" -> """
        |package demo;
        |
        |public class Client {
        |  boolean check(Engine engine) {
        |    String title = engine.getTitle();
        |    return title == null;
        |  }
        |}
        |""".stripMargin
    )
    assertEquals(found, Vector.empty)
  }

  test("a field declared non-null but assigned after construction reports its reason in constant-value") {
    val found = flow(
      "constant-value",
      marking,
      "LateField" -> """
        |package demo;
        |
        |public class LateField {
        |  private String field;
        |  void init(String f) { this.field = f; }
        |  boolean check() {
        |    return field == null;
        |  }
        |}
        |""".stripMargin,
      "NormalField" -> """
        |package demo;
        |
        |public class NormalField {
        |  private String field;
        |  NormalField(String f) { this.field = f; }
        |  boolean check() {
        |    return field == null;
        |  }
        |}
        |""".stripMargin
    )
    assertEquals(
      found.map(_.detail),
      Vector(
        "the condition is always false, as the field is declared non-null but read before it is assigned; annotate it @Nullable or assign it in the constructor",
        "the condition is always false, as the value is not nullable"
      )
    )
  }

  test("a catch block that rethrows a local derived from the caught exception does not drop the cause") {
    val model = compile("DerivedCause" -> """
      |package demo;
      |
      |import java.io.IOException;
      |import java.lang.reflect.InvocationTargetException;
      |
      |public class DerivedCause {
      |  void ternaryCause(InvocationTargetException input) throws IOException {
      |    try {
      |      throw input;
      |    } catch (InvocationTargetException e) {
      |      Throwable cause = e.getCause();
      |      throw cause instanceof IOException io ? io : new IOException(cause);
      |    }
      |  }
      |
      |  void patternBranch(InvocationTargetException input) throws IOException {
      |    try {
      |      throw input;
      |    } catch (InvocationTargetException e) {
      |      Throwable cause = e.getCause();
      |      if (cause instanceof IOException io) throw io;
      |      throw new IOException("failed", cause);
      |    }
      |  }
      |
      |  void localWrapped(String value) {
      |    try {
      |      Integer.parseInt(value);
      |    } catch (IllegalArgumentException e) {
      |      IllegalStateException failure = new IllegalStateException("failed", e);
      |      throw failure;
      |    }
      |  }
      |
      |  void helperWrapped(String value) {
      |    try {
      |      Integer.parseInt(value);
      |    } catch (IllegalArgumentException e) {
      |      throw wrap(e);
      |    }
      |  }
      |
      |  private IllegalStateException wrap(Throwable cause) {
      |    return new IllegalStateException(cause);
      |  }
      |
      |  int fallback(String value) {
      |    try {
      |      return Integer.parseInt(value);
      |    } catch (IllegalArgumentException e) {
      |      return -1;
      |    }
      |  }
      |
      |  void dropped(String value) {
      |    try {
      |      Integer.parseInt(value);
      |    } catch (IllegalArgumentException e) {
      |      throw new IllegalStateException("message");
      |    }
      |  }
      |}
      |""".stripMargin)

    val swallowed = model.lints.filter(_.id == "swallowed-exception")
    assertEquals(
      swallowed.map(f => (f.method, f.detail)),
      Vector(
        ("demo.DerivedCause.fallback", "the exception is caught and never looked at"),
        (
          "demo.DerivedCause.dropped",
          "the exception thrown from the catch block drops the caught exception as its cause"
        )
      )
    )
  }

  test("constructor calls that already use the diamond operator are not reported by convert-2-diamond") {
    val model = compile("AlreadyDiamond" -> """
      |package demo;
      |
      |import java.util.ArrayList;
      |import java.util.Comparator;
      |import java.util.concurrent.CompletableFuture;
      |import java.util.concurrent.atomic.AtomicReference;
      |
      |public class AlreadyDiamond {
      |  private final AtomicReference<CompletableFuture<?>> active = new AtomicReference<>();
      |
      |  void run() {
      |    final ArrayList<String> diamondList = new ArrayList<>();
      |    final Comparator<String> diamondAnon = new Comparator<>() {
      |      @Override
      |      public int compare(String left, String right) {
      |        return 0;
      |      }
      |    };
      |    final ArrayList<String> explicitList = new ArrayList<String>();
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "convert-2-diamond")
    assertEquals(found.map(_.line), Vector(20))
  }

  private val nullableReturn = "the method can return null: return Optional<T> (or OptionalInt, OptionalLong, " +
    "OptionalDouble) instead"

  test("a method that returns the null literal can return null, and an Optional return is the way out") {
    val found = compile(
      "Finders" ->
        """package demo;
          |
          |import java.util.Optional;
          |
          |public class Finders {
          |  String literal(boolean flag) {
          |    if (flag) return "x";
          |    return null;
          |  }
          |
          |  String wrapped() {
          |    return (null);
          |  }
          |
          |  String choice(boolean flag) {
          |    return flag ? "x" : null;
          |  }
          |
          |  Optional<String> optional(boolean flag) {
          |    return flag ? Optional.of("x") : Optional.empty();
          |  }
          |
          |  String never(boolean flag) {
          |    return flag ? "x" : "y";
          |  }
          |
          |  int primitive() {
          |    return 1;
          |  }
          |
          |  void nothing() {}
          |}
          |""".stripMargin
    ).lints.filter(_.id == "nullable-return")
    assertEquals(
      found.map(lint => lint.line -> lint.method),
      Vector(
        8  -> "demo.Finders.literal",
        12 -> "demo.Finders.wrapped",
        16 -> "demo.Finders.choice"
      ),
      found.mkString("\n")
    )
    assertEquals(found.map(_.detail).distinct, Vector(nullableReturn))
  }

  test("a nullable annotation on the return type or the method is reported whichever annotation it is") {
    val found = compile(
      "Nullable"  -> "package demo; public @interface Nullable {}",
      "Annotated" ->
        """package demo;
          |
          |import java.util.Optional;
          |import org.jspecify.annotations.Nullable;
          |
          |public interface Annotated {
          |  @Nullable String jspecify();
          |
          |  @demo.Nullable String own();
          |
          |  @Nullable
          |  String onMethod();
          |
          |  @Nullable Optional<String> optional();
          |
          |  String plain();
          |
          |  Optional<String> wrapped();
          |
          |  default @Nullable String body() {
          |    return "x";
          |  }
          |}
          |""".stripMargin
    ).lints.filter(_.id == "nullable-return")
    assertEquals(found.map(_.line).sorted, Vector(7, 9, 12, 14, 20), found.mkString("\n"))
    assertEquals(found.map(_.detail).distinct, Vector(nullableReturn))
  }

  test("a value the data flow proves may be null is reported, and one it proves present is not") {
    val found = compile(
      "Flows" ->
        """package demo;
          |
          |import java.util.Map;
          |import org.jspecify.annotations.NullMarked;
          |import org.jspecify.annotations.Nullable;
          |
          |@NullMarked
          |public class Flows {
          |  private @Nullable String field;
          |
          |  private static @Nullable String find(String key) {
          |    return key.isEmpty() ? null : key;
          |  }
          |
          |  String indirect(String key) {
          |    return find(key);
          |  }
          |
          |  String stored() {
          |    return field;
          |  }
          |
          |  String local(String key) {
          |    String value = find(key);
          |    return value;
          |  }
          |
          |  String checked(String key) {
          |    String value = find(key);
          |    if (value == null) return "none";
          |    return value;
          |  }
          |
          |  String defaulted(String key) {
          |    String value = find(key);
          |    return value == null ? "none" : value;
          |  }
          |
          |  String unknown(Map<String, String> map, String key) {
          |    return map.getClass().getName();
          |  }
          |}
          |""".stripMargin
    ).lints.filter(_.id == "nullable-return")
    assertEquals(
      found.map(lint => lint.line -> lint.method),
      Vector(
        11 -> "demo.Flows.find",
        16 -> "demo.Flows.indirect",
        20 -> "demo.Flows.stored",
        25 -> "demo.Flows.local"
      ),
      found.mkString("\n")
    )
  }

  test("a method of a type that implements a method of the library keeps the signature the library fixes") {
    val found = compile(
      "Fixed" ->
        """package demo;
          |
          |import java.util.concurrent.Callable;
          |import java.util.function.Supplier;
          |import org.jspecify.annotations.Nullable;
          |
          |public class Fixed implements Callable<@Nullable String>, Supplier<String> {
          |  @Override
          |  public @Nullable String call() {
          |    return null;
          |  }
          |
          |  @Override
          |  public String get() {
          |    return null;
          |  }
          |
          |  @Override
          |  public String toString() {
          |    return null;
          |  }
          |
          |  Callable<String> anonymous() {
          |    return new Callable<String>() {
          |      @Override
          |      public String call() {
          |        return null;
          |      }
          |    };
          |  }
          |
          |  String own() {
          |    return null;
          |  }
          |}
          |""".stripMargin
    ).lints.filter(_.id == "nullable-return")
    assertEquals(found.map(lint => lint.line -> lint.method), Vector(33 -> "demo.Fixed.own"), found.mkString("\n"))
  }

  test("an implementation of a method of the analysed code is reported, as its signature is the code's to change") {
    val found = compile(
      "Source" -> "package demo; public interface Source { String load(); }",
      "Files"  ->
        """package demo;
          |
          |public class Files implements Source {
          |  @Override
          |  public String load() {
          |    return null;
          |  }
          |}
          |""".stripMargin
    ).lints.filter(_.id == "nullable-return")
    assertEquals(found.map(lint => lint.line -> lint.method), Vector(6 -> "demo.Files.load"), found.mkString("\n"))
  }

  test("a null returned for Void, or from a lambda, is not a method that can return null") {
    val found = compile(
      "Frameworks" ->
        """package demo;
          |
          |import java.util.Optional;
          |import java.util.concurrent.Callable;
          |import java.util.concurrent.CompletableFuture;
          |
          |public class Frameworks {
          |  Void nothing() {
          |    return null;
          |  }
          |
          |  CompletableFuture<Void> clear(CompletableFuture<String> future) {
          |    return future.thenApply(ignored -> null);
          |  }
          |
          |  Optional<String> lambda(Optional<String> value) {
          |    return value.map(text -> {
          |      if (text.isEmpty()) return null;
          |      return text;
          |    });
          |  }
          |}
          |""".stripMargin
    ).lints.filter(_.id == "nullable-return")
    assertEquals(found, Vector.empty, found.mkString("\n"))
  }

  test("a method of a type marked as generated that can return null is reported like any other") {
    val found = compile(
      "Built" ->
        """package demo;
          |
          |import javax.annotation.processing.Generated;
          |
          |@Generated("tool") public class Built {
          |  String name() {
          |    return null;
          |  }
          |}
          |""".stripMargin
    ).lints.filter(_.id == "nullable-return")
    assertEquals(found.map(_.line), Vector(7), found.mkString("\n"))
  }

  test("a bare type variable returned from a method is reported only for a null the method returns itself") {
    val found = compile(
      "Passthrough" ->
        """package demo;
          |
          |import java.util.concurrent.CompletableFuture;
          |import org.jspecify.annotations.NullMarked;
          |import org.jspecify.annotations.Nullable;
          |
          |@NullMarked
          |public final class Passthrough {
          |  static <T extends @Nullable Object> T await(CompletableFuture<T> future) throws Exception {
          |    return future.get();
          |  }
          |
          |  static <T> T identity(T value) {
          |    return value;
          |  }
          |
          |  static <T> @Nullable T annotated(T value) {
          |    return value;
          |  }
          |
          |  static <T> T none() {
          |    return null;
          |  }
          |
          |  static <T> T guarded(@Nullable T value, T fallback) {
          |    return value == null ? fallback : value;
          |  }
          |
          |  static <T> T choice(@Nullable T value) {
          |    return value == null ? null : value;
          |  }
          |}
          |""".stripMargin
    ).lints.filter(_.id == "nullable-return")
    assertEquals(
      found.map(lint => lint.line -> lint.method),
      Vector(
        17 -> "demo.Passthrough.annotated",
        22 -> "demo.Passthrough.none",
        30 -> "demo.Passthrough.choice"
      ),
      found.mkString("\n")
    )
  }

  test("a null passed as an argument does not make the value a call returns null") {
    val found = compile(
      "NullableBoundary" -> "package demo; public @interface NullableBoundary { String value(); }",
      "Writers"          ->
        """package demo;
          |
          |import org.jspecify.annotations.NullMarked;
          |import org.jspecify.annotations.Nullable;
          |
          |@NullMarked
          |@NullableBoundary("paint callbacks use null for an unspecified region")
          |public interface Writers<I> {
          |  default I stamp(int[] pixels) {
          |    return stamp(null, pixels);
          |  }
          |
          |  I stamp(@Nullable I prev, int[] pixels);
          |
          |  default String name(@Nullable String prev) {
          |    return describe(null, prev);
          |  }
          |
          |  String describe(@Nullable String first, @Nullable String second);
          |}
          |""".stripMargin
    ).lints.filter(_.id == "nullable-return")
    assertEquals(found, Vector.empty, found.mkString("\n"))
  }

  test("a ternary that returns the null literal beside a value that may be null is reported") {
    val found = compile(
      "Ternary" ->
        """package demo;
          |
          |import org.jspecify.annotations.NullMarked;
          |import org.jspecify.annotations.Nullable;
          |
          |@NullMarked
          |public class Ternary {
          |  static class Name {
          |    String label() {
          |      return "x";
          |    }
          |  }
          |
          |  String named(@Nullable Name name) {
          |    return name == null ? null : name.label();
          |  }
          |}
          |""".stripMargin
    ).lints.filter(_.id == "nullable-return")
    assertEquals(found.map(lint => lint.line -> lint.method), Vector(15 -> "demo.Ternary.named"), found.mkString("\n"))
  }

  test("a null in a branch of a returned ternary is reported, whatever the other branch is") {
    val found = compile(
      "Branches" ->
        """package demo;
          |
          |import org.jspecify.annotations.NullMarked;
          |
          |@NullMarked
          |public class Branches {
          |  String choose(boolean flag, String text) {
          |    return flag ? null : text.trim();
          |  }
          |
          |  String parenthesized(boolean flag, String text) {
          |    return (flag ? "x" : (null));
          |  }
          |
          |  <T> T pick(boolean flag, T value) {
          |    return flag ? null : value;
          |  }
          |}
          |""".stripMargin
    ).lints.filter(_.id == "nullable-return")
    assertEquals(
      found.map(lint => lint.line -> lint.method),
      Vector(8 -> "demo.Branches.choose", 12 -> "demo.Branches.parenthesized", 16 -> "demo.Branches.pick"),
      found.mkString("\n")
    )
  }

  test("a nullable annotation on a type variable return is reported, abstract or not") {
    val found = compile(
      "Holder" ->
        """package demo;
          |
          |import org.jspecify.annotations.Nullable;
          |
          |public interface Holder {
          |  <T> @Nullable T typed();
          |
          |  @Nullable <T> T marked(T value);
          |}
          |""".stripMargin
    ).lints.filter(_.id == "nullable-return")
    assertEquals(
      found.map(lint => lint.line -> lint.method),
      Vector(6 -> "demo.Holder.typed", 8 -> "demo.Holder.marked"),
      found.mkString("\n")
    )
  }

  test("a comparison or a connective with a boolean literal is simplified to the other operand") {
    val model = compile("Literals" -> """
      |package demo;
      |
      |public class Literals {
      |  public boolean a(boolean v) { return v == true; }
      |  public boolean b(boolean v) { return true == v; }
      |  public boolean c(boolean v) { return v == false; }
      |  public boolean d(boolean v) { return false == v; }
      |  public boolean e(boolean v) { return v != true; }
      |  public boolean f(boolean v) { return true != v; }
      |  public boolean g(boolean v) { return false != v; }
      |  public boolean h(boolean v) { return v && true; }
      |  public boolean i(boolean v) { return false || v; }
      |  public boolean j(boolean v, boolean w) { return v && w; }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "pointless-boolean").map(lint => lint.method.split('.').last -> lint.detail)
    assertEquals(
      found.toSet,
      Set(
        "a" -> "v == true can be simplified to v",
        "b" -> "true == v can be simplified to v",
        "c" -> "v == false can be simplified to !v",
        "d" -> "false == v can be simplified to !v",
        "e" -> "v != true can be simplified to !v",
        "f" -> "true != v can be simplified to !v",
        "g" -> "false != v can be simplified to v",
        "h" -> "v && true can be simplified to v",
        "i" -> "false || v can be simplified to v"
      ),
      found.mkString("\n")
    )
  }

  test("a //noinspection reaches the types of a file that the compiler attributes one after another") {
    val model = compile("Pair" -> """
      |package demo;
      |
      |public class Pair {
      |  public boolean first(boolean x) {
      |    return x && true;
      |  }
      |}
      |
      |class Second {
      |  //noinspection PointlessBooleanExpression
      |  public boolean quiet(boolean x) {
      |    return x && true;
      |  }
      |
      |  public boolean noisy(boolean x) {
      |    return x && true;
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "pointless-boolean").map(_.method)
    assertEquals(found.toSet, Set("demo.Pair.first", "demo.Second.noisy"), found.mkString("\n"))
  }

  test("a doc comment that another doc comment follows documents nothing") {
    val found = flow(
      "dangling-doc-comment",
      "Stacked" ->
        """package demo;
          |
          |public class Stacked {
          |  /** Orphaned by the next one. */
          |  /** Documents the field. */
          |  int field;
          |
          |  /** First of three. */
          |  /** Second of three. */
          |  /**
          |   * Documents the method.
          |   */
          |  void method() {}
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(4, 8, 9), found.mkString("\n"))
  }

  test("a doc comment that a closing brace or the end of the file follows documents nothing") {
    val found = flow(
      "dangling-doc-comment",
      "Closed" ->
        """package demo;
          |
          |public class Closed {
          |  int field;
          |
          |  /** Orphaned by the brace. */
          |}
          |
          |class Other {
          |  // a note
          |  /** Orphaned across a note. */
          |  /* another note */
          |}
          |
          |/** Orphaned by the end of the file. */
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(6, 11, 15), found.mkString("\n"))
  }

  test("a doc comment that a declaration follows documents it") {
    val found = flow(
      "dangling-doc-comment",
      "Documented" ->
        """/** Documents the package. */
          |package demo;
          |
          |/** Documents the class. */
          |public class Documented {
          |  /** Documents the field. */
          |  @Deprecated
          |  int field;
          |
          |  /** Documents the method across a note. */
          |  // a note
          |  /* another note */
          |  void method() {}
          |
          |  /** Documents the constructor. */ // a trailing note
          |  Documented() {}
          |
          |  /**/
          |  int empty;
          |
          |  // a note
          |  /* another note */
          |}
          |""".stripMargin
    )
    assertEquals(found, Vector.empty, found.mkString("\n"))
  }

  test("a doc comment marker inside a literal or another comment is no doc comment") {
    val found = flow(
      "dangling-doc-comment",
      "Literals" ->
        """package demo;
          |
          |public class Literals {
          |  String url = "http://x\"y"; /** Orphaned after a string. */
          |  /** Documents the field. */
          |  int a;
          |  char quote = '"'; /** Orphaned after a character. */
          |  /** Documents the field. */
          |  int b;
          |  String text = "/** in a string */";
          |  // /** in a line comment */
          |  /* /** in a block comment */
          |}
          |""".stripMargin
    )
    assertEquals(found.map(_.line), Vector(4, 7), found.mkString("\n"))
  }

  test("a loop that returns on the first match is reported for a stream match") {
    val model = compile("Matches" -> """
      |package demo;
      |
      |import java.util.List;
      |
      |public class Matches {
      |  private boolean analyses(Object project) {
      |    return project != null;
      |  }
      |
      |  public boolean lastOfTheReactor(List<Object> projects, Object project) {
      |    int index = projects.indexOf(project);
      |    for (int later = index + 1; later < projects.size(); later++) {
      |      if (analyses(projects.get(later))) {
      |        return false;
      |      }
      |    }
      |    return true;
      |  }
      |
      |  public boolean anyEmpty(List<String> values) {
      |    for (String value : values) {
      |      if (value.isEmpty()) return true;
      |    }
      |    return false;
      |  }
      |
      |  public boolean allShort(List<String> values, int limit) {
      |    for (String value : values) {
      |      if (value.length() > limit) {
      |        return false;
      |      }
      |    }
      |    return true;
      |  }
      |
      |  public boolean allFilled(String[] values) {
      |    for (String value : values)
      |      if (!value.isEmpty()) return false;
      |    return true;
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "loop-to-match")
    assertEquals(
      found.map(lint => lint.method -> lint.detail.takeWhile(_ != ' ')),
      Vector(
        "demo.Matches.lastOfTheReactor" -> "use",
        "demo.Matches.anyEmpty"         -> "use",
        "demo.Matches.allShort"         -> "use",
        "demo.Matches.allFilled"        -> "use"
      ),
      model.lints.mkString("\n")
    )
    assertEquals(
      found.map(_.detail.split(' ')(1)),
      Vector("noneMatch", "anyMatch", "noneMatch", "allMatch"),
      found.mkString("\n")
    )
  }

  test("a loop that captures a pattern binding is reported only while the binding is not reassigned") {
    val model = compile("Captures" -> """
      |package demo;
      |
      |import java.util.List;
      |
      |public class Captures {
      |  public boolean unassignedCapture(List<String> values, Object other) {
      |    if (!(other instanceof String text)) return false;
      |    for (String value : values) {
      |      if (value.equals(text)) return true;
      |    }
      |    return false;
      |  }
      |
      |  public boolean reassignedCapture(List<String> values, Object other) {
      |    if (!(other instanceof String text)) return false;
      |    text = text.trim();
      |    for (String value : values) {
      |      if (value.equals(text)) return true;
      |    }
      |    return false;
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "loop-to-match")
    assertEquals(found.map(_.method), Vector("demo.Captures.unassignedCapture"), model.lints.mkString("\n"))
  }

  test("a loop that does more than return on the first match is not reported for a stream match") {
    val model = compile("Matches" -> """
      |package demo;
      |
      |import java.io.IOException;
      |import java.util.List;
      |
      |public class Matches {
      |  private int count;
      |
      |  private boolean read(String value) throws IOException {
      |    return value.isEmpty();
      |  }
      |
      |  public boolean counting(List<String> values) {
      |    for (String value : values) {
      |      if (value.isEmpty()) {
      |        count++;
      |        return false;
      |      }
      |    }
      |    return true;
      |  }
      |
      |  public boolean beforehand(List<String> values) {
      |    for (String value : values) {
      |      count++;
      |      if (value.isEmpty()) return false;
      |    }
      |    return true;
      |  }
      |
      |  public boolean otherwise(List<String> values) {
      |    for (String value : values) {
      |      if (value.isEmpty()) return false;
      |      else count++;
      |    }
      |    return true;
      |  }
      |
      |  public boolean same(List<String> values) {
      |    for (String value : values) {
      |      if (value.isEmpty()) return false;
      |    }
      |    return false;
      |  }
      |
      |  public boolean computed(List<String> values) {
      |    for (String value : values) {
      |      if (value.isEmpty()) return false;
      |    }
      |    return values.isEmpty();
      |  }
      |
      |  public boolean separated(List<String> values) {
      |    for (String value : values) {
      |      if (value.isEmpty()) return true;
      |    }
      |    count++;
      |    return false;
      |  }
      |
      |  public boolean checked(List<String> values) throws IOException {
      |    for (String value : values) {
      |      if (read(value)) return true;
      |    }
      |    return false;
      |  }
      |
      |  public boolean assigning(List<String> values) {
      |    String last = "";
      |    for (String value : values) {
      |      if (value.equals(last = value)) return true;
      |    }
      |    return false;
      |  }
      |
      |  public boolean changing(List<String> values) {
      |    int limit = 3;
      |    limit++;
      |    for (String value : values) {
      |      if (value.length() > limit) return true;
      |    }
      |    return false;
      |  }
      |
      |  public boolean indexed(List<Integer> values) {
      |    for (int i = 0; i < values.size(); i++) {
      |      if (values.get(i) > i) return true;
      |    }
      |    return false;
      |  }
      |
      |  public boolean crossed(List<Integer> values, List<Integer> others) {
      |    for (int i = 0; i < others.size(); i++) {
      |      if (values.get(i) > 0) return true;
      |    }
      |    return false;
      |  }
      |
      |  public boolean iterable(Iterable<String> values) {
      |    for (String value : values) {
      |      if (value.isEmpty()) return true;
      |    }
      |    return false;
      |  }
      |
      |  public boolean primitives(int[] values, int target) {
      |    for (int value : values) {
      |      if (value == target) return true;
      |    }
      |    return false;
      |  }
      |
      |  public boolean bytes(byte[] values) {
      |    for (byte value : values) {
      |      if (value != 0) return false;
      |    }
      |    return true;
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id == "loop-to-match"), Vector.empty)
  }

  test("a null check that picks a default is reported for requireNonNullElse") {
    val model = compile("Defaults" -> """
      |package demo;
      |
      |import java.util.ArrayList;
      |import java.util.Collections;
      |import java.util.List;
      |import java.util.Map;
      |import java.util.Set;
      |
      |public class Defaults {
      |  static final class Settings {
      |    Set<String> hot;
      |    String name;
      |  }
      |
      |  private final Set<String> hot;
      |  private final Set<String> checked;
      |
      |  Defaults(Settings settings) {
      |    this.hot = settings.hot != null ? settings.hot : Set.of();
      |    this.checked = settings.hot == null ? Collections.emptySet() : settings.hot;
      |  }
      |
      |  String name(Settings settings) {
      |    return settings.name != null ? settings.name : "unnamed";
      |  }
      |
      |  List<String> fresh(List<String> given) {
      |    return given != null ? given : new ArrayList<>();
      |  }
      |
      |  String lookup(Map<String, String> map, String key) {
      |    return map.get(key) != null ? map.get(key) : "none";
      |  }
      |
      |  String guard(String value) {
      |    if (value != null) {
      |      return value;
      |    }
      |    return "";
      |  }
      |
      |  String invert(String value) {
      |    if (value == null) return "none";
      |    return value;
      |  }
      |
      |  String otherwise(String value) {
      |    if (value != null) return value;
      |    else return "";
      |  }
      |
      |  private static final String NONE = "none";
      |
      |  String constant(String value) {
      |    return value == null ? NONE : value;
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "null-default")
    assertEquals(
      found.map(lint => lint.method -> lint.detail),
      Vector(
        "demo.Defaults.Defaults" -> "use Objects.requireNonNullElse(settings.hot, Set.of()) instead of the null check",
        "demo.Defaults.Defaults" ->
          "use Objects.requireNonNullElse(settings.hot, Collections.emptySet()) instead of the null check",
        "demo.Defaults.name" ->
          "use Objects.requireNonNullElse(settings.name, \"unnamed\") instead of the null check",
        "demo.Defaults.fresh" ->
          "use Objects.requireNonNullElseGet(given, () -> new ArrayList<>()) instead of the null check",
        "demo.Defaults.lookup" ->
          "use Objects.requireNonNullElse(map.get(key), \"none\") instead of the null check",
        "demo.Defaults.guard"     -> "use Objects.requireNonNullElse(value, \"\") instead of the null check",
        "demo.Defaults.invert"    -> "use Objects.requireNonNullElse(value, \"none\") instead of the null check",
        "demo.Defaults.otherwise" -> "use Objects.requireNonNullElse(value, \"\") instead of the null check",
        "demo.Defaults.constant"  -> "use Objects.requireNonNullElse(value, NONE) instead of the null check"
      ),
      model.lints.mkString("\n")
    )
  }

  test("a null check that does not pick a default of the value is not reported") {
    val model = compile("Defaults" -> """
      |package demo;
      |
      |import java.util.Iterator;
      |
      |public class Defaults {
      |  private String compute() {
      |    return "computed";
      |  }
      |
      |  private void log(String value) {
      |  }
      |
      |  String effect(Iterator<String> it) {
      |    return it.next() != null ? it.next() : "x";
      |  }
      |
      |  String unknown(String value, String other) {
      |    return value != null ? value : other;
      |  }
      |
      |  String nothing(String value) {
      |    return value != null ? value : null;
      |  }
      |
      |  String different(String a, String b) {
      |    return a != null ? b : "x";
      |  }
      |
      |  String transformed(String a) {
      |    return a != null ? a.trim() : "";
      |  }
      |
      |  String called(String a) {
      |    return a != null ? a : compute();
      |  }
      |
      |  String logged(String a) {
      |    if (a != null) {
      |      log(a);
      |      return a;
      |    }
      |    return "";
      |  }
      |
      |  String compound(String a, boolean flag) {
      |    return a != null && flag ? a : "";
      |  }
      |
      |  long widened(Integer count) {
      |    return count != null ? count : 0L;
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id == "null-default"), Vector.empty)
  }

  test("a null check is not reported for requireNonNullElse where the release has none") {
    val model = compileEnabled(
      Vector("release=8"),
      "Defaults" -> """
      |package demo;
      |
      |public class Defaults {
      |  String name(String value) {
      |    return value != null ? value : "unnamed";
      |  }
      |}
      |""".stripMargin
    )

    assertEquals(model.lints.filter(_.id == "null-default"), Vector.empty)
  }

  test("a stream reduced by concatenation is reported for String.join") {
    val model = compile("Reductions" -> """
      |package demo;
      |
      |import java.io.File;
      |import java.util.List;
      |
      |public class Reductions {
      |  String path(List<File> files) {
      |    return files.stream().map(File::getAbsolutePath).reduce((a, b) -> a + File.pathSeparator + b).orElse("");
      |  }
      |
      |  String strict(List<File> files) {
      |    return files.stream().map(File::getAbsolutePath).reduce((a, b) -> a + java.io.File.pathSeparator + b).orElseThrow();
      |  }
      |
      |  String literal(List<String> values) {
      |    return values.stream().reduce((first, second) -> first + ", " + second).orElse("");
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "stream-join")
    assertEquals(
      found.map(lint => lint.method -> lint.detail),
      Vector(
        "demo.Reductions.path"   -> "use String.join instead of reducing a stream by concatenation",
        "demo.Reductions.strict" ->
          "use String.join instead of reducing a stream by concatenation, which returns an empty text where orElseThrow fails",
        "demo.Reductions.literal" -> "use String.join instead of reducing a stream by concatenation"
      ),
      model.lints.mkString("\n")
    )
  }

  test("a stream reduced by something other than a separated concatenation is not reported for String.join") {
    val model = compile("Reductions" -> """
      |package demo;
      |
      |import java.util.List;
      |
      |public class Reductions {
      |  int total(List<Integer> values) {
      |    return values.stream().reduce((a, b) -> a + b).orElse(0);
      |  }
      |
      |  int shifted(List<Integer> values) {
      |    return values.stream().reduce((a, b) -> a + 1 + b).orElse(0);
      |  }
      |
      |  String longest(List<String> values) {
      |    return values.stream().reduce((a, b) -> a.length() >= b.length() ? a : b).orElse("");
      |  }
      |
      |  String doubled(List<String> values) {
      |    return values.stream().reduce((a, b) -> a + a + b).orElse("");
      |  }
      |
      |  String flat(List<String> values) {
      |    return values.stream().reduce((a, b) -> a + b).orElse("");
      |  }
      |
      |  String defaulted(List<String> values) {
      |    return values.stream().reduce((a, b) -> a + "," + b).orElse("none");
      |  }
      |
      |  String seeded(List<String> values) {
      |    return values.stream().reduce("", (a, b) -> a + "," + b);
      |  }
      |
      |  String reversed(List<String> values) {
      |    return values.stream().reduce((a, b) -> b + "," + a).orElse("");
      |  }
      |
      |  String hidden(List<String> values) {
      |    return values.stream().reduce((a, b) -> a + "," + b).map(String::trim).orElse("");
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id == "stream-join"), Vector.empty)
  }

  test("adjacent loops with the same body over different sources are reported") {
    val model = compile("Loops" -> """
      |package demo;
      |
      |import java.util.ArrayList;
      |import java.util.List;
      |
      |public class Loops {
      |  private final List<String> files = new ArrayList<>();
      |
      |  private static List<String> sources(List<String> roots) {
      |    return roots;
      |  }
      |
      |  void add(List<String> compile, List<String> test) {
      |    for (String source : sources(compile)) files.add(source.trim());
      |    for (String source : sources(test)) files.add(source.trim());
      |  }
      |
      |  void renamed(List<String> first, List<String> second, List<String> out) {
      |    for (String x : first) {
      |      out.add(x);
      |    }
      |    for (String y : second) {
      |      out.add(y);
      |    }
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "consecutive-identical-loops")
    assertEquals(found.map(_.method), Vector("demo.Loops.add", "demo.Loops.renamed"), model.lints.mkString("\n"))
    assertEquals(found.map(_.line), Vector(16, 23))
  }

  test("adjacent loops that cannot be merged are not reported") {
    val model = compile("Loops" -> """
      |package demo;
      |
      |import java.util.HashMap;
      |import java.util.List;
      |import java.util.Map;
      |
      |public class Loops {
      |  private int count;
      |
      |  void bodies(List<String> a, List<String> b, List<String> out) {
      |    for (String x : a) out.add(x);
      |    for (String x : b) out.add(x.trim());
      |  }
      |
      |  void sources(List<String> a, List<String> out) {
      |    for (String x : a) out.add(x);
      |    for (String x : a) out.add(x);
      |  }
      |
      |  void separated(List<String> a, List<String> b, List<String> out) {
      |    for (String x : a) out.add(x);
      |    count++;
      |    for (String x : b) out.add(x);
      |  }
      |
      |  void breaking(List<String> a, List<String> b, List<String> out) {
      |    for (String x : a) {
      |      if (x.isEmpty()) break;
      |      out.add(x);
      |    }
      |    for (String x : b) {
      |      if (x.isEmpty()) break;
      |      out.add(x);
      |    }
      |  }
      |
      |  void feeding(List<String> a, Map<String, Integer> counts) {
      |    for (String x : a) counts.merge(x, 1, Integer::sum);
      |    for (String x : counts.keySet()) counts.merge(x, 1, Integer::sum);
      |  }
      |
      |  void types(List<String> a, List<Object> b, List<Object> out) {
      |    for (String x : a) out.add(x);
      |    for (Object x : b) out.add(x);
      |  }
      |
      |  void nested(List<String> a, List<String> b, List<String> out) {
      |    for (String x : a) {
      |      out.add(x);
      |      for (String y : b) out.add(y);
      |    }
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id == "consecutive-identical-loops"), Vector.empty)
  }

  test("a list that one loop fills and another iterates once is reported") {
    val model = compile("Temporaries" -> """
      |package demo;
      |
      |import java.util.ArrayList;
      |import java.util.List;
      |import java.util.Set;
      |import java.util.TreeSet;
      |
      |public class Temporaries {
      |  String render(Set<String> ids, Set<String> disabled) {
      |    List<String> rules = new ArrayList<>();
      |    for (String rule : new TreeSet<>(ids)) {
      |      if (!disabled.contains(rule)) {
      |        rules.add(rule);
      |      }
      |    }
      |    StringBuilder scalafix = new StringBuilder("rules = [\n");
      |    for (String rule : rules) {
      |      scalafix.append("  \"").append(rule).append("\"\n");
      |    }
      |    return scalafix.toString();
      |  }
      |
      |  void print(List<String> names) {
      |    List<String> kept = new ArrayList<>();
      |    for (String name : names) kept.add(name.trim());
      |    for (String name : kept) System.out.println(name);
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "temporary-list-iterated-once")
    assertEquals(
      found.map(_.method),
      Vector("demo.Temporaries.render", "demo.Temporaries.print"),
      model.lints.mkString("\n")
    )
    assertEquals(found.map(_.line), Vector(11, 25))
  }

  test("a list that one loop fills from a pattern binding and another iterates once is reported") {
    val model = compile("Prefixes" -> """
      |package demo;
      |
      |import java.util.ArrayList;
      |import java.util.List;
      |
      |public class Prefixes {
      |  void print(List<String> names, Object prefix) {
      |    if (!(prefix instanceof String text)) return;
      |    List<String> kept = new ArrayList<>();
      |    for (String name : names) kept.add(text + name);
      |    for (String name : kept) System.out.println(name);
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "temporary-list-iterated-once")
    assertEquals(found.map(_.method), Vector("demo.Prefixes.print"), model.lints.mkString("\n"))
  }

  test("a list that is used beyond a fill and one iteration is not reported") {
    val model = compile("Temporaries" -> """
      |package demo;
      |
      |import java.util.ArrayList;
      |import java.util.Collections;
      |import java.util.List;
      |import java.util.Map;
      |
      |public class Temporaries {
      |  private int count;
      |
      |  int sized(List<String> names) {
      |    List<String> kept = new ArrayList<>();
      |    for (String name : names) kept.add(name.trim());
      |    for (String name : kept) count++;
      |    return kept.size();
      |  }
      |
      |  void twice(List<String> names) {
      |    List<String> kept = new ArrayList<>();
      |    for (String name : names) kept.add(name.trim());
      |    for (String name : kept) count++;
      |    for (String name : kept) count--;
      |  }
      |
      |  void sorted(List<String> names) {
      |    List<String> kept = new ArrayList<>();
      |    for (String name : names) kept.add(name.trim());
      |    Collections.sort(kept);
      |    for (String name : kept) count++;
      |  }
      |
      |  void snapshot(Map<String, String> map) {
      |    List<String> dead = new ArrayList<>();
      |    for (String key : map.keySet()) {
      |      if (map.get(key).isEmpty()) dead.add(key);
      |    }
      |    for (String key : dead) map.remove(key);
      |  }
      |
      |  void seeded(List<String> names) {
      |    List<String> kept = new ArrayList<>(names);
      |    for (String name : names) kept.add(name.trim());
      |    for (String name : kept) count++;
      |  }
      |
      |  void unfilled(List<String> names) {
      |    List<String> kept = new ArrayList<>();
      |    kept.add("a");
      |    for (String name : kept) count++;
      |  }
      |
      |  void counted(List<String> names) {
      |    List<String> kept = new ArrayList<>();
      |    int seen = 0;
      |    for (String name : names) {
      |      seen++;
      |      kept.add(name.trim());
      |    }
      |    for (String name : kept) count += seen;
      |  }
      |
      |  List<String> passed(List<String> names) {
      |    List<String> kept = new ArrayList<>();
      |    for (String name : names) kept.add(name.trim());
      |    for (String name : kept) count++;
      |    return kept;
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id == "temporary-list-iterated-once"), Vector.empty)
  }

  test("a condition inside a loop that the loop never changes is reported") {
    val model = compile(
      "Configuration" -> """
        |package demo;
        |
        |import java.util.Set;
        |
        |public final class Configuration {
        |  private final Set<String> disabled;
        |
        |  public Configuration(Set<String> disabled) {
        |    this.disabled = disabled;
        |  }
        |
        |  public Set<String> disabled() {
        |    return disabled;
        |  }
        |}
        |""".stripMargin,
      "Scalafix" -> """
        |package demo;
        |
        |import java.util.List;
        |
        |public class Scalafix {
        |  static String render(Configuration configuration, List<String> bans) {
        |    StringBuilder scalafix = new StringBuilder();
        |    for (String ban : bans) {
        |      if (!configuration.disabled().contains("DisableSyntax")) {
        |        scalafix.append(ban).append(" = ").append(!configuration.disabled().contains(ban)).append("\n");
        |      }
        |    }
        |    return scalafix.toString();
        |  }
        |
        |  static int flagged(List<String> values, boolean verbose) {
        |    int total = 0;
        |    for (String value : values) {
        |      if (verbose) {
        |        total += value.length();
        |      }
        |    }
        |    return total;
        |  }
        |
        |  static int counted(List<String> allowed, String user) {
        |    int total = 0;
        |    for (int i = 0; i < 10; i++) {
        |      if (allowed.contains(user)) {
        |        total += 2;
        |      }
        |    }
        |    return total;
        |  }
        |}
        |""".stripMargin
    )

    val found = model.lints.filter(_.id == "loop-invariant-condition")
    assertEquals(
      found.map(_.method),
      Vector("demo.Scalafix.render", "demo.Scalafix.flagged", "demo.Scalafix.counted"),
      model.lints.mkString("\n")
    )
    assertEquals(found.map(_.line), Vector(10, 20, 30))
  }

  test("a condition inside a loop that the loop can change is not reported") {
    val model = compile("Invariants" -> """
      |package demo;
      |
      |import java.util.ArrayList;
      |import java.util.Iterator;
      |import java.util.List;
      |import java.util.Set;
      |
      |public class Invariants {
      |  private static final boolean DEBUG = false;
      |  private boolean strict;
      |  private int total;
      |
      |  void element(List<String> values) {
      |    for (String value : values) {
      |      if (value.isEmpty()) total++;
      |    }
      |  }
      |
      |  void counted(List<String> values) {
      |    for (int i = 0; i < values.size(); i++) {
      |      if (i > 2) total++;
      |    }
      |  }
      |
      |  void assigned(List<String> values) {
      |    boolean first = true;
      |    for (String value : values) {
      |      if (first) {
      |        total++;
      |        first = false;
      |      }
      |    }
      |  }
      |
      |  void grown(List<String> values) {
      |    Set<String> seen = new java.util.HashSet<>();
      |    for (String value : values) {
      |      if (seen.contains("a")) total++;
      |      seen.add(value);
      |    }
      |  }
      |
      |  void escaped(List<String> values) {
      |    List<String> seen = new ArrayList<>();
      |    for (String value : values) {
      |      if (seen.isEmpty()) total++;
      |      remember(seen);
      |    }
      |  }
      |
      |  private void remember(List<String> seen) {
      |    seen.add("a");
      |  }
      |
      |  void called(List<String> values, Iterator<String> it) {
      |    for (String value : values) {
      |      if (it.hasNext()) total++;
      |    }
      |  }
      |
      |  void field(List<String> values) {
      |    for (String value : values) {
      |      if (strict) total++;
      |    }
      |  }
      |
      |  void constant(List<String> values) {
      |    for (String value : values) {
      |      if (DEBUG) total++;
      |    }
      |  }
      |
      |  void declared(List<String> values) {
      |    for (String value : values) {
      |      boolean empty = value.isEmpty();
      |      if (empty) total++;
      |    }
      |  }
      |
      |  void lambda(List<String> values, boolean verbose) {
      |    values.forEach(value -> {
      |      if (verbose) total++;
      |    });
      |  }
      |
      |  void outside(List<String> values, boolean verbose) {
      |    if (verbose) total++;
      |    for (String value : values) {
      |      total += value.length();
      |    }
      |  }
      |
      |  void mixed(List<String> values) {
      |    for (String value : values) {
      |      if (values.isEmpty() && value == null) total++;
      |    }
      |  }
      |
      |  void among(List<String> values, boolean verbose) {
      |    for (String value : values) {
      |      total += value.length();
      |      if (verbose) total++;
      |    }
      |  }
      |
      |  void branching(List<String> values, boolean verbose) {
      |    for (String value : values) {
      |      if (verbose) total++;
      |      else total--;
      |    }
      |  }
      |}
      |""".stripMargin)

    val found = model.lints.filter(_.id == "loop-invariant-condition")
    assertEquals(found.map(lint => lint.method -> lint.line), Vector.empty, model.lints.mkString("\n"))
  }

  test("a condition read through the accessor of a record is invariant and through a getter that can change is not") {
    val model = compile(
      "Open" -> """
        |package demo;
        |
        |import java.util.Set;
        |
        |public class Open {
        |  private final Set<String> disabled = Set.of();
        |
        |  public Set<String> disabled() {
        |    return disabled;
        |  }
        |}
        |""".stripMargin,
      "Loose" -> """
        |package demo;
        |
        |import java.util.Set;
        |
        |public final class Loose {
        |  private Set<String> disabled = Set.of();
        |
        |  public Set<String> disabled() {
        |    return disabled;
        |  }
        |}
        |""".stripMargin,
      "Named" -> """
        |package demo;
        |
        |import java.util.Set;
        |
        |public record Named(Set<String> disabled) {
        |}
        |""".stripMargin,
      "Users" -> """
        |package demo;
        |
        |import java.util.List;
        |
        |public class Users {
        |  static int open(Open open, List<String> values) {
        |    int total = 0;
        |    for (String value : values) {
        |      if (open.disabled().contains("a")) total++;
        |    }
        |    return total;
        |  }
        |
        |  static int loose(Loose loose, List<String> values) {
        |    int total = 0;
        |    for (String value : values) {
        |      if (loose.disabled().contains("a")) total++;
        |    }
        |    return total;
        |  }
        |
        |  static int named(Named named, List<String> values) {
        |    int total = 0;
        |    for (String value : values) {
        |      if (named.disabled().contains("a")) total++;
        |    }
        |    return total;
        |  }
        |}
        |""".stripMargin
    )

    val found = model.lints.filter(_.id == "loop-invariant-condition")
    assertEquals(found.map(_.method), Vector("demo.Users.named"), model.lints.mkString("\n"))
  }

  test("a match of an equality test with a constant is reported as a contains") {
    val model = compile("Matches" -> """
      |package demo;
      |
      |import java.util.HashSet;
      |import java.util.List;
      |import java.util.Objects;
      |import java.util.Set;
      |
      |public class Matches {
      |  public boolean lambda(List<String> values, String c) {
      |    return values.stream().anyMatch(x -> x.equals(c));
      |  }
      |
      |  public boolean reversed(List<String> values, String c) {
      |    return values.stream().anyMatch(x -> c.equals(x));
      |  }
      |
      |  public boolean reference(Set<String> values, String c) {
      |    return values.stream().anyMatch(c::equals);
      |  }
      |
      |  public boolean objects(HashSet<String> values, String c) {
      |    return values.stream().anyMatch(x -> Objects.equals(x, c));
      |  }
      |
      |  public boolean none(List<String> values, String c) {
      |    return values.stream().noneMatch(x -> x.equals(c));
      |  }
      |
      |  public boolean all(List<String> values, String c) {
      |    return values.stream().allMatch(x -> !x.equals(c));
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      model.lints.filter(lint => lint.id == "exists-to-contains" || lint.id == "forall-to-not-contains")
        .map(lint => (lint.id, lint.method, lint.detail)),
      Vector(
        (
          "exists-to-contains",
          "demo.Matches.lambda",
          "use contains instead of stream and anyMatch of an equality test: values.contains(c)"
        ),
        (
          "exists-to-contains",
          "demo.Matches.reversed",
          "use contains instead of stream and anyMatch of an equality test: values.contains(c)"
        ),
        (
          "exists-to-contains",
          "demo.Matches.reference",
          "use contains instead of stream and anyMatch of an equality test: values.contains(c)"
        ),
        (
          "exists-to-contains",
          "demo.Matches.objects",
          "use contains instead of stream and anyMatch of an equality test: values.contains(c)"
        ),
        (
          "forall-to-not-contains",
          "demo.Matches.none",
          "use !contains instead of stream and noneMatch of an equality test: !values.contains(c)"
        ),
        (
          "forall-to-not-contains",
          "demo.Matches.all",
          "use !contains instead of stream and allMatch of an inequality test: !values.contains(c)"
        )
      ),
      model.lints.mkString("\n")
    )
  }

  test("a match that is not an equality test of a java.util collection with a constant is not reported") {
    val model = compile("Matches" -> """
      |package demo;
      |
      |import java.util.Arrays;
      |import java.util.Comparator;
      |import java.util.List;
      |import java.util.Objects;
      |import java.util.TreeSet;
      |import java.util.stream.Stream;
      |
      |public class Matches {
      |  static class Mine {
      |    Stream<String> stream() { return Stream.empty(); }
      |  }
      |
      |  public boolean array(String[] values, String c) {
      |    return Arrays.stream(values).anyMatch(x -> x.equals(c));
      |  }
      |
      |  public boolean varargs(String[] values, String c) {
      |    return Stream.of(values).anyMatch(x -> x.equals(c));
      |  }
      |
      |  public boolean sorted(TreeSet<String> values, String c) {
      |    return values.stream().anyMatch(x -> x.equals(c));
      |  }
      |
      |  public boolean mine(Mine values, String c) {
      |    return values.stream().anyMatch(x -> x.equals(c));
      |  }
      |
      |  public boolean mapped(List<String> values, String c) {
      |    return values.stream().map(String::trim).anyMatch(x -> x.equals(c));
      |  }
      |
      |  public boolean effect(List<String> values) {
      |    return values.stream().anyMatch(x -> x.equals(next()));
      |  }
      |
      |  public boolean self(List<String> values) {
      |    return values.stream().anyMatch(x -> x.equals(x.trim()));
      |  }
      |
      |  public boolean arrays(List<String[]> values, String[] c) {
      |    return values.stream().anyMatch(x -> x.equals(c));
      |  }
      |
      |  public boolean other(List<String> values, String c) {
      |    return values.stream().anyMatch(x -> x.equalsIgnoreCase(c));
      |  }
      |
      |  public boolean mixed(List<String> values, String c) {
      |    return values.stream().allMatch(x -> x.equals(c));
      |  }
      |
      |  private String next() {
      |    return "";
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      model.lints.filter(lint => lint.id == "exists-to-contains" || lint.id == "forall-to-not-contains"),
      Vector.empty,
      model.lints.mkString("\n")
    )
  }

  test("a filtered count of an array stream compared with zero is reported") {
    val model = compile("Counts" -> """
      |package demo;
      |
      |import java.util.Arrays;
      |
      |public class Counts {
      |  public boolean none(String[] values) {
      |    return Arrays.stream(values).filter(v -> v == null).count() == 0;
      |  }
      |
      |  public boolean any(int[] values) {
      |    return Arrays.stream(values).filter(v -> v > 3).count() > 0;
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      model.lints.filter(_.id.startsWith("size-to-")).map(lint => (lint.id, lint.method, lint.detail)),
      Vector(
        (
          "size-to-is-empty",
          "demo.Counts.none",
          "use noneMatch instead of filter and a count compared with zero: " +
            "Arrays.stream(values).noneMatch((v)->v == null)"
        ),
        (
          "size-to-non-empty",
          "demo.Counts.any",
          "use anyMatch instead of filter and a count compared with zero: " +
            "Arrays.stream(values).anyMatch((v)->v > 3)"
        )
      ),
      model.lints.mkString("\n")
    )
  }

  test("a size compared with zero on this in a subtype of a collection is reported") {
    val model = compile("Sizes" -> """
      |package demo;
      |
      |import java.util.ArrayList;
      |
      |public class Sizes extends ArrayList<String> {
      |  public boolean empty() {
      |    return size() == 0;
      |  }
      |
      |  public boolean present() {
      |    return size() > 0;
      |  }
      |
      |  public boolean viaThis() {
      |    return this.size() == 0;
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      model.lints.filter(_.id.startsWith("size-to-")).map(lint => (lint.id, lint.method, lint.detail)),
      Vector(
        ("size-to-is-empty", "demo.Sizes.empty", "use isEmpty() instead of a size comparison with zero: isEmpty()"),
        (
          "size-to-non-empty",
          "demo.Sizes.present",
          "use !isEmpty() instead of a size comparison with zero: !isEmpty()"
        ),
        (
          "size-to-is-empty",
          "demo.Sizes.viaThis",
          "use isEmpty() instead of a size comparison with zero: this.isEmpty()"
        )
      ),
      model.lints.mkString("\n")
    )
  }

  test("a size compared with zero on this where the type overrides the size, or in an isEmpty, is not reported") {
    val model = compile("Sizes" -> """
      |package demo;
      |
      |import java.util.ArrayList;
      |
      |public class Sizes {
      |  static class Counted extends ArrayList<String> {
      |    @Override public int size() {
      |      return 0;
      |    }
      |
      |    boolean empty() {
      |      return size() == 0;
      |    }
      |  }
      |
      |  static class Own extends ArrayList<String> {
      |    @Override public boolean isEmpty() {
      |      return size() == 0;
      |    }
      |  }
      |
      |  static class Unrelated {
      |    int size() {
      |      return 0;
      |    }
      |
      |    boolean empty() {
      |      return size() == 0;
      |    }
      |  }
      |}
      |""".stripMargin)

    assertEquals(model.lints.filter(_.id.startsWith("size-to-")), Vector.empty, model.lints.mkString("\n"))
  }
}
