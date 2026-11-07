package net.kurobako.bonsai.analysis

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import net.kurobako.bonsai.java.Model as JavaModel
import net.kurobako.bonsai.testing.*

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
      "demo.Service.helper()",
      "false",
      "",
      "",
      s"$source:8",
      returns,
      (inheritance == Inheritance.Overriding).toString
    ),
    record("site", "demo.Service.one", "demo.Service.helper()", "", s"$source:12", "true"),
    record("site", "demo.Service.two", "demo.Service.helper()", "", s"$source:16", "true")
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
      generated = Vector("**/src/generated/**")
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
    val parameters = Vector("w", "x", "y", "z").map(name => s"$name|other|java.lang.String||false").mkString(";")
    def method(owner: String, name: String, source: String, line: Int) =
      record(
        "method",
        owner,
        s"$owner.$name",
        s"$owner.$name()",
        "false",
        "",
        parameters,
        s"$source:$line",
        "String",
        "false"
      )
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
        classpaths,
        Set.empty,
        models = Vector(Output("j", Output.Scope.Main, file)),
        jdk = targetJdk
      )

      val result = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)

      val findings = Report.findings(result)
      assertFinding(findings, "ignored-result", "Tools.compute(): its Int result is discarded at all 2 calls")
      assertFinding(findings, "ignored-result", "Service.helper: its String result is discarded at all 2 calls")
    }
  }

  test(
    "a call that passes fewer arguments than the method takes is read without failing"
  ) {
    val result = readJavaModel(Vector(
      record("node", "demo.Service", "demo/Service.java", "4", "true"),
      record(
        "method",
        "demo.Service",
        "demo.Service.helper",
        "demo.Service.helper()",
        "false",
        "",
        "name|other|||false;exact|flag|||false",
        "demo/Service.java:8"
      ),
      record("site", "demo.Service.one", "demo.Service.helper()", "present:name", "demo/Service.java:12", "true"),
      record("site", "demo.Service.two", "demo.Service.helper()", "present:name", "demo/Service.java:16", "true")
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
        "demo.Service.helper()",
        "false",
        "",
        "exact|flag|||false;limit|number|||false",
        "demo/Service.java:8"
      ),
      record(
        "site",
        "demo.Service.one",
        "demo.Service.helper()",
        "present:flag,number:3",
        "demo/Service.java:12",
        "true"
      ),
      record(
        "site",
        "demo.Service.two",
        "demo.Service.helper()",
        "present:flag,number:3",
        "demo/Service.java:16",
        "true"
      )
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
        "demo.Deadline.isExpired()",
        "false",
        "",
        "",
        "demo/Deadline.java:8",
        "boolean",
        (inheritance == Inheritance.Overriding).toString
      ),
      record(
        "site",
        "demo.Deadline.first",
        "demo.Deadline.isExpired()",
        "",
        "demo/Deadline.java:12",
        "false",
        inverted
      ),
      record(
        "site",
        "demo.Deadline.second",
        "demo.Deadline.isExpired()",
        "",
        "demo/Deadline.java:16",
        "false",
        inverted
      ),
      record(
        "site",
        "demo.Deadline.third",
        "demo.Deadline.isExpired()",
        "",
        "demo/Deadline.java:20",
        "false",
        inverted
      ),
      record(
        "site",
        "demo.Deadline.fourth",
        "demo.Deadline.isExpired()",
        "",
        "demo/Deadline.java:24",
        "false",
        inverted
      )
    )
  }

  test("a Java boolean method every caller inverts is reported as inverted") {
    val finding = reported(readJavaModel(deadline()), "inverted-boolean")

    assertEquals(finding.node.name, "demo.Deadline")
    assertEquals(finding.node.source, "demo/Deadline.java")
    assertEquals(finding.detail, "Deadline.isExpired: its boolean result is inverted at all 4 calls")
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

    assertEquals(finding.detail, "Deadline.isExpired: its boolean result is inverted at all 4 calls (all in tests)")
    assertEquals(finding.level, Finding.Level.Info)
  }

  /** The result of compiling Java main sources, then test sources against the main classes, each with the javac plugin.
    */
  private def javaBuild(
      main: Seq[(String, String)],
      test: Seq[(String, String)] = Nil,
      libraries: Set[String] = Set.empty
  ): Extraction.Result =
    Scratch.using("bonsai-") { directory =>
      def compiled(scope: String, sources: Seq[(String, String)], path: Seq[Path]) = {
        write(directory.resolve(scope), sources)
        val classes = Files.createDirectories(directory.resolve(s"$scope-classes"))
        val model   = directory.resolve(s"$scope-model.tsv")
        JavaModel.write(javac(sources.map((name, _) => directory.resolve(scope).resolve(name)), classes, path), model)
        classes -> model
      }
      val (mainClasses, mainModel) = compiled("main", main, classpath)
      val tested                   = Option.when(test.nonEmpty)(compiled("test", test, mainClasses +: classpath))
      val manifest                 = Manifest(
        Vector.empty,
        classpaths,
        libraries,
        models = Output("m", Output.Scope.Main, mainModel) +:
          tested.map((_, model) => Output("m", Output.Scope.Test, model)).toVector,
        jdk = targetJdk
      )
      Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
    }

  private val Event = "demo/Event.java" -> """
    |package demo;
    |
    |public class Event {
    |  final Object source;
    |  final boolean visible;
    |  Event(Object source, boolean visible) { this.source = source; this.visible = visible; }
    |}
    |""".stripMargin

  test("a flag a Java method only passes to a constructor is data of the value it builds, not a flag argument") {
    val result = javaBuild(Seq(
      Event,
      "demo/Panel.java" -> """
        |package demo;
        |
        |public class Panel {
        |  Event last;
        |  void fire(boolean visible) { last = new Event(this, visible); }
        |  void show() { fire(true); }
        |  void hide() { fire(false); }
        |}
        |""".stripMargin
    ))

    assertNoFinding(Report.findings(result))(finding =>
      finding.rule.id == "flag-argument" && finding.detail.contains("Panel.fire")
    )
  }

  test("a Java flag that selects a branch at literal call sites is a flag argument") {
    val result = javaBuild(Seq(
      Event,
      "demo/Panel.java" -> """
        |package demo;
        |
        |public class Panel {
        |  Event last;
        |  void fire(boolean visible) { if (visible) last = new Event(this, visible); else last = null; }
        |  void show() { fire(true); }
        |  void hide() { fire(false); }
        |}
        |""".stripMargin
    ))

    assertFinding(Report.findings(result), "flag-argument", "Panel.fire.visible is a literal at all 2 calls")
  }

  test("a Java flag that a method passes to a constructor and also branches on is a flag argument") {
    val result = javaBuild(Seq(
      Event,
      "demo/Panel.java" -> """
        |package demo;
        |
        |public class Panel {
        |  Event last;
        |  void fire(boolean visible) { last = new Event(this, visible); if (visible) last = null; }
        |  void show() { fire(true); }
        |  void hide() { fire(false); }
        |}
        |""".stripMargin
    ))

    assertFinding(Report.findings(result), "flag-argument", "Panel.fire.visible is a literal at all 2 calls")
  }

  private val Session = "demo/Session.java" -> """
    |package demo;
    |
    |public class Session {
    |  private final String name;
    |  private final boolean owned;
    |  public Session(String name) { this(name, true); }
    |  public Session(String name, boolean owned) {
    |    if (name == null) throw new IllegalArgumentException("name");
    |    this.name = name;
    |    this.owned = owned;
    |  }
    |  public Session(String name, int unused) { this(name, false); }
    |}
    |""".stripMargin

  private val Starter = "demo/Starter.java" -> """
    |package demo;
    |
    |public class Starter {
    |  Session first() { return new Session("a", 0); }
    |  Session second() { return new Session("b", 1); }
    |}
    |""".stripMargin

  test("a Java field that the constructions of the tests pass another value is not constant") {
    val result = javaBuild(
      Seq(Session, Starter),
      Seq("demo/SessionTest.java" -> """
        |package demo;
        |
        |public class SessionTest {
        |  Session open() { return new Session("c"); }
        |}
        |""".stripMargin)
    )

    assertNoFinding(Report.findings(result))(_.rule.id == "constant-field")
  }

  test("a Java field constant across every construction, in main code and tests, is reported") {
    val result = javaBuild(
      Seq(Session, Starter),
      Seq("demo/SessionTest.java" -> """
        |package demo;
        |
        |public class SessionTest {
        |  Session closed() { return new Session("c", 2); }
        |}
        |""".stripMargin)
    )

    assertFinding(Report.findings(result), "constant-field", "Session.owned is always False across 3 constructions")
  }

  test("a Java field constant across the constructions of main code alone is reported") {
    val result = javaBuild(Seq(Session, Starter))

    assertFinding(Report.findings(result), "constant-field", "Session.owned is always False across 2 constructions")
  }

  private val Handle = "demo/Handle.java" -> """
    |package demo;
    |
    |public class Handle {
    |  private final String name;
    |  private final boolean owned;
    |  public Handle(String name, boolean owned) {
    |    if (name == null) throw new IllegalArgumentException("name");
    |    this.name = name;
    |    this.owned = owned;
    |  }
    |}
    |""".stripMargin

  private val Handles = "demo/Handles.java" -> """
    |package demo;
    |
    |public class Handles {
    |  Handle first() { return new Handle("a", false); }
    |  Handle second() { return new Handle("b", false); }
    |}
    |""".stripMargin

  private val Point = "demo/Point.java" -> "package demo;\npublic record Point(String name, boolean owned) {}\n"

  private val Points = "demo/Points.java" -> """
    |package demo;
    |
    |public class Points {
    |  Point first() { return new Point("a", false); }
    |  Point second() { return new Point("b", false); }
    |}
    |""".stripMargin

  private val Token = "demo/Token.java" -> """
    |package demo;
    |
    |public class Token {
    |  private final String name;
    |  private final boolean owned;
    |  private Token(String name, boolean owned) { this.name = name; this.owned = owned; }
    |  public static Token first() { return new Token("a", false); }
    |  public static Token second() { return new Token("b", false); }
    |}
    |""".stripMargin

  private val Libraries = Set("m")

  test("a public Java class with a public constructor in a library is not constant, as other builds construct it") {
    val result = javaBuild(Seq(Handle, Handles), libraries = Libraries)

    assertNoFinding(Report.findings(result))(_.rule.id == "constant-field")
  }

  test("a public Scala case class in a library is not constant, as other builds construct it") {
    val result = extract(
      Seq(
        "Handle.scala"  -> "case class Handle(name: String, owned: Boolean)",
        "Handles.scala" -> "object Handles { val all = List(Handle(\"a\", false), Handle(\"b\", false)) }"
      ),
      libraries = Libraries
    )

    assertNoFinding(Report.findings(result))(_.rule.id == "constant-field")
  }

  test("a public Java class with a public constructor outside a library is constant") {
    val result = javaBuild(Seq(Handle, Handles))

    assertFinding(Report.findings(result), "constant-field", "Handle.owned is always False across 2 constructions")
  }

  test("a public Scala case class outside a library is constant") {
    val result = extract(
      Seq(
        "Handle.scala"  -> "case class Handle(name: String, owned: Boolean)",
        "Handles.scala" -> "object Handles { val all = List(Handle(\"a\", false), Handle(\"b\", false)) }"
      )
    )

    assertFinding(Report.findings(result), "constant-field", "Handle.owned is always False across 2 constructions")
  }

  test("a public Java record outside a library is constant") {
    val result = javaBuild(Seq(Point, Points))

    assertFinding(Report.findings(result), "constant-field", "Point.owned is always False across 2 constructions")
  }

  test("a public Java record in a library is not constant, as other builds construct it") {
    val result = javaBuild(Seq(Point, Points), libraries = Libraries)

    assertNoFinding(Report.findings(result))(_.rule.id == "constant-field")
  }

  test("a Java class of a library whose constructors are private and built through factories is constant") {
    val result = javaBuild(Seq(Token), libraries = Libraries)

    assertFinding(Report.findings(result), "constant-field", "Token.owned is always False across 2 constructions")
  }

  test("a Scala case class of a library whose constructor is private and built through factories is constant") {
    val result = extract(
      Seq(
        "Token.scala" ->
          """case class Token private (name: String, owned: Boolean)
            |object Token {
            |  def first: Token  = Token("a", false)
            |  def second: Token = Token("b", false)
            |}""".stripMargin
      ),
      libraries = Libraries
    )

    assertFinding(Report.findings(result), "constant-field", "Token.owned is always False across 2 constructions")
  }

  test(
    "a public Java class nested in a package-private class of a library is constant, as other builds cannot reach it"
  ) {
    val result = javaBuild(
      Seq(
        "demo/Hidden.java" -> """
          |package demo;
          |
          |class Hidden {
          |  public static class Inner {
          |    private final String name;
          |    private final boolean owned;
          |    public Inner(String name, boolean owned) {
          |      if (name == null) throw new IllegalArgumentException("name");
          |      this.name = name;
          |      this.owned = owned;
          |    }
          |  }
          |  Object first() { return new Inner("a", false); }
          |  Object second() { return new Inner("b", false); }
          |}
          |""".stripMargin
      ),
      libraries = Libraries
    )

    assertFinding(Report.findings(result), "constant-field", "Inner.owned is always False across 2 constructions")
  }

  test("an overload of a Java method does not share the result of the method it overloads") {
    val result = javaBuild(Seq(
      "demo/Thing.java" -> """
        |package demo;
        |
        |public class Thing {
        |  private String id = "";
        |  public String id() { return id; }
        |  public Thing id(String id) { this.id = id; return this; }
        |  public String touch() { return id; }
        |}
        |""".stripMargin,
      "demo/Panel.java" -> """
        |package demo;
        |
        |public class Panel {
        |  void rename(Thing thing) { thing.id("a"); }
        |  void poke(Thing thing) { thing.touch(); }
        |}
        |""".stripMargin
    ))

    assertNoFinding(Report.findings(result))(finding =>
      finding.rule.id == "unused-return-value" && finding.detail.startsWith("Thing.id:")
    )
    assertFinding(Report.findings(result), "unused-return-value", "Thing.touch: its String result is never used")
  }

  test("static constants of Java types are not fields of the shape they make") {
    val result = javaBuild(Seq(
      "demo/First.java" -> """
        |package demo;
        |
        |public class First {
        |  static final String ALPHA = "a";
        |  static final String BETA = "b";
        |  static final String GAMMA = "c";
        |  final int count;
        |  final int limit;
        |  First(int count, int limit) { this.count = count; this.limit = limit; }
        |}
        |""".stripMargin,
      "demo/Second.java" -> """
        |package demo;
        |
        |public class Second {
        |  static final String ALPHA = "a";
        |  static final String BETA = "b";
        |  static final String GAMMA = "c";
        |  final int size;
        |  final int width;
        |  Second(int size, int width) { this.size = size; this.width = width; }
        |}
        |""".stripMargin
    ))

    assertNoFinding(Report.findings(result))(_.rule.id == "duplicate-shape")
  }

  test("Java types with the same fields and static constants are duplicates") {
    val result = javaBuild(Seq(
      "demo/First.java" -> """
        |package demo;
        |
        |public class First {
        |  static final String ALPHA = "a";
        |  static final String BETA = "b";
        |  static final String GAMMA = "c";
        |  final int count;
        |  final int limit;
        |  First(int count, int limit) { this.count = count; this.limit = limit; }
        |}
        |""".stripMargin,
      "demo/Second.java" -> """
        |package demo;
        |
        |public class Second {
        |  static final String ALPHA = "a";
        |  static final String BETA = "b";
        |  static final String GAMMA = "c";
        |  final int count;
        |  final int limit;
        |  Second(int count, int limit) { this.count = count; this.limit = limit; }
        |}
        |""".stripMargin
    ))

    assertFinding(Report.findings(result), "duplicate-shape", "First, Second")
  }

  test("Java builders nested in the type they build are not duplicates of it") {
    val result = javaBuild(Seq(
      "demo/AudioParameters.java" -> """
        |package demo;
        |
        |public final class AudioParameters {
        |  final int rate;
        |  final int channels;
        |  final int depth;
        |  public AudioParameters(int rate, int channels, int depth) {
        |    this.rate = rate;
        |    this.channels = channels;
        |    this.depth = depth;
        |  }
        |  public static final class Builder {
        |    private int rate;
        |    private int channels;
        |    private int depth;
        |    public Builder rate(int rate) { this.rate = rate; return this; }
        |    public Builder channels(int channels) { this.channels = channels; return this; }
        |    public Builder depth(int depth) { this.depth = depth; return this; }
        |    public AudioParameters build() { return new AudioParameters(rate, channels, depth); }
        |  }
        |}
        |""".stripMargin
    ))

    assertNoFinding(Report.findings(result))(_.rule.id == "duplicate-shape")
  }

  test("Java mutables nested in the type they convert to and from are not duplicates of it") {
    val result = javaBuild(Seq(
      "demo/Point.java" -> """
        |package demo;
        |
        |public final class Point {
        |  final int x;
        |  final int y;
        |  public Point(int x, int y) {
        |    this.x = x;
        |    this.y = y;
        |  }
        |  public Mutable toMutable() { return new Mutable(x, y); }
        |  public static final class Mutable {
        |    int x;
        |    int y;
        |    Mutable(int x, int y) {
        |      this.x = x;
        |      this.y = y;
        |    }
        |    public Point toImmutable() { return new Point(x, y); }
        |  }
        |}
        |""".stripMargin
    ))

    assertNoFinding(Report.findings(result))(_.rule.id == "duplicate-shape")
  }

  test("Java types nested in one another without building the outer type are duplicates") {
    val result = javaBuild(Seq(
      "demo/AudioParameters.java" -> """
        |package demo;
        |
        |public final class AudioParameters {
        |  final int rate;
        |  final int channels;
        |  final int depth;
        |  AudioParameters(int rate, int channels, int depth) {
        |    this.rate = rate;
        |    this.channels = channels;
        |    this.depth = depth;
        |  }
        |  public static final class Snapshot {
        |    private int rate;
        |    private int channels;
        |    private int depth;
        |    Snapshot(int rate, int channels, int depth) {
        |      this.rate = rate;
        |      this.channels = channels;
        |      this.depth = depth;
        |    }
        |  }
        |}
        |""".stripMargin
    ))

    assertFinding(Report.findings(result), "duplicate-shape", "AudioParameters, Snapshot")
  }
}
