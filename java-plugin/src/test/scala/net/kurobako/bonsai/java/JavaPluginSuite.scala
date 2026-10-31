package net.kurobako.bonsai.java

import net.kurobako.bonsai.testing.Scratch

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

  test("the collector gathers the types and the methods the compiler compiled") {
    val model = Fixtures.collect("Service" -> fixture)

    val names = model.nodes.map(_.name)
    assert(names.contains("demo.Service"), names.toString)
    assert(!names.contains("demo.Service.first"), "a method is not a node of the graph, its type is")
    assert(!names.contains("demo.Service.Service"), "a constructor is not a node of the graph, its type is")
  }

  test("the collector records where each node is defined") {
    val model = Fixtures.collect("Service" -> fixture)

    val service = model.nodes.find(_.name == "demo.Service").get
    assert(service.source.endsWith("demo/Service.java"), service.source)
    assertEquals(service.line, 6)
    assert(service.exported, "a public class is exported")
  }

  test("the collector records what is not exported") {
    val model = Fixtures.collect("Service" -> fixture)

    val service = model.nodes.find(_.name == "demo.Service").get
    assert(service.exported, "a public class is exported")
  }

  test("the collector records the references between the nodes") {
    val model = Fixtures.collect("Service" -> fixture)

    val targets = model.edges.map(_.to)
    assert(targets.contains("java.util.List"), targets.toString)
  }

  test("a type that holds a native method is entered from outside the build") {
    val model = Fixtures.collect("Service" -> """
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
    val model = Fixtures.collect("Service" -> """
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
    val model = Fixtures.collect("Service" -> """
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
    val model = Fixtures.collect("Service" -> """
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
    val model = Fixtures.collect("Service" -> """
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
    val model = Fixtures.collect("Service" -> """
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
    val model = Fixtures.collect("Service" -> """
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
    assertEquals(site.arguments, Vector("unknown", "true", "number:3"))
  }

  test("a call whose argument is a text of several lines is held by one record") {
    val model = Fixtures.collect("Service" -> """
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
    Scratch.using("bonsai-model") { directory =>
      val file = directory.resolve("model.tsv")
      Model.write(model, file)
      val read = Model.read(file).fold(error => fail(error), identity)
      assertEquals(read.sites.find(_.callee == "demo.Service.helper").get.arguments, Vector("text:a\nb"))
    }
  }

  test("a call whose argument is a text of unpaired surrogates and carriage returns survives the model file") {
    val escape = "\\u"
    val model  = Fixtures.collect("Service" -> s"""
      |package demo;
      |
      |public class Service {
      |
      |  public String find() {
      |    return helper("${escape}d800a\\r${escape}dc00");
      |  }
      |
      |  private String helper(String text) {
      |    return text;
      |  }
      |}
      |""".stripMargin)

    Scratch.using("bonsai-model") { directory =>
      val file = directory.resolve("model.tsv")
      Model.write(model, file)
      assertEquals(Model.read(file), Right(model))
    }
  }

  test("the collector gathers the fields of a type") {
    val model = Fixtures.collect("Service" -> """
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
    val model = Fixtures.collect(
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

  private def runBody(statements: String) =
    s"""
       |package demo;
       |
       |import java.util.List;
       |
       |public class First {
       |
       |  static int g(int x) {
       |    return x;
       |  }
       |
       |  int run(List<Integer> xs) {
       |$statements
       |  }
       |}
       |""".stripMargin

  test("the cost of a block counts the code that is written, not its modifiers, types or wrappers") {
    val model = Fixtures.collect(
      "First" -> runBody(
        """    int total = 0;
          |    for (int x : xs) {
          |      g(x);
          |      total = x;
          |    }
          |    int a = g(1);
          |    int b = g(a);
          |    int c = g(b);
          |    return total + c;""".stripMargin
      )
    )
    val body = model.fragments.filter(_.member == "run").map(_.cost).max
    assertEquals(body, 30, model.fragments.map(f => (f.member, f.start, f.end, f.cost)).mkString("\n"))
  }

  test("a block below the cost of an expression is no fragment of its own") {
    val model = Fixtures.collect("First" -> runBody("    int a = g(1);\n    return g(a);"))
    assert(!model.fragments.exists(f => f.member == "run" && (f.roles & Model.Roles.Whole) != 0), model.fragments)
  }

  test("the collector tells the code that differs apart") {
    val model = Fixtures.collect(
      "First" -> """
      |package demo;
      |
      |public class First {
      |
      |  public String build(String name) {
      |    StringBuilder result = new StringBuilder();
      |    result.append("-").append("+").append("*");
      |    result.append("-").append("+").append("*");
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
      |    result.append("-").append("+").append("*");
      |    result.append("-").append("+").append("*");
      |    result.append(other);
      |    return result.toString();
      |  }
      |
      |  public String other() {
      |    StringBuilder result = new StringBuilder();
      |    result.append("-").append("+").append("*");
      |    result.append("-").append("+").append("*");
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
    val model = Fixtures.collect(
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
    val model = Fixtures.collect("Point" -> """
      |package demo;
      |
      |public record Point(int x, int y) {}
      |""".stripMargin)

    val product = model.products.find(_.name == "demo.Point").get
    assertEquals(product.fields.map(_.name), Vector("x", "y"))
    assertEquals(product.fields.map(_.kind), Vector("number", "number"))
  }

  test("a sealed type is a sum of the types that extend it") {
    val model = Fixtures.collect("Shape" -> """
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
    val model = Fixtures.collect(
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

  private val Annotations = Seq(
    "Nullable" -> """
      |package org.jspecify.annotations;
      |
      |public @interface Nullable {}
      |""".stripMargin,
    "NullMarked" -> """
      |package org.jspecify.annotations;
      |
      |public @interface NullMarked {}
      |""".stripMargin
  )

  test("a java.util.Optional is an optional of the type it holds") {
    val model = Fixtures.collect("Service" -> """
      |package demo;
      |
      |import java.util.Optional;
      |import java.util.OptionalInt;
      |import java.util.OptionalLong;
      |import java.util.OptionalDouble;
      |
      |public class Service {
      |  public String find(Optional<String> name, OptionalInt count, OptionalLong total, OptionalDouble ratio) {
      |    return null;
      |  }
      |}
      |""".stripMargin)

    val method = model.methods.find(_.name == "demo.Service.find").get
    assertEquals(method.parameters.map(_.kind), Vector("optional", "optional", "optional", "optional"))
    assertEquals(method.parameters.map(_.shape), Vector("java.lang.String", "int", "long", "double"))
    assertEquals(method.parameters.map(_.primitive), Vector(false, false, false, false))
  }

  test("a type that is not an optional is not read as one") {
    val model = Fixtures.collect("Service" -> """
      |package demo;
      |
      |import java.util.List;
      |import java.util.Optional;
      |
      |public class Service {
      |  public String find(List<Optional<String>> names, String name) {
      |    return null;
      |  }
      |}
      |""".stripMargin)

    val method = model.methods.find(_.name == "demo.Service.find").get
    assertEquals(method.parameters.map(_.kind), Vector("other", "other"))
  }

  test("a call that passes an Optional is told apart by how the Optional is made") {
    val model = Fixtures.collect("Service" -> """
      |package demo;
      |
      |import java.util.Optional;
      |import java.util.OptionalInt;
      |
      |public class Service {
      |  public void run(String name, Optional<String> held) {
      |    take(Optional.empty());
      |    take(Optional.of(name));
      |    take(Optional.ofNullable(name));
      |    take(held);
      |    count(OptionalInt.empty());
      |    count(OptionalInt.of(3));
      |  }
      |
      |  private void take(Optional<String> value) {}
      |
      |  private void count(OptionalInt value) {}
      |}
      |""".stripMargin)

    val takes = model.sites.filter(_.callee == "demo.Service.take").map(_.arguments)
    assertEquals(takes, Vector(Vector("absent"), Vector("present:"), Vector("unknown"), Vector("unknown")))
    val counts = model.sites.filter(_.callee == "demo.Service.count").map(_.arguments)
    assertEquals(counts, Vector(Vector("absent"), Vector("present:")))
  }

  test("an Optional made by a method of another type is not read as one") {
    val model = Fixtures.collect("Service" -> """
      |package demo;
      |
      |import java.util.Optional;
      |
      |public class Service {
      |  public void run() {
      |    take(Maker.empty());
      |    take(Maker.of(1));
      |  }
      |
      |  private void take(Optional<String> value) {}
      |
      |  static class Maker {
      |    static Optional<String> empty() { return Optional.empty(); }
      |    static Optional<String> of(int value) { return Optional.of("x"); }
      |  }
      |}
      |""".stripMargin)

    val takes = model.sites.filter(_.callee == "demo.Service.take").map(_.arguments)
    assertEquals(takes, Vector(Vector("unknown"), Vector("unknown")))
  }

  test("a variable that may be null is not passed as a value that is there") {
    val model = Fixtures.collect(
      Annotations ++ Seq("Service" -> """
        |package demo;
        |
        |import org.jspecify.annotations.Nullable;
        |
        |public class Service {
        |  public void run(@Nullable String name, String plain, int count) {
        |    take(name, plain, count);
        |  }
        |
        |  private void take(@Nullable String name, String plain, int count) {}
        |}
        |""".stripMargin)*
    )

    val site = model.sites.find(_.callee == "demo.Service.take").get
    assertEquals(site.arguments, Vector("unknown", "unknown", "present:count"))
  }

  test("a variable of a null-marked scope that is not annotated is passed as a value that is there") {
    val model = Fixtures.collect(
      Annotations ++ Seq("Service" -> """
        |package demo;
        |
        |import org.jspecify.annotations.NullMarked;
        |import org.jspecify.annotations.Nullable;
        |
        |@NullMarked
        |public class Service {
        |  public void run(@Nullable String name, String plain) {
        |    take(name, plain);
        |  }
        |
        |  private void take(@Nullable String name, String plain) {}
        |}
        |""".stripMargin)*
    )

    val site = model.sites.find(_.callee == "demo.Service.take").get
    assertEquals(site.arguments, Vector("unknown", "present:plain"))
  }

  test("a local variable of a null-marked scope is not passed as a value that is there") {
    val model = Fixtures.collect(
      Annotations ++ Seq("Service" -> """
        |package demo;
        |
        |import java.util.Map;
        |import org.jspecify.annotations.NullMarked;
        |
        |@NullMarked
        |public class Service {
        |  private final String field = "x";
        |
        |  public void run(Map<String, String> names, String key) {
        |    String found = names.get(key);
        |    int size = names.size();
        |    take(found, size, field);
        |  }
        |
        |  private void take(String name, int size, String other) {}
        |}
        |""".stripMargin)*
    )

    val site = model.sites.find(_.callee == "demo.Service.take").get
    assertEquals(site.arguments, Vector("unknown", "present:size", "present:field"))
  }

  test("a construction of a record is a call of the record with the arguments it passes") {
    val model = Fixtures.collect(
      "Point" -> """
        |package demo;
        |
        |public record Point(int x, int y) {
        |  public Point(int x) { this(x, 0); }
        |}
        |""".stripMargin,
      "Use" -> """
        |package demo;
        |
        |public class Use {
        |  Point full = new Point(1, -2);
        |  Point partial = new Point(3);
        |
        |  Point make(int x) {
        |    return new Point(x, 0);
        |  }
        |}
        |""".stripMargin
    )

    val sites = model.sites.filter(_.callee == "demo.Point")
    assertEquals(sites.map(_.caller), Vector("demo.Use", "demo.Use.make"))
    assertEquals(sites.map(_.arguments), Vector(Vector("number:1", "number:-2"), Vector("present:x", "number:0")))
  }

  test("a construction of a class is not a call of it") {
    val model = Fixtures.collect("Use" -> """
      |package demo;
      |
      |public class Use {
      |  StringBuilder builder = new StringBuilder("a");
      |  Object anonymous = new Object() {};
      |}
      |""".stripMargin)

    assertEquals(model.sites.filter(_.arguments.nonEmpty), Vector.empty)
    assertEquals(model.sites.map(_.callee).filter(_.startsWith("java")), Vector.empty)
  }

  private def constructions(sources: (String, String)*): Map[String, Model.Construction] =
    Fixtures.collect((Annotations ++ sources)*).constructions.map(construction =>
      construction.owner -> construction
    ).toMap

  test("the guard a compact constructor starts with is a fact of the record") {
    val found = constructions("Source" -> """
      |package demo;
      |
      |import java.util.Objects;
      |import org.jspecify.annotations.Nullable;
      |
      |public record Source(@Nullable String toolkit, @Nullable String image, int count, boolean flag) {
      |  public Source {
      |    Objects.requireNonNull(toolkit);
      |    if (image != null && flag || count < -1) {
      |      throw new IllegalArgumentException();
      |    }
      |  }
      |}
      |""".stripMargin)

    val guard = Model.Guard.Connective(
      Model.Junction.And,
      Model.Guard.Holds("toolkit", Model.Probe.Present),
      Model.Guard.Not(
        Model.Guard.Connective(
          Model.Junction.Or,
          Model.Guard.Connective(
            Model.Junction.And,
            Model.Guard.Holds("image", Model.Probe.Present),
            Model.Guard.Holds("flag", Model.Probe.Flag)
          ),
          Model.Guard.Compare("count", "<", BigDecimal(-1))
        )
      )
    )
    assertEquals(found("demo.Source"), Model.Construction("demo.Source", "open", Vector.empty, guard))
  }

  test("a record that checks nothing has no fact of construction") {
    val found = constructions("Plain" -> "package demo;\npublic record Plain(int count) {}\n")

    assertEquals(found, Map.empty)
  }

  test("a statement that is not a guard ends the guards of a constructor") {
    val found = constructions("Source" -> """
      |package demo;
      |
      |public record Source(int count, int limit) {
      |  public Source {
      |    if (count < 0) throw new IllegalArgumentException();
      |    limit = Math.max(limit, 0);
      |    if (limit > 9) throw new IllegalArgumentException();
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      found("demo.Source").guard,
      Model.Guard.Not(Model.Guard.Compare("count", "<", BigDecimal(0)))
    )
  }

  test("a parameter that the constructor changes is not a field that it guards") {
    val found = constructions("Source" -> """
      |package demo;
      |
      |public record Source(int count) {
      |  public Source {
      |    if (count < 0) throw new IllegalArgumentException();
      |    count = -count;
      |  }
      |}
      |""".stripMargin)

    assertEquals(found("demo.Source").guard, Model.Guard.Not(Model.Guard.Unknown))
  }

  test("the guard of a class is over the final fields its parameters are stored in") {
    val found = constructions("Counter" -> """
      |package demo;
      |
      |public class Counter {
      |  private final int size;
      |  private int moving;
      |
      |  public Counter(int size, int moving) {
      |    if (size < 0 || moving < 0) throw new IllegalArgumentException();
      |    this.size = size;
      |    this.moving = moving;
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      found("demo.Counter").guard,
      Model.Guard.Not(
        Model.Guard.Connective(Model.Junction.Or, Model.Guard.Compare("size", "<", BigDecimal(0)), Model.Guard.Unknown)
      )
    )
  }

  test("a type whose constructors are all private is private, and one with a factory for it is a factory") {
    val found = constructions(
      "Token" -> """
        |package demo;
        |
        |public class Token {
        |  private final String value;
        |  private Token(String value) { this.value = value; }
        |}
        |""".stripMargin,
      "Range" -> """
        |package demo;
        |
        |import java.util.Optional;
        |
        |public class Range {
        |  final int low;
        |  Range(int low) { this.low = low; }
        |  public static Optional<Range> of(int low) { return Optional.of(new Range(low)); }
        |  public static Range make(int low) { return new Range(low); }
        |  public static Optional<Range> parse(String text) { return Optional.empty(); }
        |}
        |""".stripMargin
    )

    assertEquals(found("demo.Token").access, "private")
    assertEquals(found("demo.Range").access, "factory")
    assertEquals(found("demo.Range").factories, Vector("Range.of"))
  }

  private val Guava = Seq(
    "guava/Preconditions" -> """
      |package com.google.common.base;
      |
      |public final class Preconditions {
      |  public static void checkArgument(boolean expression) {}
      |  public static void checkArgument(boolean expression, Object message) {}
      |  public static void checkArgument(boolean expression, String template, Object... arguments) {}
      |  public static void checkState(boolean expression) {}
      |  public static void checkState(boolean expression, Object message) {}
      |  public static <T> T checkNotNull(T reference) { return reference; }
      |  public static <T> T checkNotNull(T reference, Object message) { return reference; }
      |}
      |""".stripMargin,
    "guava/Verify" -> """
      |package com.google.common.base;
      |
      |public final class Verify {
      |  public static void verify(boolean expression) {}
      |  public static <T> T verifyNotNull(T reference) { return reference; }
      |}
      |""".stripMargin,
    "other/Preconditions" -> """
      |package demo.other;
      |
      |public final class Preconditions {
      |  public static void checkArgument(boolean expression) {}
      |  public static <T> T checkNotNull(T reference) { return reference; }
      |}
      |""".stripMargin
  )

  private def guarded(sources: (String, String)*): Model.Model = Fixtures.collect((Annotations ++ Guava ++ sources)*)

  test("checkArgument and checkState of a compact constructor read like a throwing if") {
    val found = guarded(
      "Size" -> """
        |package demo;
        |
        |import static com.google.common.base.Preconditions.checkArgument;
        |import com.google.common.base.Preconditions;
        |
        |public record Size(int count, int limit) {
        |  public Size {
        |    checkArgument(count >= 0, "negative: %s", count);
        |    Preconditions.checkState(limit > count);
        |  }
        |}
        |""".stripMargin
    ).constructions.find(_.owner == "demo.Size").get

    assertEquals(
      found.guard,
      Model.Guard.Connective(
        Model.Junction.And,
        Model.Guard.Compare("count", ">=", BigDecimal(0)),
        Model.Guard.Unknown
      )
    )
  }

  test("checkNotNull, verifyNotNull and requireNonNull with a message or supplier require presence") {
    val found = guarded(
      "Names" ->
        """
        |package demo;
        |
        |import com.google.common.base.Preconditions;
        |import com.google.common.base.Verify;
        |import java.util.Objects;
        |import org.jspecify.annotations.Nullable;
        |
        |public record Names(@Nullable String first, @Nullable String second, @Nullable String third, @Nullable String fourth) {
        |  public Names {
        |    Preconditions.checkNotNull(first, "first");
        |    Verify.verifyNotNull(second);
        |    Objects.requireNonNull(third, "third");
        |    Objects.requireNonNull(fourth, () -> "fourth");
        |  }
        |}
        |""".stripMargin
    ).constructions.find(_.owner == "demo.Names").get

    def present(name: String) = Model.Guard.Holds(name, Model.Probe.Present)
    assertEquals(
      found.guard,
      present("first").and(present("second")).and(present("third")).and(present("fourth"))
    )
  }

  test("checkNotNull of a parameter stored in a field is a guard of the class") {
    val found = guarded(
      "Holder" -> """
        |package demo;
        |
        |import static com.google.common.base.Preconditions.checkNotNull;
        |
        |public class Holder {
        |  private final java.util.Optional<String> value;
        |  private final int size;
        |
        |  public Holder(java.util.Optional<String> value, int size) {
        |    this.value = checkNotNull(value);
        |    this.size = size;
        |  }
        |}
        |""".stripMargin
    ).constructions

    assertEquals(found, Vector.empty, "a non-null Optional is no fact")
  }

  test("a method of another type named like a Guava check is not a guard") {
    val found = guarded(
      "Size" -> """
        |package demo;
        |
        |import demo.other.Preconditions;
        |
        |public record Size(int count) {
        |  public Size {
        |    Preconditions.checkArgument(count >= 0);
        |  }
        |}
        |""".stripMargin
    ).constructions

    assertEquals(found, Vector.empty)
  }

  private val Indexes = Seq(
    "other/Indexes" -> """
      |package demo.other;
      |
      |public final class Indexes {
      |  public static int checkIndex(int index, int length) { return index; }
      |}
      |""".stripMargin
  )

  test("Objects.checkIndex reads as a lower bound and an upper bound") {
    val model = guarded(
      "Slot" -> """
        |package demo;
        |
        |import java.util.Objects;
        |
        |public record Slot(int index, int size) {
        |  public Slot {
        |    Objects.checkIndex(index, size);
        |  }
        |}
        |""".stripMargin,
      "Fixed" -> """
        |package demo;
        |
        |import java.util.Objects;
        |
        |public class Fixed {
        |  private final int index;
        |
        |  public Fixed(int index) {
        |    this.index = Objects.checkIndex(index, 10);
        |  }
        |}
        |""".stripMargin
    )

    def guard(owner: String) = model.constructions.find(_.owner == owner).get.guard
    assertEquals(
      guard("demo.Slot"),
      Model.Guard.Compare("index", ">=", BigDecimal(0)).and(Model.Guard.Unknown)
    )
    assertEquals(
      guard("demo.Fixed"),
      Model.Guard.Compare("index", ">=", BigDecimal(0)).and(Model.Guard.Compare("index", "<", BigDecimal(10)))
    )
  }

  test("a checkIndex of another type, or of a value that is not a stored number, is not a bound") {
    val model = guarded(
      Indexes.head,
      "Slot" -> """
        |package demo;
        |
        |import demo.other.Indexes;
        |
        |public record Slot(int index, int size) {
        |  public Slot {
        |    Indexes.checkIndex(index, size);
        |  }
        |}
        |""".stripMargin,
      "Name" -> """
        |package demo;
        |
        |import java.util.Objects;
        |
        |public record Name(int index, int size) {
        |  public Name {
        |    Objects.checkIndex(size + 1, size);
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(model.constructions.map(_.owner).filter(_.startsWith("demo.")), Vector("demo.Name"))
    assertEquals(model.constructions.find(_.owner == "demo.Name").get.guard, Model.Guard.Unknown)
  }

  private val Commons = Seq(
    "commons/Validate" -> """
      |package org.apache.commons.lang3;
      |
      |public class Validate {
      |  public static void isTrue(boolean expression) {}
      |  public static void isTrue(boolean expression, String message, long value) {}
      |  public static void isTrue(boolean expression, String message, Object... values) {}
      |  public static void validState(boolean expression) {}
      |  public static void inclusiveBetween(long start, long end, long value) {}
      |  public static void inclusiveBetween(double start, double end, double value, String message) {}
      |  public static <T> void inclusiveBetween(T start, T end, Comparable<T> value) {}
      |  public static void exclusiveBetween(long start, long end, long value, String message, Object... values) {}
      |  public static void exclusiveBetween(double start, double end, double value) {}
      |  public static <T> T notNull(T object) { return object; }
      |  public static <T> T notNull(T object, String message, Object... values) { return object; }
      |  public static <T extends CharSequence> T notEmpty(T chars) { return chars; }
      |  public static <T extends CharSequence> T notBlank(T chars, String message, Object... values) { return chars; }
      |}
      |""".stripMargin,
    "other/Validate" -> """
      |package demo.other;
      |
      |public class Validate {
      |  public static void isTrue(boolean expression) {}
      |  public static void inclusiveBetween(long start, long end, long value) {}
      |  public static <T> T notNull(T object) { return object; }
      |}
      |""".stripMargin
  )

  test("Validate.isTrue, validState, notNull, notEmpty and notBlank are guards") {
    val model = guarded(
      (Commons ++ Seq(
        "Size" ->
          """
          |package demo;
          |
          |import org.apache.commons.lang3.Validate;
          |import org.jspecify.annotations.Nullable;
          |
          |public record Size(int count, int limit, @Nullable String first, @Nullable String second, @Nullable String third) {
          |  public Size {
          |    Validate.isTrue(count >= 0, "negative: %d", count);
          |    Validate.validState(limit > 1);
          |    Validate.notNull(first);
          |    Validate.notEmpty(second);
          |    Validate.notBlank(third, "third");
          |  }
          |}
          |""".stripMargin
      ))*
    )

    def present(name: String) = Model.Guard.Holds(name, Model.Probe.Present)
    assertEquals(
      model.constructions.find(_.owner == "demo.Size").get.guard,
      Model.Guard.Compare("count", ">=", BigDecimal(0))
        .and(Model.Guard.Compare("limit", ">", BigDecimal(1)))
        .and(present("first")).and(present("second")).and(present("third"))
    )
  }

  test("Validate.inclusiveBetween and exclusiveBetween are range comparisons") {
    val model = guarded(
      (Commons ++ Seq(
        "Range" -> """
          |package demo;
          |
          |import org.apache.commons.lang3.Validate;
          |
          |public record Range(int low, double ratio, long wide, int boxed) {
          |  public Range {
          |    Validate.inclusiveBetween(0, 10, low);
          |    Validate.exclusiveBetween(0.5, 1.5, ratio);
          |    Validate.inclusiveBetween(-1.5, 2.5, wide, "wide");
          |    Validate.exclusiveBetween(1L, 9L, boxed, "boxed: %d", boxed);
          |  }
          |}
          |""".stripMargin
      ))*
    )

    def compare(name: String, operator: String, bound: BigDecimal) = Model.Guard.Compare(name, operator, bound)
    def between(name: String, low: String, from: BigDecimal, high: String, to: BigDecimal) =
      compare(name, low, from).and(compare(name, high, to))
    assertEquals(
      model.constructions.find(_.owner == "demo.Range").get.guard,
      between("low", ">=", 0, "<=", 10)
        .and(between("ratio", ">", BigDecimal("0.5"), "<", BigDecimal("1.5")))
        .and(between("wide", ">=", BigDecimal("-1.5"), "<=", BigDecimal("2.5")))
        .and(between("boxed", ">", 1, "<", 9))
    )
  }

  test("a range check with a bound that is not a constant, or of a value that is not stored, is not known") {
    val model = guarded(
      (Commons ++ Seq(
        "Loose" -> """
          |package demo;
          |
          |import org.apache.commons.lang3.Validate;
          |
          |public record Loose(int low, int high) {
          |  public Loose {
          |    Validate.inclusiveBetween(0, high, low);
          |  }
          |}
          |""".stripMargin
      ))*
    )

    assertEquals(model.constructions.find(_.owner == "demo.Loose").get.guard, Model.Guard.Unknown)
  }

  test("a range check of another Validate is not a guard") {
    val model = guarded(
      (Commons ++ Seq(
        "Range" -> """
          |package demo;
          |
          |import demo.other.Validate;
          |
          |public record Range(int low) {
          |  public Range {
          |    Validate.inclusiveBetween(0, 10, low);
          |  }
          |}
          |""".stripMargin
      ))*
    )

    assertEquals(model.constructions, Vector.empty)
  }

  private val Spring = Seq(
    "spring/Assert" -> """
      |package org.springframework.util;
      |
      |import java.util.Collection;
      |import java.util.function.Supplier;
      |
      |public abstract class Assert {
      |  public static void isTrue(boolean expression, String message) {}
      |  public static void isTrue(boolean expression, Supplier<String> message) {}
      |  public static void state(boolean expression, String message) {}
      |  public static void state(boolean expression, Supplier<String> message) {}
      |  public static void notNull(Object object, String message) {}
      |  public static void hasLength(String text, String message) {}
      |  public static void hasText(String text, Supplier<String> message) {}
      |  public static void notEmpty(Collection<?> collection, String message) {}
      |}
      |""".stripMargin,
    "other/Assert" -> """
      |package demo.other;
      |
      |public class Assert {
      |  public static void isTrue(boolean expression, String message) {}
      |  public static void notNull(Object object, String message) {}
      |}
      |""".stripMargin
  )

  test("Assert.isTrue, state, notNull, hasLength, hasText and notEmpty of Spring are guards") {
    val model = guarded(
      (Spring ++ Seq(
        "Order" ->
          """
          |package demo;
          |
          |import org.jspecify.annotations.Nullable;
          |import org.springframework.util.Assert;
          |
          |public record Order(int count, int limit, @Nullable String id, @Nullable String name, @Nullable String text) {
          |  public Order {
          |    Assert.isTrue(count >= 0, "negative");
          |    Assert.state(limit > 1, () -> "small");
          |    Assert.notNull(id, "id");
          |    Assert.hasLength(name, "name");
          |    Assert.hasText(text, () -> "text");
          |  }
          |}
          |""".stripMargin
      ))*
    )

    def present(name: String) = Model.Guard.Holds(name, Model.Probe.Present)
    assertEquals(
      model.constructions.find(_.owner == "demo.Order").get.guard,
      Model.Guard.Compare("count", ">=", BigDecimal(0))
        .and(Model.Guard.Compare("limit", ">", BigDecimal(1)))
        .and(present("id")).and(present("name")).and(present("text"))
    )
  }

  test("an Assert of another type is not a guard") {
    val model = guarded(
      (Spring ++ Seq(
        "Order" -> """
          |package demo;
          |
          |import demo.other.Assert;
          |
          |public record Order(int count, String id) {
          |  public Order {
          |    Assert.isTrue(count >= 0, "negative");
          |    Assert.notNull(id, "id");
          |  }
          |}
          |""".stripMargin
      ))*
    )

    assertEquals(model.constructions, Vector.empty)
  }

  test("a Validate of another type is not a guard") {
    val model = guarded(
      (Commons ++ Seq(
        "Size" -> """
          |package demo;
          |
          |import demo.other.Validate;
          |
          |public record Size(int count) {
          |  public Size {
          |    Validate.isTrue(count >= 0);
          |    Validate.notNull(count);
          |  }
          |}
          |""".stripMargin
      ))*
    )

    assertEquals(model.constructions, Vector.empty)
  }

  test("requireNonNullElse is a default, not a guard") {
    val found = guarded(
      "Names" -> """
        |package demo;
        |
        |import java.util.Objects;
        |import org.jspecify.annotations.Nullable;
        |
        |public record Names(@Nullable String first) {
        |  public Names {
        |    Objects.requireNonNullElse(first, "");
        |  }
        |}
        |""".stripMargin
    ).constructions

    assertEquals(found, Vector.empty)
  }

  private def preconditions(sources: (String, String)*): Map[String, Model.Precondition] =
    guarded(sources*).preconditions.map(found => found.method -> found).toMap

  test("the guards a static method starts with are its precondition") {
    val found = preconditions(
      "Service" -> """
        |package demo;
        |
        |import static com.google.common.base.Preconditions.checkArgument;
        |import com.google.common.base.Preconditions;
        |
        |public class Service {
        |  public static int take(int count, int limit) {
        |    checkArgument(count >= 0, "negative");
        |    if (limit < count) throw new IllegalArgumentException();
        |    return count;
        |  }
        |
        |  private int give(int step) {
        |    Preconditions.checkState(step > 1);
        |    return 1;
        |  }
        |
        |  public final int keep(int count) {
        |    Preconditions.checkArgument(count > 3);
        |    return count;
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(
      found("demo.Service.take").guard,
      Model.Guard.Compare("count", ">=", BigDecimal(0)).and(Model.Guard.Not(Model.Guard.Unknown))
    )
    assertEquals(found("demo.Service.give").guard, Model.Guard.Compare("step", ">", BigDecimal(1)))
    assertEquals(found("demo.Service.keep").guard, Model.Guard.Compare("count", ">", BigDecimal(3)))
  }

  test("a method that is only a check of a flag or of presence has no precondition") {
    val found = preconditions(
      "Checks" -> """
        |package demo;
        |
        |import com.google.common.base.Preconditions;
        |import org.jspecify.annotations.Nullable;
        |
        |public final class Checks {
        |  static void check(boolean ok) {
        |    Preconditions.checkState(ok);
        |  }
        |
        |  static void unless(boolean failed) {
        |    Preconditions.checkState(!failed);
        |  }
        |
        |  static <T> T present(@Nullable T value) {
        |    return Preconditions.checkNotNull(value);
        |  }
        |
        |  static void absent(@Nullable String value, String key) {
        |    Preconditions.checkArgument(value == null, "no value for %s", key);
        |  }
        |
        |  static void both(boolean ok, int count) {
        |    Preconditions.checkArgument(ok && count > 0);
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(found.keySet, Set("demo.Checks.both"))
  }

  test("a method that overrides another has no precondition") {
    val found = preconditions(
      "Sizer"  -> "package demo;\npublic interface Sizer { void size(int count); }\n",
      "Parser" -> """
        |package demo;
        |
        |import com.google.common.base.Preconditions;
        |
        |public final class Parser implements Sizer {
        |  @Override
        |  public void size(int count) {
        |    Preconditions.checkArgument(count >= 0);
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(found, Map.empty)
  }

  test("a method that other code may override has no precondition") {
    val found = preconditions(
      "Service" -> """
        |package demo;
        |
        |import com.google.common.base.Preconditions;
        |
        |public class Service {
        |  public int take(int count) {
        |    Preconditions.checkArgument(count >= 0);
        |    return count;
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(found, Map.empty)
  }

  test("a method of a final class, a record and an enum with a guard has a precondition") {
    val found = preconditions(
      "Closed" -> """
        |package demo;
        |
        |import com.google.common.base.Preconditions;
        |
        |public final class Closed {
        |  public int take(int count) {
        |    Preconditions.checkArgument(count >= 0);
        |    return count;
        |  }
        |}
        |""".stripMargin,
      "Pair" -> """
        |package demo;
        |
        |import com.google.common.base.Preconditions;
        |
        |public record Pair(int one) {
        |  public int take(int count) {
        |    Preconditions.checkArgument(count >= 0);
        |    return count;
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(found.keySet, Set("demo.Closed.take", "demo.Pair.take"))
  }

  test("an overloaded method has no precondition") {
    val found = preconditions(
      "Service" -> """
        |package demo;
        |
        |import com.google.common.base.Preconditions;
        |
        |public final class Service {
        |  public static int take(int count) {
        |    Preconditions.checkArgument(count >= 0);
        |    return count;
        |  }
        |
        |  public static int take(String text) {
        |    return text.length();
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(found, Map.empty)
  }

  test("a guard after other statements, over a local, a field or a changed parameter is no precondition") {
    val found = preconditions(
      "Service" -> """
        |package demo;
        |
        |import com.google.common.base.Preconditions;
        |
        |public final class Service {
        |  private static boolean ready;
        |
        |  public static int late(int count) {
        |    System.out.println(count);
        |    Preconditions.checkArgument(count >= 0);
        |    return count;
        |  }
        |
        |  public static int local(int count) {
        |    int doubled = count * 2;
        |    Preconditions.checkArgument(doubled >= 0);
        |    return count;
        |  }
        |
        |  public static int state(int count) {
        |    Preconditions.checkState(ready);
        |    return count;
        |  }
        |
        |  public static int changed(int count) {
        |    Preconditions.checkArgument(count >= 0);
        |    count = -count;
        |    return count;
        |  }
        |
        |  public static int lookalike(int count) {
        |    demo.other.Preconditions.checkArgument(count >= 0);
        |    return count;
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(found, Map.empty)
  }

  private def classSites(model: Model.Model, owner: String) = model.sites.filter(_.callee == owner)

  private val Interval = "Interval" -> """
    |package demo;
    |
    |import java.util.Optional;
    |
    |public class Interval {
    |  private final int low;
    |  private final int high;
    |  private final String label;
    |  static int created;
    |
    |  public Interval(int high, String label, int low) {
    |    if (low > high) throw new IllegalArgumentException();
    |    this.low = low;
    |    this.high = high;
    |    this.label = label;
    |  }
    |
    |  public static Optional<Interval> of(int low, int high, String label) {
    |    return Optional.empty();
    |  }
    |}
    |""".stripMargin

  test("a construction of a class maps its arguments to the fields the constructor stores them in") {
    val model = guarded(
      Interval,
      "Use" -> """
        |package demo;
        |
        |public class Use {
        |  Interval fixed = new Interval(9, "a", 1);
        |
        |  Interval make(int top) {
        |    return new Interval(top, "b", -2);
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(
      classSites(model, "demo.Interval").map(_.arguments),
      Vector(
        Vector("number:1", "number:9", "text:a", "unknown"),
        Vector("number:-2", "present:top", "text:b", "unknown")
      )
    )
    assertEquals(classSites(model, "demo.Interval").map(_.caller), Vector("demo.Use", "demo.Use.make"))
  }

  test("a construction through a constructor that does not store its parameters has arguments that are unknown") {
    val model = guarded(
      "Token" -> """
        |package demo;
        |
        |import java.util.Optional;
        |
        |public class Token {
        |  private final int size;
        |
        |  public Token(int size) {
        |    this.size = Math.max(size, 0);
        |  }
        |
        |  public static Optional<Token> of(int size) { return Optional.empty(); }
        |}
        |""".stripMargin,
      "Use" -> "package demo;\npublic class Use { Token one = new Token(4); }\n"
    )

    assertEquals(classSites(model, "demo.Token").map(_.arguments), Vector(Vector("unknown")))
  }

  test("a construction through one of constructors of the same arity has arguments that are unknown") {
    val model = guarded(
      "Token" -> """
        |package demo;
        |
        |import java.util.Optional;
        |
        |public class Token {
        |  private final int size;
        |
        |  public Token(int size) {
        |    if (size < 0) throw new IllegalArgumentException();
        |    this.size = size;
        |  }
        |
        |  public Token(String size) {
        |    this.size = 0;
        |  }
        |
        |  public static Optional<Token> of(int size) { return Optional.empty(); }
        |}
        |""".stripMargin,
      "Use" -> "package demo;\npublic class Use { Token one = new Token(4); Token two = new Token(\"x\"); }\n"
    )

    assertEquals(classSites(model, "demo.Token").map(_.arguments), Vector(Vector("number:4"), Vector("unknown")))
  }

  test("a construction binds the constructor of its arguments among those of the same arity") {
    val model = guarded(
      "Pair" -> """
        |package demo;
        |
        |import java.util.Optional;
        |
        |public class Pair {
        |  private final int first;
        |  private final String second;
        |
        |  public Pair(int first, String second) {
        |    if (first < 0) throw new IllegalArgumentException();
        |    this.first = first;
        |    this.second = second;
        |  }
        |
        |  public Pair(String second, int first) {
        |    if (first < 0) throw new IllegalArgumentException();
        |    this.second = second;
        |    this.first = first;
        |  }
        |
        |  public static Optional<Pair> of(int first, String second) { return Optional.empty(); }
        |}
        |""".stripMargin,
      "Use" -> "package demo;\npublic class Use { Pair one = new Pair(1, \"a\"); Pair two = new Pair(\"b\", 2); }\n"
    )

    assertEquals(
      classSites(model, "demo.Pair").map(_.arguments),
      Vector(Vector("number:1", "text:a"), Vector("number:2", "text:b"))
    )
  }

  private val Span = "Span" -> """
    |package demo;
    |
    |import java.util.Optional;
    |
    |public class Span {
    |  private final int start;
    |  private final int end;
    |
    |  public Span(int start, int end) {
    |    if (end < start) throw new IllegalArgumentException();
    |    this.start = start;
    |    this.end = end;
    |  }
    |
    |  public Span(int end) {
    |    this(0, end);
    |  }
    |
    |  public Span(String text) {
    |    this(0, text.length());
    |  }
    |
    |  public Span(int end, boolean inclusive) {
    |    this(end, end + 1);
    |  }
    |
    |  public static Optional<Span> of(int start, int end) { return Optional.empty(); }
    |}
    |""".stripMargin

  test("a construction through a constructor that delegates maps its arguments through the delegation") {
    val model = guarded(
      Span,
      "Use" -> """
        |package demo;
        |
        |public class Use {
        |  Span direct = new Span(1, 2);
        |  Span shorter = new Span(7);
        |  Span measured = new Span("abc");
        |  Span passed = new Span(4, true);
        |}
        |""".stripMargin
    )

    assertEquals(
      classSites(model, "demo.Span").map(_.arguments),
      Vector(
        Vector("number:1", "number:2"),
        Vector("number:0", "number:7"),
        Vector("number:0", "unknown"),
        Vector("number:4", "unknown")
      )
    )
  }

  test("a construction with a diamond binds the declared constructor") {
    val model = guarded(
      "Holder" -> """
        |package demo;
        |
        |import java.util.Optional;
        |
        |public class Holder<T> {
        |  private final int size;
        |  private final T value;
        |
        |  public Holder(int size, T value) {
        |    if (size < 0) throw new IllegalArgumentException();
        |    this.size = size;
        |    this.value = value;
        |  }
        |
        |  public static <T> Optional<Holder<T>> of(int size, T value) { return Optional.empty(); }
        |}
        |""".stripMargin,
      "Use" -> "package demo;\npublic class Use { Holder<String> one = new Holder<>(3, \"x\"); }\n"
    )

    assertEquals(classSites(model, "demo.Holder").map(_.arguments), Vector(Vector("number:3", "text:x")))
  }

  test("local classes of the same name in different methods are different types") {
    val model = guarded(
      "Use" -> """
        |package demo;
        |
        |public class Use {
        |  int one() {
        |    class Local {
        |      private final int size;
        |      Local(int size) { if (size < 0) throw new IllegalArgumentException(); this.size = size; }
        |    }
        |    return new Local(1).hashCode();
        |  }
        |
        |  int two() {
        |    class Local {
        |      private final String name;
        |      Local(String name) { if (name.isEmpty()) throw new IllegalArgumentException(); this.name = name; }
        |    }
        |    return new Local("a").hashCode();
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(
      model.constructions.map(_.owner).filter(_.endsWith("Local")).sorted,
      Vector("demo.Use.one.Local", "demo.Use.two.Local")
    )
    assertEquals(
      model.fields.filter(_.owner.endsWith("Local")).map(field => field.owner -> field.parameter.name).sorted,
      Vector("demo.Use.one.Local" -> "size", "demo.Use.two.Local" -> "name")
    )
    assertEquals(classSites(model, "demo.Use.one.Local").map(_.arguments), Vector(Vector("number:1")))
    assertEquals(classSites(model, "demo.Use.two.Local").map(_.arguments), Vector(Vector("text:a")))
  }

  test("local classes of the same name in one method are different types") {
    val model = guarded(
      "Use" -> """
        |package demo;
        |
        |public class Use {
        |  int one(boolean first) {
        |    if (first) {
        |      class Local {
        |        private final int size;
        |        Local(int size) { if (size < 0) throw new IllegalArgumentException(); this.size = size; }
        |      }
        |      return new Local(1).hashCode();
        |    } else {
        |      class Local {
        |        private final String name;
        |        Local(String name) { if (name.isEmpty()) throw new IllegalArgumentException(); this.name = name; }
        |      }
        |      return new Local("a").hashCode();
        |    }
        |  }
        |}
        |""".stripMargin
    )

    val owners = model.constructions.map(_.owner).filter(_.contains("Local")).sorted
    assertEquals(owners.distinct.size, 2, owners.toString)
    val sized = model.fields.find(_.parameter.name == "size").get.owner
    val named = model.fields.find(_.parameter.name == "name").get.owner
    assertNotEquals(sized, named)
    assertEquals(classSites(model, sized).map(_.arguments), Vector(Vector("number:1")))
    assertEquals(classSites(model, named).map(_.arguments), Vector(Vector("text:a")))
  }

  test("a construction of a class that nothing guards or restricts is not a site") {
    val model = guarded(
      "Plain" -> """
        |package demo;
        |
        |public class Plain {
        |  private final int size;
        |
        |  public Plain(int size) { this.size = size; }
        |}
        |""".stripMargin,
      "Use" -> "package demo;\npublic class Use { Plain one = new Plain(4); }\n"
    )

    assertEquals(classSites(model, "demo.Plain"), Vector.empty)
  }

  test("a construction through a constructor with a variable number of arguments has arguments that are unknown") {
    val model = guarded(
      "Token" -> """
        |package demo;
        |
        |import java.util.Optional;
        |
        |public class Token {
        |  private final int size;
        |  private final int extra;
        |
        |  public Token(int size, int... extra) {
        |    this.size = size;
        |    this.extra = extra.length;
        |  }
        |
        |  public static Optional<Token> of(int size, int extra) { return Optional.empty(); }
        |}
        |""".stripMargin,
      "Use" -> "package demo;\npublic class Use { Token one = new Token(4, 5); }\n"
    )

    assertEquals(classSites(model, "demo.Token").map(_.arguments), Vector(Vector("unknown", "unknown")))
  }

  test("a construction of a class of a library or of an anonymous subclass is not a site") {
    val model = guarded(
      "Use" -> """
        |package demo;
        |
        |public class Use {
        |  Object one = new java.util.ArrayList<String>(4);
        |  Interval two = new Interval(1, "a", 0) {};
        |}
        |""".stripMargin,
      Interval
    )

    assertEquals(classSites(model, "demo.Interval"), Vector.empty)
    assertEquals(model.sites.filter(_.callee == "java.util.ArrayList"), Vector.empty)
  }

  private def writes(model: Model.Model): Set[(String, String)] =
    model.writes.map(write => write.owner -> write.name).toSet

  test("assignments, compound assignments and increments of a field are writes of it, through any qualifier") {
    val model = Fixtures.collect(
      "Box" -> """
        |package demo;
        |
        |public class Box {
        |  int plain = 0;
        |  int compound = 0;
        |  int after = 0;
        |  int before = 0;
        |  int qualified = 0;
        |  int outer = 0;
        |  int lambda = 0;
        |  int untouched = 0;
        |
        |  void run(Box other) {
        |    plain = 1;
        |    compound += 2;
        |    after++;
        |    --before;
        |    other.qualified = 3;
        |    ((Box) this).untouched = untouched;
        |    int local = 0;
        |    local = 1;
        |    int[] array = new int[1];
        |    array[0] = 2;
        |    Runnable r = () -> lambda++;
        |  }
        |
        |  class Inner {
        |    void run() { Box.this.outer = 1; }
        |  }
        |}
        |""".stripMargin,
      "Other" -> """
        |package demo;
        |
        |class Other {
        |  int plain = 0;
        |  void run(Box box) { box.plain = 4; }
        |}
        |""".stripMargin
    )

    assertEquals(
      writes(model),
      Set(
        "demo.Box" -> "plain",
        "demo.Box" -> "compound",
        "demo.Box" -> "after",
        "demo.Box" -> "before",
        "demo.Box" -> "qualified",
        "demo.Box" -> "outer",
        "demo.Box" -> "lambda",
        "demo.Box" -> "untouched"
      )
    )
  }

  test("a field that is not private, final or volatile and has an initialiser and no annotation can be reassigned") {
    val model = Fixtures.collect(
      "Fields" -> """
        |package demo;
        |
        |import java.util.List;
        |
        |public class Fields {
        |  int packaged = 1;
        |  protected int guarded = 1;
        |  public static int shared = 1;
        |  private int hidden = 1;
        |  final int sealed = 1;
        |  volatile int flagged = 1;
        |  int bare;
        |  @Deprecated int marked = 1;
        |  @SuppressWarnings("CanBeFinal") int quiet = 1;
        |  Object anonymous = new Object() { int inner = 1; };
        |  interface Constants { int LIMIT = 1; }
        |}
        |""".stripMargin
    )

    assertEquals(
      model.reassignables.map(field => (field.name, field.exported, field.suppressed)).sortBy(_._1),
      Vector(
        ("anonymous", false, Set.empty[String]),
        ("guarded", true, Set.empty[String]),
        ("packaged", false, Set.empty[String]),
        ("quiet", false, Set("can-be-final")),
        ("shared", true, Set.empty[String])
      )
    )
    assertEquals(model.reassignables.map(_.owner).toSet, Set("demo.Fields"))
  }

  for (name, members) <- Constructed.assigned ++ Constructed.written do
    test(s"a field without an initialiser that every constructor assigns once can be reassigned: $name") {
      val model = Fixtures.collect("Case" -> Constructed.source(members, ""))

      assertEquals(model.reassignables.map(field => field.name -> field.initialised), Vector("value" -> false))
    }

  for (name, members) <- Constructed.rejected ++ Constructed.repeated ++ Constructed.excluded :+ Constructed.annotated
  do
    test(
      s"a field without an initialiser that some constructor leaves open or assigns again is not reassignable: $name"
    ) {
      val model = Fixtures.collect("Case" -> Constructed.source(members, ""))

      assertEquals(model.reassignables, Vector.empty)
    }

  test("a plain assignment in a constructor of the type is a constructor write and any other write is not") {
    val model = Fixtures.collect(
      "Fields" -> """
        |package demo;
        |
        |public class Fields {
        |  int plain;
        |  int qualified;
        |  int method;
        |  int lambda;
        |  int compound;
        |  int stepped;
        |  int other;
        |  int local;
        |  int initialised = 1;
        |
        |  Fields(Fields peer) {
        |    plain = 1;
        |    this.qualified = 2;
        |    Runnable run = () -> lambda = 3;
        |    compound += 4;
        |    stepped++;
        |    peer.other = 5;
        |    new Object() { void set() { local = 6; } };
        |    initialised = 7;
        |  }
        |
        |  void set() { method = 1; }
        |}
        |""".stripMargin,
      "Sub" -> """
        |package demo;
        |
        |public class Sub extends Fields {
        |  Sub() { super(null); plain = 8; }
        |}
        |""".stripMargin
    )

    assertEquals(
      model.writes.map(write => (write.owner, write.name, write.constructor)).sorted,
      Vector(
        ("demo.Fields", "compound", false),
        ("demo.Fields", "initialised", true),
        ("demo.Fields", "lambda", false),
        ("demo.Fields", "local", false),
        ("demo.Fields", "method", false),
        ("demo.Fields", "other", false),
        ("demo.Fields", "plain", false),
        ("demo.Fields", "plain", true),
        ("demo.Fields", "qualified", true),
        ("demo.Fields", "stepped", false)
      )
    )
  }

  test("the fields of an anonymous class are not fields of the type or of the method that holds it") {
    val model = Fixtures.collect(
      "Outer" -> """
        |package demo;
        |
        |public class Outer {
        |  private final int size = 1;
        |
        |  public Runnable run() {
        |    return new Runnable() {
        |      private boolean started;
        |      public void run() { started = true; }
        |    };
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(model.fields.map(field => field.owner -> field.parameter.name), Vector("demo.Outer" -> "size"))
  }

  test("a file of two classes is collected once") {
    val model = Fixtures.collect("Two" -> """
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
    val model = Fixtures.collect("Service" -> fixture)
    val file  = Scratch.directory("bonsai-java").resolve("model.tsv")

    Model.write(model, file)

    val read = Model.read(file).fold(problem => fail(problem), identity)
    assertEquals(read.nodes.map(_.name).toSet, model.nodes.map(_.name).toSet)
    assertEquals(read.edges.size, model.edges.size)
  }

  private val Named =
    """
      |package demo;
      |
      |public class Names {
      |
      |  public static String last(String key) {
      |    return key.substring(key.lastIndexOf('.') + 1).trim();
      |  }
      |}
      |""".stripMargin

  private def helpers(model: Model.Model) = model.fragments.filter(f => (f.roles & Model.Roles.Helper) != 0)

  test("the collector takes the body of a method of a single expression as a helper, with its parameters") {
    val model = Fixtures.collect("Names" -> Named)

    val helper = helpers(model).headOption.getOrElse(fail(model.fragments.mkString("\n")))
    assertEquals((helper.node, helper.member), ("demo.Names", "last"))
    assertEquals(helper.reach, "", "a public static method of a public type is visible to all code")
    assert(
      helper.variables.exists {
        case Clones.Variable('R', _, "key", 0, "") => true
        case _                                     => false
      },
      helper.variables.mkString("|")
    )
  }

  test("a chain of calls elsewhere that the helper makes with other values has the helper's broad hash") {
    val model = Fixtures.collect(
      "Names" -> Named,
      "Uses"  -> """
        |package demo;
        |
        |public class Uses {
        |
        |  private String name = "a.b";
        |
        |  public String run(String other) {
        |    String local = other.substring(other.lastIndexOf('.') + 1).trim().toUpperCase();
        |    return this.name.substring(this.name.lastIndexOf('.') + 1).trim() + local;
        |  }
        |}
        |""".stripMargin
    )

    val helper = helpers(model).head
    val sites  = model.fragments.filter(f => f.node == "demo.Uses" && f.broad == helper.broad)
    assertEquals(sites.size, 2, model.fragments.map(f => (f.node, f.member, f.cost, f.broad)).mkString("\n"))
    assert(sites.forall(_.member == "run"))
    assert(sites.forall(site => (site.roles & Model.Roles.Entire) == 0), "a copy inside a method is no body")
  }

  test("a method that overrides another, or holds several statements, is no helper") {
    val model = Fixtures.collect(
      "Named" -> """
        |package demo;
        |
        |public class Named {
        |
        |  @Override
        |  public String toString() {
        |    return String.valueOf(hashCode()).trim().toUpperCase();
        |  }
        |
        |  public String twice(String key) {
        |    String result = key.substring(key.lastIndexOf('.') + 1).trim();
        |    return result.toUpperCase();
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(helpers(model), Vector.empty)
  }

  test("a method that is not public and static is visible to the code of its type only") {
    val model = Fixtures.collect(
      "Local" -> """
        |package demo;
        |
        |public class Local {
        |
        |  private String last(String key) {
        |    return key.substring(key.lastIndexOf('.') + 1).trim();
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(helpers(model).map(_.reach), Vector("demo.Local"))
  }
}
