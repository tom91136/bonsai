package net.kurobako.bonsai.analysis

import java.nio.file.Files
import net.kurobako.bonsai.testing.*

/** The facts of Java sources that the algebra and invariant engines read, from the sources through the javac plugin. */
class JavaFactsSuite extends AnalysisSuite {

  private val Annotations = Seq(
    "org/jspecify/annotations/Nullable.java" ->
      "package org.jspecify.annotations;\npublic @interface Nullable {}\n",
    "org/jspecify/annotations/NullMarked.java" ->
      "package org.jspecify.annotations;\npublic @interface NullMarked {}\n"
  )

  /** The result of analysing Java sources, each named by its path, as the analysis reads the model the plugin writes.
    */
  private def read(sources: (String, String)*): Extraction.Result = javaFacts(Set.empty, (Annotations ++ sources)*)

  private def product(result: Extraction.Result, name: String): Algebra.Product =
    result.algebra.products.find(_.name == name).getOrElse(fail(s"no product $name in ${result.algebra.products}"))

  private def findings(result: Extraction.Result): Vector[Finding] = Report.findings(result)

  private val String = Polynomial.atom("java.lang.String")

  test("a java.util.Optional is one plus the type it holds, like an option") {
    val result = read("demo/Pair.java" -> """
      |package demo;
      |
      |import java.util.Optional;
      |import java.util.OptionalInt;
      |import org.jspecify.annotations.Nullable;
      |
      |public record Pair(Optional<String> name, OptionalInt count, @Nullable String text, String plain) {}
      |""".stripMargin)

    val fields = product(result, "demo.Pair").fields
    assertEquals(
      fields.map(_.shape),
      Vector(Polynomial.One + String, Polynomial.One + Polynomial.atom("int"), Polynomial.One + String, String)
    )
    assertEquals(
      fields.map(_.kind),
      Vector(Algebra.Kind.Optional, Algebra.Kind.Optional, Algebra.Kind.Optional, Algebra.Kind.Text)
    )
  }

  test("a record of an Optional and a nullable reference has the shape of the equivalent options") {
    val result = read(
      "demo/First.java" -> """
        |package demo;
        |
        |import java.util.Optional;
        |
        |public record First(Optional<String> name, boolean flag) {}
        |""".stripMargin,
      "demo/Second.java" -> """
        |package demo;
        |
        |import org.jspecify.annotations.Nullable;
        |
        |public record Second(@Nullable String name, boolean flag) {}
        |""".stripMargin
    )

    assertEquals(product(result, "demo.First").shape, product(result, "demo.Second").shape)
  }

  private def service(calls: String*): Extraction.Result =
    read("demo/Service.java" -> s"""
      |package demo;
      |
      |import java.util.Optional;
      |import org.jspecify.annotations.Nullable;
      |
      |public class Service {
      |  public void run(String name, @Nullable String other) {
      |${calls.map(call => s"    $call;").mkString("\n")}
      |  }
      |
      |  private void take(Optional<String> value) {}
      |}
      |""".stripMargin)

  private def smells(result: Extraction.Result): Vector[Finding] =
    Smells.findings(result.smells, result.algebra.products, result.graph.nodes)

  test("an optional parameter every call leaves empty is constant") {
    val result = service("take(Optional.empty())", "take(Optional.empty())")

    assertFinding(smells(result), "constant-argument", "take.value is always Absent across 2 calls")
  }

  test("an optional parameter some calls fill and some leave empty is not constant") {
    val result = service("take(Optional.empty())", "take(Optional.of(name))")

    assertNoFinding(smells(result))(_.rule.id == "constant-argument")
  }

  test("an optional parameter that may be filled or empty is not constant") {
    val result = service("take(Optional.empty())", "take(Optional.ofNullable(name))")

    assertNoFinding(smells(result))(_.rule.id == "constant-argument")
  }

  private def invariants(result: Extraction.Result): Vector[Finding] =
    Invariants.findings(result.algebra, result.graph.nodes)

  private def record(name: String, components: String, body: String, imports: String = ""): (String, String) =
    s"demo/$name.java" -> s"""
      |package demo;
      |
      |import java.util.Objects;
      |import java.util.Optional;
      |import org.jspecify.annotations.NullMarked;
      |import org.jspecify.annotations.Nullable;
      |$imports
      |
      |public record $name($components) {
      |$body
      |}
      |""".stripMargin

  test("a guard of a compact constructor over optional fields describes a sum") {
    val result = read(record(
      "Source",
      "@Nullable String toolkit, @Nullable String image",
      "  public Source {\n    if (toolkit != null && image != null) {\n      throw new IllegalArgumentException();\n    }\n  }"
    ))

    assertFinding(
      invariants(result),
      "invariant-sum",
      "rejects 1 of 4 states of (toolkit, image)",
      "as a sum: none | image | toolkit"
    )
  }

  test("requireNonNull of a nullable component forces it to be present") {
    val result = read(record(
      "Ready",
      "@Nullable String value",
      "  public Ready {\n    Objects.requireNonNull(value, \"value\");\n  }"
    ))

    assertFinding(invariants(result), "invariant-constant", "forces value to Present")
  }

  test("requireNonNull of a component that cannot be null says nothing") {
    val result = read(record(
      "Named",
      "String name, @Nullable String other",
      "  public Named {\n    Objects.requireNonNull(name);\n  }"
    ))

    assertEquals(product(result, "demo.Named").invariant, Algebra.Predicate.Known(true))
    assertEquals(invariants(result), Vector.empty)
  }

  test("a flag that mirrors the presence of an Optional is redundant") {
    val result = read(record(
      "Load",
      "boolean done, Optional<Integer> value",
      "  public Load {\n    if (done != value.isPresent()) throw new IllegalArgumentException();\n  }"
    ))

    assertFinding(invariants(result), "invariant-redundant", "done is determined by (value)")
  }

  test("an enum case and a presence take part in the decomposition") {
    val result = read(
      "demo/Mode.java" -> "package demo;\npublic enum Mode { LOAD, STORE }\n",
      record(
        "Access",
        "Mode mode, @Nullable String value",
        "  public Access {\n    if (mode == Mode.LOAD && value != null) throw new IllegalArgumentException();\n  }"
      )
    )

    assertFinding(
      invariants(result),
      "invariant-sum",
      "rejects 1 of 4 states",
      "as a sum: mode=LOAD | mode=STORE | mode=STORE+value"
    )
  }

  private val Sized = record(
    "Size",
    "int count",
    "  public Size {\n    if (count < 0 || count > 64) {\n      throw new IllegalArgumentException();\n    }\n  }"
  )

  test("constructions with literal arguments that break the guard are violations") {
    val result = read(
      Sized,
      "demo/Main.java" -> """
        |package demo;
        |
        |public class Main {
        |  Size ok = new Size(2);
        |  Size negative = new Size(-1);
        |  Size large = new Size(65);
        |}
        |""".stripMargin
    )

    assertFinding(invariants(result), "invariant-violation", "Size: 2 constructions violate the invariant")
  }

  test("a construction with an argument that is not known is not a violation") {
    val result = read(
      Sized,
      "demo/Main.java" -> """
        |package demo;
        |
        |public class Main {
        |  Size run(int count) {
        |    return new Size(count);
        |  }
        |}
        |""".stripMargin
    )

    assertNoFinding(invariants(result))(_.rule.id == "invariant-violation")
  }

  test("an Optional made empty and present violates a guard that allows one of them") {
    val result = read(
      record(
        "Pair",
        "Optional<String> left, Optional<String> right",
        "  public Pair {\n    if (left.isPresent() && right.isPresent()) throw new IllegalArgumentException();\n  }"
      ),
      "demo/Main.java" -> """
        |package demo;
        |
        |import java.util.Optional;
        |
        |public class Main {
        |  Pair none = new Pair(Optional.empty(), Optional.empty());
        |  Pair one = new Pair(Optional.of("a"), Optional.empty());
        |  Pair both = new Pair(Optional.of("a"), Optional.of("b"));
        |}
        |""".stripMargin
    )

    assertFinding(invariants(result), "invariant-violation", "Pair: 1 constructions violate the invariant")
  }

  private val Exclusive = record(
    "Pair",
    "@Nullable String left, @Nullable String right",
    "  public Pair {\n    if (left != null && right != null) throw new IllegalArgumentException();\n  }"
  )

  test("a variable that may be null is not a value that is there") {
    val result = read(
      Exclusive,
      "demo/Main.java" -> """
        |package demo;
        |
        |import org.jspecify.annotations.Nullable;
        |
        |public class Main {
        |  Pair run(@Nullable String left, @Nullable String right) {
        |    return new Pair(left, right);
        |  }
        |}
        |""".stripMargin
    )

    assertNoFinding(invariants(result))(_.rule.id == "invariant-violation")
  }

  test("a variable that cannot be null is a value that is there") {
    val result = read(
      Exclusive,
      "demo/Main.java" -> """
        |package demo;
        |
        |import org.jspecify.annotations.NullMarked;
        |
        |@NullMarked
        |public class Main {
        |  Pair run(String left, String right) {
        |    return new Pair(left, right);
        |  }
        |}
        |""".stripMargin
    )

    assertFinding(invariants(result), "invariant-violation", "Pair: 1 constructions violate the invariant")
  }

  test("a guard after other statements is not read") {
    val result = read(record(
      "Source",
      "@Nullable String toolkit, @Nullable String image",
      """  public Source {
        |    System.out.println("built");
        |    if (toolkit != null && image != null) throw new IllegalArgumentException();
        |  }""".stripMargin
    ))

    assertEquals(product(result, "demo.Source").invariant, Algebra.Predicate.Known(true))
    assertEquals(invariants(result), Vector.empty)
  }

  test("a guard that calls a method says nothing about the states") {
    val result = read(
      record(
        "Size",
        "int count",
        "  public Size {\n    if (invalid(count)) throw new IllegalArgumentException();\n  }\n  private static boolean invalid(int count) { return count < 0; }"
      ),
      "demo/Main.java" -> "package demo;\npublic class Main { Size negative = new Size(-1); }\n"
    )

    assertEquals(invariants(result), Vector.empty)
  }

  test("a guard on a local is not read") {
    val result = read(record(
      "Size",
      "int count",
      "  public Size {\n    int copy = count;\n    if (copy < 0) throw new IllegalArgumentException();\n  }"
    ))

    assertEquals(product(result, "demo.Size").invariant, Algebra.Predicate.Known(true))
  }

  test("a condition that does not throw is not a guard") {
    val result = read(
      record(
        "Size",
        "int count",
        "  public Size {\n    if (count < 0) {\n      count = 0;\n    }\n  }"
      ),
      "demo/Main.java" -> "package demo;\npublic class Main { Size negative = new Size(-1); }\n"
    )

    assertEquals(product(result, "demo.Size").invariant, Algebra.Predicate.Known(true))
    assertEquals(invariants(result), Vector.empty)
  }

  test("a condition that throws only after other work is not a guard") {
    val result = read(
      record(
        "Size",
        "int count",
        "  public Size {\n    if (count < 0) {\n      System.out.println(count);\n      throw new IllegalArgumentException();\n    }\n  }"
      ),
      "demo/Main.java" -> "package demo;\npublic class Main { Size negative = new Size(-1); }\n"
    )

    assertEquals(invariants(result), Vector.empty)
  }

  test("a method of a nullable text is not a test of its presence") {
    val result = read(record(
      "Name",
      "@Nullable String text, boolean flag",
      "  public Name {\n    if (text.isEmpty()) throw new IllegalArgumentException();\n  }"
    ))

    assertEquals(invariants(result), Vector.empty)
  }

  test("the guard of the constructor of a class is read over the fields it stores") {
    val result = read("demo/Counter.java" -> """
      |package demo;
      |
      |public class Counter {
      |  private final int size;
      |  private final String name;
      |
      |  public Counter(int size, String name) {
      |    if (size < 0) throw new IllegalArgumentException();
      |    this.size = size;
      |    this.name = name;
      |  }
      |}
      |""".stripMargin)

    val counter = product(result, "demo.Counter")
    assertEquals(counter.invariant, Algebra.Predicate.Not(Algebra.Predicate.Compare(0, "<", BigDecimal(0))))
  }

  test("a class with constructors that differ in their guards has a guard that is not known") {
    val result = read("demo/Counter.java" -> """
      |package demo;
      |
      |public class Counter {
      |  private final int size;
      |
      |  public Counter(int size) {
      |    if (size < 0) throw new IllegalArgumentException();
      |    this.size = size;
      |  }
      |
      |  public Counter(String size) {
      |    this.size = size.length();
      |  }
      |}
      |""".stripMargin)

    assertEquals(product(result, "demo.Counter").invariant, Algebra.Predicate.Unknown)
  }

  test("a constructor that delegates to another one has the guard of the other") {
    val result = read("demo/Counter.java" -> """
      |package demo;
      |
      |public class Counter {
      |  private final int size;
      |
      |  public Counter(int size) {
      |    if (size < 0) throw new IllegalArgumentException();
      |    this.size = size;
      |  }
      |
      |  public Counter() {
      |    this(0);
      |  }
      |}
      |""".stripMargin)

    assertEquals(
      product(result, "demo.Counter").invariant,
      Algebra.Predicate.Not(Algebra.Predicate.Compare(0, "<", BigDecimal(0)))
    )
  }

  private val Range = record(
    "Range",
    "int low, int high",
    """  public Range {
      |    if (low > high) throw new IllegalArgumentException();
      |  }
      |
      |  public static Optional<Range> of(int low, int high) {
      |    return low > high ? Optional.empty() : Optional.of(new Range(low, high));
      |  }""".stripMargin
  )

  test("a type with a factory that returns an Optional of it is constructed through the factory") {
    val result = read(
      Range,
      "demo/Main.java" ->
        "package demo;\npublic class Main { Range one = new Range(1, 2); Range two = new Range(2, 3); }\n"
    )

    assertEquals(product(result, "demo.Range").access, Algebra.Access.Factory(Vector("Range.of")))
    assertFinding(invariants(result), "bypassed-factory", "Range: 2 of 3 constructions bypass Range.of")
  }

  test("a type whose factory has other parameters is not bypassed") {
    val result = read(
      record(
        "Range",
        "int low, int high",
        "  public static Optional<Range> of(String text) {\n    return Optional.empty();\n  }"
      ),
      "demo/Main.java" -> "package demo;\npublic class Main { Range one = new Range(1, 2); }\n"
    )

    assertEquals(product(result, "demo.Range").access, Algebra.Access.Open)
  }

  test("a type whose constructors are private is not open") {
    val result = read("demo/Token.java" -> """
      |package demo;
      |
      |public class Token {
      |  private final String value;
      |
      |  private Token(String value) {
      |    this.value = value;
      |  }
      |
      |  public static Token of(String value) {
      |    return new Token(value);
      |  }
      |}
      |""".stripMargin)

    assertEquals(product(result, "demo.Token").access, Algebra.Access.Private)
  }

  test("a type with one private constructor and one that is not is open") {
    val result = read("demo/Token.java" -> """
      |package demo;
      |
      |public class Token {
      |  private final String value;
      |
      |  private Token(String value) {
      |    this.value = value;
      |  }
      |
      |  public Token() {
      |    this("");
      |  }
      |}
      |""".stripMargin)

    assertEquals(product(result, "demo.Token").access, Algebra.Access.Open)
  }

  private val Guava = Seq(
    "com/google/common/base/Preconditions.java" -> """
      |package com.google.common.base;
      |
      |public final class Preconditions {
      |  public static void checkArgument(boolean expression) {}
      |  public static void checkArgument(boolean expression, String template, Object... arguments) {}
      |  public static void checkState(boolean expression) {}
      |  public static <T> T checkNotNull(T reference) { return reference; }
      |}
      |""".stripMargin,
    "demo/other/Preconditions.java" -> """
      |package demo.other;
      |
      |public final class Preconditions {
      |  public static void checkArgument(boolean expression) {}
      |  public static <T> T checkNotNull(T reference) { return reference; }
      |}
      |""".stripMargin
  )

  test("a Guava checkArgument of a compact constructor is a guard whose violations are found") {
    val result = read(
      (Guava :+ record(
        "Size",
        "int count",
        "  public Size {\n    Preconditions.checkArgument(count >= 0 && count <= 64, \"bad %s\", count);\n  }",
        "import com.google.common.base.Preconditions;"
      ) :+ "demo/Main.java" -> """
        |package demo;
        |
        |public class Main {
        |  Size ok = new Size(2);
        |  Size negative = new Size(-1);
        |}
        |""".stripMargin)*
    )

    assertFinding(invariants(result), "invariant-violation", "Size: 1 constructions violate the invariant")
  }

  test("a Guava checkState and a static import of checkArgument are guards") {
    val result = read(
      (Guava :+ record(
        "Size",
        "int count, boolean open",
        "  public Size {\n    checkArgument(count >= 0);\n    Preconditions.checkState(open);\n  }",
        "import static com.google.common.base.Preconditions.checkArgument;\nimport com.google.common.base.Preconditions;"
      ) :+ "demo/Main.java" -> """
        |package demo;
        |
        |public class Main {
        |  Size negative = new Size(-1, true);
        |  Size closed = new Size(1, false);
        |}
        |""".stripMargin)*
    )

    assertFinding(invariants(result), "invariant-violation", "Size: 2 constructions violate the invariant")
  }

  test("a Guava checkNotNull of a nullable component forces it to be present") {
    val result = read(
      (Guava :+ record(
        "Ready",
        "@Nullable String value",
        "  public Ready {\n    Preconditions.checkNotNull(value);\n  }",
        "import com.google.common.base.Preconditions;"
      ))*
    )

    assertFinding(invariants(result), "invariant-constant", "forces value to Present")
  }

  test("a check of another type with the name of a Guava check is not a guard") {
    val result = read(
      (Guava :+ record(
        "Size",
        "int count",
        "  public Size {\n    Preconditions.checkArgument(count >= 0);\n  }",
        "import demo.other.Preconditions;"
      ) :+ "demo/Main.java" -> "package demo;\npublic class Main { Size negative = new Size(-1); }\n")*
    )

    assertEquals(product(result, "demo.Size").invariant, Algebra.Predicate.Known(true))
    assertEquals(invariants(result), Vector.empty)
  }

  private val Service = "demo/Service.java" -> """
    |package demo;
    |
    |import com.google.common.base.Preconditions;
    |
    |public final class Service {
    |  public static int take(int count, boolean open) {
    |    Preconditions.checkArgument(count >= 0);
    |    return open ? count : 0;
    |  }
    |}
    |""".stripMargin

  test("calls with literal arguments that break the precondition of a method are violations") {
    val result = read(
      (Guava :+ Service :+ "demo/Main.java" -> """
        |package demo;
        |
        |public class Main {
        |  int ok = Service.take(2, true);
        |  int negative = Service.take(-1, true);
        |  int unknown(int count) { return Service.take(count, false); }
        |}
        |""".stripMargin)*
    )

    assertEquals(result.algebra.preconditions.values.map(_.name).toVector, Vector("demo.Service.take(count, open)"))
    assertFinding(
      invariants(result),
      "invariant-violation",
      "Service.take(count, open): 1 calls violate the precondition"
    )
  }

  test("calls of a method with a precondition that are not known to break it are not violations") {
    val result = read(
      (Guava :+ Service :+ "demo/Main.java" -> """
        |package demo;
        |
        |public class Main {
        |  int ok = Service.take(2, true);
        |  int unknown(int count) { return Service.take(count, false); }
        |}
        |""".stripMargin)*
    )

    assertEquals(invariants(result), Vector.empty)
  }

  test("a method that other code may override has no precondition for its callers to break") {
    val result = read(
      (Guava :+ "demo/Service.java"          -> """
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
        |""".stripMargin :+ "demo/Main.java" -> """
        |package demo;
        |
        |public class Main {
        |  int negative = new Service().take(-1);
        |}
        |""".stripMargin)*
    )

    assertEquals(result.algebra.preconditions, Map.empty[String, Algebra.Product])
    assertEquals(invariants(result), Vector.empty)
  }

  test("a precondition that rules out one state of two parameters describes a sum") {
    val result = read(
      (Guava :+ "demo/Service.java" -> """
        |package demo;
        |
        |import com.google.common.base.Preconditions;
        |
        |public final class Service {
        |  public static void run(boolean open, boolean closed) {
        |    Preconditions.checkArgument(!(open && closed));
        |  }
        |}
        |""".stripMargin)*
    )

    assertFinding(invariants(result), "invariant-sum", "Service.run(open, closed)", "rejects 1 of 4 states")
  }

  test("a method that only checks a flag is not a precondition for its callers to break") {
    val result = read(
      (Guava :+ "demo/Checks.java" -> """
        |package demo;
        |
        |import com.google.common.base.Preconditions;
        |
        |public final class Checks {
        |  static void check(boolean ok) {
        |    Preconditions.checkState(ok);
        |  }
        |
        |  static void fail() {
        |    check(false);
        |  }
        |}
        |""".stripMargin)*
    )

    assertEquals(result.algebra.preconditions, Map.empty[String, Algebra.Product])
    assertEquals(invariants(result), Vector.empty)
  }

  test("a local variable of a null-marked scope is not a value that is there") {
    val result = read(
      (Guava :+ "demo/Names.java" -> """
        |package demo;
        |
        |import com.google.common.base.Preconditions;
        |import java.util.Map;
        |import org.jspecify.annotations.NullMarked;
        |import org.jspecify.annotations.Nullable;
        |
        |@NullMarked
        |public final class Names {
        |  public static void absent(@Nullable String value, int count) {
        |    Preconditions.checkArgument(value == null || count > 0);
        |  }
        |
        |  static void run(Map<String, String> names) {
        |    String found = names.get("a");
        |    absent(found, 0);
        |  }
        |}
        |""".stripMargin)*
    )

    assertEquals(invariants(result).filter(_.rule.id == "invariant-violation"), Vector.empty)
  }

  private val Interval = "demo/Interval.java" -> """
    |package demo;
    |
    |public class Interval {
    |  private final int low;
    |  private final int high;
    |
    |  public Interval(int high, int low) {
    |    if (low < 0) throw new IllegalArgumentException();
    |    this.low = low;
    |    this.high = high;
    |  }
    |}
    |""".stripMargin

  test("a construction of a class with a literal argument that breaks its guard is a violation") {
    val result = read(
      Interval,
      "demo/Main.java" -> """
        |package demo;
        |
        |public class Main {
        |  Interval ok = new Interval(9, 1);
        |  Interval negative = new Interval(9, -3);
        |  Interval unknown(int low) { return new Interval(9, low); }
        |}
        |""".stripMargin
    )

    assertFinding(invariants(result), "invariant-violation", "Interval: 1 constructions violate the invariant")
  }

  test("a construction through a delegating constructor, or one of several of the same arity, is a violation") {
    val result = read(
      "demo/Span.java" -> """
        |package demo;
        |
        |public class Span {
        |  private final int start;
        |  private final int end;
        |
        |  public Span(int start, int end) {
        |    if (end < 1) throw new IllegalArgumentException();
        |    this.start = start;
        |    this.end = end;
        |  }
        |
        |  public Span(int end) {
        |    this(0, end);
        |  }
        |
        |  public Span(String start, String end) {
        |    this(start.length(), end.length());
        |  }
        |}
        |""".stripMargin,
      "demo/Main.java" -> """
        |package demo;
        |
        |public class Main {
        |  Span fine = new Span(0, 3);
        |  Span shorter = new Span(4);
        |  Span backwards = new Span(-2);
        |  Span measured = new Span("a", "b");
        |}
        |""".stripMargin
    )

    assertFinding(invariants(result), "invariant-violation", "Span: 1 constructions violate the invariant")
  }

  test("a construction of a class whose constructor changes its arguments is not a violation") {
    val result = read(
      "demo/Interval.java" -> """
        |package demo;
        |
        |public class Interval {
        |  private final int low;
        |
        |  public Interval(int low) {
        |    if (low < 0) throw new IllegalArgumentException();
        |    this.low = Math.abs(low);
        |  }
        |}
        |""".stripMargin,
      "demo/Main.java" -> "package demo;\npublic class Main { Interval negative = new Interval(-3); }\n"
    )

    assertEquals(invariants(result), Vector.empty)
  }

  test("a type of a class with a factory that returns an Optional of it is constructed through the factory") {
    val result = read(
      "demo/Range.java" -> """
        |package demo;
        |
        |import java.util.Optional;
        |
        |public class Range {
        |  private final int low;
        |  private final int high;
        |
        |  Range(int low, int high) {
        |    this.low = low;
        |    this.high = high;
        |  }
        |
        |  public static Optional<Range> of(int low, int high) {
        |    return low > high ? Optional.empty() : Optional.of(new Range(low, high));
        |  }
        |}
        |""".stripMargin,
      "demo/Main.java" ->
        "package demo;\npublic class Main { Range one = new Range(1, 2); Range two = new Range(2, 3); }\n"
    )

    assertEquals(product(result, "demo.Range").access, Algebra.Access.Factory(Vector("Range.of")))
    assertFinding(invariants(result), "bypassed-factory", "Range: 2 of 3 constructions bypass Range.of")
  }

  test("a construction of a class with no guard and no factory is not a site") {
    val result = read(
      "demo/Plain.java" -> """
        |package demo;
        |
        |public class Plain {
        |  private final int low;
        |
        |  public Plain(int low) { this.low = low; }
        |}
        |""".stripMargin,
      "demo/Main.java" -> "package demo;\npublic class Main { Plain one = new Plain(1); Plain two = new Plain(1); }\n"
    )

    assertEquals(result.algebra.sites.get("demo.Plain"), None)
    assertEquals(findings(result).filter(_.rule.id == "constant-field"), Vector.empty)
  }

  private def modelled(models: Vector[Vector[String]]): Extraction.Result =
    Scratch.using("bonsai-") { directory =>
      val files = models.indices.map(index => directory.resolve(s"model-$index.tsv"))
      models.zip(files).foreach((lines, file) => Files.writeString(file, lines.mkString("", "\n", "\n")))
      val outputs = files.zipWithIndex.map((file, index) => Output(s"m$index", Output.Scope.Main, file)).toVector
      Extraction.read(Manifest(Vector.empty, classpaths, Set.empty, models = outputs, jdk = targetJdk))
        .fold(errors => fail(errors.mkString("\n")), identity)
    }

  private val Shape = Vector(
    "node\tdemo.Shape\tdemo/Shape.java\t3\ttrue",
    "sum\tdemo.Shape\tdemo.Shape\tdemo.Circle;demo.Square"
  )

  private val Variants = Vector(
    "node\tdemo.Circle\tdemo/Circle.java\t3\ttrue",
    "node\tdemo.Square\tdemo/Square.java\t3\ttrue",
    "product\tdemo.Circle\tdemo.Circle\tradius|number|double||true\t\ttrue",
    "product\tdemo.Square\tdemo.Square\tside|number|double||true\t\ttrue"
  )

  test("a sum takes the fields of its variants from the model of another module") {
    for models <- Vector(Vector(Shape, Variants), Vector(Variants, Shape)) do {
      val sum = modelled(models).algebra.sums.find(_.name == "demo.Shape").getOrElse(fail("no sum"))

      assertEquals(
        sum.variants.map(variant => variant.name -> variant.fields.map(_.name)),
        Vector(
          "demo.Circle" -> Vector("radius"),
          "demo.Square" -> Vector("side")
        )
      )
    }
  }

  test("a variant that no model holds has no fields") {
    val sum = modelled(Vector(Shape)).algebra.sums.find(_.name == "demo.Shape").getOrElse(fail("no sum"))

    assertEquals(sum.variants.map(_.fields), Vector(Vector.empty, Vector.empty))
  }

  private def finals(result: Extraction.Result): Vector[Finding] =
    findings(result).filter(_.rule.id == "can-be-final")

  private def fields(result: Extraction.Result): Vector[String] = finals(result).map(_.detail).sorted

  test("a field no code of the build writes can be final whatever its access") {
    val result = read(
      "demo/Shared.java" -> """
        |package demo;
        |
        |public class Shared {
        |  int packaged = 0;
        |  protected int guarded = 0;
        |  public int open = 0;
        |  static int limit = 5;
        |  private int hidden = 0;
        |}
        |""".stripMargin
    )

    assertEquals(
      fields(result),
      Vector(
        "the field guarded can be final",
        "the field limit can be final",
        "the field open can be final",
        "the field packaged can be final"
      )
    )
  }

  test("a field some class of the build writes, through any qualifier, cannot be final") {
    val result = read(
      "demo/Shared.java" -> """
        |package demo;
        |
        |public class Shared {
        |  int assigned = 0;
        |  int compound = 0;
        |  int incremented = 0;
        |  int constructed = 0;
        |  int inLambda = 0;
        |  int kept = 0;
        |
        |  Shared() { constructed = 1; }
        |  void run(Runnable[] actions) { actions[0] = () -> inLambda++; }
        |}
        |""".stripMargin,
      "demo/Writer.java" -> """
        |package demo;
        |
        |class Writer {
        |  void run(Shared shared) {
        |    shared.assigned = 1;
        |    shared.compound += 2;
        |    shared.incremented++;
        |  }
        |}
        |""".stripMargin
    )

    assertEquals(fields(result), Vector("the field kept can be final"))
  }

  test("a write to a field of another class of the same name does not settle the field") {
    val result = read(
      "demo/Left.java"  -> "package demo;\npublic class Left { int count = 0; }\n",
      "demo/Right.java" -> "package demo;\npublic class Right { int count = 0; void bump() { count++; } }\n"
    )

    assertEquals(fields(result), Vector("the field count can be final"))
    assertEquals(finals(result).map(_.node.name), Vector("demo.Left"))
  }

  test("a field without a value, a volatile or final field, an annotated field and a suppressed one are not reported") {
    val result = read(
      "demo/Fields.java" -> """
        |package demo;
        |
        |public class Fields {
        |  int bare;
        |  final int sealed = 1;
        |  volatile int flagged = 1;
        |  @Deprecated int marked = 1;
        |  @SuppressWarnings("CanBeFinal") int quiet = 1;
        |  Object anonymous = new Object() { int inner = 1; };
        |}
        |""".stripMargin
    )

    assertEquals(fields(result), Vector("the field anonymous can be final"))
  }

  test("the public API of a library is written by the builds that depend on it") {
    val result = javaFacts(
      Set("j"),
      "demo/Api.java" -> """
        |package demo;
        |
        |public class Api {
        |  public int open = 0;
        |  protected int guarded = 0;
        |  int packaged = 0;
        |  public static class Nested { public int deep = 0; }
        |}
        |""".stripMargin,
      "demo/Internal.java" -> """
        |package demo;
        |
        |class Internal {
        |  public int open = 0;
        |  protected int guarded = 0;
        |}
        |""".stripMargin
    )

    assertEquals(
      finals(result).map(finding => finding.node.name -> finding.detail).sorted,
      Vector(
        "demo.Api"      -> "the field packaged can be final",
        "demo.Internal" -> "the field guarded can be final",
        "demo.Internal" -> "the field open can be final"
      )
    )
  }

  test("a field only the constructors of its class assign can be final whatever its access") {
    val result = read(
      "demo/Shared.java" -> """
        |package demo;
        |
        |public class Shared {
        |  int packaged;
        |  protected int guarded;
        |  public int open;
        |  int delegated;
        |
        |  Shared(int value) { packaged = value; this.guarded = value; open = value; delegated = value; }
        |  Shared() { this(0); }
        |}
        |""".stripMargin,
      "demo/Reader.java" -> "package demo;\nclass Reader { int read(Shared shared) { return shared.open; } }\n"
    )

    assertEquals(
      fields(result),
      Vector(
        "the field delegated can be final",
        "the field guarded can be final",
        "the field open can be final",
        "the field packaged can be final"
      )
    )
  }

  test("a field some code outside the constructors of its class writes cannot be final") {
    val result = read(
      "demo/Shared.java" ->
        """
        |package demo;
        |
        |public class Shared {
        |  int outside;
        |  int inherited;
        |  int block;
        |  int method;
        |  int peer;
        |  int kept;
        |
        |  { block = 1; }
        |  Shared(Shared other) { outside = 1; inherited = 2; block = 3; method = 4; kept = 5; other.peer = 6; peer = 7; }
        |  void set() { method = 8; }
        |}
        |""".stripMargin,
      "demo/Sub.java"    -> "package demo;\nclass Sub extends Shared { Sub() { super(null); this.inherited = 9; } }\n",
      "demo/Writer.java" -> "package demo;\nclass Writer { void run(Shared shared) { shared.outside = 2; } }\n"
    )

    assertEquals(fields(result), Vector("the field kept can be final"))
  }

  test("a constructor write of a field that starts with a value still settles it") {
    val result = read(
      "demo/Shared.java" -> """
        |package demo;
        |
        |public class Shared {
        |  int started = 0;
        |  int bare;
        |
        |  Shared(int value) { started = value; bare = value; }
        |  Shared() { }
        |}
        |""".stripMargin
    )

    assertEquals(fields(result), Vector.empty)
  }

  test("a field the constructors of a library assign is part of its API") {
    val result = javaFacts(
      Set("j"),
      "demo/Api.java" -> """
        |package demo;
        |
        |public class Api {
        |  public int open;
        |  int packaged;
        |  Api(int value) { open = value; packaged = value; }
        |}
        |""".stripMargin
    )

    assertEquals(fields(result), Vector("the field packaged can be final"))
  }
}
