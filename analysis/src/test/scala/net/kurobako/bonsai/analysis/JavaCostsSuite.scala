package net.kurobako.bonsai.analysis

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import java.io.File
import java.lang.invoke.MethodType
import java.nio.file.Files
import net.kurobako.bonsai.java.Model as JavaModel
import net.kurobako.bonsai.testing.*

/** Loops, library work and calls of Java methods, bound against the methods of the build in either language. */
class JavaCostsSuite extends AnalysisSuite {

  private def build(
      sources: Seq[(String, String)],
      others: Seq[(String, String)] = Nil,
      bounded: Vector[Complexity.Bounded] = Vector.empty
  ): Extraction.Result =
    Scratch.using("bonsai-") { directory =>
      write(directory.resolve("java"), sources)
      write(directory.resolve("scala"), others)
      val javaFiles  = sources.map((name, _) => directory.resolve("java").resolve(name))
      val scalaFiles = others.map((name, _) => directory.resolve("scala").resolve(name))
      val scalaOut   = Files.createDirectories(directory.resolve("scala-classes"))
      val javaOut    = Files.createDirectories(directory.resolve("java-classes"))
      if (others.nonEmpty) {
        val reporter = Driver().process(
          Array("-classpath", classpath.mkString(File.pathSeparator), "-d", scalaOut.toString, "-color:never") ++
            (scalaFiles ++ javaFiles).map(_.toString),
          StoreReporter()
        )
        assert(!reporter.hasErrors, reporter.allErrors.map(_.message).mkString("\n"))
      }
      val model = directory.resolve("java-model.tsv")
      JavaModel.write(javac(javaFiles, javaOut, scalaOut +: classpath), model)
      val outputs  = if (others.nonEmpty) Vector(Output("m", Output.Scope.Main, scalaOut)) else Vector.empty
      val manifest = Manifest(
        outputs,
        scoped(Map("m" -> (javaOut +: classpath))),
        Set.empty,
        sources = scalaFiles.map(Output("m", Output.Scope.Main, _)).toVector,
        models = Vector(Output("j", Output.Scope.Main, model)),
        jdk = targetJdk
      )
      val result = Extraction.read(manifest, bounded = bounded).fold(errors => fail(errors.mkString("\n")), identity)
      assert(result.problems.isEmpty, result.problems.mkString("\n"))
      result
    }

  private def found(result: Extraction.Result) =
    Complexity.findings(result.complexities, result.graph.nodes, result.hot)

  private given Finding.Level = Finding.Level.Warning

  private def space(finding: Finding): Boolean = finding.rule.id == "space-complexity"

  private def scan(body: String) = "demo/Scan.java" -> s"""
    |package demo;
    |
    |import java.util.*;
    |
    |public class Scan {
    |  List<String> items = new ArrayList<>();
    |$body
    |}
    |""".stripMargin

  private lazy val loops = build(Seq(scan("""
    |  int pairs(List<String> left, List<String> right) {
    |    int n = 0;
    |    for (String a : left) { for (String b : right) { n++; } }
    |    return n;
    |  }
    |  int linear(List<String> left) { int n = 0; for (String a : left) { n++; } return n; }
    |  int indexed(String[] values) { int n = 0; for (int i = 0; i < values.length; i++) { n += helper(values[i]); } return n; }
    |  int helper(String value) { return value.length(); }
    |  int through(List<String> rows, List<String> keys) { int n = 0; for (String r : rows) { n += linear(keys); } return n; }
    |  int member(List<String> rows) { int n = 0; for (String r : rows) { n += linear(items); } return n; }
    |  int streamed(List<String> rows, List<String> keys) { return rows.stream().mapToInt(r -> linear(keys)).sum(); }
    |  int unknown(List<String> rows) { int n = 0; for (String r : rows) { for (String k : source()) { n++; } } return n; }
    |  List<String> source() { return items; }
    |  @SuppressWarnings("complexity")
    |  int quiet(List<String> left, List<String> right) { int n = 0; for (String a : left) { for (String b : right) { n++; } } return n; }
    |""".stripMargin)))

  test("nested loops over parameters are super-linear in the sizes of those parameters") {
    assertFinding(found(loops), "complexity", "Scan.pairs is O(|left| * |right|)")
    assertNoFinding(found(loops))(finding => finding.detail.startsWith("Scan.linear"))
    assertNoFinding(found(loops))(finding => finding.detail.startsWith("Scan.indexed"))
  }

  test("a call inside a loop costs its callee's bound with the callee's parameters bound to the arguments") {
    assertFinding(found(loops), "complexity", "Scan.through is O(|keys| * |rows|)")
    assertFinding(found(loops), "complexity", "Scan.member is O(|rows| * |this.items|)")
    assertFinding(found(loops), "complexity", "Scan.streamed is O(|keys| * |rows|)")
  }

  test("loops of unknown length are not reported") {
    assertNoFinding(found(loops))(_.detail.startsWith("Scan.unknown"))
  }

  test("a Java method suppresses a finding by its annotation") {
    val quiet = found(loops).filter(_.detail.startsWith("Scan.quiet"))
    assert(quiet.nonEmpty && quiet.forall(_.suppressed.nonEmpty), quiet.toString)
  }

  private lazy val library = build(Seq(scan("""
    |  int searches(List<String> rows, List<String> keys) { int n = 0; for (String r : rows) { if (keys.contains(r)) n++; } return n; }
    |  int hashed(List<String> rows, Set<String> keys) { int n = 0; for (String r : rows) { if (keys.contains(r)) n++; } return n; }
    |  int sorts(List<String> rows, List<String> keys) { for (String r : rows) { Collections.sort(keys); } return 0; }
    |  List<List<String>> sorted(List<String> rows, List<String> keys) { List<List<String>> out = new ArrayList<>(); for (String r : rows) { out.add(keys.stream().sorted().collect(java.util.stream.Collectors.toList())); } return out; }
    |  int copies(List<String> rows, List<String> keys) { for (String r : rows) { List<String> c = new ArrayList<>(keys); } return 0; }
    |  void fronts(List<String> rows, List<String> out) { for (String r : rows) { out.add(0, r); } }
    |  void linked(List<String> rows, LinkedList<String> out) { for (String r : rows) { out.add(0, r); } }
    |  String joined(List<String> rows) { String text = ""; for (String r : rows) { text += r; } return text; }
    |  String built(List<String> rows) { StringBuilder text = new StringBuilder(); for (String r : rows) { text.append(r); } return text.toString(); }
    |""".stripMargin)))

  test("searches, sorts, copies, shifts and growing texts inside loops are super-linear") {
    assertFinding(found(library), "complexity", "Scan.searches is O(|keys| * |rows|)")
    assertFinding(found(library), "complexity", "Scan.sorts is O(|keys| * |rows| * log|keys|)")
    assertFinding(found(library), "complexity", "Scan.copies is O(|keys| * |rows|)")
    assertFinding(found(library), "complexity", "Scan.fronts is O(|out| * |rows|)")
    assertFinding(found(library), "complexity", "Scan.joined is O(|rows|^2)")
  }

  test("hashed lookups, linked lists and builders are linear") {
    assertNoFinding(found(library))(finding =>
      Vector("hashed", "linked", "built").exists(name => finding.detail.startsWith(s"Scan.$name"))
    )
  }

  test("a sort allocates a copy that is kept, and a discarded sort and a search do not") {
    assertFinding(found(library), "space-complexity", "Scan.sorted allocates O(|keys| * |rows|)")
    assertNoFinding(found(library))(finding =>
      space(finding) && Vector("sorts", "searches").exists(name => finding.detail.startsWith(s"Scan.$name"))
    )
  }

  private lazy val precise = build(Seq(scan("""
    |  private static final int[] BITS = {1, 2, 4, 8};
    |  private static final List<String> NAMES = List.of("a", "b");
    |  int tabled(String[] rows) { int n = 0; for (String r : rows) { for (int bit : BITS) { n += bit; } } return n; }
    |  int listed(String[] rows) { int n = 0; for (String r : rows) { for (String s : NAMES) { n++; } } return n; }
    |  int merge(String[] left, String[] right) { int c = 0; for (String l : left) { while (c < right.length) { c++; } } return c; }
    |  int nested(String[] left, String[] right) { int n = 0; for (int i = 0; i < left.length; i++) { for (int j = 0; j < right.length; j++) { n++; } } return n; }
    |  boolean leaves(String[] rows, String[] keys) { for (String r : rows) { if (r.isEmpty()) { return scanAll(keys) > 0; } } return false; }
    |  static int scanAll(String[] keys) { int n = 0; for (String k : keys) { n += k.length(); } return n; }
    |""".stripMargin)))

  test("tables the class fixes, counters that outlive loops and work that leaves the method are not super-linear") {
    assertNoFinding(found(precise))(finding =>
      Vector("tabled", "listed", "merge", "leaves").exists(name => finding.detail.startsWith(s"Scan.$name"))
    )
    assertFinding(found(precise), "complexity", "Scan.nested is O(|left| * |right|)")
  }

  private val util = "demo/Util.scala" -> """
    |package demo
    |
    |object Util {
    |  def scan(keys: Array[String]): Int = keys.map(_.length).sum
    |  def pairs(left: Array[String], right: Array[String]): Int = left.map(l => right.count(_ == l)).sum
    |}
    |""".stripMargin

  private lazy val crossing = build(
    Seq(scan("""
      |  int fromScala(String[] rows, String[] keys) { int n = 0; for (String r : rows) { n += Util.scan(keys); } return n; }
      |  int onceFromScala(String[] rows, String[] keys) { return Util.scan(keys) + Util.scan(rows); }
      |  int quadratic(String[] rows, String[] keys) { return Util.pairs(rows, keys); }
      |  static int scanAll(String[] keys) { int n = 0; for (String k : keys) { n += k.length(); } return n; }
      |  static int scanAll(String key) { return key.length(); }
      |  int overloaded(String[] rows, String[] keys) { int n = 0; for (String r : rows) { n += scanAll(r); } return n; }
      |  int overloadedLoop(String[] rows, String[] keys) { int n = 0; for (String r : rows) { n += scanAll(keys); } return n; }
      |""".stripMargin)),
    Seq(
      util,
      "demo/Caller.scala" -> """
        |package demo
        |
        |object Caller {
        |  def run(rows: Array[String], keys: Array[String]): Int = rows.map(row => Scan.scanAll(keys)).sum
        |  def single(rows: Array[String]): Int = Scan.scanAll(rows)
        |  def overloaded(rows: Array[String]): Int = rows.map(row => Scan.scanAll(row)).sum
        |}
        |""".stripMargin
    )
  )

  test("a Java loop that calls a Scala method sees the loops of the Scala method") {
    assertFinding(found(crossing), "complexity", "Scan.fromScala is O(|keys| * |rows|)")
    assertFinding(found(crossing), "complexity", "Util.pairs is O(|left| * |right|)")
    assertNoFinding(found(crossing))(finding =>
      Vector("quadratic", "onceFromScala").exists(name => finding.detail.startsWith(s"Scan.$name"))
    )
  }

  test("a Scala loop that calls a Java method sees the loops of the Java method") {
    assertFinding(found(crossing), "complexity", "Caller.run is O(|keys| * |rows|)")
    assertNoFinding(found(crossing))(finding =>
      Vector("single", "overloaded").exists(name => finding.detail.startsWith(s"Caller.$name"))
    )
  }

  test("overloads with different descriptors have bounds of their own") {
    assertFinding(found(crossing), "complexity", "Scan.overloadedLoop is O(|keys| * |rows|)")
    assertNoFinding(found(crossing))(_.detail.startsWith("Scan.overloaded "))
  }

  private lazy val cycle = build(
    Seq(scan("""
      |  static int ping(String[] rows) { return Echo.pong(rows) + 1; }
      |""".stripMargin)),
    Seq("demo/Echo.scala" -> """
      |package demo
      |
      |object Echo {
      |  def pong(rows: Array[String]): Int = Scan.ping(rows) + rows.length
      |}
      |""".stripMargin)
  )

  test("methods that call each other across the two languages are bounded rather than followed forever") {
    assertNoFinding(found(cycle))(_ => true)
  }

  private val shell = "demo/Shell.java" ->
    """
    |package demo;
    |
    |import java.util.*;
    |
    |public class Shell {
    |  public class Inner {
    |    public final int n;
    |    public Inner(String[] values) { int c = 0; for (String a : values) { for (String b : values) { c++; } } n = c; }
    |  }
    |  public enum Mode {
    |    ONE(new String[0]);
    |    final int n;
    |    Mode(String[] values) { int c = 0; for (String a : values) { for (String b : values) { c++; } } n = c; }
    |  }
    |  public class Plain { }
    |  public static class Nested {
    |    public final int n;
    |    public Nested(String[] values) { int c = 0; for (String a : values) { for (String b : values) { c++; } } n = c; }
    |  }
    |  int viaInner(List<String> rows, String[] keys) { int n = 0; for (String r : rows) { n += new Inner(keys).n; } return n; }
    |}
    |""".stripMargin

  private lazy val inner = build(
    Seq(shell),
    Seq(
      "demo/Maker.scala" -> """
        |package demo
        |
        |object Maker {
        |  def make(shell: Shell, rows: Array[String], keys: Array[String]): Int = {
        |    var n = 0
        |    var i = 0
        |    while (i < rows.length) { n += new shell.Inner(keys).n; i += 1 }
        |    n
        |  }
        |  def nested(rows: Array[String], keys: Array[String]): Int = {
        |    var n = 0
        |    var i = 0
        |    while (i < rows.length) { n += new Shell.Nested(keys).n; i += 1 }
        |    n
        |  }
        |  def plain(shell: Shell, rows: Array[String]): Int = {
        |    var n = 0
        |    var i = 0
        |    while (i < rows.length) { n += new shell.Plain().hashCode; i += 1 }
        |    n
        |  }
        |}
        |""".stripMargin
    )
  )

  test("constructors of inner classes and enums have the descriptor the class file gives them") {
    Scratch.using("bonsai-") { directory =>
      write(directory, Seq(shell))
      val files   = Seq(directory.resolve(shell._1))
      val classes = Files.createDirectories(directory.resolve("classes"))
      val keys    = javac(files, classes, classpath).costs.map(_.key).filter(_.contains(".<init>(")).toSet
      val loader  = java.net.URLClassLoader(Array(classes.toUri.toURL))
      val actual  =
        Seq("demo.Shell", "demo.Shell$Inner", "demo.Shell$Mode", "demo.Shell$Plain", "demo.Shell$Nested").flatMap {
          name =>
            val cls = loader.loadClass(name)
            cls.getDeclaredConstructors.map { constructor =>
              val descriptor =
                MethodType.methodType(classOf[Unit], constructor.getParameterTypes).toMethodDescriptorString
              s"${name.replace('$', '.')}.<init>$descriptor"
            }
        }.toSet
      assertEquals(keys, actual)
    }
  }

  test("a Scala caller of a constructor of a Java inner class sees the loops of the constructor") {
    assertFinding(found(inner), "complexity", "Maker.make is O(|keys| * |rows|)")
    assertFinding(found(inner), "complexity", "Maker.nested is O(|keys| * |rows|)")
    assertNoFinding(found(inner))(_.detail.startsWith("Maker.plain"))
  }

  test("a Java caller of a constructor of a Java inner class sees the loops of the constructor") {
    assertFinding(found(inner), "complexity", "Shell.viaInner is O(|keys| * |rows|)")
  }

  private lazy val scalaConstructors = build(
    Seq(scan("""
      |  int perKey(String[] rows, String[] keys) { int n = 0; for (String k : keys) { n += new Copy(rows).size(); } return n; }
      |  int once(String[] rows) { return new Copy(rows).size(); }
      |  int plain(String[] rows, String[] keys) { int n = 0; for (String k : keys) { n += new Plain(rows).rows().length; } return n; }
      |  int secondary(String[] rows, String[] keys) { int n = 0; for (String k : keys) { n += new Copy(rows, k).size(); } return n; }
      |""".stripMargin)),
    Seq("demo/Copy.scala" -> """
      |package demo
      |
      |class Copy(items: Array[String]) {
      |  val all = items.toList
      |  def this(items: Array[String], key: String) = this(items)
      |  def size: Int = all.size
      |}
      |
      |class Plain(val rows: Array[String])
      |""".stripMargin)
  )

  test("a Java caller of a Scala constructor sees the work of the class body") {
    assertFinding(found(scalaConstructors), "complexity", "Scan.perKey is O(|keys| * |rows|)")
    assertFinding(found(scalaConstructors), "complexity", "Scan.secondary is O(|keys| * |rows|)")
    assertNoFinding(found(scalaConstructors))(finding =>
      Vector("once", "plain").exists(name => finding.detail.startsWith(s"Scan.$name"))
    )
  }

  private lazy val references = build(Seq(scan("""
    |  void visit(String row) { for (String item : items) { } }
    |  int one(String row) { return row.length(); }
    |  static int each(String row) { return 1; }
    |  void viaForEach(List<String> rows) { rows.forEach(this::visit); }
    |  long viaStream(List<String> rows) { return rows.stream().mapToInt(this::one).sum(); }
    |  long viaStatic(List<String> rows) { return rows.stream().mapToInt(Scan::each).sum(); }
    |  long viaLibrary(List<String> rows) { return rows.stream().mapToInt(String::length).sum(); }
    |  Runnable stored() { java.util.function.Consumer<String> c = this::visit; return null; }
    |""".stripMargin)))

  test("a method reference run per element costs its callee's bound per element") {
    assertFinding(found(references), "complexity", "Scan.viaForEach is O(|rows| * |this.items|)")
    assertNoFinding(found(references))(finding =>
      Vector("viaStream", "viaStatic", "viaLibrary", "stored").exists(name => finding.detail.startsWith(s"Scan.$name"))
    )
  }

  private lazy val descents = build(Seq(scan("""
    |  int count(List<String> rows) { int n = 0; for (String r : rows) { n++; } return n; }
    |  int linearDescent(List<String> rows) { return rows.isEmpty() ? 0 : linearDescent(rows.subList(1, rows.size())) + 1; }
    |  int quadraticDescent(List<String> rows) {
    |    return rows.isEmpty() ? 0 : quadraticDescent(rows.subList(1, rows.size())) + count(rows);
    |  }
    |  int textDescent(String text) { return text.isEmpty() ? 0 : textDescent(text.substring(1)) + count(items); }
    |  int arrayDescent(String[] values) { return values.length == 0 ? 0 : arrayDescent(Arrays.copyOfRange(values, 1, values.length)); }
    |  int aliasDescent(List<String> rows) { List<String> rest = rows.subList(1, rows.size()); return rows.isEmpty() ? 0 : aliasDescent(rest); }
    |  int countDown(String[] keys, int n) { return n <= 0 ? 0 : countDown(keys, n - 1) + count(items); }
    |  int stuck(List<String> rows) { return rows.isEmpty() ? 0 : stuck(rows); }
    |  int swapped(List<String> rows, List<String> keys) { return rows.isEmpty() ? 0 : swapped(keys.subList(1, keys.size()), rows); }
    |  int twice(List<String> rows) { return rows.isEmpty() ? 0 : twice(rows.subList(1, rows.size())) + twice(rows.subList(1, rows.size())); }
    |  int looped(List<String> rows, List<String> keys) {
    |    int n = 0;
    |    for (String k : keys) { n += looped(rows.subList(1, rows.size()), keys); }
    |    return n;
    |  }
    |  int viaLoop(List<String> rows, List<String> keys) { int n = 0; for (String k : keys) { n += linearDescent(rows); } return n; }
    |  int mutualFirst(List<String> rows) { return rows.isEmpty() ? 0 : mutualSecond(rows.subList(1, rows.size())); }
    |  int mutualSecond(List<String> rows) { return rows.isEmpty() ? 0 : mutualFirst(rows.subList(1, rows.size())); }
    |""".stripMargin)))

  test("a Java method that calls itself on a smaller part of a collection or text parameter is bounded by its size") {
    assertFinding(found(descents), "complexity", "Scan.quadraticDescent is O(|rows|^2)")
    assertFinding(found(descents), "complexity", "Scan.viaLoop is O(|keys| * |rows|)")
    assertFinding(found(descents), "complexity", "Scan.arrayDescent is O(|values|^2)")
    assertNoFinding(found(descents))(finding =>
      Vector("linearDescent", "aliasDescent").exists(name => finding.detail.startsWith(s"Scan.$name"))
    )
  }

  test("a Java method that counts an integer parameter down is bounded by that parameter") {
    assertFinding(found(descents), "complexity", "Scan.countDown is O(|n| * |this.items|)")
  }

  test("recursion that does not shrink, branches, repeats or sits in a loop, and mutual recursion stay unbounded") {
    assertNoFinding(found(descents))(finding =>
      Vector("stuck", "swapped", "twice", "looped", "mutualFirst", "mutualSecond").exists(name =>
        finding.detail.startsWith(s"Scan.$name")
      )
    )
  }

  private lazy val branches = build(Seq(scan("""
    |  int count(List<String> rows) { int n = 0; for (String r : rows) { n++; } return n; }
    |  int ifElse(List<String> rows, List<String> keys) {
    |    if (rows.isEmpty()) { return count(keys) + count(keys); } else { return ifElse(rows.subList(1, rows.size()), keys) + 1; }
    |  }
    |  int ternary(List<String> rows, List<String> keys) {
    |    return rows.isEmpty() ? count(keys) : ternary(rows.subList(1, rows.size()), keys) + 1;
    |  }
    |  int early(List<String> rows, List<String> keys) {
    |    if (rows.isEmpty()) { return count(keys); }
    |    return early(rows.subList(1, rows.size()), keys) + 1;
    |  }
    |  int late(List<String> rows, List<String> keys) {
    |    if (!rows.isEmpty()) { return late(rows.subList(1, rows.size()), keys) + 1; }
    |    return count(keys);
    |  }
    |  int frame(List<String> rows, List<String> keys) {
    |    return rows.isEmpty() ? 0 : frame(rows.subList(1, rows.size()), keys) + count(keys);
    |  }
    |  int arm(List<String> rows, List<String> keys) {
    |    if (rows.isEmpty()) { return 0; }
    |    return arm(rows.subList(1, rows.size()), keys) + count(keys);
    |  }
    |  int viaExit(List<String> rows, List<String> keys, List<String> others) {
    |    int n = 0;
    |    for (String o : others) { n += ifElse(rows, keys); }
    |    return n;
    |  }
    |""".stripMargin)))

  test("work in a branch that ends a recursion runs once, not in every frame") {
    assertNoFinding(found(branches))(finding =>
      Vector("ifElse", "ternary", "early", "late").exists(name => finding.detail.startsWith(s"Scan.$name "))
    )
  }

  test(
    "work in the frames of a recursion still multiplies by its depth, and a loop around a call multiplies the exits"
  ) {
    assertFinding(found(branches), "complexity", "Scan.frame is O(|keys| * |rows|)")
    assertFinding(found(branches), "complexity", "Scan.arm is O(|keys| * |rows|)")
    assertFinding(found(branches), "complexity", "Scan.viaExit is O(|keys| * |others| + |others| * |rows|)")
  }

  private lazy val shapes = build(Seq(scan("""
    |  int count(List<String> rows) { int n = 0; for (String r : rows) { n++; } return n; }
    |  int chain(List<String> rows, List<String> keys) {
    |    if (rows.isEmpty()) { return count(keys); }
    |    else if (rows.size() == 1) { return count(keys); }
    |    else { return chain(rows.subList(1, rows.size()), keys) + 1; }
    |  }
    |  int switched(List<String> rows, List<String> keys, int k) {
    |    switch (k) {
    |      case 0: return count(keys);
    |      default: return switched(rows.subList(1, rows.size()), keys, k) + 1;
    |    }
    |  }
    |  int nested(List<String> rows, List<String> keys) {
    |    if (!rows.isEmpty()) {
    |      if (rows.size() > 3) { return nested(rows.subList(1, rows.size()), keys) + 1; }
    |      return count(keys);
    |    }
    |    return count(keys);
    |  }
    |  int tried(List<String> rows, List<String> keys) {
    |    try {
    |      if (rows.isEmpty()) { return count(keys); }
    |      return tried(rows.subList(1, rows.size()), keys) + 1;
    |    } finally { }
    |  }
    |  int arrow(List<String> rows, List<String> keys, int k) {
    |    return switch (k) { case 0 -> count(keys); default -> arrow(rows.subList(1, rows.size()), keys, k) + 1; };
    |  }
    |""".stripMargin)))

  test("work in every branch that ends a recursion runs once, whatever the shape of the branches") {
    assertNoFinding(found(shapes))(finding =>
      Vector("chain", "switched", "nested", "tried", "arrow").exists(name =>
        finding.detail.startsWith(s"Scan.$name ")
      )
    )
  }

  private lazy val scalaBodies = build(
    Seq(scan("""
      |  int walked(String[] rows, String[] keys) { int n = 0; for (String k : keys) { n += new Walking(rows).hashCode(); } return n; }
      |  int nested(String[] rows, String[] keys) { int n = 0; for (String k : keys) { n += new Shelf.Inner(rows).hashCode(); } return n; }
      |  int generic(String[] rows, String[] keys) { int n = 0; for (String k : keys) { n += new Boxed<String>(rows).hashCode(); } return n; }
      |  int defaulted(String[] rows, String[] keys) { int n = 0; for (String k : keys) { n += new Defaulted(rows, 1).hashCode(); } return n; }
      |  int extended(String[] rows, String[] keys) { int n = 0; for (String k : keys) { n += new Extended(rows).hashCode(); } return n; }
      |  int record(String[] rows, String[] keys) { int n = 0; for (String k : keys) { n += new Plain(rows).hashCode(); } return n; }
      |""".stripMargin)),
    Seq("demo/Bodies.scala" -> """
      |package demo
      |
      |class Walking(items: Array[String]) {
      |  val all = items.toList
      |}
      |
      |object Shelf {
      |  class Inner(items: Array[String]) {
      |    val all = items.toList
      |  }
      |}
      |
      |class Boxed[T](items: Array[String]) {
      |  val all = items.toList
      |}
      |
      |class Defaulted(items: Array[String], extra: Int = 0) {
      |  val all = items.toList
      |}
      |
      |class Base(items: Array[String]) {
      |  val all = items.toList
      |}
      |
      |class Extended(items: Array[String]) extends Base(items)
      |
      |class Plain(val rows: Array[String])
      |""".stripMargin)
  )

  test("a Java caller of a Scala constructor sees the work of the class body whatever the shape of the class") {
    Vector("walked", "nested", "generic", "defaulted", "extended").foreach(name =>
      assertFinding(found(scalaBodies), "complexity", s"Scan.$name is O(|keys| * |rows|)")
    )
    assertNoFinding(found(scalaBodies))(_.detail.startsWith("Scan.record"))
  }

  private def fixed(bounded: Vector[Complexity.Bounded], others: Seq[(String, String)] = Nil) = build(
    Seq("demo/Fixed.java" ->
      """
      |package demo;
      |
      |import java.util.*;
      |
      |public class Fixed {
      |  List<String> fixed = new ArrayList<>();
      |  List<String> open = new ArrayList<>();
      |  int bounded(List<String> rows) { int n = 0; for (String r : rows) { for (String t : fixed) { n++; } } return n; }
      |  int unbounded(List<String> rows) { int n = 0; for (String r : rows) { for (String t : open) { n++; } } return n; }
      |}
      |""".stripMargin),
    others = others,
    bounded = bounded
  )

  private val fixedEntry = Complexity.Bounded("demo.Fixed.fixed", 4, "four at most")

  private lazy val fixedBounded   = fixed(Vector(fixedEntry))
  private lazy val fixedUnbounded = fixed(Vector.empty)

  test("a Java loop over a bounded field is not super-linear in the field's size") {
    assertNoFinding(found(fixedBounded))(_.detail.startsWith("Fixed.bounded "))
    assertFinding(found(fixedBounded), "complexity", "Fixed.unbounded is O(|rows| * |this.open|)")
  }

  test("a Java loop over an unbounded field is super-linear in the field's size") {
    assertFinding(found(fixedUnbounded), "complexity", "Fixed.bounded is O(|rows| * |this.fixed|)")
    assertFinding(found(fixedUnbounded), "complexity", "Fixed.unbounded is O(|rows| * |this.open|)")
  }

  test("a bounded entry naming a Java field is not reported as unused") {
    val scala =
      Seq("demo/Sized.scala" -> "package demo\n\nobject Sized {\n  def size(xs: List[Int]): Int = xs.size\n}\n")
    assertEquals(fixed(Vector(fixedEntry), scala).unusedBounds, Vector.empty)
  }

  private lazy val retention = build(Seq("demo/Retention.java" ->
    """
    |package demo;
    |
    |import java.util.*;
    |
    |public class Retention {
    |  List<List<String>> shared = new ArrayList<>();
    |  List<String> command(String header, List<String> defines) {
    |    List<String> arguments = new ArrayList<>(defines);
    |    arguments.add(header);
    |    return arguments;
    |  }
    |  List<String> passed(List<String> headers, List<String> defines) {
    |    List<String> results = new ArrayList<>();
    |    ProcessBuilder builder = new ProcessBuilder();
    |    for (String header : headers) {
    |      List<String> arguments = new ArrayList<>(defines);
    |      arguments.add(header);
    |      results.add(builder.command(arguments).toString());
    |    }
    |    return results;
    |  }
    |  List<String> copied(List<String> headers, List<String> defines) {
    |    List<String> results = new ArrayList<>();
    |    ProcessBuilder builder = new ProcessBuilder();
    |    for (String header : headers) {
    |      results.add(builder.command(List.copyOf(defines)).toString());
    |    }
    |    return results;
    |  }
    |  List<String> delegated(List<String> headers, List<String> defines) {
    |    List<String> results = new ArrayList<>();
    |    ProcessBuilder builder = new ProcessBuilder();
    |    for (String header : headers) {
    |      results.add(builder.command(command(header, defines)).toString());
    |    }
    |    return results;
    |  }
    |  List<List<String>> added(List<String> headers, List<String> defines) {
    |    List<List<String>> all = new ArrayList<>();
    |    for (String header : headers) {
    |      List<String> arguments = new ArrayList<>(defines);
    |      arguments.add(header);
    |      all.add(arguments);
    |    }
    |    return all;
    |  }
    |  List<List<String>> returned(List<String> headers, List<String> defines) {
    |    List<List<String>> all = new ArrayList<>();
    |    for (String header : headers) {
    |      all.add(command(header, defines));
    |    }
    |    return all;
    |  }
    |  List<String> reassigned(List<String> headers, List<String> defines) {
    |    List<String> latest = null;
    |    for (String header : headers) {
    |      latest = new ArrayList<>(defines);
    |    }
    |    return latest;
    |  }
    |  void stored(List<String> headers, List<String> defines) {
    |    for (String header : headers) {
    |      List<String> arguments = new ArrayList<>(defines);
    |      arguments.add(header);
    |      shared.add(arguments);
    |    }
    |  }
    |  void accumulated(List<String> headers, List<String> defines) {
    |    List<String> all = new ArrayList<>();
    |    for (String header : headers) {
    |      List<String> arguments = new ArrayList<>(defines);
    |      all.addAll(arguments);
    |    }
    |    shared.add(all);
    |  }
    |  List<List<String>> streamed(List<String> headers, List<String> defines) {
    |    return headers.stream().map(header -> new ArrayList<>(defines)).collect(java.util.stream.Collectors.toList());
    |  }
    |  List<String> dropped(List<String> headers, List<String> defines) {
    |    return headers.stream().map(header -> new ArrayList<>(defines).toString()).collect(java.util.stream.Collectors.toList());
    |  }
    |}
    |""".stripMargin))

  private val spaceless = Vector("passed", "copied", "delegated", "dropped")
  private val retained  = Vector("added", "returned", "reassigned", "stored", "accumulated", "streamed")

  test("a list built per iteration and handed to a call that keeps nothing is not live across the loop") {
    spaceless.foreach { name =>
      assertFinding(found(retention), "complexity", s"Retention.$name is O(|defines| * |headers|) in time:")
      assertNoFinding(found(retention))(finding => space(finding) && finding.detail.startsWith(s"Retention.$name "))
    }
  }

  test("lists a Java loop returns, stores, or adds to a collection that outlives it are live space") {
    retained.foreach { name =>
      assertFinding(
        found(retention),
        "complexity",
        s"Retention.$name is O(|defines| * |headers|) in time and space"
      )
    }
  }

  private lazy val leaving = build(Seq("demo/Leaving.java" ->
    """
    |package demo;
    |
    |import java.util.*;
    |
    |public class Leaving {
    |  int returnsHeld(List<String> rows, List<String> cols, List<String> defines) {
    |    int total = 0;
    |    for (String row : rows) {
    |      List<String> latest = null;
    |      for (String col : cols) {
    |        if (col.isEmpty()) return (latest = new ArrayList<>(defines)).size();
    |      }
    |      total += latest == null ? 0 : latest.size();
    |    }
    |    return total;
    |  }
    |  int keepsHeld(List<String> rows, List<String> cols, List<String> defines) {
    |    int total = 0;
    |    for (String row : rows) {
    |      List<String> latest = null;
    |      for (String col : cols) {
    |        latest = new ArrayList<>(defines);
    |      }
    |      total += latest.size();
    |    }
    |    return total;
    |  }
    |  boolean returnsAdded(List<String> rows, List<String> cols, List<String> defines) {
    |    List<List<String>> all = new ArrayList<>();
    |    for (String row : rows) {
    |      for (String col : cols) {
    |        if (col.isEmpty()) return all.add(new ArrayList<>(defines));
    |      }
    |    }
    |    return all.isEmpty();
    |  }
    |  List<List<String>> keepsAdded(List<String> rows, List<String> cols, List<String> defines) {
    |    List<List<String>> all = new ArrayList<>();
    |    for (String row : rows) {
    |      for (String col : cols) {
    |        all.add(new ArrayList<>(defines));
    |      }
    |    }
    |    return all;
    |  }
    |}
    |""".stripMargin))

  test("a list built under a return in a nested loop is built once, however far out its holder lives") {
    Vector("returnsHeld", "returnsAdded").foreach { name =>
      assertNoFinding(found(leaving))(finding =>
        finding.detail.startsWith(s"Leaving.$name ") && finding.detail.contains("|defines|")
      )
    }
  }

  test("a list built in a nested loop and kept across it is live space of the loops it stays across") {
    assertFinding(found(leaving), "space-complexity", "Leaving.keepsHeld allocates O(|cols| * |defines|)")
    assertFinding(
      found(leaving),
      "complexity",
      "Leaving.keepsAdded is O(|cols| * |defines| * |rows|) in time and space"
    )(
      using Finding.Level.Error
    )
  }
}
