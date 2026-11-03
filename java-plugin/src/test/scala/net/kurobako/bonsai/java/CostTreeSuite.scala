package net.kurobako.bonsai.java

/** The unevaluated costs of methods that the collector records. */
class CostTreeSuite extends munit.FunSuite {

  private def collect(sources: (String, String)*): Vector[Model.Cost] = Fixtures.collect(sources*).costs

  private val Lines = "\\{(loop|each|op|call|exit)(:\\d+)? \\d+ ".r

  private val Sinks = " @[-\\d]+r?s?(?=\\})".r

  private def sinked(sources: (String, String)*): Map[String, String] =
    collect(sources*).map(cost => cost.key -> Lines.replaceAllIn(Model.Cost.render(cost.body), "{$1$2 ")).toMap

  private def trees(sources: (String, String)*): Map[String, String] =
    sinked(sources*).view.mapValues(Sinks.replaceAllIn(_, "")).toMap

  private def service(body: String) = "Service" -> s"""
    |package demo;
    |
    |import java.util.*;
    |import java.util.stream.*;
    |
    |public class Service {
    |  List<String> items = new ArrayList<>();
    |  static class Box { List<String> items; String[] names; Map<String, String> index; }
    |  static List<String> shared = new ArrayList<>();
    |$body
    |}
    |""".stripMargin

  private def of(source: (String, String)): Map[String, String] = trees(source)

  test("a loop over a parameter, a field or a field of a parameter has that collection as its extent") {
    val found = of(service("""
      |  void names(List<String> names) { for (String s : names) { } }
      |  void field() { for (String s : items) { } for (String s : this.items) { } }
      |  void nested(Box box) { for (String s : box.items) { } for (String n : box.names) { } }
      |  static void global() { for (String s : shared) { } for (String s : Service.shared) { } }
      |""".stripMargin))
    assertEquals(found("demo.Service.names(Ljava/util/List;)V"), "{loop $0}")
    assertEquals(found("demo.Service.field()V"), "{loop this.items} {loop this.items}")
    assertEquals(found("demo.Service.nested(Ldemo/Service$Box;)V"), "{loop $0.items} {loop $0.names}")
    assertEquals(found("demo.Service.global()V"), "{loop demo.Service.shared} {loop demo.Service.shared}")
  }

  test("an indexed loop to the size or length of a path has that path as its extent") {
    val found = of(service("""
      |  void sized(List<String> names) { for (int i = 0; i < names.size(); i++) { } }
      |  void sizedWhile(List<String> names) { int i = 0; while (i < names.size()) { i++; } }
      |  void lengths(int[] values) { for (int i = 0; i < values.length; i++) { } }
      |  void bound(int[] values) { int n = values.length; for (int i = 0; i < n; i++) { } }
      |  void backwards(List<String> names) { for (int i = names.size() - 1; i >= 0; i--) { } }
      |  void unrelated(List<String> names, int limit) { for (int i = 0; i < limit; i++) { } }
      |  void text(String text) { for (int i = 0; i < text.length(); i++) { } }
      |""".stripMargin))
    assertEquals(found("demo.Service.sized(Ljava/util/List;)V"), "{loop $0}")
    assertEquals(found("demo.Service.sizedWhile(Ljava/util/List;)V"), "{loop $0}")
    assertEquals(found("demo.Service.lengths([I)V"), "{loop $0}")
    assertEquals(found("demo.Service.bound([I)V"), "{loop $0}")
    assertEquals(found("demo.Service.backwards(Ljava/util/List;)V"), "{loop $0}")
    assertEquals(found("demo.Service.unrelated(Ljava/util/List;I)V"), "{loop ?}")
    assertEquals(found("demo.Service.text(Ljava/lang/String;)V"), "{loop $0}")
  }

  test("a loop over something that is not a path has an unknown extent") {
    val found = of(service("""
      |  List<String> source() { return items; }
      |  void accessor() { for (String s : source()) { } }
      |  void local() { List<String> copy = source(); for (String s : copy) { } }
      |  void forever() { while (true) { break; } }
      |  void inner(List<List<String>> rows) { for (List<String> row : rows) { for (String s : row) { } } }
      |""".stripMargin))
    assertEquals(found("demo.Service.accessor()V"), "{call demo.Service.source()Ljava/util/List; this} {loop ?}")
    assertEquals(found("demo.Service.local()V"), "{call demo.Service.source()Ljava/util/List; this} {loop ?}")
    assertEquals(found("demo.Service.forever()V"), "{loop ?}")
    assertEquals(found("demo.Service.inner(Ljava/util/List;)V"), "{loop $0 {loop ?}}")
  }

  test("a table that the class fixes has a constant number of elements") {
    val found = of(service("""
      |  private static final int[] BITS = {1, 2, 4};
      |  private static final List<String> NAMES = List.of("a", "b");
      |  private static final List<String> LOOKUP = Collections.singletonList("a");
      |  private static final List<String> GROWN = new ArrayList<>();
      |  private final String[] own = new String[3];
      |  void constants() { for (int b : BITS) { } for (String s : NAMES) { } for (String s : LOOKUP) { } }
      |  void lengths() { for (int i = 0; i < BITS.length; i++) { } for (int i = 0; i < NAMES.size(); i++) { } }
      |  void growing() { for (String s : GROWN) { } for (String s : own) { } }
      |""".stripMargin))
    assertEquals(found("demo.Service.constants()V"), "{loop 1} {loop 1} {loop 1}")
    assertEquals(found("demo.Service.lengths()V"), "{loop 1} {loop 1}")
    assertEquals(found("demo.Service.growing()V"), "{loop demo.Service.GROWN} {loop this.own}")
  }

  test("a loop driven by a counter that outlives the loops around it takes its turns across all of them") {
    val found = of(service("""
      |  void merge(String[] left, String[] right) { int c = 0; for (String l : left) { while (c < right.length) { c++; } } }
      |  void fresh(String[] left, String[] right) {
      |    for (int i = 0; i < left.length; i++) { for (int j = 0; j < right.length; j++) { } }
      |  }
      |  void shared(String[] left, String[] right) {
      |    int i = 0;
      |    for (String l : right) { for (; i < left.length; i++) { } }
      |  }
      |  void iterated(List<String> names, String[] other) {
      |    Iterator<String> it = names.iterator();
      |    for (String o : other) { while (it.hasNext()) { it.next(); } }
      |  }
      |  void inner(List<String> names, String[] other) {
      |    for (String o : other) { Iterator<String> it = names.iterator(); while (it.hasNext()) { it.next(); } }
      |  }
      |""".stripMargin))
    assertEquals(found("demo.Service.merge([Ljava/lang/String;[Ljava/lang/String;)V"), "{loop $0 {loop:1 $1}}")
    assertEquals(found("demo.Service.fresh([Ljava/lang/String;[Ljava/lang/String;)V"), "{loop $0 {loop $1}}")
    assertEquals(found("demo.Service.shared([Ljava/lang/String;[Ljava/lang/String;)V"), "{loop $1 {loop:1 $0}}")
    assertEquals(found("demo.Service.iterated(Ljava/util/List;[Ljava/lang/String;)V"), "{loop $1 {loop:1 $0}}")
    assertEquals(found("demo.Service.inner(Ljava/util/List;[Ljava/lang/String;)V"), "{loop $1 {loop $0}}")
  }

  test("work in a statement that leaves the method runs once however many loops it is in") {
    val found = of(service("""
      |  boolean found(List<String> names, String[] keys) {
      |    for (String k : keys) { if (k.isEmpty()) { return check(names); } }
      |    return false;
      |  }
      |  boolean check(List<String> names) { return names.isEmpty(); }
      |  void fails(List<String> names, String[] keys) {
      |    for (String k : keys) { if (k.isEmpty()) { throw new IllegalStateException(describe(names)); } }
      |  }
      |  String describe(List<String> names) { return names.toString(); }
      |  long inLambda(List<String> names, String[] keys) {
      |    return names.stream().map(n -> { for (String k : keys) { if (k.isEmpty()) { return describe(names); } } return ""; }).count();
      |  }
      |""".stripMargin))
    assertEquals(
      found("demo.Service.found(Ljava/util/List;[Ljava/lang/String;)Z"),
      "{call demo.Service.check(Ljava/util/List;)Z this $0} {loop $1}"
    )
    assertEquals(
      found("demo.Service.fails(Ljava/util/List;[Ljava/lang/String;)V"),
      "{call demo.Service.describe(Ljava/util/List;)Ljava/lang/String; this $0} {loop $1}"
    )
    assertEquals(
      found("demo.Service.inLambda(Ljava/util/List;[Ljava/lang/String;)J"),
      "{each $0 {call demo.Service.describe(Ljava/util/List;)Ljava/lang/String; this $0} {loop $1}}"
    )
  }

  test("a local that only aliases a path is that path") {
    val found = of(service("""
      |  void alias(Box box) { List<String> own = box.items; for (String s : own) { } }
      |  void moved(Box box) { List<String> own = box.items; own = new ArrayList<>(); for (String s : own) { } }
      |""".stripMargin))
    assertEquals(found("demo.Service.alias(Ldemo/Service$Box;)V"), "{loop $0.items}")
    assertEquals(found("demo.Service.moved(Ldemo/Service$Box;)V"), "{loop ?}")
  }

  test("forEach, streams and iterators loop over the collection they start from") {
    val found = of(service("""
      |  void each(List<String> names) { names.forEach(n -> helper(n)); }
      |  void helper(String name) { }
      |  int chain(List<String> names) { return names.stream().filter(n -> check(n)).mapToInt(n -> weigh(n)).sum(); }
      |  boolean check(String name) { return true; }
      |  int weigh(String name) { return 1; }
      |  void mapped(Box box) { box.index.forEach((k, v) -> helper(k)); }
      |  void arrays(String[] values) { for (String s : Arrays.asList(values)) { } Arrays.stream(values).forEach(v -> helper(v)); }
      |  void views(Box box) { for (String k : box.index.keySet()) { } for (String v : box.index.values()) { } }
      |  void iterating(List<String> names) { Iterator<String> it = names.iterator(); while (it.hasNext()) { helper(it.next()); } }
      |  long counted(List<String> names) { return names.stream().flatMap(n -> Stream.of(n)).filter(n -> check(n)).count(); }
      |""".stripMargin))
    assertEquals(
      found("demo.Service.each(Ljava/util/List;)V"),
      "{each $0 {call demo.Service.helper(Ljava/lang/String;)V this ?}}"
    )
    assertEquals(
      found("demo.Service.chain(Ljava/util/List;)I"),
      "{each $0 {call demo.Service.check(Ljava/lang/String;)Z this ?}} {each $0 {call demo.Service.weigh(Ljava/lang/String;)I this ?}}"
    )
    assertEquals(
      found("demo.Service.mapped(Ldemo/Service$Box;)V"),
      "{each $0.index {call demo.Service.helper(Ljava/lang/String;)V this ?}}"
    )
    assertEquals(
      found("demo.Service.arrays([Ljava/lang/String;)V"),
      "{loop $0} {each $0 {call demo.Service.helper(Ljava/lang/String;)V this ?}}"
    )
    assertEquals(found("demo.Service.views(Ldemo/Service$Box;)V"), "{loop $0.index} {loop $0.index}")
    assertEquals(
      found("demo.Service.iterating(Ljava/util/List;)V"),
      "{loop $0 {call demo.Service.helper(Ljava/lang/String;)V this ?}}"
    )
    assertEquals(
      found("demo.Service.counted(Ljava/util/List;)J"),
      "{each $0} {each ? {call demo.Service.check(Ljava/lang/String;)Z this ?}}"
    )
  }

  test("a method reference run per element of a stream or a collection is a call per element") {
    val found = of(service("""
      |  void visit(String name) { }
      |  static int weigh(String name) { return 1; }
      |  int size() { return 0; }
      |  void bound(List<String> names, Box box) { names.forEach(this::visit); }
      |  int statics(List<String> names) { return names.stream().mapToInt(Service::weigh).sum(); }
      |  long unbound(List<Service> services) { return services.stream().mapToInt(Service::size).sum(); }
      |  void made(List<String> names) { names.stream().map(Holder::new).count(); }
      |  void library(List<String> names) { names.stream().map(String::length).count(); names.forEach(System.out::println); }
      |  void stored(List<String> names) { java.util.function.Consumer<String> c = this::visit; c.accept(names.get(0)); }
      |  void supplied(List<String> names) { names.forEach(Objects.requireNonNull(items)::add); }
      |  void arrays(List<String> names) { Object[] all = names.stream().toArray(List<?>[]::new); }
      |  static class Holder { Holder(String name) { } }
      |""".stripMargin))
    assertEquals(
      found("demo.Service.bound(Ljava/util/List;Ldemo/Service$Box;)V"),
      "{each $0 {call demo.Service.visit(Ljava/lang/String;)V this ?}}"
    )
    assertEquals(
      found("demo.Service.statics(Ljava/util/List;)I"),
      "{each $0 {call demo.Service.weigh(Ljava/lang/String;)I - ?}}"
    )
    assertEquals(
      found("demo.Service.unbound(Ljava/util/List;)J"),
      "{each $0 {call demo.Service.size()I ?}}"
    )
    assertEquals(
      found("demo.Service.made(Ljava/util/List;)V"),
      "{each $0 {call demo.Service.Holder.<init>(Ljava/lang/String;)V - ?}}"
    )
    assertEquals(found("demo.Service.library(Ljava/util/List;)V"), "{each $0} {each $0}")
    assertEquals(found("demo.Service.stored(Ljava/util/List;)V"), "")
    assertEquals(found("demo.Service.supplied(Ljava/util/List;)V"), "{each $0}")
    assertEquals(found("demo.Service.arrays(Ljava/util/List;)V"), "{each $0}")
  }

  test("loops nest as they are written") {
    val found = of(service("""
      |  void pairs(List<String> left, List<String> right) {
      |    for (String a : left) { for (String b : right) { } }
      |    for (int i = 0; i < left.size(); i++) { right.forEach(b -> helper(b)); }
      |  }
      |  void helper(String name) { }
      |""".stripMargin))
    assertEquals(
      found("demo.Service.pairs(Ljava/util/List;Ljava/util/List;)V"),
      "{loop $0 {loop $1}} {loop $0 {each $1 {call demo.Service.helper(Ljava/lang/String;)V this ?}}}"
    )
  }

  test("searches, sorts, copies and shifts of lists are operations over the list") {
    val found = of(service("""
      |  boolean search(List<String> names, String name) { return names.contains(name) || names.indexOf(name) > 0; }
      |  void removal(List<String> names, String name) { names.remove(name); names.remove(0); }
      |  void sorting(List<String> names, int[] values) { Collections.sort(names); names.sort(null); Arrays.sort(values); }
      |  void copying(List<String> names, Box box) {
      |    List<String> a = new ArrayList<>(names);
      |    Set<String> b = new HashSet<>(box.items);
      |    items.addAll(names);
      |    Object[] c = names.toArray();
      |    List<String> d = List.copyOf(names);
      |  }
      |  void shifting(List<String> names) { names.add(0, "x"); }
      |  void linked(LinkedList<String> names) { names.add(0, "x"); names.remove(0); }
      |  void hashed(Set<String> names, Map<String, String> index) { names.contains("x"); index.containsKey("x"); }
      |  void indexed(List<String> names) { names.add("x"); names.get(0); names.set(0, "y"); }
      |""".stripMargin))
    assertEquals(found("demo.Service.search(Ljava/util/List;Ljava/lang/String;)Z"), "{op search $0} {op search $0}")
    assertEquals(found("demo.Service.removal(Ljava/util/List;Ljava/lang/String;)V"), "{op search $0} {op shift $0}")
    assertEquals(found("demo.Service.sorting(Ljava/util/List;[I)V"), "{op sort $0} {op sort $0} {op sort $1}")
    assertEquals(
      found("demo.Service.copying(Ljava/util/List;Ldemo/Service$Box;)V"),
      "{op copy $0} {op copy $1.items} {op copy $0} {op copy $0} {op copy $0}"
    )
    assertEquals(found("demo.Service.shifting(Ljava/util/List;)V"), "{op shift $0}")
    assertEquals(found("demo.Service.linked(Ljava/util/LinkedList;)V"), "")
    assertEquals(found("demo.Service.hashed(Ljava/util/Set;Ljava/util/Map;)V"), "")
    assertEquals(found("demo.Service.indexed(Ljava/util/List;)V"), "")
  }

  test("a text grown in a loop is an operation over the loops since it was declared") {
    val found = of(service("""
      |  String joined(List<String> names) {
      |    String text = "";
      |    for (String s : names) { text += s; }
      |    return text;
      |  }
      |  String nested(List<String> names, List<String> keys) {
      |    String text = "";
      |    for (String s : names) { for (String k : keys) { text = text + k; } }
      |    return text;
      |  }
      |  String fresh(List<String> names) {
      |    for (String s : names) { String text = ""; text += s; }
      |    return "";
      |  }
      |  String once(String a, String b) { String text = a; text += b; return text; }
      |""".stripMargin))
    assertEquals(found("demo.Service.joined(Ljava/util/List;)Ljava/lang/String;"), "{loop $0 {op concat:1 -}}")
    assertEquals(
      found("demo.Service.nested(Ljava/util/List;Ljava/util/List;)Ljava/lang/String;"),
      "{loop $0 {loop $1 {op concat:2 -}}}"
    )
    assertEquals(found("demo.Service.fresh(Ljava/util/List;)Ljava/lang/String;"), "{loop $0}")
    assertEquals(found("demo.Service.once(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;"), "")
  }

  test("calls of methods outside the JDK carry their key, receiver and argument paths") {
    val found = trees(
      service("""
        |  void run(List<String> names, Box box, int n) {
        |    for (String s : names) { Util.scan(names, box.items); this.helper(s); new Service().helper(s); }
        |    Util.count(3);
        |    String.valueOf(n);
        |  }
        |  void helper(String name) { }
        |""".stripMargin),
      "Util" -> """
        |package demo;
        |import java.util.*;
        |public class Util {
        |  static int scan(List<String> a, List<String> b) { return 0; }
        |  static int count(int n) { return n; }
        |  static int count(long n) { return 0; }
        |  Util(List<String> values) { }
        |  void build(List<String> values) { new Util(values); }
        |}
        |""".stripMargin
    )
    assertEquals(
      found("demo.Service.run(Ljava/util/List;Ldemo/Service$Box;I)V"),
      "{loop $0 {call demo.Util.scan(Ljava/util/List;Ljava/util/List;)I - $0 $1.items} " +
        "{call demo.Service.helper(Ljava/lang/String;)V this ?} {call demo.Service.<init>()V -} " +
        "{call demo.Service.helper(Ljava/lang/String;)V ? ?}} " +
        "{call demo.Util.count(I)I - ?}"
    )
    assertEquals(found("demo.Util.build(Ljava/util/List;)V"), "{call demo.Util.<init>(Ljava/util/List;)V - $0}")
  }

  test("constructors of inner classes and enums are keyed by the descriptor the JVM gives them") {
    val costs = collect(
      "Shell" -> """
        |package demo;
        |import java.util.*;
        |public class Shell {
        |  public class Inner { Inner(List<String> values) { } }
        |  public class Plain { }
        |  public static class Nested { Nested(List<String> values) { } }
        |  public interface Face { class Held { Held(int n) { } } }
        |  public enum Mode { ONE(List.of()), TWO(List.of()); Mode(List<String> values) { } }
        |  public enum Bare { A, B }
        |  void make(Shell shell, List<String> rows) {
        |    new Inner(rows);
        |    shell.new Inner(rows);
        |    new Nested(rows);
        |    new Face.Held(1);
        |  }
        |}
        |""".stripMargin
    )
    val keys = costs.map(_.key).toSet
    Vector(
      "demo.Shell.Inner.<init>(Ldemo/Shell;Ljava/util/List;)V",
      "demo.Shell.Plain.<init>(Ldemo/Shell;)V",
      "demo.Shell.Nested.<init>(Ljava/util/List;)V",
      "demo.Shell.Face.Held.<init>(I)V",
      "demo.Shell.Mode.<init>(Ljava/lang/String;ILjava/util/List;)V",
      "demo.Shell.Bare.<init>(Ljava/lang/String;I)V"
    ).foreach(key => assert(keys(key), s"$key in $keys"))
    val calls = Model.Cost.render(costs.find(_.name == "demo.Shell.make").get.body)
    assert(calls.contains("demo.Shell.Inner.<init>(Ldemo/Shell;Ljava/util/List;)V"), calls)
    assert(!calls.contains("demo.Shell.Inner.<init>(Ljava/util/List;)V"), calls)
  }

  test(
    "an argument that is a strictly smaller part of the parameter in its position, or the parameter less a constant"
  ) {
    val found = of(service("""
      |  int peel(List<String> names) { return names.isEmpty() ? 0 : peel(names.subList(1, names.size())); }
      |  int drop(List<String> names, int n) { return n == 0 ? 0 : drop(names.subList(0, names.size() - 2), n - 1); }
      |  int text(String s, String t) { return s.isEmpty() ? 0 : text(s.substring(1), t.substring(1)); }
      |  int array(String[] values) { return values.length == 0 ? 0 : array(Arrays.copyOfRange(values, 1, values.length)); }
      |  int local(List<String> names) { List<String> rest = names.subList(1, names.size()); return local(rest); }
      |  int count(int n, int m) { return count(m - 1, n - 1) + count(n - 0, m + 1); }
      |  int same(List<String> names) { return same(names.subList(0, names.size())) + same(names.subList(0, 1)); }
      |  int swapped(List<String> names, List<String> keys) { return swapped(keys.subList(1, keys.size()), names); }
      |  int other(List<String> names, List<String> keys) { return other(names, keys.subList(1, keys.size())); }
      |  int dropped(List<String> names) { return dropped(names.subList(1, names.size()), 1); }
      |  int dropped(List<String> names, int n) { return n; }
      |""".stripMargin))
    assertEquals(found("demo.Service.peel(Ljava/util/List;)I"), "{call demo.Service.peel(Ljava/util/List;)I this $0<}")
    assertEquals(
      found("demo.Service.drop(Ljava/util/List;I)I"),
      "{call demo.Service.drop(Ljava/util/List;I)I this $0< $1-}"
    )
    assertEquals(
      found("demo.Service.text(Ljava/lang/String;Ljava/lang/String;)I"),
      "{call demo.Service.text(Ljava/lang/String;Ljava/lang/String;)I this $0< $1<}"
    )
    assertEquals(
      found("demo.Service.array([Ljava/lang/String;)I"),
      "{op copy $0} {call demo.Service.array([Ljava/lang/String;)I this $0<}"
    )
    assertEquals(
      found("demo.Service.local(Ljava/util/List;)I"),
      "{call demo.Service.local(Ljava/util/List;)I this $0<}"
    )
    assertEquals(
      found("demo.Service.count(II)I"),
      "{call demo.Service.count(II)I this ? ?} {call demo.Service.count(II)I this ? ?}"
    )
    assertEquals(
      found("demo.Service.same(Ljava/util/List;)I"),
      "{call demo.Service.same(Ljava/util/List;)I this ?} {call demo.Service.same(Ljava/util/List;)I this ?}"
    )
    assertEquals(
      found("demo.Service.swapped(Ljava/util/List;Ljava/util/List;)I"),
      "{call demo.Service.swapped(Ljava/util/List;Ljava/util/List;)I this ? $0}"
    )
    assertEquals(
      found("demo.Service.other(Ljava/util/List;Ljava/util/List;)I"),
      "{call demo.Service.other(Ljava/util/List;Ljava/util/List;)I this $0 $1<}"
    )
    assertEquals(
      found("demo.Service.dropped(Ljava/util/List;)I"),
      "{call demo.Service.dropped(Ljava/util/List;I)I this ? ?}"
    )
  }

  test("a method that calls itself keeps the call") {
    val found = of(service("""
      |  int walk(List<String> names) { return names.isEmpty() ? 0 : walk(names.subList(1, names.size())); }
      |""".stripMargin))
    assertEquals(found("demo.Service.walk(Ljava/util/List;)I"), "{call demo.Service.walk(Ljava/util/List;)I this $0<}")
  }

  test("work in the branch of a recursive method that makes no recursive call is an exit") {
    val found = of(service("""
      |  int count(List<String> rows) { int n = 0; for (String r : rows) { n++; } return n; }
      |  int ifElse(List<String> rows, List<String> keys) {
      |    if (rows.isEmpty()) { return count(keys); } else { return ifElse(rows.subList(1, rows.size()), keys) + 1; }
      |  }
      |  int ternary(List<String> rows, List<String> keys) {
      |    return rows.isEmpty() ? count(keys) : ternary(rows.subList(1, rows.size()), keys) + 1;
      |  }
      |  int early(List<String> rows, List<String> keys) {
      |    if (rows.isEmpty()) { return count(keys); }
      |    return early(rows.subList(1, rows.size()), keys) + count(rows);
      |  }
      |  int late(List<String> rows, List<String> keys) {
      |    if (!rows.isEmpty()) { return late(rows.subList(1, rows.size()), keys) + 1; }
      |    return count(keys);
      |  }
      |  int nested(List<String> rows, List<String> keys) {
      |    if (rows.isEmpty()) { return count(keys); }
      |    else if (keys.isEmpty()) { return count(rows); }
      |    else { return nested(rows.subList(1, rows.size()), keys); }
      |  }
      |  int after(List<String> rows) {
      |    int n = rows.isEmpty() ? count(items) : after(rows.subList(1, rows.size()));
      |    return n + count(rows);
      |  }
      |  int looped(List<String> rows, List<String> keys) {
      |    if (rows.isEmpty()) { int n = 0; for (String k : keys) { n += count(keys); } return n; }
      |    return looped(rows.subList(1, rows.size()), keys);
      |  }
      |""".stripMargin))
    val count = "{call demo.Service.count(Ljava/util/List;)I this"
    val self  = (name: String) => s"{call demo.Service.$name(Ljava/util/List;Ljava/util/List;)I this $$0< $$1}"
    assertEquals(
      found("demo.Service.ifElse(Ljava/util/List;Ljava/util/List;)I"),
      s"{exit $count $$1}} ${self("ifElse")}"
    )
    assertEquals(
      found("demo.Service.ternary(Ljava/util/List;Ljava/util/List;)I"),
      s"{exit $count $$1}} ${self("ternary")}"
    )
    assertEquals(
      found("demo.Service.early(Ljava/util/List;Ljava/util/List;)I"),
      s"{exit $count $$1}} ${self("early")} $count $$0}"
    )
    assertEquals(found("demo.Service.late(Ljava/util/List;Ljava/util/List;)I"), s"${self("late")} {exit $count $$1}}")
    assertEquals(
      found("demo.Service.nested(Ljava/util/List;Ljava/util/List;)I"),
      s"{exit $count $$1}} {exit $count $$0}} ${self("nested")}"
    )
    assertEquals(
      found("demo.Service.after(Ljava/util/List;)I"),
      s"{exit $count this.items}} {call demo.Service.after(Ljava/util/List;)I this $$0<} $count $$0}"
    )
    assertEquals(
      found("demo.Service.looped(Ljava/util/List;Ljava/util/List;)I"),
      s"{exit {loop $$1 $count $$1}}} ${self("looped")}"
    )
  }

  test("branches that all or none make a recursive call, and branches of a method that does not recur, are not exits") {
    val found = of(service("""
      |  int count(List<String> rows) { int n = 0; for (String r : rows) { n++; } return n; }
      |  int both(List<String> rows) {
      |    return rows.isEmpty() ? both(rows.subList(1, rows.size())) : both(rows.subList(2, rows.size())) + count(rows);
      |  }
      |  int neither(List<String> rows, List<String> keys) { return rows.isEmpty() ? count(keys) : count(rows); }
      |  int plain(List<String> rows) { if (rows.isEmpty()) { return count(items); } return count(rows); }
      |""".stripMargin))
    assert(found.values.forall(!_.contains("exit")), found.toString)
  }

  test("every method with a body has a cost, and a method without one has none") {
    val costs = collect(
      "Service" -> """
        |package demo;
        |public abstract class Service {
        |  abstract void open();
        |  void closed() { }
        |  Service() { }
        |  @SuppressWarnings("complexity")
        |  void quiet() { }
        |}
        |""".stripMargin
    )
    assertEquals(
      costs.map(_.key).sorted,
      Vector("demo.Service.<init>()V", "demo.Service.closed()V", "demo.Service.quiet()V")
    )
    assertEquals(costs.find(_.name == "demo.Service.quiet").map(_.suppressed), Some(Set("complexity")))
    assertEquals(costs.find(_.name == "demo.Service.quiet").map(_.owner), Some("demo.Service"))
  }

  test("a cost holds the names of the parameters") {
    val costs = collect(service("  void run(List<String> rows, int limit, String[] keys) { }"))
    assertEquals(costs.find(_.name == "demo.Service.run").map(_.parameters), Some(Vector("rows", "limit", "keys")))
  }

  test("a key spells primitives, arrays, nested classes and erased generics as descriptors") {
    val keys = collect(service("""
      |  static class Pair<A, B extends Number> { A first; B second; Pair(A first, B second) { for (int i = 0; i < 1; i++) { } } }
      |  void primitives(int a, long b, boolean c, char d, double e, byte f, short g, float h) { for (int i = 0; i < 1; i++) { } }
      |  int[][] grids(int[][] grid, String[] names) { for (String s : names) { } return grid; }
      |  void nested(Box box, Box[] boxes) { for (Box b : boxes) { } }
      |  <T extends Comparable<T>> void sorted(List<T> values) { for (T v : values) { } }
      |  <T extends Number> void numbers(T value, T[] values) { for (T v : values) { } }
      |  <T> List<T> copied(List<T> values) { List<T> out = new ArrayList<>(); for (T v : values) { out.add(v); } return out; }
      |""".stripMargin)).map(_.key).toSet
    Vector(
      "demo.Service.Pair.<init>(Ljava/lang/Object;Ljava/lang/Number;)V",
      "demo.Service.primitives(IJZCDBSF)V",
      "demo.Service.grids([[I[Ljava/lang/String;)[[I",
      "demo.Service.nested(Ldemo/Service$Box;[Ldemo/Service$Box;)V",
      "demo.Service.sorted(Ljava/util/List;)V",
      "demo.Service.numbers(Ljava/lang/Number;[Ljava/lang/Number;)V",
      "demo.Service.copied(Ljava/util/List;)Ljava/util/List;"
    ).foreach(key => assert(keys.contains(key), key))
  }

  test("a copy or a call records where its value goes") {
    val found = sinked(service("""
      |  List<String> returned(List<String> a) { return new ArrayList<>(a); }
      |  void dropped(List<String> a) { List<String> c = new ArrayList<>(a); }
      |  int measured(List<String> a) { return new ArrayList<>(a).size(); }
      |  void stored(List<String> a) { items = new ArrayList<>(a); }
      |  void listed(List<String> a) { shared.addAll(new ArrayList<>(a)); }
      |  List<String> held(List<String> a) { List<String> c = new ArrayList<>(a); return c; }
      |  int helper(List<String> a) { return a.size(); }
      |  int calls(List<String> a) { return helper(a); }
      |  void discards(List<String> a) { helper(a); }
      |  List<String> keeps(List<String> a) { return List.copyOf(a); }
      |""".stripMargin))
    assertEquals(found("demo.Service.returned(Ljava/util/List;)Ljava/util/List;"), "{op copy $0}")
    assertEquals(found("demo.Service.dropped(Ljava/util/List;)V"), "{op copy $0 @-}")
    assertEquals(found("demo.Service.measured(Ljava/util/List;)I"), "{op copy $0 @-}")
    assertEquals(found("demo.Service.stored(Ljava/util/List;)V"), "{op copy $0 @0s}")
    assertEquals(found("demo.Service.listed(Ljava/util/List;)V"), "{op copy $0 @0s} {op copy ? @0s}")
    assertEquals(found("demo.Service.held(Ljava/util/List;)Ljava/util/List;"), "{op copy $0}")
    assertEquals(
      found("demo.Service.calls(Ljava/util/List;)I"),
      "{call demo.Service.helper(Ljava/util/List;)I this $0}"
    )
    assertEquals(
      found("demo.Service.discards(Ljava/util/List;)V"),
      "{call demo.Service.helper(Ljava/util/List;)I this $0 @-}"
    )
    assertEquals(found("demo.Service.keeps(Ljava/util/List;)Ljava/util/List;"), "{op copy $0}")
  }

  test("a value built in a loop records the loops it stays live across") {
    val found = sinked(service("""
      |  List<List<String>> added(List<String> a, List<String> names) {
      |    List<List<String>> all = new ArrayList<>();
      |    for (String n : names) { all.add(new ArrayList<>(a)); }
      |    return all;
      |  }
      |  void local(List<String> a, List<String> names) {
      |    List<List<String>> all = new ArrayList<>();
      |    for (String n : names) { all.add(new ArrayList<>(a)); }
      |  }
      |  void passed(List<String> a, List<String> names) {
      |    for (String n : names) { List<String> c = new ArrayList<>(a); c.add(n); System.out.println(c); }
      |  }
      |  void kept(List<String> a, List<String> names) {
      |    for (String n : names) { List<String> c = new ArrayList<>(a); shared.addAll(c); }
      |  }
      |  List<String> reassigned(List<String> a, List<String> names) {
      |    List<String> latest = null;
      |    for (String n : names) { latest = new ArrayList<>(a); }
      |    return latest;
      |  }
      |  int overwritten(List<String> a, List<String> names) {
      |    List<String> latest = null;
      |    for (String n : names) { latest = new ArrayList<>(a); }
      |    return latest.size();
      |  }
      |  int nested(List<String> a, List<String> names) {
      |    int total = 0;
      |    for (String n : names) {
      |      List<List<String>> all = new ArrayList<>();
      |      for (String m : names) { all.add(new ArrayList<>(a)); }
      |      total += all.size();
      |    }
      |    return total;
      |  }
      |  List<ArrayList<String>> streamed(List<String> a, List<String> names) {
      |    return names.stream().map(n -> new ArrayList<>(a)).collect(Collectors.toList());
      |  }
      |  long counted(List<String> a, List<String> names) {
      |    return names.stream().map(n -> new ArrayList<>(a)).count();
      |  }
      |""".stripMargin))
    assertEquals(
      found("demo.Service.added(Ljava/util/List;Ljava/util/List;)Ljava/util/List;"),
      "{loop $1 {op copy $0}}"
    )
    assertEquals(found("demo.Service.local(Ljava/util/List;Ljava/util/List;)V"), "{loop $1 {op copy $0 @-}}")
    assertEquals(found("demo.Service.passed(Ljava/util/List;Ljava/util/List;)V"), "{loop $1 {op copy $0 @-}}")
    assertEquals(
      found("demo.Service.kept(Ljava/util/List;Ljava/util/List;)V"),
      "{loop $1 {op copy $0 @0s} {op copy ? @0s}}"
    )
    assertEquals(
      found("demo.Service.reassigned(Ljava/util/List;Ljava/util/List;)Ljava/util/List;"),
      "{loop $1 {op copy $0}}"
    )
    assertEquals(found("demo.Service.overwritten(Ljava/util/List;Ljava/util/List;)I"), "{loop $1 {op copy $0 @0}}")
    assertEquals(found("demo.Service.nested(Ljava/util/List;Ljava/util/List;)I"), "{loop $1 {loop $1 {op copy $0 @-}}}")
    assertEquals(
      found("demo.Service.streamed(Ljava/util/List;Ljava/util/List;)Ljava/util/List;"),
      "{each $1 {op copy $0}}"
    )
    assertEquals(found("demo.Service.counted(Ljava/util/List;Ljava/util/List;)J"), "{each $1 {op copy $0 @-}}")
  }
}
