package net.kurobako.bonsai.java

import com.sun.source.tree.MethodInvocationTree
import com.sun.source.tree.MethodTree
import com.sun.source.util.JavacTask
import com.sun.source.util.TreePath
import com.sun.source.util.TreePathScanner
import com.sun.source.util.Trees
import java.net.URI
import javax.tools.JavaFileObject
import javax.tools.SimpleJavaFileObject
import javax.tools.ToolProvider
import net.kurobako.bonsai.collections.Comparison
import net.kurobako.bonsai.collections.Kind
import net.kurobako.bonsai.collections.Ops
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

class ChainLifterSuite extends munit.FunSuite {

  private val imports = "import java.util.*; import java.util.function.*; import java.util.stream.*;"

  private def lifts(members: String, release: Int = 17): Map[String, Vector[Lifted]] = {
    val source = new SimpleJavaFileObject(URI.create("string:///T.java"), JavaFileObject.Kind.SOURCE) {
      override def getCharContent(ignoreEncodingErrors: Boolean): CharSequence = s"$imports\nclass T {\n$members\n}"
    }
    val compiler = ToolProvider.getSystemJavaCompiler
    val task     = compiler.getTask(null, null, null, Seq("-proc:none").asJava, null, Seq(source).asJava)
      .asInstanceOf[JavacTask]
    val units = task.parse().asScala.toVector
    task.analyze()
    val trees    = Trees.instance(task)
    val registry = new Registry(Some(release))
    val found    = mutable.LinkedHashMap.empty[String, Vector[Lifted]]
    units.foreach { unit =>
      new TreePathScanner[Unit, Unit] {
        override def visitMethodInvocation(node: MethodInvocationTree, unused: Unit): Unit = {
          val path      = getCurrentPath
          val ancestors =
            Iterator.iterate(path.getParentPath)(_.getParentPath).takeWhile(_ != null).map(_.getLeaf).toList
          val context = new Context(trees, task.getElements, task.getTypes, unit, path, "T", None, ancestors, registry)
          val method  = ancestors.collectFirst { case method: MethodTree => method.getName.toString }.getOrElse("")
          ChainLifter.lift(context).foreach(lifted => found(method) = found.getOrElse(method, Vector.empty) :+ lifted)
          super.visitMethodInvocation(node, unused)
        }
      }.scan(new TreePath(unit), ())
    }
    val unknown = found.values.flatten.flatMap(_.chain.steps.map(_.method)).filter(Ops(_).isEmpty)
    assertEquals(unknown.toSet, Set.empty[String])
    found.toMap
  }

  private def only(members: String, method: String = "run", release: Int = 17): Lifted =
    lifts(members, release).getOrElse(method, Vector.empty) match {
      case Vector(found) => found
      case other         => fail(s"expected one chain in $method, got ${other.map(_.chain.steps.map(_.method))}")
    }

  private def methods(members: String, release: Int = 17): Vector[String] =
    only(members, release = release).chain.steps.map(_.method)

  test("every step the table lowers to has a cost") {
    val tables = ChainMethods.Streams ++ ChainMethods.Collections ++ ChainMethods.Maps ++ ChainMethods.Optionals
    val named  = tables.values.toSet ++ ChainMethods.Factories.values.flatten ++ ChainMethods.Unmodifiable.keySet
    assertEquals(named.filter(Ops(_).isEmpty), Set.empty[String])
  }

  test("calls of a stream are steps in the order they run") {
    val lifted = only(
      "boolean run(List<String> values) { return values.stream().filter(v -> v.isEmpty()).findFirst().isPresent(); }"
    )
    assertEquals(lifted.chain.steps.map(_.method), Vector("stream", "find", "isDefined"))
    assertEquals(lifted.chain.source.text, "values")
  }

  test("a chain is lifted once, from its last call") {
    val all = lifts(
      "boolean run(List<String> values) { return values.stream().map(v -> v.trim()).filter(v -> !v.isEmpty()).findFirst().isPresent(); }"
    )
    assertEquals(all("run").size, 1)
    assertEquals(all("run").head.chain.steps.map(_.method), Vector("stream", "map", "find", "isDefined"))
  }

  test("a chain ends where a call the table does not know begins") {
    val lifted =
      only("Stream<String> run(List<String> values) { return values.stream().filter(v -> v.isEmpty()).peek(v -> {}); }")
    assertEquals(lifted.chain.steps.map(_.method), Vector("stream", "filter"))
  }

  test("a chain starts after a call the table does not know") {
    val lifted = only(
      "boolean run(Stream<String> values) { return values.peek(v -> {}).filter(v -> v.isEmpty()).findFirst().isPresent(); }"
    )
    assertEquals(lifted.chain.steps.map(_.method), Vector("find", "isDefined"))
    assert(lifted.chain.source.text.startsWith("values.peek("), lifted.chain.source.text)
  }

  test("a chain inside a lambda is lifted on its own") {
    val all = lifts(
      "boolean run(List<List<String>> values) { return values.stream().anyMatch(inner -> inner.stream().filter(v -> v.isEmpty()).count() == 0); }"
    )
    assertEquals(
      all("run").map(_.chain.steps.map(_.method)).toSet,
      Set(
        Vector("stream", "exists"),
        Vector("stream", "filter", "sizeIsZero")
      )
    )
  }

  test("a size compared with zero lowers to the method it states") {
    val expected = Map(
      "values.size() == 0"   -> "sizeIsZero",
      "0 == values.size()"   -> "sizeIsZero",
      "values.size() != 0"   -> "sizeNonZero",
      "values.size() > 0"    -> "sizeNonZero",
      "0 < values.size()"    -> "sizeNonZero",
      "values.size() <= 0"   -> "sizeIsZero",
      "values.size() < 1"    -> "sizeIsZero",
      "values.size() >= 1"   -> "sizeNonZero",
      "(values.size()) == 0" -> "sizeIsZero"
    )
    expected.foreach { (expression, method) =>
      assertEquals(methods(s"boolean run(List<String> values) { return $expression; }"), Vector(method), expression)
    }
  }

  test("a size compared with anything else stays a size") {
    assertEquals(methods("boolean run(List<String> values) { return values.size() == 2; }"), Vector("size"))
    assertEquals(methods("boolean run(List<String> values) { return values.size() > 1; }"), Vector("size"))
    assertEquals(methods("int run(List<String> values) { return values.size(); }"), Vector("size"))
  }

  test("the count of a stream compared with zero lowers to the method it states") {
    assertEquals(
      methods("boolean run(List<String> values) { return values.stream().filter(v -> v.isEmpty()).count() > 0; }"),
      Vector("stream", "filter", "sizeNonZero")
    )
    assertEquals(
      methods("boolean run(List<String> values) { return values.stream().count() == 0L; }"),
      Vector("stream", "sizeIsZero")
    )
  }

  test("the calls that find, match and collect lower to the methods of the algebra") {
    val expected = Map(
      "values.stream().findFirst()"                       -> Vector("stream", "headOption"),
      "values.stream().findAny()"                         -> Vector("stream", "headOption"),
      "values.stream().filter(v -> true).findAny()"       -> Vector("stream", "find"),
      "values.stream().anyMatch(v -> true)"               -> Vector("stream", "exists"),
      "values.stream().allMatch(v -> true)"               -> Vector("stream", "forall"),
      "values.stream().noneMatch(v -> true)"              -> Vector("stream", "noneMatch"),
      "values.stream().count()"                           -> Vector("stream", "size"),
      "values.stream().collect(Collectors.toList())"      -> Vector("stream", "toList"),
      "values.stream().collect(Collectors.toSet())"       -> Vector("stream", "toSet"),
      "values.stream().toList()"                          -> Vector("stream", "toList"),
      "values.stream().sorted()"                          -> Vector("stream", "sorted"),
      "values.stream().sorted(Comparator.naturalOrder())" -> Vector("stream", "sortWith"),
      "values.stream().distinct().limit(2).skip(1)"       -> Vector("stream", "distinct", "take", "drop"),
      "values.stream().flatMap(v -> Stream.of(v))"        -> Vector("stream", "flatMap"),
      "values.stream().mapToInt(v -> v.length())"         -> Vector("stream", "map"),
      "values.stream().min(Comparator.naturalOrder())"    -> Vector("stream", "min"),
      "values.stream().takeWhile(v -> true)"              -> Vector("stream", "takeWhile")
    )
    expected.foreach { (expression, steps) =>
      assertEquals(methods(s"Object run(List<String> values) { return $expression; }"), steps, expression)
    }
  }

  test("collecting by joining lowers to join with its separator") {
    val joined = only("String run(List<String> values) { return values.stream().collect(Collectors.joining(\", \")); }")
    assertEquals(joined.chain.steps.map(_.method), Vector("stream", "join"))
    assertEquals(joined.chain.steps(1).args.map(_.text), Vector("\", \""))
    val bare = only("String run(List<String> values) { return values.stream().collect(Collectors.joining()); }")
    assertEquals(bare.chain.steps.map(_.method), Vector("stream", "join"))
    assertEquals(bare.chain.steps(1).args, Vector.empty)
  }

  test("collecting by joining with a prefix and a suffix is no join") {
    val lifted = only(
      "String run(List<String> values) { return values.stream().collect(Collectors.joining(\",\", \"[\", \"]\")); }"
    )
    assertEquals(lifted.chain.steps.map(_.method), Vector("stream"))
  }

  test("a reduction by a separated concatenation lowers to joinReduce") {
    val lenient =
      only("String run(List<String> values) { return values.stream().reduce((a, b) -> a + \",\" + b).orElse(\"\"); }")
    assertEquals(lenient.chain.steps.map(_.method), Vector("stream", "joinReduce"))
    assertEquals(lenient.chain.steps(1).args.map(_.text), Vector("\",\""))
    val strict =
      only("String run(List<String> values) { return values.stream().reduce((a, b) -> a + \",\" + b).orElseThrow(); }")
    assertEquals(strict.chain.steps.map(_.method), Vector("stream", "joinReduceStrict"))
  }

  test("a reduction that is not a separated concatenation stays unlowered") {
    val members =
      """int shifted(List<Integer> values) { return values.stream().reduce((a, b) -> a + b).orElse(0); }
        |String flat(List<String> values) { return values.stream().reduce((a, b) -> a + b).orElse(""); }
        |String mirrored(List<String> values) { return values.stream().reduce((a, b) -> b + "," + a).orElse(""); }
        |""".stripMargin
    lifts(members).foreach { (name, found) =>
      assertEquals(found.map(_.chain.steps.map(_.method)).toSet, Set(Vector("stream"), Vector("getOrElse")), name)
    }
  }

  test("presence checks of an optional lower to the algebra, negated ones to a check against none") {
    val expected = Map(
      "value.isPresent()"                 -> Vector("isDefined"),
      "value.isEmpty()"                   -> Vector("isEmpty"),
      "!value.isPresent()"                -> Vector("isNone"),
      "!value.isEmpty()"                  -> Vector("isSome"),
      "!(value.isPresent())"              -> Vector("isNone"),
      "value.map(v -> v + 1).isPresent()" -> Vector("map", "isDefined"),
      "value.map(v -> v + 1).orElse(0)"   -> Vector("map", "getOrElse")
    )
    expected.foreach { (expression, steps) =>
      assertEquals(methods(s"Object run(Optional<Integer> value) { return $expression; }"), steps, expression)
    }
  }

  test("a negated presence check is a check against none only where isEmpty exists") {
    assertEquals(
      methods("boolean run(Optional<String> value) { return !value.isPresent(); }", release = 8),
      Vector("isDefined")
    )
    assertEquals(
      methods("boolean run(Optional<String> value) { return !value.isEmpty(); }", release = 8),
      Vector("isSome")
    )
  }

  test("the primitive optionals are optionals") {
    assertEquals(methods("boolean run(OptionalInt value) { return !value.isPresent(); }"), Vector("isNone"))
    assertEquals(methods("boolean run(OptionalDouble value) { return value.isEmpty(); }"), Vector("isEmpty"))
  }

  test("a factory is the first step, on the class that has it as the source") {
    val listed = only("Stream<String> run() { return List.of(\"a\", \"b\").stream(); }")
    assertEquals(listed.chain.steps.map(_.method), Vector("of", "stream"))
    assertEquals(listed.chain.source.text, "List")
    assertEquals(listed.chain.steps.head.args.map(_.text), Vector("\"a\"", "\"b\""))
    val arrays = only("Stream<String> run() { return Arrays.asList(\"a\").stream(); }")
    assertEquals(arrays.chain.steps.map(_.method), Vector("asList", "stream"))
    assertEquals(arrays.chain.source.text, "Arrays")
  }

  test("an unmodifiable copy lowers to the factory of the collection it copies") {
    val lists =
      only("List<String> run(List<String> values) { return Collections.unmodifiableList(new ArrayList<>(values)); }")
    assertEquals(lists.chain.steps.map(_.method), Vector("unmodifiableList"))
    assertEquals(lists.chain.source.text, "values")
    assertEquals(
      methods("Set<String> run(Set<String> values) { return Collections.unmodifiableSet(new HashSet<>(values)); }"),
      Vector("unmodifiableSet")
    )
    assertEquals(
      methods(
        "Map<String, String> run(Map<String, String> values) { return Collections.unmodifiableMap(new HashMap<>(values)); }"
      ),
      Vector("unmodifiableMap")
    )
  }

  test("an unmodifiable view, or the copy of a capacity, is no copy") {
    assertEquals(
      lifts("List<String> run(List<String> values) { return Collections.unmodifiableList(values); }").get("run"),
      None
    )
    assertEquals(
      lifts("List<String> run() { return Collections.unmodifiableList(new ArrayList<>(10)); }").get("run"),
      None
    )
  }

  test("an unmodifiable copy is lifted from the release that has copyOf") {
    val members =
      "List<String> run(List<String> values) { return Collections.unmodifiableList(new ArrayList<>(values)); }"
    assertEquals(lifts(members, release = 9).get("run"), None)
    assertEquals(lifts(members, release = 10)("run").size, 1)
  }

  test("receivers get the kind their type has") {
    val members =
      """boolean list(List<String> values) { return values.isEmpty(); }
        |boolean linked(LinkedList<String> values) { return values.isEmpty(); }
        |boolean set(Set<String> values) { return values.isEmpty(); }
        |boolean sorted(SortedSet<String> values) { return values.isEmpty(); }
        |boolean hashed(HashSet<String> values) { return values.isEmpty(); }
        |boolean collection(Collection<String> values) { return values.isEmpty(); }
        |boolean queue(Deque<String> values) { return values.isEmpty(); }
        |boolean map(Map<String, String> values) { return values.isEmpty(); }
        |boolean stream(Stream<String> values) { return values.findFirst().isPresent(); }
        |boolean ints(IntStream values) { return values.findFirst().isPresent(); }
        |boolean optional(Optional<String> value) { return value.isPresent(); }
        |""".stripMargin
    val kinds = lifts(members).view.mapValues(_.head.chain.steps.head.receiver).toMap
    assertEquals(kinds("list"), Kind(library = true, name = "java.util.List"))
    assertEquals(kinds("linked"), Kind(library = true, name = "java.util.LinkedList"))
    assertEquals(kinds("set"), Kind(library = true, ordered = false, distinct = true, name = "java.util.Set"))
    assertEquals(kinds("sorted"), Kind(library = true, distinct = true, sorted = true, name = "java.util.SortedSet"))
    assertEquals(kinds("hashed"), Kind(library = true, ordered = false, distinct = true, name = "java.util.HashSet"))
    assertEquals(kinds("collection"), Kind(library = true, ordered = false, name = "java.util.Collection"))
    assertEquals(kinds("queue"), Kind(library = true, name = "java.util.Deque"))
    assertEquals(kinds("map"), Kind(library = true, ordered = false, keyed = true, name = "java.util.Map"))
    assertEquals(kinds("stream"), Kind(library = true, strict = false, name = "java.util.stream.Stream"))
    assertEquals(kinds("ints"), Kind(library = true, strict = false, name = "java.util.stream.IntStream"))
    assertEquals(kinds("optional"), Kind(library = true, single = true, name = "java.util.Optional"))
  }

  test("the result of a stream step is lazy, and the result of a terminal is no collection") {
    val steps =
      only("boolean run(List<String> values) { return values.stream().filter(v -> true).count() == 0; }").chain.steps
    assertEquals(steps.map(_.result.strict), Vector(false, false, true))
    assertEquals(steps.last.result.library, false)
  }

  test("a user type with the names of a collection is not lifted") {
    val members =
      """static class Mine {
        |  Mine filter(Predicate<String> p) { return this; }
        |  long count() { return 0; }
        |  Mine stream() { return this; }
        |  int size() { return 0; }
        |}
        |boolean run(Mine mine) { return mine.stream().filter(v -> true).count() == 0 || mine.size() == 0; }
        |""".stripMargin
    assertEquals(lifts(members).get("run"), None)
  }

  test("a user subtype of a collection is library where it overrides nothing") {
    val members =
      """static class Plain extends ArrayList<String> {}
        |static class Own extends ArrayList<String> {
        |  @Override public int size() { return 0; }
        |}
        |boolean plain(Plain values) { return values.size() == 0; }
        |boolean own(Own values) { return values.size() == 0; }
        |""".stripMargin
    val all = lifts(members)
    assertEquals(all("plain").head.chain.steps.map(_.receiver.library), Vector(true))
    assertEquals(all.get("own"), None)
  }

  test("a function written in place knows whether it is pure") {
    val members =
      """int counter;
        |final int limit = 3;
        |boolean literal(List<Integer> values) { return values.stream().filter(v -> v > 1 + 2).count() > 0; }
        |boolean final_(List<Integer> values) { return values.stream().filter(v -> v > limit && v != 0).count() > 0; }
        |boolean effect(List<Integer> values) { return values.stream().filter(v -> counter++ > 0).count() > 0; }
        |boolean call(List<Integer> values) { return values.stream().filter(v -> check(v)).count() > 0; }
        |boolean reference(List<Integer> values) { return values.stream().filter(this::check).count() > 0; }
        |boolean mutable(List<Integer> values) { return values.stream().filter(v -> v > counter).count() > 0; }
        |boolean divided(List<Integer> values) { return values.stream().filter(v -> 10 / v > 1).count() > 0; }
        |boolean halved(List<Integer> values) { return values.stream().filter(v -> v / 2 > 1).count() > 0; }
        |boolean check(Integer value) { return true; }
        |""".stripMargin
    val all                = lifts(members)
    def pure(name: String) = all(name).head.chain.steps(1).args.head.pure
    val expected           = Map(
      "literal"   -> true,
      "final_"    -> true,
      "halved"    -> true,
      "effect"    -> false,
      "call"      -> false,
      "reference" -> false,
      "mutable"   -> false,
      "divided"   -> false
    )
    expected.foreach((name, value) => assertEquals(pure(name), value, name))
  }

  test("an identity function is known as one") {
    val members =
      """boolean lambda(List<List<String>> values) { return values.stream().flatMap(v -> v.stream()).count() == 0; }
        |Stream<String> same(Stream<Stream<String>> values) { return values.flatMap(v -> v); }
        |Stream<String> function(Stream<Stream<String>> values) { return values.flatMap(Function.identity()); }
        |""".stripMargin
    val all = lifts(members)
    assertEquals(all("lambda").head.chain.steps(1).args.head.identity, false)
    assertEquals(all("same").head.chain.steps.head.args.head.identity, true)
    assertEquals(all("function").head.chain.steps.head.args.head.identity, true)
  }

  test("literals are known") {
    val lifted = only("Stream<String> run(List<String> values) { return values.stream().limit(3).skip(0); }")
    assertEquals(lifted.chain.steps(1).args.head.literal, Some(3))
    assertEquals(lifted.chain.steps(2).args.head.literal, Some(0))
  }

  test("a call with explicit type arguments is typed") {
    val lifted = only("Stream<Object> run(List<String> values) { return values.stream().<Object>map(v -> v); }")
    assertEquals(lifted.chain.steps.map(_.typed), Vector(false, true))
  }

  test("the end of each step is the expression that runs it") {
    val lifted =
      only("boolean run(List<String> values) { return values.stream().filter(v -> v.isEmpty()).count() == 0; }")
    assertEquals(
      lifted.ends.map(_.toString),
      Vector(
        "values.stream()",
        "values.stream().filter((v)->v.isEmpty())",
        "values.stream().filter((v)->v.isEmpty()).count() == 0"
      )
    )
    assertEquals(lifted.receiver(0), "values")
    assertEquals(lifted.receiver(2), "values.stream().filter((v)->v.isEmpty())")
  }

  test("a call on this, or on a type that is not a collection, starts no chain") {
    assertEquals(lifts("boolean run() { return size() == 0; }\nint size() { return 0; }").get("run"), None)
    assertEquals(lifts("String run(String text) { return text.trim().toString(); }").get("run"), None)
  }

  private def comparison(members: String): Option[Comparison] =
    only(members).chain.steps.last.args.headOption.flatMap(_.comparison)

  private def matching(predicate: String, element: String = "String", quantifier: String = "anyMatch"): String =
    s"boolean run(List<$element> values, String c, $element[] arrays) { return values.stream().$quantifier($predicate); }"

  test("a match of an equality test with a constant lowers to a comparison") {
    val forms = Map(
      "x -> x.equals(c)"             -> "c",
      "x -> c.equals(x)"             -> "c",
      "c::equals"                    -> "c",
      "x -> Objects.equals(x, c)"    -> "c",
      "x -> Objects.equals(c, x)"    -> "c",
      "x -> x.equals(\"a\")"         -> "\"a\"",
      "x -> (x.equals(c))"           -> "c",
      "(String x) -> x.equals(c)"    -> "c",
      "x -> x.equals(arrays.length)" -> "arrays.length"
    )
    forms.foreach { (predicate, constant) =>
      assertEquals(comparison(matching(predicate)), Some(Comparison(equal = true, constant)), predicate)
    }
  }

  test("a negated equality test lowers to an inequality") {
    assertEquals(
      comparison(matching("x -> !x.equals(c)", quantifier = "allMatch")),
      Some(Comparison(equal = false, "c"))
    )
    assertEquals(
      comparison(matching("x -> !Objects.equals(c, x)", quantifier = "allMatch")),
      Some(Comparison(equal = false, "c"))
    )
    assertEquals(
      only(matching("x -> x.equals(c)", quantifier = "noneMatch")).chain.steps.map(_.method),
      Vector("stream", "noneMatch")
    )
  }

  test("a function that is no equality test with a constant is no comparison") {
    val forms = Vector(
      "x -> x.equals(x)",
      "x -> x.equals(x.trim())",
      "x -> x.equals(c.trim())",
      "x -> c.equals(x.trim())",
      "x -> x.isEmpty()",
      "x -> x.equalsIgnoreCase(c)",
      "x -> x.equals(c) && true",
      "x -> x.compareTo(c) == 0",
      "x -> x == c",
      "x -> Objects.equals(x, x)",
      "x -> Objects.hash(x, c) == 1",
      "String::isEmpty",
      "x -> { return x.equals(c); }"
    )
    forms.foreach(predicate => assertEquals(comparison(matching(predicate)), None, predicate))
  }

  test("an equality test of arrays is no comparison") {
    assertEquals(comparison(matching("x -> x.equals(arrays)", element = "String[]")), None)
    assertEquals(comparison(matching("x -> Objects.equals(arrays, x)", element = "String[]")), None)
  }

  test("an array stream is a stream of the array kind") {
    val forms = Map(
      "Arrays.stream(values).filter(v -> v.isEmpty()).count() == 0" -> "values",
      "Stream.of(values).filter(v -> v.isEmpty()).count() == 0"     -> "values",
      "Arrays.stream(values).anyMatch(v -> v.isEmpty())"            -> "values"
    )
    forms.foreach { (expression, source) =>
      val lifted = only(s"boolean run(String[] values) { return $expression; }")
      assertEquals(lifted.chain.steps.head.method, "stream", expression)
      assertEquals(lifted.chain.steps.head.receiver, Kind(library = true, name = "java.array"), expression)
      assert(!lifted.chain.steps.head.result.strict, expression)
      assertEquals(lifted.chain.source.text, source, expression)
      assertEquals(lifted.receiver(0), source, expression)
    }
    val filtered = only("boolean run(String[] values) { return Arrays.stream(values).filter(v -> true).count() == 0; }")
    assertEquals(filtered.chain.steps.map(_.method), Vector("stream", "filter", "sizeIsZero"))
    assertEquals(filtered.receiver(1), "Arrays.stream(values)")
  }

  test("primitive arrays stream too") {
    val lifted = only("boolean run(int[] values) { return Arrays.stream(values).anyMatch(v -> v == 1); }")
    assertEquals(lifted.chain.steps.map(_.method), Vector("stream", "exists"))
    assertEquals(lifted.chain.steps.head.receiver, Kind(library = true, name = "java.array"))
  }

  test("a stream of elements, of a range or of a primitive array as one element starts no array chain") {
    val forms = Vector(
      "Stream.of(values[0], values[1]).filter(v -> true).count() == 0",
      "Stream.of(values[0]).filter(v -> true).count() == 0",
      "Arrays.stream(values, 0, 1).filter(v -> true).count() == 0"
    )
    forms.foreach { expression =>
      val steps = only(s"boolean run(String[] values) { return $expression; }").chain.steps
      assertEquals(steps.map(_.method), Vector("filter", "sizeIsZero"), expression)
    }
    val ints = only("boolean run(int[] values) { return Stream.of(values).filter(v -> true).count() == 0; }")
    assertEquals(ints.chain.steps.map(_.method), Vector("filter", "sizeIsZero"))
  }

  test("a call on this in a subtype of a collection is a step of that collection") {
    val members =
      """static class Plain extends ArrayList<String> {
        |  boolean empty() { return size() == 0; }
        |  boolean none() { return stream().filter(v -> v.isEmpty()).count() == 0; }
        |  boolean nested() { return ((Supplier<Boolean>) () -> size() > 0).get(); }
        |  boolean present() { return !isEmpty(); }
        |}
        |""".stripMargin
    val all = lifts(members)
    assertEquals(all("empty").head.chain.steps.map(_.method), Vector("sizeIsZero"))
    assertEquals(all("empty").head.chain.steps.head.receiver, Kind(library = true, name = "T.Plain"))
    assertEquals(all("empty").head.chain.source.text, "this")
    assertEquals(all("none").head.chain.steps.map(_.method), Vector("stream", "filter", "sizeIsZero"))
    assertEquals(all("none").head.chain.source.text, "this")
    assertEquals(all("nested").head.chain.steps.map(_.method), Vector("sizeNonZero"))
  }

  test("a call on this is a step of the set or map the type extends") {
    val members =
      """static class Names extends HashSet<String> { boolean empty() { return size() == 0; } }
        |static class Table extends HashMap<String, String> { boolean empty() { return size() == 0; } }
        |""".stripMargin
    val all = lifts(members)
    assertEquals(all("empty").map(_.chain.steps.map(_.receiver.name)).toSet, Set(Vector("T.Names"), Vector("T.Table")))
  }

  test("a call on this of a method the class overrides, or of a type that is no collection, is no step") {
    val members =
      """static class Own extends ArrayList<String> {
        |  @Override public int size() { return 0; }
        |  boolean empty() { return size() == 0; }
        |}
        |static class Plain extends ArrayList<String> {
        |  boolean outer() { return new Object() { int size() { return 0; } boolean empty() { return size() == 0; } }.empty(); }
        |}
        |static class Unrelated {
        |  int size() { return 0; }
        |  boolean empty() { return size() == 0; }
        |}
        |""".stripMargin
    assertEquals(lifts(members).get("empty"), None)
  }
}
