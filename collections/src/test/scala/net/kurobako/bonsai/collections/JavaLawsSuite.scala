package net.kurobako.bonsai.collections

import Fixtures.*

class JavaLawsSuite extends munit.FunSuite {

  private val Values   = named("java.util.List")
  private val Stream   = named("java.util.stream.Stream", Lazy)
  private val Optional = named("java.util.Optional", Opt)
  private val Text     = Kind(library = false)

  private def java(chain: Chain): Vector[(String, String, Option[String])] =
    Laws.outcomes(chain).flatMap(outcome =>
      outcome.java.map(found => (outcome.law, found.message, found.render.map(_("values"))))
    )

  private def only(chain: Chain): (String, String, Option[String]) = java(chain) match {
    case Vector(found) => found
    case other         => fail(s"expected one Java outcome, got $other")
  }

  private def onStream(method: String, args: Arg*): Step = Step(method, args.toVector, Stream, Stream)

  private def terminal(method: String, kind: Kind, args: Arg*): Step = Step(method, args.toVector, kind, Text)

  test("a size compared with zero is isEmpty on a collection") {
    assertEquals(
      only(chain("values", terminal("sizeIsZero", Values))),
      ("size-to-is-empty", "use isEmpty() instead of a size comparison with zero", Some("values.isEmpty()"))
    )
    assertEquals(
      only(chain("values", terminal("sizeNonZero", Values))),
      ("size-to-non-empty", "use !isEmpty() instead of a size comparison with zero", Some("!values.isEmpty()"))
    )
  }

  test("a count compared with zero is no emptiness check of a stream") {
    assertEquals(java(chain("values", terminal("sizeIsZero", Stream))), Vector.empty)
  }

  test("a filtered count compared with zero is a match of the stream") {
    val p = fn("p")
    assertEquals(
      only(chain("values", onStream("filter", p), terminal("sizeIsZero", Stream))),
      (
        "size-to-is-empty",
        "use noneMatch instead of filter and a count compared with zero",
        Some("values.noneMatch(p)")
      )
    )
    assertEquals(
      only(chain("values", onStream("filter", p), terminal("sizeNonZero", Stream))),
      (
        "size-to-non-empty",
        "use anyMatch instead of filter and a count compared with zero",
        Some("values.anyMatch(p)")
      )
    )
  }

  test("a filtered count with an effect in the predicate is not rewritten") {
    assertEquals(
      java(chain("values", onStream("filter", effect("p")), terminal("sizeIsZero", Stream))),
      Vector.empty
    )
  }

  test("the first match of a stream checked for presence is a match") {
    val p = fn("p")
    assertEquals(
      only(chain("values", Step("find", Vector(p), Stream, Optional), terminal("isDefined", Optional))),
      ("find-to-exists", "use anyMatch for filter, findFirst and isPresent", Some("values.anyMatch(p)"))
    )
    assertEquals(
      only(chain("values", Step("find", Vector(p), Stream, Optional), terminal("isEmpty", Optional))),
      ("find-to-exists", "use noneMatch for filter, findFirst and isEmpty", Some("values.noneMatch(p)"))
    )
  }

  test("a sort followed by the first element is a minimum") {
    assertEquals(
      only(chain("values", onStream("sortWith", fn("order")), Step("headOption", Vector.empty, Stream, Optional))),
      (
        "sorted-min-max",
        "sorted then findFirst sorts every element; min compares in one pass",
        Some("values.min(order)")
      )
    )
    assertEquals(
      only(chain("values", onStream("sorted"), Step("headOption", Vector.empty, Stream, Optional)))._3,
      Some("values.min(Comparator.naturalOrder())")
    )
  }

  test("a stream of a factory is Stream.of") {
    val factory = Step("of", Vector(fn("a"), fn("b")), Values, Values)
    assertEquals(
      only(chain("List", factory, Step("stream", Vector.empty, Values, Stream))),
      ("stream-of", "use Stream.of instead of List.of(...).stream()", Some("Stream.of(a, b)"))
    )
    val listed = Step("asList", Vector(fn("a")), Values, Values)
    assertEquals(
      only(chain("Arrays", listed, Step("stream", Vector.empty, Values, Stream)))._2,
      "use Stream.of instead of Arrays.asList(...).stream()"
    )
  }

  test("a stream of a collection collected by joining is String.join") {
    val joined = Step("join", Vector(fn("\", \"")), Stream, Text)
    assertEquals(
      only(chain("values", Step("stream", Vector.empty, Values, Stream), joined)),
      (
        "stream-join",
        "use String.join instead of collecting a stream with joining",
        Some("String.join(\", \", values)")
      )
    )
  }

  test("a stream that other steps changed is not joined by String.join") {
    val joined = Step("join", Vector(fn("\", \"")), Stream, Text)
    assertEquals(
      java(chain("values", Step("stream", Vector.empty, Values, Stream), onStream("map", fn("f")), joined)),
      Vector.empty
    )
  }

  test("a reduction by concatenation is reported with what it does on an empty stream") {
    assertEquals(
      only(chain("values", terminal("joinReduce", Stream, fn("\",\""))))._2,
      "use String.join instead of reducing a stream by concatenation"
    )
    assertEquals(
      only(chain("values", terminal("joinReduceStrict", Stream, fn("\",\""))))._2,
      "use String.join instead of reducing a stream by concatenation, which returns an empty text where orElseThrow fails"
    )
  }

  test("an unmodifiable copy is a copyOf") {
    assertEquals(
      only(chain("values", Step("unmodifiableList", Vector.empty, Values, Values))),
      ("java9-collection-factory", "use List.copyOf instead of unmodifiableList of a copy", Some("List.copyOf(values)"))
    )
    assertEquals(
      only(chain("values", Step("unmodifiableMap", Vector.empty, Values, Values)))._3,
      Some("Map.copyOf(values)")
    )
  }

  test("a negated presence check of an optional is its opposite") {
    assertEquals(
      only(chain("values", terminal("isNone", Optional))),
      ("none-check", "use isEmpty() instead of !isPresent()", Some("values.isEmpty()"))
    )
    assertEquals(
      only(chain("values", terminal("isSome", Optional))),
      ("none-check", "use isPresent() instead of !isEmpty()", Some("values.isPresent()"))
    )
  }

  private val Plain  = named("java.util.ArrayList")
  private val Hashed = named("java.util.HashSet", Unique)
  private val Sorted = named("java.util.TreeSet", Unique).copy(sorted = true)
  private val Array  = named("java.array")

  private def streamed(source: Kind, quantifier: String, test: Fn): Chain =
    chain(
      "values",
      Step("stream", Vector.empty, source, Stream),
      Step(quantifier, Vector(test), Stream, Text)
    )

  test("a match of an equality test with a constant is a contains of the collection") {
    Vector(Plain, Hashed).foreach { source =>
      assertEquals(
        only(streamed(source, "exists", equalTo("c"))),
        (
          "exists-to-contains",
          "use contains instead of stream and anyMatch of an equality test",
          Some("values.contains(c)")
        )
      )
      assertEquals(
        only(streamed(source, "forall", differsFrom("c"))),
        (
          "forall-to-not-contains",
          "use !contains instead of stream and allMatch of an inequality test",
          Some("!values.contains(c)")
        )
      )
      assertEquals(
        only(streamed(source, "noneMatch", equalTo("c"))),
        (
          "forall-to-not-contains",
          "use !contains instead of stream and noneMatch of an equality test",
          Some("!values.contains(c)")
        )
      )
    }
  }

  test("a match that is no equality test with a constant, or not of its sense, is no contains") {
    assertEquals(java(streamed(Plain, "exists", fn("p"))), Vector.empty)
    assertEquals(java(streamed(Plain, "exists", differsFrom("c"))), Vector.empty)
    assertEquals(java(streamed(Plain, "forall", equalTo("c"))), Vector.empty)
    assertEquals(java(streamed(Plain, "noneMatch", differsFrom("c"))), Vector.empty)
  }

  test("a match on a stream of an array, a sorted set or a user collection is no contains") {
    assertEquals(java(streamed(Array, "exists", equalTo("c"))), Vector.empty)
    assertEquals(java(streamed(Sorted, "exists", equalTo("c"))), Vector.empty)
    assertEquals(java(streamed(named("demo.Mine"), "exists", equalTo("c"))), Vector.empty)
  }

  test("the laws that read as Java are named") {
    assertEquals(
      Laws.javaIds.toSet,
      Set(
        "size-to-is-empty",
        "size-to-non-empty",
        "find-to-exists",
        "sorted-min-max",
        "stream-of",
        "stream-join",
        "java9-collection-factory",
        "none-check",
        "exists-to-contains",
        "forall-to-not-contains"
      )
    )
    assert(Laws.javaIds.forall(Laws.ids.contains))
  }

  test("a law without a Java form reports nothing as Java") {
    assertEquals(java(chain("values", on(ListOf, "filter", fn("p")), on(ListOf, "size"))), Vector.empty)
  }
}
