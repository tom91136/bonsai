package net.kurobako.bonsai.collections

import Fixtures.*

class IdiomLawsSuite extends munit.FunSuite {

  private def rewrite(chain: Chain, id: String, message: String, expected: String): Unit =
    Laws.outcomes(chain).filter(_.lint) match {
      case Vector(found: Outcome.Rewrite) =>
        assertEquals(found.law, id)
        assertEquals(found.message, message)
        assertEquals((found.from, found.until), (0, chain.steps.size))
        assertEquals(found.render(chain.source.text), expected)
      case other => fail(s"expected one rewrite, got $other")
    }

  private def report(chain: Chain, id: String, message: String): Unit =
    Laws.outcomes(chain).filter(_.lint) match {
      case Vector(found: Outcome.Report) =>
        assertEquals(found.law, id)
        assertEquals(found.message, message)
      case other => fail(s"expected one report, got $other")
    }

  private def silent(chain: Chain): Unit = assertEquals(Laws.outcomes(chain).map(_.law), Vector.empty[String])

  private val unknown = Strict.copy(strict = false, ordered = false)

  test("exists of an equality becomes contains") {
    rewrite(
      chain("list", on(ListOf, "exists", equalTo("2"))),
      "exists-to-contains",
      "use contains for equality with a constant",
      "list.contains(2)"
    )
  }

  test("forall of an inequality becomes a negated contains") {
    rewrite(
      chain("list", on(VectorOf, "forall", differsFrom("2"))),
      "forall-to-not-contains",
      "use contains for equality with a constant",
      "!(list.contains(2))"
    )
  }

  test("exists and forall need the matching comparison on a known strict sequence") {
    silent(chain("list", on(ListOf, "exists", differsFrom("2"))))
    silent(chain("list", on(ListOf, "forall", equalTo("2"))))
    silent(chain("list", on(ListOf, "exists", fn("_ > 2"))))
    silent(chain("list", on(named("immutable.LazyList", Lazy), "exists", equalTo("2"))))
    silent(chain("list", on(Strict, "exists", equalTo("2"))))
    silent(chain("list", on(User, "exists", equalTo("2"))))
  }

  private val hashed = List("immutable.Set", "immutable.HashSet", "mutable.Set", "mutable.HashSet", "Set")

  for (name <- hashed)
    test(s"exists, forall and find of an equality on a $name become contains") {
      val set = named(name, Unique)
      rewrite(
        chain("set", on(set, "exists", equalTo("2"))),
        "exists-to-contains",
        "use contains for equality with a constant",
        "set.contains(2)"
      )
      rewrite(
        chain("set", on(set, "forall", differsFrom("2"))),
        "forall-to-not-contains",
        "use contains for equality with a constant",
        "!(set.contains(2))"
      )
      rewrite(
        chain("set", on(set, "find", equalTo("2"))),
        "exists-to-contains",
        "use contains for equality with a constant",
        "Option.when(set.contains(2))(2)"
      )
    }

  test("a count of an equality compared with zero becomes contains on sets and sequences") {
    for (kind <- List(SetOf, ListOf, VectorOf)) {
      rewrite(
        chain("xs", Step("countNonZero", Vector(equalTo("2")), kind, User)),
        "exists-to-contains",
        "use contains for equality with a constant",
        "xs.contains(2)"
      )
      rewrite(
        chain("xs", Step("countZero", Vector(equalTo("2")), kind, User)),
        "forall-to-not-contains",
        "use contains for equality with a constant",
        "!xs.contains(2)"
      )
    }
  }

  test("the keys of a hashed map searched for an equality become contains on the map") {
    for (name <- List("immutable.Map", "immutable.HashMap", "mutable.Map", "Map")) {
      val keys = Step("keys", Vector.empty, named(name, Dict), Lazy)
      rewrite(
        chain("map", keys, Step("exists", Vector(equalTo("2")), Lazy, User)),
        "exists-to-contains",
        "use contains for equality with a constant",
        "map.contains(2)"
      )
      rewrite(
        chain("map", keys, Step("forall", Vector(differsFrom("2")), Lazy, User)),
        "forall-to-not-contains",
        "use contains for equality with a constant",
        "!(map.contains(2))"
      )
    }
  }

  test("equality searches of anything that is not hashed, or that compares otherwise, stay") {
    val sorted     = named("immutable.TreeSet", Unique)
    val listed     = named("immutable.ListSet", Unique)
    val sortedKeys = Step("keys", Vector.empty, named("immutable.TreeMap", Dict), Lazy)
    val unrelated  = Fn.Compares("_ == other", equal = true, constant = "other", assignable = false)
    val opposite   = Fn.Compares("_ != other", equal = false, constant = "other", assignable = false)
    silent(chain("set", on(sorted, "exists", equalTo("2"))))
    silent(chain("set", on(sorted, "find", equalTo("2"))))
    silent(chain("set", on(listed, "exists", equalTo("2"))))
    silent(chain("set", on(SetOf, "exists", unrelated)))
    silent(chain("set", on(SetOf, "forall", opposite)))
    silent(chain("set", on(SetOf, "find", unrelated)))
    silent(chain("set", on(SetOf, "find", differsFrom("2"))))
    silent(chain("set", on(SetOf, "exists", differsFrom("2"))))
    silent(chain("set", on(SetOf, "forall", equalTo("2"))))
    silent(chain("set", on(SetOf, "exists", fn("_ > 2"))))
    silent(chain("set", Step("countNonZero", Vector(unrelated), SetOf, User)))
    silent(chain("set", Step("countNonZero", Vector(fn("_ > 2")), SetOf, User)))
    silent(chain("map", sortedKeys, Step("exists", Vector(equalTo("2")), Lazy, User)))
    silent(chain("map", Step("keys", Vector.empty, MapOf, Lazy), Step("exists", Vector(unrelated), Lazy, User)))
    silent(chain("map", on(MapOf, "exists", equalTo("2"))))
    silent(chain("list", Step("countNonZero", Vector(unrelated), Strict, User)))
    silent(chain("list", Step("countNonZero", Vector(equalTo("2")), User, User)))
  }

  test("a find of an equality on a list is left to the cost model") {
    silent(chain("list", on(ListOf, "find", equalTo("2"))))
  }

  test("filter by the membership of another set becomes intersect") {
    rewrite(
      chain("set", on(SetOf, "filter", member("other"))),
      "filter-contains-to-intersect",
      "use intersect instead of filter and contains",
      "set.intersect(other)"
    )
  }

  test("filter by membership needs a set receiver and a membership test") {
    silent(chain("list", on(ListOf, "filter", member("other"))))
    silent(chain("set", on(SetOf, "filter", fn("_ == 1"))))
  }

  test("get then getOrElse on a strict map becomes getOrElse with the key") {
    rewrite(
      chain(
        "map",
        Step("get", Vector(fn("key")), MapOf, OptionOf),
        Step("getOrElse", Vector(fn("0")), OptionOf, Strict)
      ),
      "get-get-or-else",
      "use getOrElse(key, default)",
      "map.getOrElse(key, 0)"
    )
  }

  test("get then getOrElse needs a strict immutable map") {
    val mutable = named("mutable.Map", Dict)
    silent(chain(
      "map",
      Step("get", Vector(fn("key")), mutable, OptionOf),
      Step("getOrElse", Vector(fn("0")), OptionOf, Strict)
    ))
  }

  test("drop then take with constant bounds becomes slice") {
    rewrite(
      chain("list", on(ListOf, "drop", literal(1)), on(ListOf, "take", literal(2))),
      "drop-take-to-slice",
      "use slice for constant nonnegative bounds",
      "list.slice(1, 3)"
    )
  }

  test("take then drop with constant bounds becomes slice") {
    rewrite(
      chain("list", on(ListOf, "take", literal(2)), on(ListOf, "drop", literal(1))),
      "take-drop-to-slice",
      "use slice for constant nonnegative bounds",
      "list.slice(1, 2)"
    )
  }

  test("slices need constant nonnegative bounds that do not overflow on a known strict sequence") {
    def dropTake(drop: Arg, take: Arg, kind: Kind = ListOf) =
      chain("list", on(kind, "drop", drop), on(kind, "take", take))
    silent(dropTake(literal(-1), literal(2)))
    silent(dropTake(literal(1), literal(-1)))
    silent(dropTake(literal(1), literal(Int.MaxValue)))
    silent(dropTake(fn("next()"), literal(2)))
    silent(dropTake(literal(1), fn("next()")))
    silent(dropTake(literal(1), literal(2), named("immutable.LazyList", Lazy)))
    silent(dropTake(literal(1), literal(2), User))
    silent(dropTake(literal(1), literal(2), SetOf))
  }

  test("drop of zero is the collection itself") {
    rewrite(
      chain("list", on(RangeOf, "drop", literal(0))),
      "drop-zero",
      "drop(0) returns the collection itself",
      "list"
    )
  }

  test("take of zero is reported as always empty") {
    report(
      chain("list", on(ListOf, "take", literal(0))),
      "take-zero",
      "take(0) is always empty; return an empty collection or check the bound"
    )
  }

  test("drop and take of zero need a known strict sequence") {
    silent(chain("list", on(Strict, "drop", literal(0))))
    silent(chain("list", on(SetOf, "take", literal(0))))
    silent(chain("list", on(ListOf, "drop", literal(1))))
  }

  test("size of an array or a string becomes length") {
    for (kind <- List(ArrayOf, StringOf))
      rewrite(
        chain("values", on(kind, "size")),
        "size-to-length",
        "use length on an array or string",
        "values.length"
      )
  }

  test("size of anything else stays") {
    silent(chain("values", on(ListOf, "size")))
    silent(chain("values", on(Strict, "size")))
  }

  test("a size compared with zero becomes an emptiness check") {
    rewrite(
      chain("list", on(ListOf, "sizeIsZero")),
      "size-to-is-empty",
      "use isEmpty instead of a size comparison with zero",
      "list.isEmpty"
    )
    rewrite(
      chain("list", on(ListOf, "sizeNonZero")),
      "size-to-non-empty",
      "use nonEmpty instead of a size comparison with zero",
      "list.nonEmpty"
    )
  }

  test("a string size or a size of another type compared with zero stays") {
    silent(chain("text", on(StringOf, "sizeNonZero")))
    silent(chain("custom", on(User, "sizeNonZero")))
  }

  test("filter then a size compared with zero becomes exists or forall") {
    val cases = List(
      ("filter", "sizeIsZero", "size-to-is-empty", "exists", "filter", "!list.exists(_ > 0)"),
      ("filterNot", "sizeIsZero", "size-to-is-empty", "forall", "filterNot", "list.forall(_ > 0)"),
      ("filter", "sizeNonZero", "size-to-non-empty", "exists", "filter", "list.exists(_ > 0)"),
      ("filterNot", "sizeNonZero", "size-to-non-empty", "forall", "filterNot", "!list.forall(_ > 0)")
    )
    for ((filter, size, id, quantifier, written, expected) <- cases)
      rewrite(
        chain("list", on(ListOf, filter, fn("_ > 0")), on(ListOf, size)),
        id,
        s"use $quantifier instead of $written and a size comparison with zero",
        expected
      )
  }

  test("filter then a size that is counted is still a count") {
    rewrite(
      chain("list", on(ListOf, "filter", fn("_ > 0")), on(ListOf, "size")),
      "filter-to-count",
      "use count for filter and size",
      "list.count(_ > 0)"
    )
  }

  test("filter after sorted is reported") {
    report(
      chain("list", on(ListOf, "sorted"), on(ListOf, "filter", fn("_ > 1"))),
      "filter-before-sorted",
      "prefer filter(...).sorted when changing predicate and ordering evaluation is safe"
    )
  }

  test("filter before sorted and other methods after sorted are left alone") {
    silent(chain("list", on(ListOf, "filter", fn("_ > 1")), on(ListOf, "sorted")))
    silent(chain("list", on(ListOf, "sorted"), on(ListOf, "map", fn("_ + 1"))))
    silent(chain("list", on(User, "sorted"), on(User, "filter", fn("_ > 1"))))
  }

  test("a conversion to the type the receiver has is removed") {
    val cases = List(
      ("toSeq", SeqOf),
      ("toList", ListOf),
      ("toVector", VectorOf),
      ("toMap", MapOf),
      ("toSet", SetOf)
    )
    for ((conversion, kind) <- cases)
      rewrite(
        chain("values", on(kind, conversion)),
        "redundant-collection-conversion",
        "remove the identity collection conversion",
        "values"
      )
  }

  test("a conversion to another type stays") {
    silent(chain("values", on(ListOf, "toSet")))
    silent(chain("values", on(ArrayOf, "toSeq")))
    silent(chain("values", on(named("mutable.ArrayBuffer"), "toSeq")))
    silent(chain("values", on(Strict, "toList")))
    silent(chain("values", on(named("immutable.LazyList", Lazy), "toList")))
    silent(chain("values", on(User, "toList")))
  }

  test("a conversion with a type argument that changes the element type stays") {
    silent(chain("values", Step("toSet", Vector.empty, SetOf, SetOf, typed = true, retyped = true)))
    assertEquals(
      Laws.outcomes(chain("values", Step("toSet", Vector.empty, SetOf, SetOf, typed = true))).map(_.law),
      Vector("redundant-collection-conversion")
    )
  }

  test("toSeq of a strict sequence is removed in an assertion only") {
    val steps = Vector(on(ListOf, "toSeq"))
    silent(Chain(fn("values"), steps))
    rewrite(
      Chain(fn("values"), steps, asserted = true),
      "redundant-collection-conversion",
      "remove the identity collection conversion",
      "values"
    )
    silent(Chain(fn("values"), Vector(on(ArrayOf, "toSeq")), asserted = true))
  }

  test("a step with type arguments is reported where it would be rewritten") {
    val typed = Step("map", Vector(fn("_.toList")), Strict, Strict, typed = true)
    report(
      chain("lists", typed, step("flatten")),
      "map-flatten-to-flat-map",
      "use flatMap for map and flatten"
    )
  }

  test("a call written for a for-comprehension is reported where it would be rewritten") {
    val generated = Step("map", Vector(fn("_.toList")), Strict, Strict, desugared = true)
    report(
      chain("lists", generated, step("flatten")),
      "map-flatten-to-flat-map",
      "use flatMap for map and flatten"
    )
    report(
      chain("option", Step("map", Vector(same), OptionOf, OptionOf, desugared = true)),
      "redundant-identity-map",
      "map of identity is the option itself"
    )
  }

  test("a guard then a yield of a for-comprehension is not a collect") {
    val guard   = Step("filter", Vector(reading(pair)), Strict, Strict.copy(strict = false), desugared = true)
    val yielded = Step("map", Vector(matching(pair)), Strict.copy(strict = false), Strict, desugared = true)
    silent(chain("list", guard, yielded))
    silent(Chain(fn("list"), Vector(guard, yielded), hot = true))
  }

  test("a filter then a map that take the same shape apart is a collect") {
    val message = "filter then map is a collect; a partial function with a guard filters and maps in one pass"
    val tested  = Fn.Facts("_.isInstanceOf[Foo]", inspects = Some("_: Foo"))
    for (
      (filter, map) <- List(reading(pair) -> matching(pair), matching(pair) -> matching(pair), tested -> casting("Foo"))
    )
      report(chain("list", step("filter", filter), step("map", map)), "filter-map-to-collect", message)
  }

  test("a filter then a map that take different shapes apart is a collect only in a hot class") {
    val steps = Vector(step("filter", fn("_.nonEmpty")), step("map", fn("parseLong")))
    silent(Chain(fn("list"), steps))
    silent(chain("list", step("filter", reading(pair)), step("map", matching("(_, _, _)"))))
    silent(chain("list", step("filter", reading(pair)), step("map", fn("_._2"))))
    assertEquals(Laws.outcomes(Chain(fn("list"), steps, hot = true)).map(_.law), Vector("filter-map-to-collect"))
  }

  test("a filter then a map on an option, a view or an iterator is not a collect, even in a hot class") {
    for (kind <- Seq(OptionOf, Opt, Lazy)) {
      val steps = Vector(on(kind, "filter", reading(pair)), on(kind, "map", matching(pair)))
      silent(Chain(fn("value"), steps))
      silent(Chain(fn("value"), steps, hot = true))
    }
  }

  test("a guard then an identity yield is not a collect") {
    val guard = Step("filter", Vector(fn("_ > 0")), Strict, Strict.copy(strict = false), desugared = true)
    val kept  = Step("map", Vector(same), Strict.copy(strict = false), Strict, desugared = true)
    silent(chain("list", guard, kept))
    report(
      chain(
        "option",
        guard.copy(receiver = OptionOf, result = OptionOf.copy(strict = false)),
        kept.copy(receiver = OptionOf.copy(strict = false), result = OptionOf)
      ),
      "redundant-identity-map",
      "map of identity is the option itself"
    )
  }

  test("a for-comprehension that yields a component of a map is reported in its own words") {
    val yielded = Step("map", Vector(projection(1)), MapOf, Strict, desugared = true)
    report(
      chain("map", yielded),
      "map-to-keys",
      "use keys for a for-comprehension that yields the first component"
    )
    report(
      chain("map", yielded.copy(args = Vector(projection(2)))),
      "map-to-values",
      "use values for a for-comprehension that yields the second component"
    )
    rewrite(
      chain("map", yielded.copy(desugared = false)),
      "map-to-keys",
      "use keys for map of the first component",
      "map.keys"
    )
  }

  test("nested generators that yield the elements of the elements are reported as a flatten") {
    val nest = Step("flatMap", Vector(same), Strict, Strict, desugared = true)
    report(
      chain("lists", nest),
      "flat-map-identity-to-flatten",
      "use flatten for generators that yield the elements of the elements"
    )
    rewrite(
      chain("lists", nest.copy(desugared = false)),
      "flat-map-identity-to-flatten",
      "use flatten for flatMap of identity",
      "lists.flatten"
    )
  }

  test("get then get becomes apply") {
    rewrite(
      chain("map", Step("get", Vector(fn("key")), MapOf, OptionOf), Step("get", Vector.empty, OptionOf, Strict)),
      "map-get-to-apply",
      "use the map as a function instead of get and get",
      "map(key)"
    )
  }

  test("get then an emptiness check becomes contains") {
    val cases = List(
      ("isEmpty", "!map.contains(key)"),
      ("nonEmpty", "map.contains(key)"),
      ("isDefined", "map.contains(key)")
    )
    for ((check, expected) <- cases)
      rewrite(
        chain("map", Step("get", Vector(fn("key")), MapOf, OptionOf), Step(check, Vector.empty, OptionOf, Strict)),
        "map-get-emptiness",
        "use contains instead of get and an emptiness check",
        expected
      )
  }

  test("get then an emptiness check stays on maps keyed by primitives, where contains boxes") {
    for (name <- List("immutable.LongMap", "immutable.IntMap"))
      silent(
        chain(
          "map",
          Step("get", Vector(fn("key")), named(name, Dict), OptionOf),
          Step("isEmpty", Vector.empty, OptionOf, Strict)
        )
      )
  }

  test("get then get or an emptiness check needs a map") {
    silent(chain("map", Step("get", Vector(fn("key")), User, OptionOf), Step("get", Vector.empty, OptionOf, Strict)))
    silent(chain(
      "map",
      Step("get", Vector(fn("key")), ListOf, OptionOf),
      Step("isEmpty", Vector.empty, OptionOf, Strict)
    ))
  }

  test("lift on a map becomes get") {
    rewrite(
      chain("map", Step("lift", Vector(fn("key")), MapOf, OptionOf)),
      "map-lift-to-get",
      "use get for lift on a map",
      "map.get(key)"
    )
  }

  test("headOption and lastOption of an option are the option") {
    for (method <- List("headOption", "lastOption"))
      rewrite(
        chain("option", Step(method, Vector.empty, OptionOf, OptionOf)),
        "redundant-option-iteration",
        s"Option.$method is the option itself",
        "option"
      )
  }

  test("map then getOrElse on an option is reported as a fold") {
    report(
      chain(
        "option",
        Step("map", Vector(fn("_ + 1")), OptionOf, OptionOf),
        Step("getOrElse", Vector(fn("0")), OptionOf, Strict)
      ),
      "map-get-or-else-to-fold",
      "use fold for map and getOrElse"
    )
  }

  private def folding(default: Arg, function: Arg): Chain =
    chain("option", Step("fold", Vector(default, function), OptionOf, Strict))

  private def spelled(chain: Chain): Vector[String] = Laws.outcomes(chain, OptionFold.MapGetOrElse).map(_.law)

  test("the default spelling reports map then getOrElse and leaves fold alone") {
    val mapped = chain(
      "option",
      Step("map", Vector(fn("_ + 1")), OptionOf, OptionOf),
      Step("getOrElse", Vector(fn("0")), OptionOf, Strict)
    )
    assertEquals(Laws.outcomes(mapped, OptionFold.Fold).map(_.law), Vector("map-get-or-else-to-fold"))
    assertEquals(Laws.outcomes(mapped).map(_.law), Vector("map-get-or-else-to-fold"))
    assertEquals(
      Laws.outcomes(folding(valued("0", "scala.Int"), giving("_ + 1", "scala.Int"))).map(_.law),
      Vector.empty[String]
    )
  }

  test("the map and getOrElse spelling reports fold and leaves map then getOrElse alone") {
    val mapped = chain(
      "option",
      Step("map", Vector(fn("_ + 1")), OptionOf, OptionOf),
      Step("getOrElse", Vector(fn("0")), OptionOf, Strict)
    )
    assertEquals(spelled(mapped), Vector.empty[String])
  }

  test("fold of a default and a function of the same type becomes map then getOrElse") {
    val folded = folding(valued("\"\"", "java.lang.String"), giving("_.name", "java.lang.String"))
    Laws.outcomes(folded, OptionFold.MapGetOrElse).filter(_.lint) match {
      case Vector(found: Outcome.Rewrite) =>
        assertEquals(found.law, "fold-to-map-get-or-else")
        assertEquals(found.message, "use map and getOrElse for fold")
        assertEquals(found.render("option"), "option.map(_.name).getOrElse(\"\")")
        assert(!found.analysis)
        assertEquals(found.java, None)
      case other => fail(s"expected one rewrite, got $other")
    }
  }

  test("fold is only reported where the types of the default and the function differ or are unknown") {
    def reported(default: Arg, function: Arg): Unit =
      Laws.outcomes(folding(default, function), OptionFold.MapGetOrElse).filter(_.lint) match {
        case Vector(found: Outcome.Report) => assertEquals(found.law, "fold-to-map-get-or-else")
        case other                         => fail(s"expected one report, got $other")
      }
    reported(valued("None", "scala.None$"), giving("Some(_)", "scala.Some[scala.Int]"))
    reported(valued("0", "scala.Long"), giving("_ + 1", "scala.Long"))
    reported(fn("0"), giving("_ + 1", "scala.Int"))
    reported(valued("0", "scala.Int"), fn("_ + 1"))
  }

  test("fold of identity is not rewritten, and fold needs an option") {
    assertEquals(spelled(folding(valued("0", "scala.Int"), same)), Vector.empty[String])
    assertEquals(
      spelled(chain("list", on(ListOf, "fold", valued("0", "scala.Int"), giving("_ + 1", "scala.Int")))),
      Vector.empty[String]
    )
  }

  test("map of identity then getOrElse is an identity map, not a fold") {
    val steps = chain(
      "option",
      Step("map", Vector(same), OptionOf, OptionOf),
      Step("getOrElse", Vector(fn("0")), OptionOf, Strict)
    )
    assertEquals(Laws.outcomes(steps).map(_.law), Vector("redundant-identity-map"))
  }

  test("map then getOrElse of a value that never returns is not a fold") {
    val steps = (default: Arg) =>
      chain(
        "option",
        Step("map", Vector(fn("_ + 1")), OptionOf, OptionOf),
        Step("getOrElse", Vector(default), OptionOf, Strict)
      )
    silent(steps(diverging("sys.error(\"none\")")))
    assertEquals(Laws.outcomes(steps(fn("0"))).map(_.law), Vector("map-get-or-else-to-fold"))
  }

  test("map of identity that yields the last generator of a for-comprehension is not an identity map") {
    val yielded = Step("map", Vector(same), OptionOf, OptionOf, desugared = true)
    silent(Chain(fn("option"), Vector(yielded), yielded = true))
    assertEquals(Laws.outcomes(chain("option", yielded)).map(_.law), Vector("redundant-identity-map"))
  }

  test("map of identity written with a type argument that widens is not an identity map") {
    silent(chain("option", Step("map", Vector(same), OptionOf, OptionOf, typed = true, retyped = true)))
    assertEquals(
      Laws.outcomes(chain("option", Step("map", Vector(same), OptionOf, OptionOf, typed = true))).map(_.law),
      Vector("redundant-identity-map")
    )
  }

  test("map of identity on an option is the option") {
    rewrite(
      chain("option", Step("map", Vector(same), OptionOf, OptionOf)),
      "redundant-identity-map",
      "map of identity is the option itself",
      "option"
    )
  }

  test("map of identity needs an option and an identity function") {
    silent(chain("list", on(ListOf, "map", same)))
    silent(chain("custom", on(User, "map", same)))
    silent(chain("option", Step("map", Vector(fn("_ + 1")), OptionOf, OptionOf)))
  }

  test("map then getOrElse needs an option") {
    silent(chain("list", on(ListOf, "map", fn("_ + 1")), on(ListOf, "getOrElse", fn("0"))))
    silent(chain("custom", on(User, "map", fn("_ + 1")), on(User, "getOrElse", fn("0"))))
  }

  test("equality with Some becomes contains") {
    rewrite(
      chain("option", Step("equalsSome", Vector(fn("1")), OptionOf, Strict)),
      "some-to-contains",
      "use contains for an equality check with Some",
      "option.contains(1)"
    )
    rewrite(
      chain("option", Step("notEqualsSome", Vector(fn("1")), OptionOf, Strict)),
      "some-to-contains",
      "use contains for an equality check with Some",
      "!option.contains(1)"
    )
  }

  test("comparison with None becomes an emptiness check") {
    rewrite(
      chain("option", Step("isNone", Vector.empty, OptionOf, Strict)),
      "none-check",
      "use isEmpty for a check against None",
      "option.isEmpty"
    )
    rewrite(
      chain("option", Step("isSome", Vector.empty, OptionOf, Strict)),
      "none-check",
      "use isDefined for a check against None",
      "option.isDefined"
    )
  }

  test("a comparison with Some or None needs an option") {
    silent(chain("list", on(ListOf, "isNone")))
    silent(chain("list", on(ListOf, "equalsSome", fn("1"))))
  }

  test("find compared with None is still an exists") {
    rewrite(
      chain("list", on(ListOf, "find", fn("_ > 0")), Step("isSome", Vector.empty, OptionOf, Strict)),
      "find-to-exists",
      "use exists for find and an emptiness check",
      "list.exists(_ > 0)"
    )
    rewrite(
      chain("list", on(ListOf, "find", fn("_ > 0")), Step("isNone", Vector.empty, OptionOf, Strict)),
      "find-to-exists",
      "use exists for find and an emptiness check",
      "!list.exists(_ > 0)"
    )
  }

  test("lift of the first and last index becomes headOption and lastOption") {
    rewrite(
      chain("list", on(ListOf, "lift", literal(0))),
      "head-or-last-option",
      "use headOption for lift of the first index",
      "list.headOption"
    )
    rewrite(
      chain("list", on(VectorOf, "liftLast")),
      "head-or-last-option",
      "use lastOption for lift of the last index",
      "list.lastOption"
    )
  }

  test("lift of another index stays") {
    silent(chain("list", on(ListOf, "lift", literal(1))))
    silent(chain("list", on(Strict, "lift", literal(0))))
  }

  test("the first and last element by index are head and last of a linear sequence") {
    rewrite(
      chain("list", on(ListOf, "apply", literal(0))),
      "zero-index-to-head",
      "use head for the first element",
      "list.head"
    )
    rewrite(
      chain("list", on(SeqOf, "applyLast")),
      "last-index-to-last",
      "use last for the last element",
      "list.last"
    )
  }

  test("indexing an indexed sequence or at another position stays") {
    silent(chain("list", on(VectorOf, "apply", literal(0))))
    silent(chain("list", on(VectorOf, "applyLast")))
    silent(chain("list", on(ArrayOf, "apply", literal(0))))
    silent(chain("list", on(ListOf, "apply", literal(1))))
    silent(chain("list", on(ListOf, "apply", fn("i"))))
  }

  test("fusion and collect are rewritten or reported by the compiler and reported by the analysis") {
    val pure = Laws.outcomes(chain("list", step("map", fn("_ + 1")), step("map", fn("_ * 2"))))
    assertEquals(pure.map(outcome => (outcome.lint, outcome.analysis)), Vector((true, true)))
    val effects = Laws.outcomes(chain("list", step("map", effect("f")), step("map", fn("_ * 2"))))
    assertEquals(effects.map(outcome => (outcome.lint, outcome.analysis)), Vector((false, true)))
    val collect = Laws.outcomes(chain("list", step("filter", reading(pair)), step("map", matching(pair))))
    assertEquals(collect.map(outcome => (outcome.lint, outcome.analysis)), Vector((true, true)))
  }

  test("advice to use a view belongs to the analysis") {
    val advice = Laws.outcomes(chain("list", step("map", fn("f")), step("filter", fn("p"))))
    assert(advice.nonEmpty && advice.forall(outcome => !outcome.lint && outcome.analysis))
  }

  test("a law yields the steps to a longer law, so the patches of both never conflict") {
    val outcomes =
      Laws.outcomes(chain("list", on(ListOf, "find", fn("_ > 0")), Step("isNone", Vector.empty, OptionOf, Strict)))
    assertEquals(outcomes.map(_.law), Vector("find-to-exists"))
  }

  test("an unknown library receiver reports the idioms of its methods") {
    rewrite(
      chain("values", on(unknown, "sizeNonZero")),
      "size-to-non-empty",
      "use nonEmpty instead of a size comparison with zero",
      "values.nonEmpty"
    )
  }

}
