package net.kurobako.bonsai.collections

import Fixtures.*

class ExactLawsSuite extends munit.FunSuite {

  private def exact(name: String, id: String, message: String, expected: String)(chain: => Chain): Unit =
    test(name) {
      val c = chain
      Laws.outcomes(c) match {
        case Vector(r: Outcome.Rewrite) =>
          assertEquals(r.law, id)
          assertEquals(r.message, message)
          assertEquals((r.from, r.until), (0, c.steps.size))
          assertEquals(r.render(c.source.text), expected)
        case other => fail(s"expected one rewrite, got $other")
      }
    }

  exact(
    "map then flatten becomes flatMap",
    "map-flatten-to-flat-map",
    "use flatMap for map and flatten",
    "lists.flatMap(_.map(_ * 2))"
  ) {
    chain("lists", step("map", fn("_.map(_ * 2)")), step("flatten"))
  }

  exact(
    "flatMap of identity becomes flatten",
    "flat-map-identity-to-flatten",
    "use flatten for flatMap of identity",
    "lists.flatten"
  ) {
    chain("lists", step("flatMap", same))
  }

  for (size <- List("size", "length"))
    exact(s"filter then $size becomes count", "filter-to-count", "use count for filter and size", "list.count(_ > 0)") {
      chain("list", step("filter", fn("_ > 0")), step(size))
    }

  for (check <- List("isDefined", "nonEmpty"))
    exact(
      s"find then $check becomes exists",
      "find-to-exists",
      "use exists for find and an emptiness check",
      "list.exists(_ > 0)"
    ) {
      chain("list", step("find", fn("_ > 0")), step(check))
    }

  exact(
    "find then isEmpty becomes a negated exists",
    "find-to-exists",
    "use exists for find and an emptiness check",
    "!list.exists(_ > 0)"
  ) {
    chain("list", step("find", fn("_ > 0")), step("isEmpty"))
  }

  exact(
    "reverse take reverse becomes takeRight",
    "reverse-take-to-take-right",
    "use takeRight for reverse and take and reverse",
    "list.takeRight(2)"
  ) {
    chain("list", step("reverse"), step("take", fn("2")), step("reverse"))
  }

  exact(
    "map of the first component on a map becomes keys",
    "map-to-keys",
    "use keys for map of the first component",
    "map.keys"
  ) {
    chain("map", Step("map", Vector(projection(1)), Dict, Strict))
  }

  exact(
    "map of the second component on a map becomes values",
    "map-to-values",
    "use values for map of the second component",
    "map.values"
  ) {
    chain("map", Step("map", Vector(projection(2)), Dict, Strict))
  }

  for (back <- List("toSeq", "toList"))
    exact(
      s"toSet then $back becomes distinct",
      "to-set-and-back-to-distinct",
      s"use distinct for toSet then $back",
      "list.distinct"
    ) {
      chain("list", Step("toSet", Vector.empty, Strict, Unique), Step(back, Vector.empty, Unique, Strict))
    }

  exact(
    "zip of the own indices becomes zipWithIndex",
    "zip-indices-to-zip-with-index",
    "use zipWithIndex for zip of the indices",
    "list.zipWithIndex"
  ) {
    chain("list", step("zip", fn("list.indices")))
  }

  exact(
    "a range until the size becomes indices",
    "range-to-indices",
    "use indices for a range over the size",
    "list.indices"
  ) {
    chain("list", step("untilSize"))
  }

  exact(
    "a range to the last index becomes indices",
    "range-to-indices",
    "use indices for a range over the size",
    "list.indices"
  ) {
    chain("list", step("toLastIndex"))
  }

  for (field <- List("_1", "_2"))
    exact(
      s"unzip then $field becomes map",
      "unzip-to-map",
      s"use map(_.$field) for unzip and a component",
      s"pairs.map(_.$field)"
    ) {
      chain("pairs", step("unzip"), step(field))
    }

  exact("unzip3 then _2 becomes map", "unzip-to-map", "use map(_._2) for unzip and a component", "triples.map(_._2)") {
    chain("triples", step("unzip3"), step("_2"))
  }

  exact(
    "a fold of and becomes forall",
    "fold-true-and-to-forall",
    "use forall for a fold that ands a predicate",
    "list.forall(value => value > 0)"
  ) {
    chain("list", step("foldTrueAnd", fn("value => value > 0")))
  }

  test("a rewrite renders on the text of the receiver it is given") {
    val found = rewrites(chain("list", step("map"), step("filter", fn("_ > 0")), step("size")))
    assertEquals(found.map(r => (r.from, r.until, r.render("xs.map(f)"))), Vector((1, 3, "xs.map(f).count(_ > 0)")))
  }

  test("a rewrite window sits among other steps") {
    val found =
      rewrites(chain("list", step("sorted"), step("reverse"), step("take", fn("2")), step("reverse"), step("toList")))
    assertEquals(found.map(r => (r.law, r.from, r.until)), Vector(("reverse-take-to-take-right", 1, 4)))
  }

  private val sequential: List[(String, String, Kind => Chain)] = List(
    ("filter then size", "filter-to-count", k => chain("list", on(k, "filter", fn("_ > 0")), on(k, "size"))),
    ("find then isDefined", "find-to-exists", k => chain("list", on(k, "find", fn("_ > 0")), on(k, "isDefined"))),
    ("map then flatten", "map-flatten-to-flat-map", k => chain("list", on(k, "map", fn("f")), on(k, "flatten"))),
    ("flatMap of identity", "flat-map-identity-to-flatten", k => chain("list", on(k, "flatMap", same))),
    (
      "reverse take reverse",
      "reverse-take-to-take-right",
      k => chain("list", on(k, "reverse"), on(k, "take", fn("2")), on(k, "reverse"))
    ),
    ("unzip then a component", "unzip-to-map", k => chain("pairs", on(k, "unzip"), on(k, "_1"))),
    ("a fold of and", "fold-true-and-to-forall", k => chain("list", on(k, "foldTrueAnd", fn("v => v > 0")))),
    ("filter then isEmpty", "filter-emptiness", k => chain("list", on(k, "filter", fn("_ > 0")), on(k, "isEmpty"))),
    (
      "filter then nonEmpty",
      "filter-emptiness",
      k => chain("list", on(k, "filterNot", fn("_ > 0")), on(k, "nonEmpty"))
    ),
    (
      "filter then headOption",
      "filter-head-option",
      k => chain("list", on(k, "filter", fn("_ > 0")), on(k, "headOption"))
    ),
    (
      "filter then lastOption",
      "filter-head-option",
      k => chain("list", on(k, "filter", fn("_ > 0")), on(k, "lastOption"))
    ),
    (
      "collect then headOption",
      "collect-head-option",
      k => chain("list", on(k, "collect", fn("pf")), on(k, "headOption"))
    ),
    (
      "filter then map",
      "filter-map-to-collect",
      k => chain("list", on(k, "filter", fn("_ > 0")), on(k, "map", fn("_ * 2")))
    ),
    (
      "map then contains",
      "map-contains",
      k => chain("list", on(k, "map", fn("_ > 0")), on(k, "contains", literal(true)))
    ),
    ("reverse then find", "reverse-find", k => chain("list", on(k, "reverse"), on(k, "find", fn("_ > 0")))),
    ("sorted then head", "sorted-min-max", k => chain("list", on(k, "sorted"), on(k, "head"))),
    ("sorted then last", "sorted-min-max", k => chain("list", on(k, "sorted"), on(k, "last"))),
    (
      "sortBy then headOption",
      "sorted-min-max",
      k => chain("list", on(k, "sortBy", fn("_.length")), on(k, "headOption"))
    ),
    (
      "map of the first component",
      "map-to-keys",
      k => chain("map", Step("map", Vector(projection(1)), k.copy(ordered = false, keyed = true), Strict))
    )
  )

  private val strictOnly: List[(String, Kind => Chain)] = List(
    "toSet then toSeq" ->
      (k => chain("list", Step("toSet", Vector.empty, k, Unique), Step("toSeq", Vector.empty, Unique, k))),
    "zip of the indices"    -> (k => chain("list", on(k, "zip", fn("list.indices")))),
    "a range over the size" -> (k => chain("list", on(k, "untilSize")))
  )

  private val Iter     = Kind(library = true, strict = false)
  private val LazyList = Kind(library = true, strict = false)

  for ((name, _, build) <- sequential)
    test(s"$name on a user type gives nothing") {
      assertEquals(Laws.outcomes(build(User)), Vector.empty)
    }

  for ((name, build) <- strictOnly)
    test(s"$name on a user type gives nothing") {
      assertEquals(Laws.outcomes(build(User)), Vector.empty)
    }

  for {
    (name, build) <- strictOnly
    (label, kind) <- List("a view" -> Lazy, "a lazy list" -> LazyList, "an iterator" -> Iter)
  }
    test(s"$name on $label gives nothing") {
      assertEquals(Laws.outcomes(build(kind)), Vector.empty)
    }

  for {
    (name, id, build) <- sequential
    (lazily, kind)    <- List("a view" -> Lazy, "a lazy list" -> LazyList, "an iterator" -> Iter)
  }
    test(s"$name on $lazily gives $id") {
      assertEquals(Laws.outcomes(build(kind)).map(_.law), Vector(id))
    }

  for ((name, id, build) <- sequential)
    test(s"$name on a strict receiver gives $id") {
      assertEquals(Laws.outcomes(build(Strict)).map(_.law), Vector(id))
    }

  test("toSet then toSeq on a set gives nothing") {
    assertEquals(Laws.outcomes(chain("set", Step("toSet", Vector.empty, Unique, Unique), step("toSeq"))), Vector.empty)
  }

  test("toSet then toSeq on an option gives nothing") {
    assertEquals(Laws.outcomes(chain("opt", Step("toSet", Vector.empty, Opt, Unique), step("toList"))), Vector.empty)
  }

  test("map of a projection on a sequence gives nothing") {
    assertEquals(Laws.outcomes(chain("pairs", step("map", projection(1)))), Vector.empty)
  }

  test("map of the third component on a map gives nothing") {
    assertEquals(Laws.outcomes(chain("map", Step("map", Vector(projection(3)), Dict, Strict))), Vector.empty)
  }

  test("zip of another collection gives nothing") {
    assertEquals(Laws.outcomes(chain("list", step("zip", fn("List(1)")))), Vector.empty)
  }

  test("zip of the indices of another collection gives nothing") {
    assertEquals(Laws.outcomes(chain("list", step("zip", fn("other.indices")))), Vector.empty)
  }

  test("zip of the indices on a set gives nothing") {
    assertEquals(Laws.outcomes(chain("set", Step("zip", Vector(fn("set.indices")), Unique, Strict))), Vector.empty)
  }

  test("a range over the size of a set gives nothing") {
    assertEquals(Laws.outcomes(chain("set", Step("untilSize", Vector.empty, Unique, Strict))), Vector.empty)
  }

  test("flatMap of another function gives nothing") {
    assertEquals(Laws.outcomes(chain("lists", step("flatMap", fn("_.toList")))), Vector.empty)
  }

  test("unzip then a third component gives nothing") {
    assertEquals(Laws.outcomes(chain("pairs", step("unzip"), step("_3"))), Vector.empty)
  }

  test("contains of a non-boolean after map gives nothing") {
    assertEquals(Laws.outcomes(chain("list", step("map", fn("_ + 1")), step("contains", literal(3)))), Vector.empty)
  }

  test("sorted with an explicit ordering then head gives nothing") {
    assertEquals(Laws.outcomes(chain("list", step("sorted", fn("Ordering.Int.reverse")), step("head"))), Vector.empty)
  }
}
