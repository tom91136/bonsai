package net.kurobako.bonsai.collections

import Fixtures.*

class FusionSuite extends munit.FunSuite {

  private val two   = "two passes and an intermediate collection become one pass"
  private val three = "three passes and two intermediate collections become one pass"

  private def only[A <: Outcome](outcomes: Vector[A]): A = {
    assertEquals(outcomes.size, 1, outcomes.toString)
    outcomes.head
  }

  test("map then map with pure functions becomes one map") {
    val c = chain("list", step("map", fn("_ + 1")), step("map", fn("_ * 2")))
    val r = only(rewrites(c))
    assertEquals((r.law, r.from, r.until), ("map-map-to-map", 0, 2))
    assertEquals(r.message, s"use one map for map then map: $two")
    assertEquals(r.render("list"), "list.map(x => (x + 1) * 2)")
    assertEquals(reports(c), Vector.empty)
  }

  test("map then map with pure functions is reported as it is where a call was written for a for-comprehension") {
    val generated = Step("map", Vector(fn("_ * 2")), Strict, Strict, desugared = true)
    val c         = chain("list", step("map", fn("_ + 1")), generated)
    val r         = only(reports(c))
    assertEquals((r.law, r.from, r.until), ("map-map-to-map", 0, 2))
    assertEquals(r.message, s"use one map for map then map: $two")
    assertEquals(rewrites(c), Vector.empty)
  }

  test("the maps written before a call for a for-comprehension are fused without it") {
    val generated = Step("map", Vector(fn("_ - 3")), Strict, Strict, desugared = true)
    val c         = chain("list", step("map", fn("_ + 1")), step("map", fn("_ * 2")), generated)
    val r         = only(rewrites(c))
    assertEquals((r.law, r.from, r.until), ("map-map-to-map", 0, 2))
    assertEquals(r.render("list"), "list.map(x => (x + 1) * 2)")
  }

  test("three maps become one map") {
    val c = chain("list", step("map", fn("_ + 1")), step("map", fn("_ * 2")), step("map", fn("_ - 3")))
    val r = only(rewrites(c))
    assertEquals((r.from, r.until), (0, 3))
    assertEquals(r.message, s"use one map for map then map then map: $three")
    assertEquals(r.render("list"), "list.map(x => ((x + 1) * 2) - 3)")
  }

  test("maps of named functions compose by application") {
    val c =
      chain("list", step("map", Fn.Called("f", v => s"f($v)")), step("map", Fn.Called("g", v => s"g($v)")))
    assertEquals(only(rewrites(c)).render("list"), "list.map(x => g(f(x)))")
  }

  test("map then map with an effect is reported") {
    val c = chain("list", step("map", effect("log(_)")), step("map", fn("_ * 2")))
    val r = only(reports(c))
    assertEquals((r.law, r.from, r.until), ("map-map-to-map", 0, 2))
    assertEquals(
      r.message,
      s"map then map fuse into one map, but the order of their effects would change unless both functions are pure: $two"
    )
    assertEquals(rewrites(c), Vector.empty)
  }

  test("map then map whose second function has an effect is reported") {
    assert(reports(chain("list", step("map", fn("_ + 1")), step("map", effect("log(_)")))).nonEmpty)
  }

  test("filter then filter with pure predicates becomes one filter") {
    val c = chain("list", step("filter", fn("_ > 0")), step("filter", fn("_ < 10")))
    val r = only(rewrites(c))
    assertEquals((r.law, r.from, r.until), ("filter-filter-to-filter", 0, 2))
    assertEquals(r.message, s"use one filter for filter then filter: $two")
    assertEquals(r.render("list"), "list.filter(x => x > 0 && x < 10)")
  }

  test("filter predicates that bind looser than and are parenthesised") {
    val c = chain("list", step("filter", fn("_ < 0 || _ > 5")), step("filter", fn("_ % 2 == 0")))
    assertEquals(only(rewrites(c)).render("list"), "list.filter(x => (x < 0 || x > 5) && x % 2 == 0)")
  }

  test("filter then filter with an effect is reported") {
    val r = only(reports(chain("list", step("filter", fn("_ > 0")), step("filter", effect("seen(_)")))))
    assertEquals(r.law, "filter-filter-to-filter")
    assertEquals(
      r.message,
      s"filter then filter fuse into one filter, but the order of their effects would change unless both functions are pure: $two"
    )
  }

  test("filterNot then filterNot with pure predicates becomes one filterNot with or") {
    val c = chain("list", step("filterNot", fn("_ > 0")), step("filterNot", fn("_ < -5")))
    val r = only(rewrites(c))
    assertEquals((r.law, r.from, r.until), ("filter-not-filter-not-to-filter-not", 0, 2))
    assertEquals(r.message, s"use one filterNot for filterNot then filterNot: $two")
    assertEquals(r.render("list"), "list.filterNot(x => x > 0 || x < -5)")
  }

  test("filterNot then filterNot with an effect is reported") {
    val r = only(reports(chain("list", step("filterNot", effect("a(_)")), step("filterNot", fn("_ > 0")))))
    assertEquals(r.law, "filter-not-filter-not-to-filter-not")
  }

  test("the fresh parameter avoids names the functions already use") {
    val c = chain("list", step("map", fn("_ + x")), step("map", fn("_ * y")))
    assertEquals(only(rewrites(c)).render("list"), "list.map(z => (z + x) * y)")
  }

  test("the fresh parameter avoids names in the receiver") {
    val c = chain("list", step("map", fn("_ + 1")), step("map", fn("_ * 2")))
    assertEquals(only(rewrites(c)).render("x.map(_.x)"), "x.map(_.x).map(y => (y + 1) * 2)")
  }

  test("a fusion sits among other steps") {
    val c = chain("list", step("sorted"), step("map", fn("_ + 1")), step("map", fn("_ * 2")), step("toList"))
    assertEquals(only(rewrites(c)).render("list.sorted"), "list.sorted.map(x => (x + 1) * 2)")
    assertEquals((only(rewrites(c)).from, only(rewrites(c)).until), (1, 3))
  }

  for (method <- List("map", "filter", "filterNot"))
    test(s"$method then $method on an option gives nothing") {
      assertEquals(
        Laws.outcomes(chain("opt", on(Opt, method, fn("_ > 1")), on(Opt, method, fn("_ > 2")))),
        Vector.empty
      )
    }

  test("maps after a step that gives an option give nothing") {
    val c =
      chain("list", Step("headOption", Vector.empty, Strict, Opt), on(Opt, "map", fn("f")), on(Opt, "map", fn("g")))
    assertEquals(Laws.outcomes(c), Vector.empty)
  }

  test("maps on a lazy receiver give nothing") {
    assertEquals(Laws.outcomes(chain("list", on(Lazy, "map", fn("_ + 1")), on(Lazy, "map", fn("_ * 2")))), Vector.empty)
  }

  test("filters on a lazy receiver give nothing") {
    assertEquals(
      Laws.outcomes(chain("list", on(Lazy, "filter", fn("_ > 0")), on(Lazy, "filter", fn("_ < 9")))),
      Vector.empty
    )
  }

  test("maps on a user type give nothing") {
    assertEquals(Laws.outcomes(chain("list", on(User, "map", fn("_ + 1")), on(User, "map", fn("_ * 2")))), Vector.empty)
  }

  test("a strict map into a lazy map gives nothing") {
    val c = chain("list", Step("map", Vector(fn("_ + 1")), Strict, Lazy), Step("map", Vector(fn("_ * 2")), Lazy, Lazy))
    assertEquals(Laws.outcomes(c), Vector.empty)
  }

  test("maps apart do not fuse") {
    assertEquals(
      Laws.outcomes(chain("list", step("map", fn("_ + 1")), step("reverse"), step("map", fn("_ * 2")))),
      Vector.empty
    )
  }

  test("a map and a filter are not a fusion") {
    val found = Laws.outcomes(chain("list", step("map", fn("_ + 1")), step("filter", fn("_ > 0"))))
    assertEquals(found.map(_.law), Vector("intermediate-collections"))
  }

  private val viewed =
    "map then collect then flatMap: use a view to fuse them, three passes and two intermediate collections become one pass"

  test("a run of strict element-wise steps with no exact law is advised as a view") {
    val c = chain("list", step("map", fn("f")), step("collect", fn("pf")), step("flatMap", fn("g")))
    val r = only(reports(c))
    assertEquals((r.law, r.from, r.until), ("intermediate-collections", 0, 3))
    assertEquals(r.message, viewed)
    assertEquals(rewrites(c), Vector.empty)
  }

  test("a two step run states two passes and an intermediate collection") {
    val r = only(reports(chain("list", step("map", fn("f")), step("takeWhile", fn("p")))))
    assertEquals(r.message, s"map then takeWhile: use a view to fuse them, $two")
  }

  test("the view advice holds for effectful functions") {
    assertEquals(reports(chain("list", step("map", effect("f(_)")), step("flatMap", effect("g(_)")))).size, 1)
  }

  test("an exact law beats the view advice over the same steps") {
    val found = Laws.outcomes(chain("list", step("filter", reading(pair)), step("map", matching(pair))))
    assertEquals(found.map(_.law), Vector("filter-map-to-collect"))
  }

  test("an exact law in the middle of a run leaves the rest to the advice") {
    val c = chain("list", step("map", fn("f")), step("collect", fn("g")), step("filter", fn("_ > 0")), step("size"))
    assertEquals(
      Laws.outcomes(c).map(o => (o.law, o.from, o.until)),
      Vector(("intermediate-collections", 0, 2), ("filter-to-count", 2, 4))
    )
  }

  test("an exact law at the end of a run leaves a single step with no advice") {
    val c = chain("list", step("map", fn("f")), step("filter", fn("_ > 0")), step("size"))
    assertEquals(Laws.outcomes(c).map(o => (o.law, o.from, o.until)), Vector(("filter-to-count", 1, 3)))
  }

  test("a run broken by a step that is not element-wise is two single steps") {
    assertEquals(
      Laws.outcomes(chain("list", step("map", fn("f")), step("sorted"), step("flatMap", fn("g")))),
      Vector.empty
    )
  }

  test("a single element-wise step gives no advice") {
    assertEquals(Laws.outcomes(chain("list", step("map", fn("f")), step("toList"))), Vector.empty)
  }

  test("a run on options gives no view advice") {
    assertEquals(Laws.outcomes(chain("opt", on(Opt, "map", fn("f")), on(Opt, "flatMap", fn("g")))), Vector.empty)
  }

  test("a run on a lazy receiver gives no advice") {
    assertEquals(Laws.outcomes(chain("list", on(Lazy, "map", fn("f")), on(Lazy, "flatMap", fn("g")))), Vector.empty)
  }

  test("a run on a user type gives no advice") {
    assertEquals(Laws.outcomes(chain("list", on(User, "map", fn("f")), on(User, "flatMap", fn("g")))), Vector.empty)
  }

  test("outcomes never overlap and come in step order") {
    val chains = List(
      chain(
        "l",
        step("map", fn("f")),
        step("map", fn("g")),
        step("filter", fn("p")),
        step("map", fn("h")),
        step("size")
      ),
      chain("l", step("filter", fn("p")), step("filter", fn("q")), step("size")),
      chain(
        "l",
        step("reverse"),
        step("take", fn("2")),
        step("reverse"),
        step("map", fn("f")),
        step("flatMap", fn("g"))
      ),
      chain("l", step("filter", reading(pair)), step("map", matching(pair)), step("map", fn("g")), step("headOption")),
      chain("l", step("sorted"), step("filter", fn("p")), step("isEmpty"))
    )
    for (c <- chains) {
      val found = Laws.outcomes(c)
      assert(found.nonEmpty, c.toString)
      assertEquals(found, found.sortBy(_.from))
      found.zip(found.drop(1)).foreach((a, b) => assert(a.until <= b.from, s"$a overlaps $b"))
    }
  }

  test("the longest match wins, then the earliest") {
    val c = chain("l", step("filter", reading(pair)), step("map", matching(pair)), step("map", fn("g")))
    assertEquals(Laws.outcomes(c).map(o => (o.law, o.from, o.until)), Vector(("filter-map-to-collect", 0, 2)))
  }

  test("the longest run of maps wins over a shorter one inside it") {
    val c = chain("l", step("map", fn("f")), step("map", fn("g")), step("map", fn("h")))
    assertEquals(Laws.outcomes(c).map(o => (o.from, o.until)), Vector((0, 3)))
  }
}
