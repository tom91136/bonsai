package net.kurobako.bonsai.collections

import Fixtures.*

class ReportedLawsSuite extends munit.FunSuite {

  private def report(name: String, id: String, message: String)(chain: => Chain): Unit =
    test(name) {
      val c = chain
      Laws.outcomes(c) match {
        case Vector(r: Outcome.Report) =>
          assertEquals(r.law, id)
          assertEquals(r.message, message)
          assertEquals((r.from, r.until), (0, c.steps.size))
        case other => fail(s"expected one report, got $other")
      }
    }

  private val everyElement = "exists and forall stop at the first deciding one"

  for {
    filter <- List("filter", "filterNot")
    check  <- List("isEmpty", "nonEmpty")
  }
    report(s"$filter then $check", "filter-emptiness", s"filtering then $check traverses every element; $everyElement") {
      chain("list", step(filter, fn("_ > 0")), step(check))
    }

  for (end <- List("headOption", "lastOption"))
    report(
      s"filter then $end",
      "filter-head-option",
      s"filter then $end traverses every element; find stops at the first match"
    ) {
      chain("list", step("filter", fn("_ > 0")), step(end))
    }

  for (end <- List("headOption", "lastOption"))
    report(
      s"collect then $end",
      "collect-head-option",
      s"collect then $end traverses every element; collectFirst stops at the first match"
    ) {
      chain("list", step("collect", fn("{ case v if v > 0 => v }")), step(end))
    }

  val guard = "a partial function with a guard filters and maps in one pass"

  for (filter <- List("filter", "filterNot"))
    report(s"$filter then map", "filter-map-to-collect", s"$filter then map is a collect; $guard") {
      chain("list", step(filter, reading(pair)), step("map", matching(pair)))
    }

  for (value <- List(true, false))
    report(s"map then contains($value)", "map-contains", s"map then contains traverses every element; $everyElement") {
      chain("list", step("map", fn("_ > 0")), step("contains", literal(value)))
    }

  report(
    "reverse then find",
    "reverse-find",
    "reverse then find reverses the collection and searches from the front; findLast searches from the end"
  ) {
    chain("list", step("reverse"), step("find", fn("_ > 0")))
  }

  private val first = "min and minBy compare in one pass"
  private val last  =
    "max and maxBy compare in one pass, but return the first of equal elements where the sort ends with the last"

  for {
    sort        <- List("sorted", "sortBy", "sortWith")
    (end, tail) <- List("head" -> first, "headOption" -> first, "last" -> last, "lastOption" -> last)
  }
    report(s"$sort then $end", "sorted-min-max", s"$sort then $end sorts every element; $tail") {
      chain(
        "list",
        Step(sort, if (sort == "sorted") Vector.empty else Vector(fn("_.length")), Strict, Strict),
        step(end)
      )
    }

  test("a report carries the span of its steps among others") {
    val found = reports(chain("list", step("reverse"), step("reverse"), step("find", fn("_ > 0")), step("toList")))
    assertEquals(found.map(r => (r.law, r.from, r.until)), Vector(("reverse-find", 1, 3)))
  }
}
