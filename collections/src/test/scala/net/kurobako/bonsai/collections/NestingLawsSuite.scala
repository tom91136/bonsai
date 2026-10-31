package net.kurobako.bonsai.collections

import Fixtures.*

class NestingLawsSuite extends munit.FunSuite {

  private val two = "two passes and an intermediate collection become one pass"

  private def nested(second: String, argument: Arg, expected: String): Unit = {
    val c     = chain("xs", step("flatMap", fn("_.children")), step(second, argument))
    val found = rewrites(c)
    assertEquals(found.map(r => (r.law, r.from, r.until)), Vector(("flat-map-nesting", 0, 2)))
    assertEquals(found.head.message, s"use one flatMap for flatMap then $second: $two")
    assertEquals(found.head.render("xs"), expected)
    assertEquals(reports(c), Vector.empty)
  }

  test("flatMap then map nests the map in the flatMap") {
    nested("map", fn("_.name"), "xs.flatMap(x => x.children.map(_.name))")
  }

  test("flatMap then filter nests the filter in the flatMap") {
    nested("filter", fn("_.active"), "xs.flatMap(x => x.children.filter(_.active))")
  }

  test("flatMap then filterNot nests the filter in the flatMap") {
    nested("filterNot", fn("_.active"), "xs.flatMap(x => x.children.filterNot(_.active))")
  }

  test("flatMap then flatMap nests the second in the first") {
    nested("flatMap", fn("_.parts"), "xs.flatMap(x => x.children.flatMap(_.parts))")
  }

  test("flatMap then collect nests the collect in the flatMap") {
    nested("collect", fn("{ case Leaf(n) => n }"), "xs.flatMap(x => x.children.collect({ case Leaf(n) => n }))")
  }

  test("the function of the flatMap is applied to a name the receiver and the functions do not use") {
    val c = chain("x", step("flatMap", fn("_.xs")), step("map", fn("_.x")))
    assertEquals(rewrites(c).head.render("x"), "x.flatMap(y => y.xs.map(_.x))")
  }

  test("a function that is not an atom is parenthesised before the second call") {
    val c = chain("xs", step("flatMap", fn("_ ++ _.more")), step("map", fn("_.name")))
    assertEquals(rewrites(c).head.render("xs"), "xs.flatMap(x => (x ++ x.more).map(_.name))")
  }

  test("a flatMap later in a chain keeps the steps before it") {
    val c = chain("xs", step("sorted"), step("flatMap", fn("_.children")), step("map", fn("_.name")))
    val r = rewrites(c).head
    assertEquals((r.from, r.until), (1, 3))
    assertEquals(r.render("xs.sorted"), "xs.sorted.flatMap(x => x.children.map(_.name))")
  }

  test("a map or flatMap after the nest of a for-comprehension is folded into it, a filter or collect is not") {
    val nest  = Step("flatMap", Vector(fn("_.children")), Strict, Strict, desugared = true)
    val found = reports(chain("xs", nest, step("map", fn("_.name"))))
    assertEquals(
      found.map(r => (r.law, r.from, r.until, r.lint, r.analysis)),
      Vector(("flat-map-nesting", 0, 2, false, true))
    )
    assertEquals(found.head.message, s"fold the map into the yield of the for-comprehension: $two")
    assertEquals(rewrites(chain("xs", nest, step("map", fn("_.name")))), Vector.empty)
    assertEquals(
      reports(chain("xs", nest, step("flatMap", fn("_.parts")))).map(_.message),
      Vector(s"fold the flatMap into a generator of the for-comprehension: $two")
    )
    Seq("filter", "filterNot", "collect").foreach { second =>
      assertEquals(
        Laws.outcomes(chain("xs", nest, step(second, fn("_.active")))).map(_.law).contains("flat-map-nesting"),
        false,
        second
      )
    }
  }

  test("a map with effects after the nest of a for-comprehension is reported with the order of its effects") {
    val nest  = Step("flatMap", Vector(effect("load(_)")), Strict, Strict, desugared = true)
    val found = reports(chain("xs", nest, step("map", fn("_.name"))))
    assert(found.head.message.contains("the map after a for-comprehension belongs in the yield"), found.head.message)
    assert(found.head.message.contains("order of effects would change"), found.head.message)
  }

  test("effectful functions are reported for the analysis, without a rewrite") {
    val c = chain("xs", step("flatMap", effect("load(_)")), step("map", fn("_.name")))
    assertEquals(rewrites(c), Vector.empty)
    val found = reports(c)
    assertEquals(found.map(_.law), Vector("flat-map-nesting"))
    assert(found.head.message.contains("order of their effects would change"), found.head.message)
    assert(!found.head.lint && found.head.analysis)
  }

  test("a nesting needs strict library receivers and a single function each") {
    val first = Step("flatMap", Vector(fn("_.children")), Lazy, Lazy)
    assertEquals(Laws.outcomes(chain("xs", first, Step("map", Vector(fn("_.name")), Lazy, Lazy))), Vector.empty)
    assertEquals(
      Laws.outcomes(chain("xs", on(User, "flatMap", fn("_.children")), on(User, "map", fn("_.name")))),
      Vector.empty
    )
    assertEquals(
      Laws.outcomes(chain("xs", on(Opt, "flatMap", fn("_.children")), on(Opt, "map", fn("_.name")))),
      Vector.empty
    )
    assertEquals(rewrites(chain("xs", step("flatMap", fn("_.children")), step("map", fn("a"), fn("b")))), Vector.empty)
  }

  test("a collect before the flatMap, or a step between them, is not a nesting") {
    assertEquals(
      Laws.outcomes(chain("xs", step("collect", fn("pf")), step("flatMap", fn("_.children")))).map(_.law),
      Vector("intermediate-collections")
    )
    assertEquals(
      Laws.outcomes(chain("xs", step("flatMap", fn("_.children")), step("sorted"), step("map", fn("_.name")))),
      Vector.empty
    )
  }

  test("arrays are left to the view advice, since the nested call would need a class tag") {
    val c = chain("xs", on(ArrayOf, "flatMap", fn("_.children")), on(ArrayOf, "map", fn("_.name")))
    assertEquals(Laws.outcomes(c).filter(_.lint), Vector.empty)
  }

  test("a run of element-wise steps that a view fuses is marked, and marked pure when every function is") {
    val pure = reports(chain("xs", step("map", fn("_.a")), step("filter", fn("_.b"))))
    assertEquals(pure.map(r => (r.law, r.viewable, r.pure)), Vector(("intermediate-collections", true, true)))
    val effectful = reports(chain("xs", step("map", effect("load(_)")), step("filter", fn("_.b"))))
    assertEquals(effectful.map(r => (r.law, r.viewable, r.pure)), Vector(("intermediate-collections", true, false)))
  }

  test("other reports are not marked as views") {
    val found = reports(chain("xs", on(ListOf, "take", literal(0))))
    assertEquals(found.map(r => (r.viewable, r.pure)), Vector((false, false)))
  }
}
