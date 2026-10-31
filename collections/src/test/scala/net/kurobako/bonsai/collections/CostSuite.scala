package net.kurobako.bonsai.collections

import Fixtures.*

class CostSuite extends munit.FunSuite {

  test("costs add component by component") {
    assertEquals(
      Cost(passes = 1, allocations = 2, early = 1, scans = 3) + Cost(passes = 2, allocations = 1, sorts = 1),
      Cost(passes = 3, allocations = 3, early = 1, sorts = 1, scans = 3)
    )
  }

  test("full passes are the passes that do not stop early") {
    assertEquals(Cost(passes = 3, early = 1).full, 2)
  }

  test("a cost accepts a replacement that is no worse and better in one component") {
    assert(Cost.improves(Cost(passes = 2, allocations = 2), Cost(passes = 1, allocations = 1)))
    assert(Cost.improves(Cost(passes = 1, allocations = 1), Cost(passes = 1)))
    assert(Cost.improves(Cost(passes = 1), Cost(passes = 1, early = 1)))
    assert(Cost.improves(Cost(passes = 1, allocations = 1, sorts = 1), Cost(passes = 1)))
    assert(Cost.improves(Cost(passes = 2, scans = 1), Cost(passes = 2)))
  }

  test("a cost rejects an equal replacement unless it is an idiom") {
    assert(!Cost.improves(Cost(passes = 1, allocations = 1), Cost(passes = 1, allocations = 1)))
    assert(Cost.neverWorse(Cost(passes = 1, allocations = 1), Cost(passes = 1, allocations = 1)))
  }

  test("a cost rejects a replacement that is worse in any component") {
    assert(!Cost.improves(Cost(passes = 2, allocations = 1), Cost(passes = 1, allocations = 2)))
    assert(!Cost.improves(Cost(passes = 1, early = 1), Cost(passes = 1)))
    assert(!Cost.neverWorse(Cost(passes = 1, allocations = 1), Cost(passes = 1, sorts = 1)))
  }

  test("the gain states passes and intermediate collections") {
    assertEquals(Cost.gain(Cost(2, 2), Cost(1, 1)), "two passes and an intermediate collection become one pass")
    assertEquals(Cost.gain(Cost(3, 3), Cost(1, 1)), "three passes and two intermediate collections become one pass")
    assertEquals(Cost.gain(Cost(12, 12), Cost(1, 1)), "12 passes and 11 intermediate collections become one pass")
  }

  test("a strict map is a pass and a collection") {
    assertEquals(Ops.cost(step("map", fn("f"))), Cost(passes = 1, allocations = 1))
  }

  test("a lazy transformer is free") {
    assertEquals(Ops.cost(Step("map", Vector(fn("f")), Lazy, Lazy)), Cost())
  }

  test("a terminal on a lazy receiver is not free") {
    assertEquals(Ops.cost(Step("find", Vector(fn("p")), Lazy, Lazy)), Ops.cost(step("find", fn("p"))))
  }

  test("a size on a lazy receiver forces a pass where a strict one reads a field") {
    assertEquals(Ops.cost(step("size")), Cost())
    assertEquals(Ops.cost(Step("size", Vector.empty, Lazy, Lazy)), Cost(passes = 1))
  }

  test("an emptiness check on a lazy receiver stops at the first element") {
    assertEquals(Ops.cost(Step("isEmpty", Vector.empty, Lazy, Lazy)), Cost(passes = 1, early = 1))
  }

  test("a sort on a lazy receiver is not deferred") {
    assertEquals(Ops.cost(Step("sorted", Vector.empty, Lazy, Lazy)), Cost(passes = 1, sorts = 1))
  }

  test("exists and find stop at a deciding element") {
    assertEquals(Ops.cost(step("exists", fn("p"))).early, 1)
    assertEquals(Ops.cost(step("find", fn("p"))).early, 1)
    assertEquals(Ops.cost(step("count", fn("p"))).early, 0)
  }

  test("sorting counts a sort") {
    assertEquals(Ops.cost(step("sorted")).sorts, 1)
  }

  test("the cost of steps is the sum of their costs") {
    val steps = Vector(step("filter", fn("p")), step("map", fn("f")), step("size"))
    assertEquals(Ops.cost(steps), Cost(passes = 2, allocations = 2))
  }

  test("only maps, filters and their kin are element-wise") {
    assertEquals(Ops.elementWise, Set("map", "filter", "filterNot", "flatMap", "collect", "takeWhile", "dropWhile"))
  }

  test("every method a law names has a cost") {
    val named = Catalogue.laws.flatMap(law => law.pattern.flatten :+ law.into).toSet
    assertEquals(named.filter(Ops(_).isEmpty), Set.empty[String])
  }
}
