package net.kurobako.bonsai.lifting

import munit.FunSuite
import net.kurobako.bonsai.collections.*
import scala.io.Source

class LifterSuite extends FunSuite {
  private val compiled = Compiled(Source.fromResource("ChainLifting.txt").mkString)
  import compiled.given

  private def steps(name: String) = compiled.lifted(name).chain.steps

  private def methods(name: String): Vector[String] = steps(name).map(_.method)

  private def argument(name: String, step: Int) = steps(name)(step).args.head

  private def text(name: String, until: Int): String = {
    val lifted = compiled.lifted(name)
    lifted.text(lifted.span(until))
  }

  private val strictList = Kind(library = true)
  private val namedList  = Kind(library = true, name = "immutable.List")
  private val scalar     = Kind(library = false)

  test("the fixture compiles") {
    assertEquals(compiled.errors, Nil)
  }

  test("methods of a chain are listed in the order they run") {
    val expected = Map(
      "filterSize"    -> Vector("filter", "size"),
      "filterLength"  -> Vector("filter", "length"),
      "plainSize"     -> Vector("size"),
      "viewed"        -> Vector("view", "map", "toList"),
      "setBack"       -> Vector("toSet", "toSeq"),
      "unzipFirst"    -> Vector("unzip", "_1"),
      "unzipThird"    -> Vector("unzip3", "_3"),
      "infixed"       -> Vector("map", "filter"),
      "folded"        -> Vector("foldTrueAnd"),
      "foldedOr"      -> Vector("foldLeft"),
      "literalSource" -> Vector("map"),
      "optional"      -> Vector("filter", "map"),
      "arrayed"       -> Vector("map", "filter"),
      "mapKeys"       -> Vector("keys", "toList"),
      "parenthesised" -> Vector("++", "filter", "size"),
      "reversed"      -> Vector("reverse", "take", "reverse"),
      "sortedHead"    -> Vector("sorted", "headOption"),
      "stringed"      -> Vector("toList", "map"),
      "ranged"        -> Vector("toList", "map"),
      "qualified"     -> Vector("filter")
    )
    expected.foreach((name, found) => assertEquals(methods(name), found, name))
  }

  test("the source is the term the calls start from") {
    assertEquals(compiled.lifted("filterSize").chain.source.text, "list")
    assertEquals(compiled.lifted("literalSource").chain.source.text, "List(1, 2)")
    assertEquals(compiled.lifted("optional").chain.source.text, "Option(1)")
    assertEquals(compiled.lifted("infixed").chain.source.text, "list")
    assertEquals(compiled.lifted("qualified").chain.source.text, "fix.ChainLifting.list")
    assertEquals(compiled.lifted("parenthesised").chain.source.text, "list")
  }

  test("arguments of curried and infix calls are gathered per step") {
    assertEquals(steps("folded").head.args.map(_.text), Vector("value => value > 0"))
    assertEquals(steps("foldedOr").head.args.map(_.text), Vector("false", "(acc, value) => acc || value > 0"))
    assertEquals(steps("infixed").map(_.args.map(_.text)), Vector(Vector("_ + 1"), Vector("_ > 1")))
    assertEquals(steps("parenthesised").head.args.map(_.text), Vector("list"))
  }

  test("comparisons ending a chain lower to the method they state") {
    val expected = Map(
      "sizeEmpty"      -> Vector("filter", "sizeIsZero"),
      "lengthPositive" -> Vector("filter", "sizeNonZero"),
      "sizeNonZero"    -> Vector("filter", "sizeNonZero"),
      "bareSize"       -> Vector("sizeIsZero"),
      "findNone"       -> Vector("find", "isSome"),
      "findIsNone"     -> Vector("find", "isNone")
    )
    expected.foreach((name, found) => assertEquals(methods(name), found, name))
  }

  test("other comparisons end the chain before them") {
    val owner = compiled.definitionTree("parallel").symbol
    val found = Map(
      "containsTrue"     -> Vector("map", "contains"),
      "containsNotFalse" -> Vector("map", "contains"),
      "containsFalse"    -> Vector("map", "contains"),
      "sizeOnePlus"      -> Vector("filter", "size")
    )
    found.foreach((name, expected) =>
      assertEquals(
        compiled.lifter().chains(compiled.definition(name), owner).map(_.chain.steps.map(_.method)),
        Vector(expected),
        name
      )
    )
  }

  test("comparisons with options and positions lower to the methods the laws name") {
    val expected = Map(
      "noneCompared" -> Vector("isNone"),
      "noneReversed" -> Vector("isSome"),
      "someCompared" -> Vector("equalsSome"),
      "someReversed" -> Vector("notEqualsSome"),
      "firstIndex"   -> Vector("apply"),
      "lastIndex"    -> Vector("applyLast"),
      "firstLifted"  -> Vector("lift"),
      "lastLifted"   -> Vector("liftLast"),
      "otherIndex"   -> Vector("apply")
    )
    expected.foreach((name, found) => assertEquals(methods(name), found, name))
    assertEquals(compiled.lifted("noneReversed").chain.source.text, "Option(1)")
    assertEquals(argument("someCompared", 0).text, "2")
    assertEquals(argument("firstIndex", 0).literal, Some(0))
    assert(steps("lastIndex").head.args.isEmpty)
    assertEquals(text("lastIndex", 1), "list(list.size - 1)")
    assertEquals(steps("noneCompared").map(_.receiver.single), Vector(true))
  }

  test("a lookup by position is not lifted where the definition reads other positions or assigns") {
    def indexing(name: String): Vector[String] = {
      val tree = compiled.definitionTree(name)
      compiled.lifter().chains(tree, tree.symbol).flatMap(_.chain.steps.map(_.method)).filter(Set("apply", "applyLast"))
    }
    assertEquals(indexing("parallel"), Vector.empty)
    assertEquals(indexing("assigned"), Vector.empty)
  }

  test("type arguments mark a step and do not end the chain") {
    assertEquals(methods("typedChain"), Vector("map", "filter"))
    assertEquals(steps("typedChain").map(_.typed), Vector(true, false))
    assertEquals(methods("typedSorted"), Vector("sorted", "filter"))
  }

  test("ranges over a size and tuple components lower to the methods the laws name") {
    assertEquals(methods("ranged2"), Vector("untilSize"))
    assertEquals(methods("inclusive"), Vector("toLastIndex"))
    assertEquals(methods("inclusiveSize"), Vector("toLastIndex"))
    assertEquals(methods("untilLoop"), Vector("untilSize", "foreach"))
    assertEquals(methods("inclusiveLoop"), Vector("toLastIndex", "foreach"))
    assertEquals(compiled.lifted("ranged2").chain.source.text, "list")
    assertEquals(text("inclusive", 1), "0 to list.length - 1")
    assertEquals(steps("ranged2").map(_.receiver), Vector(namedList))
    assertEquals(steps("unzipFirst").map(_.receiver.library), Vector(true, true))
    assertEquals(steps("unzipThird").map(_.method), Vector("unzip3", "_3"))
  }

  test("a range with a step is not lowered to indices") {
    def ranges(name: String) = compiled.chains(name).flatMap(_.chain.steps.map(_.method))
    assertEquals(ranges("steppedUntil").filter(Set("untilSize", "toLastIndex")), Vector.empty)
    assertEquals(ranges("steppedTo").filter(Set("untilSize", "toLastIndex")), Vector.empty)
  }

  test("a lowered comparison keeps the span of the whole expression") {
    val compared = compiled.lifted("sizeEmpty")
    assertEquals(text("sizeEmpty", 2), "list.filter(_ > 0).size == 0")
    assertEquals(text("sizeEmpty", 1), "list.filter(_ > 0)")
    assertEquals(compared.receiver(0), "list")
    assertEquals(compared.receiver(1), "list.filter(_ > 0)")
    assertEquals(text("findNone", 2), "list.find(_ > 0) != None")
    assertEquals(text("filterSize", 2), "list.filter(_ > 0).size")
  }

  test("receivers are parenthesised when they would not bind") {
    val chain = compiled.lifted("parenthesised")
    assertEquals(chain.receiver(1), "(list ++ list)")
    assertEquals(text("parenthesised", 3), "(list ++ list).filter(_ > 0).size")
    assertEquals(compiled.lifted("infixed").receiver(1), "(list map (_ + 1))")
    assertEquals(compiled.lifted("filterSize").receiver(1), "list.filter(_ > 0)")
  }

  test("only the outermost expression of a chain is lifted") {
    val owner = compiled.definitionTree("parallel").symbol
    val found = compiled.lifter().chains(compiled.definition("filterSize"), owner).map(_.chain.steps.map(_.method))
    assertEquals(found, Vector(Vector("filter", "size")))
    val nested = compiled.lifter().chains(compiled.definition("nested"), owner).map(_.chain.steps.map(_.method))
    assertEquals(nested, Vector(Vector("map"), Vector("filter", "size")))
  }

  test("kinds follow the types of the receivers and the results of the calls") {
    val viewed = steps("viewed")
    assertEquals(viewed.map(_.receiver.strict), Vector(true, false, false))
    assertEquals(viewed.map(_.result.strict), Vector(false, false, true))
    assertEquals(viewed.head.receiver, namedList)
    assertEquals(viewed.last.result, namedList)

    val set    = steps("setBack")
    val unique = Kind(library = true, ordered = false, distinct = true, name = "immutable.Set")
    assertEquals(set.head.receiver, namedList)
    assertEquals(set.head.result, unique)
    assertEquals(set(1).receiver, unique)
    assertEquals(set(1).result, Kind(library = true, name = "immutable.Seq"))

    assertEquals(steps("arrayed").head.receiver, Kind(library = true, name = "Array"))
    assertEquals(steps("optional").head.receiver, Kind(library = true, single = true, name = "Option"))
    assertEquals(
      steps("mapped").head.receiver,
      Kind(library = true, ordered = false, keyed = true, name = "immutable.Map")
    )
    assertEquals(steps("iterated").head.result.strict, false)
    assertEquals(steps("lazyList").head.receiver.strict, false)
    assertEquals(steps("lazyList").head.receiver.name, "immutable.LazyList")
    assertEquals(steps("ranged").head.receiver.strict, true)
    assertEquals(steps("sortedHead").last.result.single, true)
    assertEquals(steps("filterSize").last.result, scalar)
    assertEquals(steps("findNone").last.result, scalar)
    assertEquals(steps("stringed").head.receiver, Kind(library = true, name = "String"))
    assertEquals(steps("filterSize").head.receiver, namedList)
    assertEquals(strictList.name, "")
  }

  test("user types with the same method names are not chains") {
    assertEquals(
      compiled.lifter().chains(compiled.definition("customChain"), compiled.definitionTree("parallel").symbol),
      Vector.empty
    )
  }

  test("the calls the compiler writes for a for-comprehension are marked, and only its first one is a step") {
    def desugared(name: String) = compiled.chains(name).map(_.chain.steps.map(step => step.method -> step.desugared))
    assertEquals(desugared("forYield"), Vector(Vector("filter" -> false, "map" -> true)))
    assertEquals(desugared("forEach"), Vector(Vector("filter" -> false, "foreach" -> true)))
    assertEquals(desugared("forTwo"), Vector(Vector("flatMap" -> true), Vector("map" -> false, "map" -> true)))
    assertEquals(compiled.chains("filterSize").flatMap(_.chain.steps).exists(_.desugared), false)
  }

  test("guards of a single generator are filters before the yield, all marked") {
    def desugared(name: String) = compiled.chains(name).map(_.chain.steps.map(step => step.method -> step.desugared))
    assertEquals(desugared("forGuard"), Vector(Vector("filter" -> true, "map" -> true)))
    assertEquals(desugared("forGuardEach"), Vector(Vector("filter" -> true, "foreach" -> true)))
    assertEquals(desugared("forGuards"), Vector(Vector("filter" -> true, "filter" -> true, "map" -> true)))
    assertEquals(desugared("forGuardAfter"), Vector(Vector("map" -> false, "filter" -> true, "map" -> true)))
    assertEquals(desugared("forGuardOption"), Vector(Vector("filter" -> true, "map" -> true)))
    assertEquals(compiled.lifted("forGuard").chain.source.text, "list")
    assertEquals(compiled.lifted("forGuard").chain.steps.map(_.receiver.library), Vector(true, true))
    assertEquals(compiled.lifted("forGuard").chain.steps.map(_.args.size), Vector(1, 1))
    assertEquals(compiled.lifted("forGuardOption").chain.steps.map(_.receiver.single), Vector(true, true))
  }

  private def shapes(name: String) = compiled.chains(name).map(_.chain.steps.map(step => step.method -> step.desugared))

  test("guards of the first generator of a nest are filters before its flatMap") {
    assertEquals(shapes("forGuardFirst"), Vector(Vector("filter" -> true, "flatMap" -> true), Vector("map" -> true)))
    assertEquals(shapes("forGuardInner"), Vector(Vector("flatMap" -> true), Vector("filter" -> true, "map" -> true)))
    assertEquals(shapes("guardedLazily"), Vector(Vector("withFilter" -> false)))
  }

  test("a pattern that cannot fail is the generator it binds, whatever the compiler version writes for it") {
    assertEquals(shapes("forKey"), Vector(Vector("map" -> true)))
    assertEquals(shapes("forValue"), Vector(Vector("map" -> true)))
    assertEquals(shapes("forSame"), Vector(Vector("map" -> true)))
    assertEquals(shapes("forTupleEach"), Vector(Vector("foreach" -> true)))
    assertEquals(shapes("forNestedTuple"), Vector(Vector("map" -> true)))
    assertEquals(shapes("forCaseClass"), Vector(Vector("map" -> true)))
    assertEquals(shapes("forPatternFirst"), Vector(Vector("flatMap" -> true), Vector("map" -> true)))
    assertEquals(compiled.lifted("forKey").chain.source.text, "map")
    assertEquals(compiled.lifted("forKey").chain.steps.map(_.receiver.keyed), Vector(true))
    assertEquals(compiled.lifted("forTupleEach").chain.steps.map(_.receiver.strict), Vector(true))
    val lifted = compiled.lifted("forKey")
    assertEquals(lifted.text(lifted.span(1)), "for ((key, _) <- map) yield key")
  }

  test("a pattern generator selects a component or keeps the tuple as the function it is") {
    def argument(name: String) = steps(name).last.args.head
    assertEquals(argument("forKey").projection, Some(1))
    assertEquals(argument("forValue").projection, Some(2))
    assertEquals(argument("forKey").identity, false)
    assertEquals(argument("forSame").identity, true)
    assertEquals(argument("forSame").projection, None)
    assertEquals(argument("forSwap").identity, false)
    assertEquals(argument("forSwap").projection, None)
    assertEquals(argument("forComputed").projection, None)
    assertEquals(argument("forNestedTuple").projection, None)
    assertEquals(argument("forTupleEach").projection, None)
  }

  test("guards of a pattern generator are filters, and the refutable pattern is a filter of its own") {
    assertEquals(shapes("forTupleGuard"), Vector(Vector("filter" -> true, "map" -> true)))
    assertEquals(shapes("forTupleGuards"), Vector(Vector("filter" -> true, "filter" -> true, "map" -> true)))
    assertEquals(shapes("forRefutable"), Vector(Vector("withFilter" -> true, "map" -> true)))
    assertEquals(shapes("forNarrow"), Vector(Vector("withFilter" -> true, "map" -> true)))
    assertEquals(shapes("forNestedOption"), Vector(Vector("withFilter" -> true, "map" -> true)))
    assertEquals(shapes("forRefutableEach"), Vector(Vector("withFilter" -> true, "foreach" -> true)))
    assertEquals(shapes("forTypedTuple").flatten.map(_._1).last, "map")
    assertEquals(compiled.lifted("forRefutable").chain.steps.map(_.result.strict), Vector(false, true))
    assertEquals(compiled.lifted("forRefutable").chain.steps.map(_.receiver.library), Vector(true, true))
    assertEquals(compiled.lifted("forTupleGuard").chain.source.text, "pairs")
    assertEquals(shapes("forRefutableGuard"), Vector(Vector("withFilter" -> true, "filter" -> true, "map" -> true)))
  }

  test("the source of a pattern generator is a chain of its own") {
    assertEquals(shapes("forTupleAfter"), Vector(Vector("map" -> false, "map" -> false, "map" -> true)))
    assertEquals(compiled.lifted("forTupleAfter").chain.source.text, "pairs")
    val lifted = compiled.lifted("forTupleAfter")
    assertEquals(lifted.text(lifted.span(2)), "pairs.map(pair => pair).map(pair => pair)")
    assertEquals(lifted.text(lifted.span(3)), "for ((a, b) <- pairs.map(pair => pair).map(pair => pair)) yield a")
  }

  test("every generator's source is a chain, and the first of them heads the nest") {
    assertEquals(
      shapes("forThree"),
      Vector(Vector("flatMap" -> true), Vector("flatMap" -> true), Vector("map" -> true))
    )
    assertEquals(
      shapes("forSources"),
      Vector(
        Vector("map" -> false, "map" -> false, "flatMap" -> true),
        Vector("map" -> false, "map" -> false, "map"     -> true)
      )
    )
    val sources = compiled.chains("forSources")
    assertEquals(
      sources.map(lifted => lifted.text(lifted.span(2))),
      Vector(
        "list.map(_ + 1).map(_ * 2)",
        "list.map(_ + 1).map(_ * 2)"
      )
    )
    assertEquals(sources.map(_.source.start).distinct.size, 2)
    assertEquals(
      shapes("forPatternInner"),
      Vector(Vector("flatMap" -> true), Vector("map" -> false, "map" -> false, "map" -> true))
    )
    assertEquals(shapes("forPatternLater"), Vector(Vector("flatMap" -> true), Vector("filter" -> true, "map" -> true)))
  }

  test("a nest whose inner generator reads the outer element and yields the inner one is the identity of flatMap") {
    def identity(name: String) = steps(name).last.args.head.identity
    assertEquals(identity("forFlatten"), true)
    assertEquals(identity("forFlattenOption"), true)
    assertEquals(identity("forNotFlatten"), false)
    assertEquals(identity("forOtherRow"), false)
    assertEquals(identity("forThree"), false)
    assertEquals(compiled.chains("forFlatten").size, 1)
    assertEquals(compiled.chains("forFlattenOption").size, 1)
    assertEquals(compiled.chains("forNotFlatten").size, 2)
    assertEquals(methods("forFlattenEach"), Vector("foreach"))
  }

  test("the generators after the first are chains of their own, ending in the yield") {
    assertEquals(shapes("forInnerEach"), Vector(Vector("foreach" -> true), Vector("foreach" -> true)))
    assertEquals(shapes("forInnerKey"), Vector(Vector("flatMap" -> true), Vector("map" -> true)))
    assertEquals(
      shapes("forInnerRefutable"),
      Vector(Vector("flatMap" -> true), Vector("withFilter" -> true, "map" -> true))
    )
    assertEquals(
      shapes("forInnerRefutGuard"),
      Vector(Vector("flatMap" -> true), Vector("withFilter" -> true, "filter" -> true, "map" -> true))
    )
    assertEquals(
      shapes("forInnerGuards"),
      Vector(Vector("flatMap" -> true), Vector("filter" -> true, "filter" -> true, "map" -> true))
    )
    assertEquals(
      shapes("forInnerBinding"),
      Vector(Vector("flatMap" -> true), Vector("filter" -> true, "map" -> true))
    )
    assertEquals(shapes("forInnerPlain"), Vector(Vector("flatMap" -> true), Vector("map" -> true)))
    assertEquals(shapes("forInnerEffect"), Vector(Vector("flatMap" -> true)))
    assertEquals(shapes("forInnerOption"), Vector(Vector("flatMap" -> true), Vector("filter" -> true, "map" -> true)))
    assertEquals(shapes("forInnerOptions"), Vector(Vector("flatMap" -> true), Vector("filter" -> true, "map" -> true)))
    assertEquals(
      shapes("forInnerPairs"),
      Vector(Vector("flatMap" -> true), Vector("map" -> false, "map" -> false, "filter" -> true, "map" -> true))
    )
    assertEquals(
      shapes("forInnerThreeLast"),
      Vector(Vector("flatMap" -> true), Vector("flatMap" -> true), Vector("filter" -> true, "map" -> true))
    )
    assertEquals(
      shapes("forInnerNested"),
      Vector(Vector("flatMap" -> true), Vector("flatMap" -> true), Vector("map" -> false, "map" -> true))
    )
    assertEquals(shapes("forInnerBound"), Vector(Vector("flatMap" -> true), Vector("map" -> true)))
    assertEquals(shapes("forInnerLater"), Vector(Vector("flatMap" -> true), Vector("map" -> true)))
  }

  test("the chain of a later generator reads the kinds of its own source and the pattern's component") {
    val key = compiled.chains("forInnerKey").last.chain
    assertEquals(key.source.text, "map")
    assertEquals(key.steps.map(_.receiver.keyed), Vector(true))
    assertEquals(key.steps.head.args.head.projection, Some(1))
    val guarded = compiled.chains("forInnerGuards").last.chain
    assertEquals(guarded.source.text, "list")
    assertEquals(guarded.steps.map(_.receiver.library), Vector(true, true, true))
    assertEquals(compiled.chains("forInnerOption").last.chain.steps.map(_.receiver.single), Vector(true, true))
    val lifted = compiled.chains("forInnerGuards").last
    assertEquals(lifted.text(lifted.span(3)).startsWith("other <- list if other > 0 if other < 9"), true)
  }

  test("a binding of a for-comprehension with a guard is read through the tuple the compiler makes of it") {
    assertEquals(shapes("forBinding"), Vector(Vector("filter" -> true, "map" -> true)))
    assertEquals(shapes("forBindings"), Vector(Vector("filter" -> true, "map" -> true)))
    assertEquals(shapes("forBindingGuards"), Vector(Vector("filter" -> true, "filter" -> true, "map" -> true)))
    assertEquals(shapes("forBindingEach"), Vector(Vector("filter" -> true, "foreach" -> true)))
    assertEquals(
      shapes("forBindingAfter"),
      Vector(Vector("map" -> false, "map" -> false, "filter" -> true, "map" -> true))
    )
    assertEquals(compiled.lifted("forBinding").chain.source.text, "list")
    assertEquals(compiled.lifted("forBinding").chain.steps.map(_.receiver.library), Vector(true, true))
    assertEquals(compiled.lifted("forBinding").chain.steps.map(_.receiver.strict), Vector(true, false))
    val lifted = compiled.lifted("forBinding")
    assertEquals(lifted.text(lifted.span(2)), "for { value <- list; doubled = value * 2 if doubled > 1 } yield doubled")
  }

  test("a binding is not read through when it is not a pure projection, or when nothing guards it") {
    assertEquals(shapes("forBindingEffect"), Vector.empty)
    assertEquals(shapes("forBindingMethod"), Vector.empty)
    assertEquals(shapes("forBindingNext"), Vector(Vector("flatMap" -> true), Vector("map" -> true)))
    assertEquals(shapes("forBindingPlain"), Vector(Vector("map" -> true)))
  }

  test("purity is syntactic and conservative") {
    def pure(name: String): Vector[Boolean] = steps(name).flatMap(_.args.map(_.pure))
    assert(pure("pureLambdas").forall(identity))
    assert(pure("pureFields").forall(identity))
    assert(pure("pureTuples").forall(identity))
    assert(pure("pureBounds").forall(identity))
    assert(pure("pureNames").forall(identity))
    assert(pure("pureNegated").forall(identity))
    assertEquals(pure("impureCalls"), Vector(false, false, false))
    assertEquals(pure("impureNamed"), Vector(false, false))
    assertEquals(pure("impureOps"), Vector(false, false, false))
    assertEquals(pure("impureNames"), Vector(false))
    assertEquals(pure("impureNew"), Vector(false, false))
    assertEquals(pure("impureThunk"), Vector(false))
    assertEquals(pure("literals"), Vector(true, true, true))
  }

  test("a function that destructures a tuple or a case class is pure when its result is") {
    def pure(name: String): Vector[Boolean] = steps(name).flatMap(_.args.map(_.pure))
    assertEquals(pure("pureCases"), Vector(true, true))
    assertEquals(pure("pureNested"), Vector(true))
    assertEquals(pure("impureCases"), Vector(false, false))
    assertEquals(pure("impureTupled"), Vector(false))
    assertEquals(pure("forKey"), Vector(true))
    assertEquals(pure("forComputed"), Vector(true))
    assertEquals(pure("forSwap"), Vector(true))
    assertEquals(pure("forTupleEach"), Vector(false))
    assertEquals(pure("forTupleGuard"), Vector(true, true))
  }

  test("purity is a parameter of the lifter") {
    val owner  = compiled.definitionTree("parallel").symbol
    val always = new Purity {
      def lambda(owner: dotty.tools.dotc.core.Symbols.Symbol, definition: dotty.tools.dotc.ast.tpd.DefDef)(using
          dotty.tools.dotc.core.Contexts.Context
      ): Boolean = true
      def method(symbol: dotty.tools.dotc.core.Symbols.Symbol)(using dotty.tools.dotc.core.Contexts.Context): Boolean =
        true
    }
    val lifted = compiled.lifter(always).chains(compiled.definition("impureCalls"), owner).head
    assert(lifted.chain.steps.flatMap(_.args).forall(_.pure))
  }

  test("sources are pure when they are references") {
    assert(compiled.lifted("filterSize").chain.source.pure)
    assert(compiled.lifted("qualified").chain.source.pure)
    assert(!compiled.lifted("literalSource").chain.source.pure)
  }

  test("identities, projections and literals are recognised") {
    assert(argument("identityName", 0).identity)
    assert(argument("identityLambda", 0).identity)
    assert(!argument("projectedFirst", 0).identity)
    assertEquals(argument("projectedFirst", 0).projection, Some(1))
    assertEquals(argument("projectedSecond", 0).projection, Some(2))
    assertEquals(argument("pureFields", 1).projection, None)
    assertEquals(argument("literals", 0).literal, Some(3))
    assertEquals(argument("literals", 1).literal, Some(0))
    assertEquals(argument("literals", 2).literal, Some(5))
    assertEquals(argument("literalString", 0).literal, Some("a"))
    assertEquals(steps("literalBoolean")(1).args.head.literal, Some(true))
    assertEquals(argument("pureLambdas", 0).literal, None)
  }

  test("a function applied to a value is beta reduced where it can be") {
    assertEquals(argument("applications", 0).applied("x"), "x + 1")
    assertEquals(argument("applications", 1).applied("y"), "y * 2")
    assertEquals(argument("applications", 1).applied("a + b"), "(a + b) * 2")
    assertEquals(argument("applications", 1).applied("xs(0)"), "xs(0) * 2")
    assertEquals(argument("applications", 2).applied("z"), "z")
    assertEquals(argument("applications", 3).applied("w"), "effect(w)")
    assertEquals(argument("applications", 4).applied("v"), "({ case 1 => 2 })(v)")
    assertEquals(argument("appliedFields", 0).applied("p"), "p.x + p.y")
    assertEquals(argument("appliedPairs", 0).applied("q"), "q._2")
    assertEquals(argument("pureFields", 0).applied("point"), "point.x")
    assertEquals(argument("shadowed", 0).applied("n"), "list.map(value => value + 1).sum + n")
  }

  test("a reduced body is left for the caller to delimit") {
    assertEquals(argument("pureLambdas", 3).applied("v"), "v != bound && v < 9")
    assertEquals(argument("pureNegated", 1).applied("v"), "-v")
  }

  test("every chain of the fixture lifts without failing") {
    val owner = compiled.definitionTree("parallel").symbol
    val all   = compiled.lifter().chains(compiled.tree, owner)
    assert(all.nonEmpty)
    all.foreach { found =>
      assert(found.chain.steps.nonEmpty)
      assertEquals(found.ends.size, found.chain.steps.size)
    }
  }
}
