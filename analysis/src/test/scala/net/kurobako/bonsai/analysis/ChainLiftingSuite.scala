package net.kurobako.bonsai.analysis

import net.kurobako.bonsai.collections.Chain

class ChainLiftingSuite extends AnalysisSuite {
  private lazy val found = extract(Seq(
    "Main.scala"  -> "object Main { def main(args: Array[String]): Unit = println(Lifts) }",
    "Lifts.scala" ->
      """object Lifts {
        |  final class Box[A](value: A) {
        |    def map[B](f: A => B): Box[B]        = Box(f(value))
        |    def filter(p: A => Boolean): Box[A]  = this
        |    def raw: A                           = value
        |  }
        |  private var counter = 0
        |  private def log(x: Int): Int = { println(x); x }
        |  private def twice(x: Int): Int = x * 2
        |
        |  def pure(values: List[Int]): Int = values.map(_ + 1).filter(_ > 0).size
        |  def effectful(values: List[Int]): List[Int] = values.map { x => println(x); x }.filter(_ > 0)
        |  def reference(values: List[Int]): List[Int] = values.map(log).map(twice)
        |  def capturing(values: List[Int]): List[Int] = values.map { x => counter += 1; x }
        |  def reading(values: List[Int]): List[Int] = values.map(_ + counter)
        |  def callback(values: List[Int], f: Int => Int): List[Int] = values.map(x => f(x)).map(f)
        |  def option(value: Option[Int]): Int = value.map(_ + 1).filter(_ > 0).getOrElse(0)
        |  def lookup(values: Map[String, Int]): Int = values.filter(_._2 > 0).map(_._1).size
        |  def unique(values: Set[Int]): List[Int] = values.map(_ + 1).toList
        |  def viewed(values: List[Int]): Int = values.view.map(_ + 1).filter(_ > 0).size
        |  def iterated(values: List[Int]): List[Int] = values.iterator.map(_ + 1).toList
        |  def lazily(n: Int): List[Int] = LazyList.from(n).map(_ + 1).take(3).toList
        |  def arrays(values: Array[Int]): Int = values.map(_ + 1).filter(_ > 0).size
        |  def strings(value: String): String = value.filter(_ != ' ').reverse
        |  def forms(values: List[List[Int]], pairs: List[(Int, Int)]): Int =
        |    values.flatMap(identity).take(3).size + pairs.map(_._2).size + pairs.map(p => p._1).size
        |  def restated(values: List[Int]): List[Int] = values.map(x => x + 1)
        |  def sorting(values: List[Int]): Option[Int] = values.sortBy(_.abs).headOption
        |  def folding(values: List[Int]): Int = values.foldLeft(0)(_ + _)
        |  def terminals(values: List[Int]): Boolean = values.isEmpty || values.find(_ > 1).isDefined
        |  def emptiness(values: List[Int]): Boolean = values.filter(_ > 0).size == 0
        |  def presence(values: List[Int]): Boolean = values.find(_ > 0) != None
        |  def nested(values: List[List[Int]]): Int = values.map(row => row.filter(_ > 0).size).sum
        |  def user(box: Box[Int]): Box[Int] = box.map(_ + 1).filter(_ > 0)
        |  def plain(box: Box[Int]): Int = box.raw
        |  def none(value: Int): Int = value + 1
        |}""".stripMargin
  ))

  private def chains(method: String): Vector[Chain] =
    found.pipelines.collect { case lifted if lifted.name.endsWith(s"Lifts.$method") => lifted.chain }

  private def only(method: String): Chain = {
    val all = chains(method)
    assertEquals(all.size, 1, all.mkString("\n"))
    all.head
  }

  private def methods(chain: Chain): Vector[String] = chain.steps.map(_.method)

  private def purity(chain: Chain): Vector[Boolean] = chain.steps.flatMap(_.args.map(_.pure))

  test("every call of a chain is a step, terminals included") {
    val chain = only("pure")
    assertEquals(methods(chain), Vector("map", "filter", "size"))
    assertEquals(chain.source.text, "values")
    assertEquals(methods(only("sorting")), Vector("sortBy", "headOption"))
    assertEquals(methods(only("folding")), Vector("foldLeft"))
    assertEquals(chains("terminals").map(methods), Vector(Vector("isEmpty"), Vector("find", "isDefined")))
  }

  test("implicit arguments are not step arguments") {
    assertEquals(only("sorting").steps.head.args.size, 1)
    assertEquals(only("folding").steps.head.args.map(_.literal), Vector(Some(0), None))
  }

  test("a function is pure only when its closure is proven to have no effect") {
    assertEquals(purity(only("pure")), Vector(true, true))
    assertEquals(purity(only("effectful")), Vector(false, true))
    assertEquals(purity(only("reference")), Vector(false, true))
    assertEquals(purity(only("capturing")), Vector(false))
    assertEquals(purity(only("reading")), Vector(false))
    assertEquals(purity(only("callback")), Vector(false, false))
  }

  test("a receiver is recognised as a standard collection with its kind") {
    val steps = only("pure").steps
    assert(steps.forall(_.receiver.library), steps.mkString("\n"))
    val list = steps.head.receiver
    assert(list.strict && list.ordered && !list.distinct && !list.keyed && !list.single)
    val size = steps.last
    assertEquals((size.method, size.result.library), ("size", false))
  }

  test("options, maps, sets and arrays have their own kinds") {
    val option = only("option")
    assertEquals(methods(option), Vector("map", "filter", "getOrElse"))
    assert(option.steps.forall(step => step.receiver.single && step.receiver.library && step.receiver.ordered))
    assertEquals(option.steps.last.args.map(_.literal), Vector(Some(0)))

    val map = only("lookup")
    assertEquals(methods(map), Vector("filter", "map", "size"))
    assert(map.steps.head.receiver.keyed && !map.steps.head.receiver.ordered)
    assert(map.steps(1).receiver.keyed && !map.steps(1).result.keyed, map.steps(1).toString)

    val set = only("unique").steps.head
    assert(set.receiver.distinct && !set.receiver.ordered && set.receiver.library)

    val array = only("arrays")
    assertEquals(methods(array), Vector("map", "filter", "size"))
    assert(array.steps.forall(step => step.receiver.ordered && step.receiver.library))
    assert(array.steps.take(2).forall(_.result.library))

    val string = only("strings")
    assertEquals(methods(string), Vector("filter", "reverse"))
    assert(string.steps.head.receiver.ordered && string.steps.head.receiver.library)
  }

  test("views, iterators and lazy lists are lazy") {
    val view = only("viewed")
    assertEquals(methods(view), Vector("view", "map", "filter", "size"))
    assertEquals(view.steps.map(_.receiver.strict), Vector(true, false, false, false))
    assertEquals(view.steps.map(_.result.strict).take(3), Vector(false, false, false))

    val iterator = only("iterated")
    assertEquals(methods(iterator), Vector("iterator", "map", "toList"))
    assertEquals(iterator.steps.map(_.receiver.strict), Vector(true, false, false))

    val lazyList = only("lazily")
    assertEquals(methods(lazyList), Vector("map", "take", "toList"))
    assert(lazyList.steps.take(2).forall(!_.receiver.strict))
    assert(lazyList.steps.head.receiver.ordered)
  }

  test("identity, projection, literal and text are read from the tree") {
    val Vector(flat, take) = chains("forms").head.steps.take(2)
    assertEquals(flat.method, "flatMap")
    assert(flat.args.head.identity)
    assertEquals(flat.args.head.applied("row"), "row")
    assertEquals(take.args.map(_.literal), Vector(Some(3)))
    assert(!take.args.head.identity)

    val projections =
      chains("forms").collect { case chain if chain.steps.head.method == "map" => chain.steps.head.args.head }
    assertEquals(projections.map(_.projection), Vector(Some(2), Some(1)))
    assertEquals(projections.head.applied("pair"), "pair._2")

    val restated = only("restated").steps.head.args.head
    assertEquals(restated.text, "x => x + 1")
    assertEquals(restated.applied("v"), "v + 1")
    assert(restated.pure && !restated.identity && restated.projection.isEmpty)
  }

  test("a comparison that ends a chain is lowered to the method it states") {
    assertEquals(methods(only("emptiness")), Vector("filter", "sizeIsZero"))
    assertEquals(methods(only("presence")), Vector("find", "isSome"))
    val Vector(filter, empty) = only("emptiness").steps
    assertEquals(empty.receiver, filter.result)
  }

  test("each step locates the line of the chain up to it") {
    val located = found.pipelines.find(_.name.endsWith("Lifts.pure")).get
    assertEquals(located.lines.size, located.chain.steps.size)
    assert(located.lines.forall(_ > 0), located.lines.mkString(", "))
  }

  test("chains inside functions are lifted on their own") {
    assertEquals(chains("nested").map(methods).toSet, Set(Vector("map", "sum"), Vector("filter", "size")))
  }

  test("a user type with collection-like names is not lifted") {
    assertEquals(chains("user"), Vector.empty)
  }

  test("calls that are not collection operations are not lifted") {
    assertEquals(chains("plain"), Vector.empty)
    assertEquals(chains("none"), Vector.empty)
  }
}
