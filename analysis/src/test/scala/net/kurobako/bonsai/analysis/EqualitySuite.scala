package net.kurobako.bonsai.analysis

class EqualitySuite extends AnalysisSuite {
  private def findings(
      main: Seq[(String, String)],
      test: Seq[(String, String)] = Nil,
      generated: Vector[String] = Vector.empty
  ): Vector[Finding] = {
    val result = extract(main, test, generated = generated)
    Equality.findings(result.equality, result.graph.nodes)
  }

  private def reported(found: Vector[Finding]): Vector[String] = found.map(_.node.name).sorted

  private def source(body: String): Seq[(String, String)] = Seq("Lints.scala" -> body)

  test("a value compared with an operator or an equality method reports its class") {
    val found = findings(source(
      """case class ByEq(values: Array[Int])
        |case class ByNe(values: Array[Int])
        |case class ByEquals(values: Array[Int])
        |case class ByHashCode(values: Array[Int])
        |case class ByHash(values: Array[Int])
        |object Use {
        |  def eq(a: ByEq, b: ByEq): Boolean         = a == b
        |  def ne(a: ByNe, b: ByNe): Boolean         = a != b
        |  def equal(a: ByEquals, b: ByEquals): Boolean = a.equals(b)
        |  def hashed(a: ByHashCode): Int            = a.hashCode
        |  def hashes(a: ByHash): Int                = a.##
        |}""".stripMargin
    ))
    assertEquals(reported(found), Vector("ByEq", "ByEquals", "ByHash", "ByHashCode", "ByNe"))
    assertFinding(found, "unstable-equality", "values: arrays compare by reference, so ByEq has unstable equality")
    assertFinding(found, "unstable-equality", "a value of ByEq is compared by == at Use (line 7)")
    assertFinding(found, "unstable-equality", "a value of ByNe is compared by != at Use (line 8)")
    assertFinding(found, "unstable-equality", "a value of ByEquals is compared by equals at Use (line 9)")
    assertFinding(found, "unstable-equality", "a value of ByHashCode is compared by hashCode at Use (line 10)")
    assertFinding(found, "unstable-equality", "a value of ByHash is compared by ## at Use (line 11)")
  }

  test("a comparison with a singleton compares identity, not the values a case holds") {
    val found = findings(source(
      """case class Bits(lanes: Array[Long])
        |enum Status {
        |  case Running
        |  case Waiting(bits: Bits)
        |}
        |case object Idle
        |object Use {
        |  def running(status: Status): Boolean = status == Status.Running
        |  def idle(state: Status | Idle.type): Boolean = state != Idle
        |}""".stripMargin
    ))
    assertEquals(reported(found), Vector.empty)
  }

  test("the finding is at the declaration of the class") {
    val found = findings(source(
      """object Holder {
        |  case class Bits(typeId: Int, lanes: Array[Long])
        |  def same(a: Bits, b: Bits): Boolean = a == b
        |}""".stripMargin
    ))
    assertEquals(found.map(finding => (finding.node.name, finding.node.line)), Vector(("Holder.Bits", 2)))
  }

  test("a class used as a set element or a map key reports, whichever collection holds it") {
    val found = findings(source(
      """import scala.collection.mutable
        |case class InSet(values: Array[Int])
        |case class InMutableSet(values: Array[Int])
        |case class InMap(values: Array[Int])
        |case class InMutableMap(values: Array[Int])
        |case class InJavaMap(values: Array[Int])
        |case class InJavaSet(values: Array[Int])
        |case class AsValue(values: Array[Int])
        |object Use {
        |  val set: Set[InSet]                          = Set.empty
        |  val mutableSet: mutable.Set[InMutableSet]    = mutable.Set.empty
        |  val map: Map[InMap, Int]                     = Map.empty
        |  val mutableMap: mutable.HashMap[InMutableMap, Int] = mutable.HashMap.empty
        |  val javaMap: java.util.HashMap[InJavaMap, Int] = java.util.HashMap()
        |  val javaSet: java.util.Set[InJavaSet]        = java.util.HashSet()
        |  val values: Map[Int, AsValue]                = Map.empty
        |}""".stripMargin
    ))
    assertEquals(reported(found), Vector("InJavaMap", "InJavaSet", "InMap", "InMutableMap", "InMutableSet", "InSet"))
    assertFinding(found, "unstable-equality", "a value of InSet is compared as a set element or map key at Use")
  }

  test("sorted collections order by their ordering and do not compare keys") {
    val found = findings(source(
      """import scala.collection.immutable.TreeSet
        |case class Ranked(values: Array[Int])
        |object Use {
        |  given Ordering[Ranked] = Ordering.by(_.values.length)
        |  val ranked: TreeSet[Ranked] = TreeSet.empty
        |}""".stripMargin
    ))
    assertEquals(reported(found), Vector.empty)
  }

  test("operations that look elements up by equality report the element class") {
    val found = findings(source(
      """case class Contained(values: Array[Int])
        |case class Indexed(values: Array[Int])
        |case class Distinct(values: Array[Int])
        |case class Diffed(values: Array[Int])
        |case class Intersected(values: Array[Int])
        |case class InArray(values: Array[Int])
        |case class InOption(values: Array[Int])
        |case class Mapped(values: Array[Int])
        |object Use {
        |  def contains(all: Vector[Contained], one: Contained): Boolean = all.contains(one)
        |  def indexOf(all: List[Indexed], one: Indexed): Int            = all.indexOf(one)
        |  def distinct(all: Seq[Distinct]): Seq[Distinct]               = all.distinct
        |  def diff(all: Seq[Diffed], other: Seq[Diffed]): Seq[Diffed]   = all.diff(other)
        |  def intersect(a: Seq[Intersected], b: Seq[Intersected]): Seq[Intersected] = a.intersect(b)
        |  def array(all: Array[InArray], one: InArray): Boolean         = all.contains(one)
        |  def option(one: Option[InOption], two: InOption): Boolean     = one.contains(two)
        |  def map(all: Seq[Mapped]): Seq[Int]                           = all.map(_.values.length)
        |}""".stripMargin
    ))
    assertEquals(
      reported(found),
      Vector("Contained", "Diffed", "Distinct", "InArray", "InOption", "Indexed", "Intersected")
    )
    assertFinding(found, "unstable-equality", "a value of Contained is compared by contains at Use (line 10)")
    assertFinding(found, "unstable-equality", "a value of Distinct is compared by distinct at Use (line 12)")
  }

  test("a lookup in a map compares its keys and not its values") {
    val found = findings(source(
      """case class Key(values: Array[Int])
        |case class Value(values: Array[Int])
        |object Use {
        |  def scala(all: Map[Key, Value], one: Key): Boolean = all.contains(one)
        |  def byValue(all: Map[Int, Value], one: Int): Boolean = all.contains(one)
        |  def mutable(all: collection.mutable.HashMap[Int, Value]): Boolean = all.contains(1)
        |}""".stripMargin
    ))
    assertEquals(reported(found), Vector("Key"))
  }

  test("a stable identifier pattern compares the scrutinee with a value of the class") {
    val found = findings(source(
      """case class Origin(values: Array[Int])
        |case class Taken(values: Array[Int])
        |object Use {
        |  val zero: Origin = Origin(Array(0))
        |  def isZero(origin: Origin): Boolean = origin match {
        |    case `zero` => true
        |    case _      => false
        |  }
        |  def taken(value: Taken): Int = value match {
        |    case Taken(values) => values.length
        |  }
        |}""".stripMargin
    ))
    assertEquals(reported(found), Vector("Origin"))
    assertFinding(found, "unstable-equality", "a value of Origin is compared by a stable identifier pattern at Use")
  }

  test("an assertion helper that compares reports the class of its arguments") {
    val found = findings(
      source("case class Expected(values: Array[Int])\ncase class Other(values: Array[Int])"),
      Seq(
        "ExpectedTest.scala" ->
          """object ExpectedTest {
            |  def check(obtained: Expected, expected: Expected): Unit = munit.Assertions.assertEquals(obtained, expected)
            |  def build: Other = Other(Array(1))
            |}""".stripMargin
      )
    )
    assertEquals(reported(found), Vector("Expected"))
    assertFinding(found, "unstable-equality", "compared by assertEquals at ExpectedTest")(using Finding.Level.Info)
  }

  test("comparisons in tests alone lower the finding to information") {
    val found = findings(
      source("case class Probe(values: Array[Int])\ncase class Used(values: Array[Int])"),
      Seq(
        "ProbeTest.scala" ->
          """object ProbeTest {
            |  def same(a: Probe, b: Probe): Boolean = a == b
            |  def used(a: Used, b: Used): Boolean   = a == b
            |}""".stripMargin
      )
    )
    assertFinding(found, "unstable-equality", "a value of Probe is compared by ==", "(all in tests)")(using
      Finding.Level.Info
    )
  }

  test("a comparison in main code is the one named when tests compare the class too") {
    val found = findings(
      source(
        """case class Both(values: Array[Int])
          |object Use { def same(a: Both, b: Both): Boolean = a == b }""".stripMargin
      ),
      Seq("BothTest.scala" -> "object BothTest { def same(a: Both, b: Both): Boolean = a != b }")
    )
    assertFinding(found, "unstable-equality", "a value of Both is compared by == at Use")
  }

  test("a class held by a compared class is compared with it") {
    val found = findings(source(
      """case class Leaf(values: Array[Int])
        |case class Middle(leaf: Leaf)
        |case class Top(middle: Middle)
        |case class InList(values: Array[Int])
        |case class ListHolder(all: List[InList])
        |case class InOption(values: Array[Int])
        |case class OptionHolder(one: Option[InOption])
        |case class Unheld(values: Array[Int])
        |case class Function(unheld: Unheld => Int)
        |object Use {
        |  def top(a: Top, b: Top): Boolean                     = a == b
        |  def list(a: ListHolder, b: ListHolder): Boolean      = a == b
        |  def option(a: OptionHolder, b: OptionHolder): Boolean = a == b
        |  def function(a: Function, b: Function): Boolean      = a == b
        |}""".stripMargin
    ))
    assertEquals(reported(found), Vector("Function", "InList", "InOption", "Leaf"))
    assertFinding(found, "unstable-equality", "Leaf is held by Middle, which is compared by == at Use (line 11)")
    assertFinding(found, "unstable-equality", "InList is held by ListHolder, which is compared by == at Use (line 12)")
    assertFinding(found, "unstable-equality", "InOption is held by OptionHolder")
  }

  test("a compared supertype compares the classes that extend it") {
    val found = findings(source(
      """sealed trait Shape
        |case class Round(values: Array[Int]) extends Shape
        |case class Square(side: Int)         extends Shape
        |trait Open
        |case class Free(values: Array[Int]) extends Open
        |case class Apart(values: Array[Int])
        |object Use {
        |  def shapes(a: Shape, b: Shape): Boolean = a == b
        |  def open(a: Open, b: Open): Boolean     = a == b
        |}""".stripMargin
    ))
    assertEquals(reported(found), Vector("Free", "Round"))
  }

  test("a class that is only built, copied, matched or read is not reported") {
    val found = findings(source(
      """enum Builtin {
        |  case Fixed
        |  case Computed(run: Int => Int)
        |}
        |case class Bits(typeId: Int, lanes: Array[Long])
        |case class Device(name: String, handler: String => Unit)
        |object Use {
        |  def build: Bits                       = Bits(1, Array(1L))
        |  def copied(bits: Bits): Bits          = bits.copy(typeId = 2)
        |  def lanes(bits: Bits): Int            = bits.lanes.length
        |  def matched(bits: Bits): Int          = bits match { case Bits(id, _) => id }
        |  def typed(value: Any): Boolean        = value match { case _: Device => true; case _ => false }
        |  def run(device: Device): Unit         = device.handler(device.name)
        |  def builtin(value: Builtin): Int      = value match {
        |    case Builtin.Fixed       => 0
        |    case Builtin.Computed(f) => f(1)
        |  }
        |  def isNull(bits: Bits): Boolean       = bits == null
        |  def sized(all: Vector[Bits]): Int     = all.size
        |}""".stripMargin
    ))
    assertEquals(reported(found), Vector.empty)
  }

  test("a class compared in its own file reports however narrow its access is") {
    val found = findings(source(
      """object Scope {
        |  private case class Hidden(values: Array[Int])
        |  private[Scope] case class Qualified(values: Array[Int])
        |  private def same(a: Hidden, b: Hidden): Boolean       = a == b
        |  private def other(a: Qualified, b: Qualified): Boolean = a == b
        |  def run: Boolean = same(Hidden(Array(1)), Hidden(Array(1))) && other(Qualified(Array(1)), Qualified(Array(1)))
        |}""".stripMargin
    ))
    assertEquals(reported(found), Vector("Scope.Hidden", "Scope.Qualified"))
  }

  test("a generated class is not reported, nor is one that suppresses the rule") {
    val result = extract(
      Seq(
        "src/generated/Generated.scala" -> "case class Generated(values: Array[Int])",
        "Lints.scala"                   ->
          """@SuppressWarnings(Array("bonsai:unstable-equality"))
            |case class Quiet(values: Array[Int])
            |case class Loud(values: Array[Int])
            |object Use {
            |  def quiet(a: Quiet, b: Quiet): Boolean = a == b
            |  def loud(a: Loud, b: Loud): Boolean = a == b
            |  def generated(a: Generated, b: Generated): Boolean = a == b
            |}""".stripMargin
      ),
      generated = GeneratedSources
    )
    val found = Equality.findings(result.equality, result.graph.nodes)
    assertEquals(reported(found), Vector("Loud", "Quiet"))
    val lines = Report(found, result.graph.nodes, Report.Options(Set.empty, Set.empty, Set.empty, info = true))
    assertEquals(reported(lines.map(_.finding)), Vector("Loud"))
  }

  test("a class with stable fields is not reported however it is compared") {
    val found = findings(source(
      """case class Plain(name: String, values: Vector[Double], cache: Map[String, Int])
        |object Use { def same(a: Plain, b: Plain): Boolean = a == b }""".stripMargin
    ))
    assertEquals(reported(found), Vector.empty)
  }

  test("a class nothing compares is not reported, whatever holds it") {
    val found = findings(source(
      """case class Plain(values: Array[Int])
        |case class Wrapper(plain: Plain)""".stripMargin
    ))
    assertEquals(reported(found), Vector.empty)
  }

  test("fields that compare by reference or change after construction give unstable equality") {
    val found = findings(source(
      """import scala.collection.mutable
        |case class Samples(
        |    name: String,
        |    values: Array[Double],
        |    transform: Double => Double,
        |    cache: mutable.Map[String, Int]
        |)
        |case class Active(client: String, var request: Vector[Byte])
        |case class Target(endpoint: String, closeFn: () => Unit)
        |case class Plain(name: String, values: Vector[Double], cache: Map[String, Int])
        |trait Reduction { def apply(a: Int, b: Int): Int }
        |trait Named { def name: String }
        |sealed trait Shape { def scale(by: Double): Shape }
        |case class Kernel(name: String, expected: Reduction)
        |case class Labelled(label: Named, shape: Shape)
        |class Holder(values: Array[Double])
        |trait Thunk { def apply(): Unit }
        |case class Deferred(name: String, thunk: Thunk)
        |case class Job(name: String, task: Runnable)
        |case class Mapper(name: String, transform: java.util.function.Function[Int, Int])
        |case class Aliased(values: Aliases.Numbers, buffer: Aliases.Buffer)
        |object Aliases { type Numbers = Array[Int]; type Buffer = mutable.ArrayBuffer[Int] }
        |case class Generic[A](items: Array[A], head: A)
        |case class Multiple(first: Int)(val second: Array[Int])
        |trait Strategy { def choose(values: List[Int]): Int }
        |object Smallest extends Strategy { def choose(values: List[Int]): Int = values.min }
        |case class Planned(name: String, strategy: Strategy)
        |case class Sorted(name: String, order: Ordering[Int], same: Equiv[Int], by: java.util.Comparator[Int])
        |case class Ranked(name: String, rank: Comparable[Int])
        |object Use {
        |  def compare(a: Any, b: Any): Boolean = a == b
        |  def samples(a: Samples, b: Samples)    = a == b
        |  def active(a: Active, b: Active)       = a == b
        |  def target(a: Target, b: Target)       = a == b
        |  def plain(a: Plain, b: Plain)          = a == b
        |  def kernel(a: Kernel, b: Kernel)       = a == b
        |  def labelled(a: Labelled, b: Labelled) = a == b
        |  def deferred(a: Deferred, b: Deferred) = a == b
        |  def job(a: Job, b: Job)                = a == b
        |  def mapper(a: Mapper, b: Mapper)       = a == b
        |  def aliased(a: Aliased, b: Aliased)    = a == b
        |  def generic(a: Generic[Int], b: Generic[Int]) = a == b
        |  def multiple(a: Multiple, b: Multiple) = a == b
        |  def planned(a: Planned, b: Planned)    = a == b
        |  def sorted(a: Sorted, b: Sorted)       = a == b
        |  def ranked(a: Ranked, b: Ranked)       = a == b
        |}""".stripMargin
    ))
    assertEquals(
      found.map(finding => finding.detail.take(finding.detail.indexOf(", and "))).sorted,
      Vector(
        "values: arrays compare by reference; buffer: mutable fields change the value after construction, so Aliased " +
          "has unstable equality",
        "values: arrays compare by reference; transform: functions compare by reference; cache: mutable fields " +
          "change the value after construction, so Samples has unstable equality",
        "closeFn: functions compare by reference, so Target has unstable equality",
        "expected: single-method traits that lambdas implement compare by reference, so Kernel has unstable equality",
        "items: arrays compare by reference, so Generic has unstable equality",
        "task: single-method traits that lambdas implement compare by reference, so Job has unstable equality",
        "thunk: single-method traits that lambdas implement compare by reference, so Deferred has unstable equality",
        "transform: single-method traits that lambdas implement compare by reference, so Mapper has unstable equality"
      ).sorted
    )
  }
}
