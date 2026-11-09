package net.kurobako.bonsai.analysis

import net.kurobako.bonsai.effects.*

class PuritySuite extends AnalysisSuite {
  private lazy val result = extract(Seq(
    "Main.scala" -> "object Main { def main(args: Array[String]): Unit = println(Pure) }",
    "Pure.scala" ->
      """import scala.collection.mutable.ArrayBuffer
        |object Pure {
        |  def double(x: Int): Int = x * 2
        |  def factorial(n: Int): Int = if (n <= 1) 1 else n * factorial(n - 1)
        |  def twice(f: Int => Int, x: Int): Int = f(f(x))
        |  def incremented: Int = twice(_ + 1, 1)
        |  def logged: Int = twice(x => { println(x); x }, 1)
        |  def summed(xs: List[Int]): Int = { val buffer = ArrayBuffer.empty[Int]; xs.foreach(buffer += _); buffer.sum }
        |  def pushed(buffer: ArrayBuffer[Int]): Unit = buffer += 1
        |  def counted(): Int = { val counter = Counter(); counter.bump() }
        |  def shared(counter: Counter): Int = counter.bump()
        |  def clock(): Long = System.nanoTime()
        |  def pool(): java.util.concurrent.ExecutorService = java.util.concurrent.Executors.newFixedThreadPool(1)
        |  def parsed(text: String): Int = text.toInt
        |  def ping(n: Int): Int = if (n <= 0) 0 else pong(n - 1)
        |  def pong(n: Int): Int = { println(n); ping(n - 1) }
        |  def named(path: java.nio.file.Path): String = path.getFileName.toString
        |  def read(path: java.nio.file.Path): String = java.nio.file.Files.readString(path)
        |}
        |class Counter { var n = 0; def bump(): Int = { n += 1; n } }""".stripMargin,
    "Uses.scala" ->
      """import scala.collection.mutable.ArrayBuffer
        |class Uses(xs: List[Int], paths: List[java.nio.file.Path]) {
        |  var total = 0
        |  def dropped(): Unit = {
        |    Pure.double(1)
        |    xs.map(_ + 1)
        |    val _ = Pure.logged
        |    val _ = Pure.parsed("1")
        |    val _ = Counter()
        |    val buffer = ArrayBuffer.empty[Int]
        |    val _ = buffer.addOne(1)
        |    val _ = (Pure.double(1), Pure.double(2))
        |    val stream = java.io.ByteArrayInputStream(Array[Byte](1))
        |    val _ = stream.read()
        |    val _ = xs.copyToArray(Array.ofDim[Int](1))
        |    val _ = Array(1, 2).copyToArray(Array.ofDim[Int](2))
        |    val _ = buffer.dropRightInPlace(1)
        |    val _ = java.util.Objects.checkIndex(0, 1)
        |  }
        |  def transformed(): Unit = {
        |    var count = 0
        |    val buffer = ArrayBuffer.empty[Int]
        |    val _ = xs.map { x => println(x); x }
        |    val _ = xs.map { x => count += 1; x }
        |    val _ = xs.filter { x => buffer += x; true }
        |    val _ = xs.map(x => Pure.logged + x)
        |    val _ = xs.map(_ + 1)
        |    val _ = xs.map { x => val local = ArrayBuffer(x); local += 1; local.size }
        |    val _ = xs.map(_ + total)
        |    xs.foreach(println)
        |    val _ = Option(1).map { x => println(x); x }
        |    val _ = xs.foldLeft(StringBuilder())((builder, x) => builder.append(x))
        |    val _ = xs.filterNot(_ => Thread.currentThread.isInterrupted)
        |    val _ = paths.map(java.nio.file.Files.readString(_))
        |    val _ = paths.map(path => java.nio.file.Files.writeString(path, ""))
        |  }
        |}""".stripMargin
  ))

  private def verdict(name: String): Purity.Verdict =
    result.purity.verdicts.getOrElse(name, fail(s"no verdict for $name"))

  private def impure(cause: String): Purity.Verdict = Purity.Verdict.Effectful(cause, Purity.Certainty.Known)

  private def isImpure(verdict: Purity.Verdict): Boolean = verdict match {
    case Purity.Verdict.Effectful(_, Purity.Certainty.Known) => true
    case _                                                   => false
  }

  private def isUnknown(verdict: Purity.Verdict): Boolean = verdict match {
    case Purity.Verdict.Effectful(_, Purity.Certainty.Unknown(_)) => true
    case _                                                        => false
  }

  test("pure methods stay pure through recursion, functions they are given, and values they allocate") {
    Vector("Pure.double", "Pure.factorial", "Pure.incremented", "Pure.summed", "Pure.counted", "Pure.parsed").foreach(
      name => assertEquals(verdict(name), Purity.Verdict.Pure, name)
    )
  }

  test("a function stored in a mutable collection after it is built is not lost") {
    val stored = extract(Seq(
      "Main.scala"   -> "object Main { def main(args: Array[String]): Unit = println(Stored) }",
      "Stored.scala" ->
        """import scala.collection.mutable
          |object Stored {
          |  private val logging: Int => Int = y => { println(y); y }
          |  def bufferUpdated: Int = { val b = mutable.ArrayBuffer[Int => Int](x => x); b(0) = logging; b(0)(1) }
          |  def bufferAppended: Int = { val b = mutable.ArrayBuffer.empty[Int => Int]; b += logging; b.head(1) }
          |  def mapUpdated: Int = { val m = mutable.Map.empty[Int, Int => Int]; m(0) = logging; m(0)(1) }
          |  def arrayUpdated: Int = { val a = Array[Int => Int](x => x); a(0) = logging; a(0)(1) }
          |  def upcast: Int = {
          |    val b = mutable.ArrayBuffer[Int => Int](x => x)
          |    val s: collection.Seq[Int => Int] = b
          |    b(0) = logging
          |    s(0)(1)
          |  }
          |  def constructed: Int = {
          |    val b = new mutable.ArrayBuffer[Int => Int]()
          |    val s: collection.Seq[Int => Int] = b
          |    b += logging
          |    s(0)(1)
          |  }
          |  def returnedUpcast: Int = { val s = viewed; s(0)(1) }
          |  private def viewed: collection.Seq[Int => Int] = {
          |    val b = mutable.ArrayBuffer[Int => Int](x => x)
          |    b(0) = logging
          |    b
          |  }
          |}""".stripMargin
    ))
    val names = Vector(
      "Stored.bufferUpdated",
      "Stored.bufferAppended",
      "Stored.mapUpdated",
      "Stored.arrayUpdated",
      "Stored.upcast",
      "Stored.returnedUpcast",
      "Stored.constructed"
    )
    assertEquals(
      names.filter(stored.purity.verdicts.getOrElse(_, fail("no verdict")) == Purity.Verdict.Pure),
      Vector.empty
    )
  }

  test("a function held by an immutable collection keeps the effects it was built with") {
    val held = extract(Seq(
      "Main.scala" -> "object Main { def main(args: Array[String]): Unit = println(Held) }",
      "Held.scala" ->
        """object Held {
          |  def pure: Int = List[Int => Int](x => x).head(1)
          |  def logging: Int = List[Int => Int](y => { println(y); y }).head(1)
          |}""".stripMargin
    ))
    assertEquals(held.purity.verdicts("Held.pure"), Purity.Verdict.Pure)
    assertEquals(held.purity.verdicts("Held.logging"), impure("calls scala.Predef.println"))
  }

  test("effects of callees, of the functions they run, and on shared values make a method impure") {
    assertEquals(verdict("Pure.logged"), impure("calls scala.Predef.println"))
    assertEquals(verdict("Pure.pushed"), impure("calls scala.collection.mutable.Growable.+="))
    assertEquals(verdict("Pure.shared"), impure("calls Counter.bump, which writes state"))
    assertEquals(verdict("Pure.clock"), impure("calls java.lang.System.nanoTime"))
    assertEquals(verdict("Pure.ping"), impure("calls Pure.pong, which performs output"))
    assertEquals(verdict("Pure.named"), Purity.Verdict.Pure)
    assertEquals(verdict("Pure.read"), impure("calls java.nio.file.Files.readString"))
    assertEquals(verdict("Counter.bump"), impure("writes n"))
  }

  test("calls without known effects are unknown, never pure") {
    assert(isUnknown(clue(verdict("Pure.pool"))))
  }

  private lazy val found = Purity.findings(result.purity, result.graph.nodes)
  private val Located    = " at [^ ]+:[0-9]+".r

  test("dropping the result of a call with no effects is reported, but not of calls that act, throw or construct") {
    assertEquals(
      found.collect {
        case finding if finding.rule == Finding.Rule.PureDiscard => finding.detail.takeWhile(_ != ' ')
      }.sorted,
      Vector("List.map", "Pure.double")
    )
  }

  test("dropping a partial function that throws is not reported") {
    val throwing = analysed(
      "Throwing",
      """object Throwing {
        |  def run(xs: List[Int]): Unit = {
        |    xs.collectFirst { case x if x > 2 => throw IllegalStateException("big") }
        |    ()
        |  }
        |}""".stripMargin,
      Seq.empty
    )
    assertEquals(
      Purity.findings(throwing.purity, throwing.graph.nodes).filter(_.rule == Finding.Rule.PureDiscard),
      Vector.empty
    )
  }

  test("dropping a partial function that is pure is reported at the line of the call") {
    val pure = analysed(
      "PurePartial",
      """object PurePartial {
        |  def run(xs: List[Int]): Unit = {
        |    xs.collectFirst { case x if x > 2 => x + 1 }
        |    ()
        |  }
        |}""".stripMargin,
      Seq.empty
    )
    assertEquals(
      Purity.findings(pure.purity, pure.graph.nodes).collect {
        case finding if finding.rule == Finding.Rule.PureDiscard => finding.node.line
      },
      Vector(3)
    )
  }

  test("a discard written as val _ is not reported") {
    val discards = analysed(
      "Discards",
      "object Discards { def run(xs: List[Int]): Unit = { val _ = xs.map(_ + 1) } }",
      Seq.empty
    )
    assertEquals(
      Purity.findings(discards.purity, discards.graph.nodes).filter(_.rule == Finding.Rule.PureDiscard),
      Vector.empty
    )
  }

  test("a pure discard is reported at the line of the call") {
    val lines = analysed(
      "Lines",
      """object Lines {
        |  def run(xs: List[Int]): Unit = {
        |    xs.map(_ + 1)
        |  }
        |}""".stripMargin,
      Seq.empty
    )
    assertEquals(
      Purity.findings(lines.purity, lines.graph.nodes).collect {
        case finding if finding.rule == Finding.Rule.PureDiscard => finding.node.line
      },
      Vector(3)
    )
  }

  test("a var local to the function passed to a transformation is not an effect of it") {
    val scoped = analysed(
      "Scoped",
      """object Scoped {
        |  def run(xs: List[Int]): List[Int] = xs.map { x =>
        |    var all = 0
        |    List(x).foreach(value => all = all + value)
        |    all
        |  }
        |}""".stripMargin,
      Seq.empty
    )
    assertEquals(
      Purity.findings(scoped.purity, scoped.graph.nodes).filter(_.rule == Finding.Rule.EffectfulTransform),
      Vector.empty
    )
  }

  test(
    "functions passed to collection transformations that write state, perform output or are nondeterministic are reported"
  ) {
    assertEquals(
      found.collect {
        case finding if finding.rule == Finding.Rule.EffectfulTransform => Located.replaceAllIn(finding.detail, "")
      }.sorted,
      Vector(
        "the function passed to filter calls scala.collection.mutable.Growable.+=",
        "the function passed to map calls java.nio.file.Files.writeString",
        "the function passed to map calls scala.Predef.println",
        "the function passed to map writes count"
      ).map(_ + "; keep transformations pure and perform effects in foreach or a loop")
    )
  }

  test("an effect a nested function performs is reported at the innermost function that performs it") {
    val nested = analysed(
      "Nested",
      """object Nested {
        |  def stamp(groups: List[List[Int]]): List[List[Long]] = groups.map { group =>
        |    group.map { value => value + System.currentTimeMillis() }
        |  }
        |  def logged(groups: List[List[Int]]): List[List[Long]] = groups.map { group =>
        |    println(group.size)
        |    group.map { value => value + System.currentTimeMillis() }
        |  }
        |  def chained(xs: List[Int]): List[Int] = xs.map { x => println(x); x }.filter { y => println(y); true }
        |}""".stripMargin,
      Seq.empty
    )
    val clockAdvice = "; read the clock once outside the transformation and pass the value in"
    val pureAdvice  = "; keep transformations pure and perform effects in foreach or a loop"
    assertEquals(
      Purity.findings(nested.purity, nested.graph.nodes).collect {
        case finding if finding.rule == Finding.Rule.EffectfulTransform => Located.replaceAllIn(finding.detail, "")
      }.sorted,
      Vector(
        "the function passed to filter calls scala.Predef.println" + pureAdvice,
        "the function passed to map calls java.lang.System.currentTimeMillis" + clockAdvice,
        "the function passed to map calls java.lang.System.currentTimeMillis" + clockAdvice,
        "the function passed to map calls scala.Predef.println" + pureAdvice,
        "the function passed to map calls scala.Predef.println" + pureAdvice
      )
    )
  }

  private def analysed(name: String, source: String, dependencies: Seq[(String, String)]): Extraction.Result =
    extract(program(name, source), dependencies = dependencies)

  private lazy val linked = analysed(
    "Invocations",
    """object Invocations {
          |  def added: Int = lib.Library.plus(1)
          |  def shouted: Int = lib.Library.shout(1)
          |  def boxed: Int = lib.Box(1).doubled
          |  def kept: Int = lib.Kept(1).value
          |  def shaped(shape: lib.Shape): Int = shape.sides
          |  def opened(open: lib.Open): Int = open.value
          |  def built: Int = lib.Open(1).value
          |  def sized(sized: lib.Sized): Int = sized.size
          |  def hidden(hidden: lib.Hidden): Int = hidden.size
          |}""".stripMargin,
    Seq(
      "Library.scala" ->
        """package lib
          |object Library {
          |  def plus(x: Int): Int = x + 1
          |  def shout(x: Int): Int = { println(x); x }
          |}
          |case class Box(value: Int) { def doubled: Int = Library.plus(value) * 2 }
          |final class Kept(base: Int) { def value: Int = base + 1 }
          |trait Shape { def sides: Int }
          |class Open(base: Int) { def value: Int = base }
          |sealed trait Sized { def size: Int }
          |final case class One() extends Sized { def size: Int = 1 }
          |case object Two extends Sized { def size: Int = 2 }
          |sealed trait Hidden { def size: Int }
          |object Hidden { val one: Hidden = new Hidden { def size: Int = { println(1); 1 } } }""".stripMargin
    )
  )

  test(
    "library methods are judged by their own bodies when no override can replace them, as on receivers built there " +
      "or of sealed classes"
  ) {
    def linkedVerdict(name: String) = linked.purity.verdicts.getOrElse(name, fail(s"no verdict for $name"))
    assertEquals(linkedVerdict("Invocations.added"), Purity.Verdict.Pure)
    assertEquals(linkedVerdict("Invocations.shouted"), impure("calls Library.shout, which performs output"))
    assertEquals(linkedVerdict("Invocations.boxed"), Purity.Verdict.Pure)
    assertEquals(linkedVerdict("Invocations.kept"), Purity.Verdict.Pure)
    assert(isUnknown(clue(linkedVerdict("Invocations.shaped"))))
    assert(isUnknown(clue(linkedVerdict("Invocations.opened"))))
    assertEquals(linkedVerdict("Invocations.built"), Purity.Verdict.Pure)
    assertEquals(linkedVerdict("Invocations.sized"), Purity.Verdict.Pure)
    assert(isUnknown(clue(linkedVerdict("Invocations.hidden"))))
  }

  private lazy val traced = analysed(
    "Traced",
    """object Handlers {
          |  val double: Int => Int = _ * 2
          |  val shout: Int => Int  = x => { println(x); x }
          |  val loud: Map[String, Int => Int]  = Map("d" -> double, "s" -> shout)
          |  val quiet: Map[String, Int => Int] = Map("d" -> double)
          |  def adder(n: Int): Int => Int = _ + n
          |}
          |object Traced {
          |  def doubled: Int = Handlers.double(1)
          |  def shouted: Int = Handlers.shout(1)
          |  def lifted(xs: Vector[Int]): Option[Int] = xs.lift(0)
          |  def loudly(key: String): Int = Handlers.loud(key)(1)
          |  def quietly(key: String): Int = Handlers.quiet(key)(1)
          |  def added: Int = Handlers.adder(1)(2)
          |  def shownInt: String = lib.Show[Int].show(1)
          |  def shownString: String = lib.Show[String].show("x")
          |  def ranQuiet: Int = lib.Quiet().run()
          |  def ranLoud: Int = lib.Loud().run()
          |  def every(xs: Vector[Int], ok: Int => Boolean): Boolean = {
          |    def loop(i: Int): Boolean = i >= xs.size || ok(xs(i)) && loop(i + 1)
          |    loop(0)
          |  }
          |  def positive(xs: Vector[Int]): Boolean = every(xs, _ > 0)
          |  def printed(xs: Vector[Int]): Boolean = every(xs, x => { println(x); true })
          |}
          |object Stored {
          |  private final class Quiet(run: Int => Int) { def apply(x: Int): Int = run(x) }
          |  private final class Loud(run: Int => Int) { def apply(x: Int): Int = run(x) }
          |  private def quiet(f: Int => Int): Quiet = Quiet(f)
          |  def doubled: Int = quiet(_ * 2)(1)
          |  def shouted: Int = Loud(x => { println(x); x })(1)
          |}
          |class Open(run: Int => Int) { def apply(x: Int): Int = run(x) }""".stripMargin,
    Seq(
      "Show.scala" ->
        """package lib
          |trait Show[A] { def show(value: A): String }
          |object Show {
          |  def apply[A](using instance: Show[A]): Show[A] = instance
          |  given Show[Int] with { def show(value: Int): String = value.toString }
          |  given Show[String] with { def show(value: String): String = { println(value); value } }
          |}
          |trait Ops { def run(): Int = step() + 1; def step(): Int }
          |final class Quiet extends Ops { def step(): Int = 1 }
          |final class Loud extends Ops { def step(): Int = { println(1); 1 } }""".stripMargin
    )
  )

  test(
    "functions are followed through values, tables, returns, lift and local methods, and receivers through instances " +
      "and this"
  ) {
    def tracedVerdict(name: String) = traced.purity.verdicts.getOrElse(name, fail(s"no verdict for $name"))
    Vector("doubled", "lifted", "quietly", "added", "shownInt", "ranQuiet", "every", "positive").foreach(name =>
      assertEquals(tracedVerdict(s"Traced.$name"), Purity.Verdict.Pure, name)
    )
    Vector("shouted", "loudly", "shownString", "ranLoud", "printed").foreach(name =>
      assert(isImpure(clue(tracedVerdict(s"Traced.$name"))), name)
    )
  }

  test("functions stored in objects only the build constructs run what every construction passes them") {
    def tracedVerdict(name: String) = traced.purity.verdicts.getOrElse(name, fail(s"no verdict for $name"))
    assertEquals(tracedVerdict("Stored.Quiet.apply"), Purity.Verdict.Pure)
    assert(isImpure(clue(tracedVerdict("Stored.Loud.apply"))))
    assert(isUnknown(clue(tracedVerdict("Open.apply"))))
  }

  private def cycles(bodies: Seq[(String, String)]): Map[String, Purity.Verdict] = extract(
    Seq(
      "Main.scala"   -> "object Main { def main(args: Array[String]): Unit = println(Cycles) }",
      "Cycles.scala" ->
        s"""object Cycles {
           |  def run(f: () => Unit): Unit = f()
           |  def bind(f: Int => Option[Int])(n: Int): Option[Int] = f(n)
           |${bodies.map((name, body) => s"  def $name(): Unit = { $body }").mkString("\n")}
           |}""".stripMargin
    )
  ).purity.verdicts

  test("functions that run themselves, directly or through other functions, are unknown, never pure") {
    val shapes = Seq(
      "direct" -> "lazy val loop: () => Unit = () => loop(); loop()",
      "passed" -> "lazy val self: () => Unit = () => run(self); run(self)",
      "mutual" -> "lazy val ping: () => Unit = () => pong(); lazy val pong: () => Unit = () => ping(); ping()",
      "nested" -> "lazy val outer: () => Unit = () => run(() => run(outer)); run(outer)"
    )
    val verdicts = cycles(shapes)
    shapes.foreach((name, _) =>
      assert(isUnknown(clue(verdicts.getOrElse(s"Cycles.$name", fail(s"no verdict for $name")))), name)
    )
  }

  test("methods that pass themselves on to functions they call are analysed") {
    val shapes = Seq(
      "local"     -> "def loop(): Unit = run(() => loop()); loop()",
      "counted"   -> "def loop(n: Int): Unit = run(() => loop(n - 1)); loop(3)",
      "bound"     -> "def loop(n: Int): Option[Int] = bind(m => loop(m + 1))(n); val _ = loop(0)",
      "anonymous" ->
        "class Steps { def loop(n: Int): Option[Int] = bind(m => loop(m + 1))(n) }; val _ = Steps().loop(0)"
    )
    val verdicts = cycles(shapes)
    shapes.foreach((name, _) => assert(verdicts.contains(s"Cycles.$name"), name))
  }

  test("functions that run each other without a cycle stay pure") {
    val verdicts = cycles(Seq(
      "acyclic" -> "val double: Int => Int = _ * 2; val twice: Int => Int = x => double(double(x)); val _ = twice(1)"
    ))
    assertEquals(verdicts("Cycles.acyclic"), Purity.Verdict.Pure)
  }

  test("a method that was pure fails the lock once it is not, and the lock records the pure methods of main code") {
    val regressed =
      Purity.regressions(result.purity, result.graph.nodes, Set("Pure.double", "Pure.logged", "Gone.method"))
    assertEquals(regressed.map(_.rule), Vector(Finding.Rule.PurityRegression))
    assertFinding(regressed, "purity-regression", "Pure.logged was pure and now calls scala.Predef.println")
    val locked = Report.keys(Purity.lock(result.purity, result.graph.nodes))
    assert(locked("Pure.double"))
    assert(!locked("Pure.logged"))
  }

  test("effects policies hold packages and classes to the kinds they allow, the most specific one deciding") {
    val violated = Purity.violations(
      result.purity,
      result.graph.nodes,
      Vector(Purity.Policy("Pure", Set.empty), Purity.Policy("Pure.read", Set(Kind.Input)))
    )
    assertFinding(
      violated,
      "effect-policy",
      "Pure.logged calls scala.Predef.println; the effects policy for Pure allows no effects"
    )
    assertFinding(violated, "effect-policy", "Pure.pool")
    assertNoFinding(violated)(finding =>
      finding.detail.startsWith("Pure.read ") || finding.detail.startsWith("Pure.double ")
    )
    val wildcard =
      Purity.violations(
        result.purity,
        result.graph.nodes,
        Vector(Purity.Policy("*", Set(Kind.Reads, Kind.Writes)))
      )
    assertFinding(
      wildcard,
      "effect-policy",
      "Pure.read calls java.nio.file.Files.readString",
      "allows only reads, writes"
    )
    assertNoFinding(wildcard)(_.detail.startsWith("Counter.bump "))
  }

  private lazy val contracted = extract(
    Seq(
      "Main.scala"      -> "object Main { def main(args: Array[String]): Unit = () }",
      "Contracts.scala" ->
        """import scala.concurrent.ExecutionContext.Implicits.global
          |import scala.concurrent.Future
          |object Counters { var calls = 0 }
          |class Loud(val n: Int) {
          |  override def equals(other: Any): Boolean = { println(n); other.isInstanceOf[Loud] }
          |  override def hashCode: Int = { Counters.calls += 1; n }
          |}
          |class Cached(val n: Int) {
          |  private var hash = 0
          |  override def hashCode: Int = { if (hash == 0) hash = n * 31; hash }
          |  override def equals(other: Any): Boolean = other match { case that: Cached => that.n == n; case _ => false }
          |}
          |object ByLength extends Ordering[String] {
          |  def compare(a: String, b: String): Int = { println(a); a.length - b.length }
          |}
          |object Even { def unapply(n: Int): Option[Int] = { Counters.calls += 1; Option.when(n % 2 == 0)(n / 2) } }
          |class Pool { def run(task: () => Unit): Unit = task() }
          |class Shared {
          |  var total = 0
          |  @volatile var done = false
          |  private val lock = new Object
          |  def counted(): Future[Unit] = { var count = 0; Future { count += 1 } }
          |  def summed(): Future[Unit] = Future { total += 1 }
          |  def guarded(): Future[Unit] = Future { lock.synchronized { total += 1 } }
          |  def flagged(): Future[Unit] = Future { done = true }
          |  def owned(): Future[Int] = Future { var own = 0; own += 1; own }
          |  def pooled(pool: Pool): Unit = pool.run(() => total += 1)
          |  def viewed(xs: List[Int]): List[Int] = xs.view.map { x => println(x); x }.toList
          |  def deferred(xs: List[Int]): scala.collection.View[Int] = xs.view.filter(_ > 0).map { x => println(x); x }
          |  def first(xs: List[Int]): Option[Int] =
          |    xs.view.map { x => println(x); x }.filter(_ > 1).collectFirst { case x if x > 2 => x }
          |}
          |class Remembering(val n: Int) { override def hashCode: Int = lib.Memo.remember(n) }
          |object Debug {
          |  final val Tracing = false
          |  def traced(n: Int): Int = { if (Tracing) println(n); n }
          |}""".stripMargin
    ),
    dependencies = Seq(
      "Memo.scala" -> "package lib\nobject Memo { private var last = 0; def remember(n: Int): Int = { last = n; n } }"
    ),
    concurrent = Set("Pool.run")
  )

  private lazy val contracts = Purity.findings(contracted.purity, contracted.graph.nodes)

  test("equals, hashCode, compare and extractors with effects beyond caching on their receiver are reported") {
    val found = contracts.collect {
      case finding if finding.rule == Finding.Rule.ImpureContract => finding.detail.takeWhile(_ != ' ')
    }.sorted
    assertEquals(found, Vector("ByLength.compare", "Even.unapply", "Loud.equals", "Loud.hashCode"))
    assertEquals(
      contracted.purity.verdicts("Remembering.hashCode"),
      impure("calls Memo.remember, which writes state")
    )
    assertFinding(contracts, "impure-contract", "Loud.equals calls scala.Predef.println", "call equals any number")
  }

  test("assignments to variables other threads share, outside synchronized and volatile ones, are reported") {
    val found = contracts.collect {
      case finding if finding.rule == Finding.Rule.SharedWrite => finding.detail.split(' ').slice(2, 3).mkString
    }
    assertEquals(found.sorted, Vector("Future.apply", "Future.apply", "Pool.run"))
    assertFinding(contracts, "shared-write", "assigns count")
    assertFinding(contracts, "shared-write", "Pool.run runs", "assigns total")
  }

  private lazy val locked = extract(
    Seq(
      "Main.scala"   -> "object Main { def main(args: Array[String]): Unit = () }",
      "Locked.scala" ->
        """import java.util.concurrent.locks.{ReentrantLock, ReentrantReadWriteLock}
          |class Pool { def run(task: () => Unit): Unit = task() }
          |class Locked {
          |  var free = 0
          |  private val lock = new ReentrantLock
          |  private val rw = new ReentrantReadWriteLock
          |  def guarded(pool: Pool): Unit = pool.run { () =>
          |    lock.lock()
          |    try { free = 1 } finally lock.unlock()
          |  }
          |  def written(pool: Pool): Unit = pool.run { () =>
          |    rw.writeLock().lock()
          |    try { free = 2 } finally rw.writeLock().unlock()
          |  }
          |  def bare(pool: Pool): Unit = pool.run(() => free = 3)
          |  def after(pool: Pool): Unit = pool.run { () =>
          |    lock.lock()
          |    try { free = 4 } finally lock.unlock()
          |    free = 5
          |  }
          |  def unreleased(pool: Pool): Unit = pool.run { () =>
          |    lock.lock()
          |    free = 6
          |    lock.unlock()
          |  }
          |  def interruptibly(pool: Pool): Unit = pool.run { () =>
          |    lock.lockInterruptibly()
          |    try { free = 7 } finally lock.unlock()
          |  }
          |  def lookalike(pool: Pool): Unit = pool.run { () =>
          |    val mine = new Lookalike
          |    mine.lock()
          |    try { free = 8 } finally mine.unlock()
          |  }
          |  def first(pool: Pool): Unit = pool.run { () =>
          |    try { free = 9 } finally lock.unlock()
          |    ()
          |  }
          |  def attempted(pool: Pool): Unit = pool.run { () =>
          |    val taken = lock.tryLock()
          |    try { free = 10 } finally lock.unlock()
          |  }
          |}
          |class Lookalike { def lock(): Unit = (); def unlock(): Unit = () }""".stripMargin
    ),
    concurrent = Set("Pool.run")
  )

  test("assignments inside a lock held until a finally releases it are not shared writes") {
    val found = Purity.findings(locked.purity, locked.graph.nodes).collect {
      case finding if finding.rule == Finding.Rule.SharedWrite => finding.detail
    }
    assertEquals(
      found.map(_.split(" runs at ")(1).split(' ')(0).split(':').last.toInt).sorted,
      Vector(15, 19, 23, 33, 36, 41)
    )
  }

  test("assignments a worker makes while holding a lock of the scheduler that runs it are not shared writes") {
    val scheduled = extract(
      Seq(
        "Main.scala" ->
          "object Main { def main(args: Array[String]): Unit = { Scheduler.run(Vector(1))(_ => ()); Scheduler.loose() } }",
        "Scheduler.scala" ->
          """import java.util.concurrent.Executors
            |import java.util.concurrent.locks.ReentrantLock
            |import scala.collection.mutable
            |object Scheduler {
            |  var unguarded = 0
            |  def loose(): Unit = Executors.newCachedThreadPool().execute(() => unguarded += 1)
            |  def run[J](jobs: Vector[J])(work: J => Unit): Vector[J] = {
            |    val lock    = ReentrantLock()
            |    val changed = lock.newCondition()
            |    val results = mutable.ArrayBuffer.empty[J]
            |    val pool    = Executors.newCachedThreadPool()
            |    val queue   = mutable.Queue.from(jobs)
            |    var free    = 4
            |    var running = 0
            |    def finish(job: J): Unit = {
            |      lock.lock()
            |      try results += job
            |      finally lock.unlock()
            |    }
            |    def launch(job: J, cost: Int): Unit =
            |      pool.execute { () =>
            |        work(job)
            |        finish(job)
            |        lock.lock()
            |        try {
            |          free = free + cost
            |          running -= 1
            |          changed.signalAll()
            |        } finally lock.unlock()
            |      }
            |    try {
            |      lock.lock()
            |      try
            |        while (queue.nonEmpty || running > 0) {
            |          if (queue.nonEmpty && free > 0) {
            |            val job = queue.dequeue()
            |            free = free - 1
            |            running += 1
            |            launch(job, 1)
            |          } else changed.await()
            |        }
            |      finally lock.unlock()
            |    } finally pool.shutdown()
            |    results.toVector
            |  }
            |}""".stripMargin
      )
    )
    val found = Purity.findings(scheduled.purity, scheduled.graph.nodes).collect {
      case finding if finding.rule == Finding.Rule.SharedWrite => finding.detail
    }
    assertEquals(found.size, 1, found.mkString("\n"))
    assert(found.head.contains("unguarded"), found.head)
  }

  test("a branch a constant condition rules out has no effects") {
    assertEquals(contracted.purity.verdicts("Debug.traced"), Purity.Verdict.Pure)
  }

  test("an effectful function passed to a view is reported as running again on every traversal it escapes to") {
    val viewed = contracts.collect { case finding if finding.rule == Finding.Rule.EffectfulTransform => finding.detail }
    assertEquals(viewed.count(_.contains("a view runs it again")), 1, viewed.mkString("\n"))
    assertEquals(viewed.count(_.contains("keep transformations pure")), 1, viewed.mkString("\n"))
    assertEquals(viewed.size, 2, viewed.mkString("\n"))
  }

  private lazy val known = analysed(
    "Behaviours",
    """object Behaviours {
          |  def uuid(): java.util.UUID = java.util.UUID.randomUUID()
          |  def math(): Double = Math.random()
          |  def random(): Int = scala.util.Random.nextInt(10)
          |  def slept(): Unit = Thread.sleep(1)
          |  def env(): String = System.getenv("HOME")
          |  def property(): String = System.getProperty("user.home")
          |  def copied(source: Array[Int], target: Array[Int]): Unit =
          |    System.arraycopy(source, 0, target, 0, source.length)
          |  def printed(): Unit = { val out = new java.io.ByteArrayOutputStream(); out.write(1) }
          |  def logged(text: String): Unit = org.slf4j.LoggerFactory.getLogger("effects").info(text)
          |  def incremented(adder: java.util.concurrent.atomic.LongAdder): Unit = adder.increment()
          |  def cleared(adder: java.util.concurrent.atomic.LongAdder): Unit = adder.reset()
          |  def exchanged(cell: java.util.concurrent.atomic.AtomicInteger): Int = cell.compareAndExchange(0, 1)
          |  def accumulated(cell: java.util.concurrent.atomic.AtomicInteger): Int = cell.accumulateAndGet(1, _ + _)
          |  def read(cell: java.util.concurrent.atomic.AtomicInteger): Int = cell.get()
          |  def buffered(): Int = {
          |    val buffer = java.nio.ByteBuffer.allocate(8)
          |    buffer.putInt(1)
          |    buffer.flip()
          |    buffer.getInt()
          |  }
          |  def ordered(): Int =
          |    java.nio.ByteBuffer.allocate(4).order(java.nio.ByteOrder.LITTLE_ENDIAN).putInt(1).flip().getInt()
          |  def stamped(): Long = java.nio.file.attribute.FileTime.fromMillis(1).toMillis
          |  def sized(attributes: java.nio.file.attribute.BasicFileAttributes): Long = attributes.size()
          |  def decoded(in: java.io.DataInputStream): Int = in.readInt()
          |  def memory(bytes: Array[Byte]): Int = java.io.DataInputStream(java.io.ByteArrayInputStream(bytes)).readInt()
          |  def opened(file: java.io.File): Int = java.io.FileInputStream(file).read()
          |  def console(): String = java.io.BufferedReader(java.io.InputStreamReader(System.in)).readLine()
          |  def current(): Thread = Thread.currentThread()
          |}""".stripMargin,
    Seq(
      "Slf4j.scala" ->
        """package org.slf4j
          |class Logger { def info(message: String): Unit = () }
          |object LoggerFactory { def getLogger(name: String): Logger = new Logger }
          |""".stripMargin
    )
  )

  private def effects(name: String): Set[Kind] =
    known.purity.methods.getOrElse(name, fail(s"no method $name")).effects.map(_.kind).toSet

  test("clocks, randomness, threads, environment, copies and output streams are known effects") {
    assertEquals(effects("Behaviours.uuid"), Set(Kind.Random))
    assertEquals(effects("Behaviours.math"), Set(Kind.Random))
    assertEquals(effects("Behaviours.random"), Set(Kind.Random))
    assertEquals(effects("Behaviours.slept"), Set(Kind.Output))
    assertEquals(effects("Behaviours.env"), Set(Kind.Input))
    assertEquals(effects("Behaviours.property"), Set(Kind.Input))
    assertEquals(effects("Behaviours.copied"), Set(Kind.Writes))
    assertEquals(effects("Behaviours.printed"), Set(Kind.Output))
    assertEquals(effects("Behaviours.logged"), Set(Kind.Output))
  }

  test("memory buffers, values and streams over memory are pure, and reading a stream given is input") {
    assertEquals(effects("Behaviours.buffered"), Set.empty[Kind])
    assertEquals(effects("Behaviours.ordered"), Set.empty[Kind])
    assertEquals(effects("Behaviours.stamped"), Set.empty[Kind])
    assertEquals(effects("Behaviours.memory"), Set.empty[Kind])
    assertEquals(effects("Behaviours.sized"), Set.empty[Kind])
    assertEquals(effects("Behaviours.decoded"), Set(Kind.Input))
  }

  test("opening a file or the console to read is input, reading the thread is not") {
    assertEquals(effects("Behaviours.opened"), Set(Kind.Input))
    assertEquals(effects("Behaviours.console"), Set(Kind.Input))
    assertEquals(effects("Behaviours.current"), Set(Kind.Reads))
  }

  test("atomic updates write, whichever of the atomic's methods performs them") {
    List("incremented", "cleared", "exchanged", "accumulated").foreach(name =>
      assertEquals(effects(s"Behaviours.$name"), Set(Kind.Writes), name)
    )
    assertEquals(effects("Behaviours.read"), Set(Kind.Reads))
  }

  test("the purity summary tallies verdicts and ranks the causes of unknown") {
    val summary = Purity.summary(result.purity)
    val counts  = """purity: \d+ pure, \d+ impure, \d+ unknown of \d+ methods, reading \d+ library methods""".r
    assert(counts.matches(summary.head), summary.mkString("\n"))
    assert(
      summary.exists(line =>
        line.startsWith("purity: unknown ") && line.contains("Executors.newFixedThreadPool")
      ),
      summary.mkString("\n")
    )
  }

  private lazy val callbacks = extract(
    Seq(
      "Main.scala"      -> "object Main { def main(args: Array[String]): Unit = println(Callbacks) }",
      "Callbacks.scala" ->
        """object Callbacks {
          |  def deferred(values: java.util.Optional[Int]): Unit = {
          |    val done = scala.concurrent.Promise[Int]()
          |    values.ifPresent(_ => { val _ = done.failure(new RuntimeException("failed")) })
          |  }
          |}""".stripMargin
    )
  )

  private lazy val callbackFindings = Purity.findings(callbacks.purity, callbacks.graph.nodes)

  test("dropping the result of an effectful call in a function nothing runs is not reported") {
    assertEquals(
      callbackFindings.collect { case finding if finding.rule == Finding.Rule.PureDiscard => finding.detail },
      Vector.empty[String]
    )
  }

  private lazy val java = extract(
    Seq(
      "Main.scala"    -> "object Main { def main(args: Array[String]): Unit = println(Dropped) }",
      "Dropped.scala" ->
        """object Dropped {
          |  def double(x: Int): Int = x * 2
          |  def matched(matcher: java.util.regex.Matcher): Unit = { val _ = matcher.find() }
          |  def called(task: java.util.concurrent.Callable[Int]): Unit = { val _ = task.call() }
          |  def encoded(text: String): Unit = { val _ = text.getBytes("UTF-8") }
          |  def named(file: java.io.File): Unit = { file.getName; () }
          |  def scala(x: Int): Unit = { double(x); () }
          |  def bytes(text: String): Array[Byte] = text.getBytes("UTF-8")
          |  def found(matcher: java.util.regex.Matcher): Boolean = matcher.find()
          |  def pending(task: java.util.concurrent.Callable[Int]): Int = task.call()
          |}""".stripMargin
    )
  )

  private lazy val javaFindings = Purity.findings(java.purity, java.graph.nodes)

  test("dropping the result of a Java method nothing is known of, or that declares what it throws, is not reported") {
    assertEquals(
      javaFindings.collect {
        case finding if finding.rule == Finding.Rule.PureDiscard => finding.detail.takeWhile(_ != ' ')
      }.sorted,
      Vector("Dropped.double", "File.getName")
    )
  }

  test("assuming a Java method only reads, or noting what it throws, leaves its callers impure or pure as before") {
    assertEquals(java.purity.verdicts("Dropped.bytes"), Purity.Verdict.Pure)
    assert(isImpure(java.purity.verdicts("Dropped.found")))
    assert(isImpure(java.purity.verdicts("Dropped.pending")))
  }

  private lazy val forwarded = extract(
    Seq(
      "Main.scala"      -> "object Main { def main(args: Array[String]): Unit = println(Forwarded) }",
      "Forwarded.scala" ->
        """object Forwarded {
          |  def double(x: Int): Int = x * 2
          |  def apply(f: Int => Int, x: Int): Unit = { val _ = f(x) }
          |  inline def inlined(inline f: Int => Int, x: Int): Unit = { val _ = f(x) }
          |  def byName(f: => Int): Unit = { val _ = f }
          |  def local(x: Int): Unit = { double(x); () }
          |  def run(): Unit = {
          |    apply(x => { println(x); x }, 1)
          |    inlined(x => { println(x); x }, 1)
          |    byName { println(1); 1 }
          |  }
          |}""".stripMargin
    )
  )

  test("dropping the result of a call through a function parameter is not reported, the caller decides what it does") {
    assertEquals(
      Purity.findings(forwarded.purity, forwarded.graph.nodes).collect {
        case finding if finding.rule == Finding.Rule.PureDiscard => finding.detail.takeWhile(_ != ' ')
      },
      Vector("Forwarded.double")
    )
  }

  private lazy val asserted = analysed(
    "Checked",
    """import org.scalatest.Assertions
      |object Checked {
      |  def positive(x: Int): Unit = { Assertions.assert(x > 0); Assertions.assertResult(1)(x); () }
      |  def doubled(x: Int): Unit = { Assertions.double(x); () }
      |}""".stripMargin,
    Seq(
      "Compatible.scala" -> "package org.scalatest.compatible\ntrait Assertion",
      "Package.scala"    ->
        """package org.scalatest
          |type Assertion = compatible.Assertion
          |object Succeeded extends compatible.Assertion""".stripMargin,
      "Assertions.scala" ->
        """package org.scalatest
          |object Assertions {
          |  def assert(condition: Boolean): Assertion = Succeeded
          |  def assertResult(expected: Int)(actual: Int): Assertion = Succeeded
          |  def double(x: Int): Int = x * 2
          |}""".stripMargin
    )
  )

  test("dropping the result of a call that declares ScalaTest's assertion type is not reported, it may throw") {
    val dropped = Purity.findings(asserted.purity, asserted.graph.nodes).filter(_.rule == Finding.Rule.PureDiscard)
    assertEquals(dropped.map(_.detail.takeWhile(_ != ' ')), Vector("Assertions.double"))
  }

  private lazy val nets = extract(
    Seq(
      "Main.scala" -> "object Main { def main(args: Array[String]): Unit = println(Nets) }",
      "Nets.scala" ->
        """object Nets {
          |  def decoded(text: String): String = java.net.URLDecoder.decode(text, "UTF-8")
          |  def encoded(text: String): String = java.net.URLEncoder.encode(text, java.nio.charset.StandardCharsets.UTF_8)
          |  def created(text: String): java.net.URI = java.net.URI.create(text)
          |  def resolved(uri: java.net.URI): java.net.URI = uri.resolve("child")
          |  def pathOf(uri: java.net.URI): String = uri.getPath
          |  def opened(text: String): java.io.InputStream = new java.net.URL(text).openStream()
          |  def mapped(texts: List[String]): Unit = {
          |    val _ = texts.map(java.net.URLDecoder.decode(_, "UTF-8"))
          |    val _ = texts.map(text => java.net.URI.create(text).getPath)
          |    val _ = texts.map(text => new java.net.URL(text).openStream())
          |  }
          |}""".stripMargin
    )
  )

  test("URL encoding and URIs are values, where the rest of java.net acts on the network") {
    Vector("Nets.decoded", "Nets.encoded", "Nets.created", "Nets.resolved", "Nets.pathOf").foreach(name =>
      assertEquals(nets.purity.verdicts(name), Purity.Verdict.Pure, name)
    )
    assertEquals(nets.purity.verdicts("Nets.opened"), impure("calls java.net.URL.openStream"))
    assertEquals(
      Purity.findings(nets.purity, nets.graph.nodes).collect {
        case finding if finding.rule == Finding.Rule.EffectfulTransform => Located.replaceAllIn(finding.detail, "")
      },
      Vector(
        "the function passed to map calls java.net.URL.openStream; keep transformations pure and perform effects in " +
          "foreach or a loop"
      )
    )
  }

  test("a function handed down a long chain of methods that each call the next twice is followed once per method") {
    val depth   = 40
    val methods = (1 to depth).map(n => s"  private def m$n(f: Int => Int): Int = m${n - 1}(f) + m${n - 1}(f)")
    val chained = extract(
      Seq(
        "Main.scala"  -> "object Main { def main(args: Array[String]): Unit = println(Chain) }",
        "Chain.scala" ->
          s"""object Chain {
             |  private class Runner(f: Int => Int) { def go: Int = f(1) }
             |  private def m0(f: Int => Int): Int = new Runner(f).go + new Runner(f).go
             |${methods.mkString("\n")}
             |  def quiet: Int = m$depth(_ + 1)
             |}""".stripMargin
      )
    )
    assertEquals(chained.purity.verdicts("Chain.quiet"), Purity.Verdict.Pure)
  }

  test("objects of a class read from a class file that two calls build alike are one value") {
    val depth   = 30
    val methods = (1 to depth).map(n =>
      s"  private def m$n(c: AtomicLong, b: Boolean): Int => Int = " +
        s"if (b) m${n - 1}(new AtomicLong(1), b) else m${n - 1}(new AtomicLong(2), b)"
    )
    val chained = extract(
      Seq(
        "Main.scala"  -> "object Main { def main(args: Array[String]): Unit = println(Fresh) }",
        "Fresh.scala" ->
          s"""import java.util.concurrent.atomic.AtomicLong
             |object Fresh {
             |  private def m0(c: AtomicLong, b: Boolean): Int => Int = x => x + c.intValue
             |${methods.mkString("\n")}
             |  def total(b: Boolean): Int = m$depth(new AtomicLong(0), b)(1)
             |}""".stripMargin
      )
    )
    assert(chained.purity.returns <= depth, chained.purity.returns)
  }
}
