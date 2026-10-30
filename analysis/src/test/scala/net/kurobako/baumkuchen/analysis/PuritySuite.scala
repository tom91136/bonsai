package net.kurobako.baumkuchen.analysis

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
        |    val _ = Pure.double(1)
        |    val _ = xs.map(_ + 1)
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

  test("pure methods stay pure through recursion, functions they are given, and values they allocate") {
    Vector("Pure.double", "Pure.factorial", "Pure.incremented", "Pure.summed", "Pure.counted", "Pure.parsed").foreach(
      name => assertEquals(verdict(name), Purity.Verdict.Pure, name)
    )
  }

  test("effects of callees, of the functions they run, and on shared values make a method impure") {
    assertEquals(verdict("Pure.logged"), Purity.Verdict.Impure("calls scala.Predef.println"))
    assertEquals(verdict("Pure.pushed"), Purity.Verdict.Impure("calls scala.collection.mutable.Growable.+="))
    assertEquals(verdict("Pure.shared"), Purity.Verdict.Impure("calls Counter.bump, which writes state"))
    assertEquals(verdict("Pure.clock"), Purity.Verdict.Impure("calls java.lang.System.nanoTime"))
    assertEquals(verdict("Pure.ping"), Purity.Verdict.Impure("calls Pure.pong, which performs output"))
    assertEquals(verdict("Pure.named"), Purity.Verdict.Pure)
    assertEquals(verdict("Pure.read"), Purity.Verdict.Impure("calls java.nio.file.Files.readString"))
    assertEquals(verdict("Counter.bump"), Purity.Verdict.Impure("writes n"))
  }

  test("calls without known effects are unknown, never pure") {
    assert(clue(verdict("Pure.pool")).isInstanceOf[Purity.Verdict.Unknown])
  }

  private lazy val found = Purity.findings(result.purity, result.graph.nodes)
  private val Located    = " at [^ ]+:[0-9]+".r

  test("dropping the result of a call with no effects is reported, but not of calls that act, throw or construct") {
    assertEquals(
      found.filter(_.rule == Finding.Rule.PureDiscard).map(_.detail.takeWhile(_ != ' ')).sorted,
      Vector("List.map", "List.map", "List.map", "List.map", "Pure.double")
    )
  }

  test(
    "functions passed to collection transformations that write state, perform output or are nondeterministic are reported"
  ) {
    assertEquals(
      found.filter(_.rule == Finding.Rule.EffectfulTransform).map(finding => Located.replaceAllIn(finding.detail, ""))
        .sorted,
      Vector(
        "the function passed to filter calls scala.collection.mutable.Growable.+=",
        "the function passed to map calls java.nio.file.Files.writeString",
        "the function passed to map calls scala.Predef.println",
        "the function passed to map writes count"
      ).map(_ + "; keep transformations pure and perform effects in foreach or a loop")
    )
  }

  private lazy val linked = extract(
    Seq(
      "Main.scala"  -> "object Main { def main(args: Array[String]): Unit = println(Calls) }",
      "Calls.scala" ->
        """object Calls {
          |  def added: Int = lib.Library.plus(1)
          |  def shouted: Int = lib.Library.shout(1)
          |  def boxed: Int = lib.Box(1).doubled
          |  def kept: Int = lib.Kept(1).value
          |  def shaped(shape: lib.Shape): Int = shape.sides
          |  def opened(open: lib.Open): Int = open.value
          |  def built: Int = lib.Open(1).value
          |  def sized(sized: lib.Sized): Int = sized.size
          |  def hidden(hidden: lib.Hidden): Int = hidden.size
          |}""".stripMargin
    ),
    dependencies = Seq(
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
    def verdict(name: String) = linked.purity.verdicts.getOrElse(name, fail(s"no verdict for $name"))
    assertEquals(verdict("Calls.added"), Purity.Verdict.Pure)
    assertEquals(verdict("Calls.shouted"), Purity.Verdict.Impure("calls Library.shout, which performs output"))
    assertEquals(verdict("Calls.boxed"), Purity.Verdict.Pure)
    assertEquals(verdict("Calls.kept"), Purity.Verdict.Pure)
    assert(clue(verdict("Calls.shaped")).isInstanceOf[Purity.Verdict.Unknown])
    assert(clue(verdict("Calls.opened")).isInstanceOf[Purity.Verdict.Unknown])
    assertEquals(verdict("Calls.built"), Purity.Verdict.Pure)
    assertEquals(verdict("Calls.sized"), Purity.Verdict.Pure)
    assert(clue(verdict("Calls.hidden")).isInstanceOf[Purity.Verdict.Unknown])
  }

  private lazy val traced = extract(
    Seq(
      "Main.scala"   -> "object Main { def main(args: Array[String]): Unit = println(Traced) }",
      "Traced.scala" ->
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
          |class Open(run: Int => Int) { def apply(x: Int): Int = run(x) }""".stripMargin
    ),
    dependencies = Seq(
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
    def verdict(name: String) = traced.purity.verdicts.getOrElse(name, fail(s"no verdict for $name"))
    Vector("doubled", "lifted", "quietly", "added", "shownInt", "ranQuiet", "every", "positive").foreach(name =>
      assertEquals(verdict(s"Traced.$name"), Purity.Verdict.Pure, name)
    )
    Vector("shouted", "loudly", "shownString", "ranLoud", "printed").foreach(name =>
      assert(clue(verdict(s"Traced.$name")).isInstanceOf[Purity.Verdict.Impure], name)
    )
  }

  test("functions stored in objects only the build constructs run what every construction passes them") {
    def verdict(name: String) = traced.purity.verdicts.getOrElse(name, fail(s"no verdict for $name"))
    assertEquals(verdict("Stored.Quiet.apply"), Purity.Verdict.Pure)
    assert(clue(verdict("Stored.Loud.apply")).isInstanceOf[Purity.Verdict.Impure])
    assert(clue(verdict("Open.apply")).isInstanceOf[Purity.Verdict.Unknown])
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
      assert(
        clue(verdicts.getOrElse(s"Cycles.$name", fail(s"no verdict for $name"))).isInstanceOf[Purity.Verdict.Unknown],
        name
      )
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
    val found = Purity.regressions(result.purity, result.graph.nodes, Set("Pure.double", "Pure.logged", "Gone.method"))
    assertEquals(found.map(_.rule), Vector(Finding.Rule.PurityRegression))
    assertFinding(found, "purity-regression", "Pure.logged was pure and now calls scala.Predef.println")
    val locked = Purity.locked(Purity.lock(result.purity, result.graph.nodes))
    assert(locked("Pure.double"))
    assert(!locked("Pure.logged"))
  }

  test("effects policies hold packages and classes to the kinds they allow, the most specific one deciding") {
    val found = Purity.violations(
      result.purity,
      result.graph.nodes,
      Vector(Purity.Policy("Pure", Set.empty), Purity.Policy("Pure.read", Set(Purity.Kind.Input)))
    )
    assertFinding(
      found,
      "effect-policy",
      "Pure.logged calls scala.Predef.println; the effects policy for Pure allows no effects"
    )
    assertFinding(found, "effect-policy", "Pure.pool")
    assertNoFinding(found)(finding =>
      finding.detail.startsWith("Pure.read ") || finding.detail.startsWith("Pure.double ")
    )
    val wildcard =
      Purity.violations(
        result.purity,
        result.graph.nodes,
        Vector(Purity.Policy("*", Set(Purity.Kind.Reads, Purity.Kind.Writes)))
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
    val found = contracts.filter(_.rule == Finding.Rule.ImpureContract).map(_.detail.takeWhile(_ != ' ')).sorted
    assertEquals(found, Vector("ByLength.compare", "Even.unapply", "Loud.equals", "Loud.hashCode"))
    assertEquals(
      contracted.purity.verdicts("Remembering.hashCode"),
      Purity.Verdict.Impure("calls Memo.remember, which writes state")
    )
    assertFinding(contracts, "impure-contract", "Loud.equals calls scala.Predef.println", "call equals any number")
  }

  test("assignments to variables other threads share, outside synchronized and volatile ones, are reported") {
    val found = contracts.filter(_.rule == Finding.Rule.SharedWrite).map(_.detail.split(' ').slice(2, 3).mkString)
    assertEquals(found.sorted, Vector("Future.apply", "Future.apply", "Pool.run"))
    assertFinding(contracts, "shared-write", "assigns count")
    assertFinding(contracts, "shared-write", "Pool.run runs", "assigns total")
  }

  test("a branch a constant condition rules out has no effects") {
    assertEquals(contracted.purity.verdicts("Debug.traced"), Purity.Verdict.Pure)
  }

  test("an effectful function passed to a view is reported as running again on every traversal it escapes to") {
    val viewed = contracts.filter(_.rule == Finding.Rule.EffectfulTransform).map(_.detail)
    assertEquals(viewed.count(_.contains("a view runs it again")), 1, viewed.mkString("\n"))
    assertEquals(viewed.count(_.contains("keep transformations pure")), 1, viewed.mkString("\n"))
    assertEquals(viewed.size, 2, viewed.mkString("\n"))
  }

  private lazy val known = extract(
    Seq(
      "Main.scala"    -> "object Main { def main(args: Array[String]): Unit = println(Effects) }",
      "Effects.scala" ->
        """object Effects {
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
          |}""".stripMargin
    ),
    dependencies = Seq(
      "Slf4j.scala" ->
        """package org.slf4j
          |class Logger { def info(message: String): Unit = () }
          |object LoggerFactory { def getLogger(name: String): Logger = new Logger }
          |""".stripMargin
    )
  )

  private def effects(name: String): Set[Purity.Kind] =
    known.purity.methods.getOrElse(name, fail(s"no method $name")).effects.map(_.kind).toSet

  test("clocks, randomness, threads, environment, copies and output streams are known effects") {
    assertEquals(effects("Effects.uuid"), Set(Purity.Kind.Random))
    assertEquals(effects("Effects.math"), Set(Purity.Kind.Random))
    assertEquals(effects("Effects.random"), Set(Purity.Kind.Random))
    assertEquals(effects("Effects.slept"), Set(Purity.Kind.Output))
    assertEquals(effects("Effects.env"), Set(Purity.Kind.Input))
    assertEquals(effects("Effects.property"), Set(Purity.Kind.Input))
    assertEquals(effects("Effects.copied"), Set(Purity.Kind.Writes))
    assertEquals(effects("Effects.printed"), Set(Purity.Kind.Output))
    assertEquals(effects("Effects.logged"), Set(Purity.Kind.Output))
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
    assertEquals(callbackFindings.filter(_.rule == Finding.Rule.PureDiscard).map(_.detail), Vector.empty[String])
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
          |  def named(file: java.io.File): Unit = { val _ = file.getName }
          |  def scala(x: Int): Unit = { val _ = double(x) }
          |  def bytes(text: String): Array[Byte] = text.getBytes("UTF-8")
          |  def found(matcher: java.util.regex.Matcher): Boolean = matcher.find()
          |  def pending(task: java.util.concurrent.Callable[Int]): Int = task.call()
          |}""".stripMargin
    )
  )

  private lazy val javaFindings = Purity.findings(java.purity, java.graph.nodes)

  test("dropping the result of a Java method nothing is known of, or that declares what it throws, is not reported") {
    assertEquals(
      javaFindings.filter(_.rule == Finding.Rule.PureDiscard).map(_.detail.takeWhile(_ != ' ')).sorted,
      Vector("Dropped.double", "File.getName")
    )
  }

  test("assuming a Java method only reads, or noting what it throws, leaves its callers impure or pure as before") {
    assertEquals(java.purity.verdicts("Dropped.bytes"), Purity.Verdict.Pure)
    assert(java.purity.verdicts("Dropped.found").isInstanceOf[Purity.Verdict.Impure])
    assert(java.purity.verdicts("Dropped.pending").isInstanceOf[Purity.Verdict.Impure])
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
    assertEquals(nets.purity.verdicts("Nets.opened"), Purity.Verdict.Impure("calls java.net.URL.openStream"))
    assertEquals(
      Purity.findings(nets.purity, nets.graph.nodes).filter(_.rule == Finding.Rule.EffectfulTransform)
        .map(finding => Located.replaceAllIn(finding.detail, "")),
      Vector(
        "the function passed to map calls java.net.URL.openStream; keep transformations pure and perform effects in " +
          "foreach or a loop"
      )
    )
  }
}
