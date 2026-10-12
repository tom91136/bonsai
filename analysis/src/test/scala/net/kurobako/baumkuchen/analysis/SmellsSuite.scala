package net.kurobako.baumkuchen.analysis

class SmellsSuite extends AnalysisSuite {
  private def findings(sources: (String, String)*): Vector[Finding] = {
    val result = extract(sources)
    Smells.findings(result.smells, result.algebra.products, result.graph.nodes)
  }

  test("the public API of a library is not judged by the callers in its own build") {
    val result = extract(
      Seq(
        "Api.scala" ->
          """package lib
            |trait Store { def get: Int }
            |class Memory extends Store { def get = 1 }
            |sealed trait Closed
            |final class Only extends Closed
            |case class Options(submit: Boolean, cache: Option[String], label: Option[String])
            |object Client {
            |  def fetch[F](mode: Boolean, retries: Int = 3): Int = retries
            |  def save(): Int = 1
            |}
            |private[lib] object Internal { def run(flag: Boolean, count: Int): Int = count }
            |object Use {
            |  def run(store: Store, closed: Closed): Unit = {
            |    println(Memory()); println(Only())
            |    println(Options(true, None, Some("a"))); println(Options(true, None, Some("b")))
            |    println(Client.fetch[Int](true)); println(Client.fetch[Int](true))
            |    Client.save(); Client.save()
            |    println(Internal.run(true, 1)); println(Internal.run(true, 2))
            |  }
            |}""".stripMargin
      ),
      library = true
    )
    val judged = Set(
      "constant-argument",
      "flag-argument",
      "ignored-result",
      "fixed-type-parameter",
      "single-implementation",
      "constant-field",
      "exclusive-options",
      "redundant-flag"
    )
    val found = Report.findings(result).filter(finding => judged(finding.rule.id)).map(_.detail).sorted
    assertEquals(
      found,
      Vector(
        "Internal.run(flag, count).flag is always True across 2 calls",
        "lib.Closed has a single implementation, lib.Only"
      )
    )
  }

  test("parameters passed the same value by every caller are constant") {
    assertFinding(
      findings(
        "Render.scala" -> "object Render { def render(text: String, width: Int): String = text.take(width) }",
        "Main.scala"   -> "object Main { val first = Render.render(\"a\", 80); val second = Render.render(\"b\", 80) }"
      ),
      "constant-argument",
      "Render.render(text, width).width is always 80 across 2 calls"
    )
  }

  test("flags every caller passes as a literal select between methods") {
    assertFinding(
      findings(
        "Draw.scala" ->
          "object Draw { def draw(shape: String, filled: Boolean): String = if (filled) shape else shape.toLowerCase }",
        "Main.scala" -> "object Main { val solid = Draw.draw(\"a\", true); val hollow = Draw.draw(\"b\", false) }"
      ),
      "flag-argument",
      "filled is a literal at all 2 calls (1 true, 1 false)"
    )
  }

  test("signatures fixed by a supertype are not blamed for their callers") {
    val found = findings(
      "Shapes.scala" ->
        """trait Shape { def area(scale: Int): Int }
                          |class Square(side: Int) extends Shape { override def area(scale: Int): Int = side * side * scale }""".stripMargin,
      "Main.scala" -> "object Main { val square = Square(2); val areas = List(square.area(1), Square(3).area(1)) }"
    )
    assertNoFinding(found)(_.detail.contains("Square.area"))
  }

  test("results every caller discards are ignored") {
    assertFinding(
      findings(
        "Files.scala" -> "object Files { def write(path: String): Boolean = { println(path); true } }",
        "Main.scala"  ->
          "object Main { def run(): Unit = { Files.write(\"a\"); val _ = Files.write(\"b\"); println(\"done\") } }"
      ),
      "ignored-result",
      "Files.write(path): its Boolean result is discarded at all 2 calls"
    )
  }

  test("results used by any caller are not ignored") {
    val found = findings(
      "Files.scala" -> "object Files { def write(path: String): Boolean = { println(path); true } }",
      "Main.scala"  -> "object Main { def run(): Boolean = { Files.write(\"a\"); Files.write(\"b\") } }"
    )
    assertNoFinding(found)(_.rule == Finding.Rule.IgnoredResult)
  }

  test("a result kept by one caller and read only by a local method is not ignored") {
    val found = findings(
      "Shapes.scala" ->
        """object Shapes {
          |  def sizes(types: Vector[Int]): Vector[Int] = {
          |    val checked = scala.collection.mutable.Map.empty[Int, Int]
          |    def validate(value: Int, depth: Int): Int = {
          |      require(depth < 8)
          |      if (value > 1) Vector(value / 2).foreach { child =>
          |        val _ = validate(child, depth + 1)
          |      }
          |      checked.getOrElseUpdate(value, value * 4)
          |    }
          |    types.flatMap { value =>
          |      val size = validate(value, 0)
          |      def fits(offset: Int): Boolean = offset <= size
          |      Option.when(fits(value))(value)
          |    }
          |  }
          |}""".stripMargin
    )
    assertNoFinding(found)(_.rule == Finding.Rule.IgnoredResult)
  }

  test("parameter groups recurring across types are data clumps") {
    assertFinding(
      findings(
        "Point.scala"  -> "case class Point(x: Int, y: Int, z: Int)",
        "Render.scala" ->
          """object Render { def plot(x: Int, y: Int, z: Int, colour: String): String = colour; def erase(x: Int, y: Int, z: Int): Unit = () }""",
        "Physics.scala" -> "object Physics { def push(x: Int, y: Int, z: Int, force: Double): Double = force }"
      ),
      "data-clump",
      "(x: Int, y: Int, z: Int) recur in 3 methods across 2 types",
      "already the fields of Point"
    )
  }

  test("an abstraction with one implementation beside it is speculative") {
    val found = findings(
      "Store.scala" -> """trait Store { def get(key: String): Option[String] }
                         |class MemoryStore extends Store { def get(key: String) = None }
                         |trait Codec { def encode(value: Int): String }
                         |class Hex extends Codec { def encode(value: Int) = value.toHexString }
                         |object Main { val decimal: Codec = value => value.toString }""".stripMargin
    )
    assertFinding(found, "single-implementation", "Store has a single implementation, MemoryStore")
    assertNoFinding(found)(_.detail.contains("Codec"))
  }

  test("type parameters always given the same type are fixed") {
    val found = findings(
      "Box.scala" -> """case class Box[A](value: A)
                       |object Lists { def first[A](values: List[A]): Option[A] = values.headOption }
                       |object Main {
                       |  val boxes = List(Box(1), Box(2))
                       |  val heads = List(Lists.first(List("a")), Lists.first(List("b")))
                       |  def wrap[B](value: B): Box[B] = Box(value)
                       |}""".stripMargin
    )
    assertFinding(found, "fixed-type-parameter", "Lists.first: type parameter A is always String across 2 uses")
    assertNoFinding(found)(_.detail.startsWith("Box:"))
  }

  test("parameters of recursive methods carry state rather than select behaviour") {
    val found = findings(
      "Scan.scala" -> """object Scan {
                        |  def loop(index: Int, quoted: Boolean): Int =
                        |    if (index > 3) index else if (quoted) loop(index + 1, false) else loop(index + 1, true)
                        |}""".stripMargin,
      "Main.scala" -> "object Main { val first = Scan.loop(0, false); val second = Scan.loop(1, false) }"
    )
    assertNoFinding(found)(finding =>
      finding.rule == Finding.Rule.FlagArgument || finding.rule == Finding.Rule.ConstantArgument
    )
  }

  test("type arguments chosen by given resolution are not fixed by callers") {
    val found = findings(
      "Orders.scala" ->
        """object Orders { given lists[A](using order: Ordering[A]): Ordering[List[A]] = Ordering.by(_.headOption) }
                          |object Main {
                          |  import Orders.given
                          |  val first = summon[Ordering[List[Int]]]
                          |  val second = summon[Ordering[List[Int]]]
                          |}""".stripMargin
    )
    assertNoFinding(found)(_.rule == Finding.Rule.FixedTypeParameter)
  }

  test("arguments equal to their parameter's default are redundant") {
    val found = findings(
      "Net.scala" -> """object Net { def connect(host: String, port: Int = 80, secure: Boolean = false): String = host }
                       |case class Config(name: String, retries: Int = 3)""".stripMargin,
      "Main.scala" -> """object Main {
                        |  val first = Net.connect("a", 80)
                        |  val second = Net.connect("b", port = 443)
                        |  val third = Net.connect("c", secure = false)
                        |  val config = Config("x", 3)
                        |}""".stripMargin
    )
    assertFinding(
      found,
      "redundant-default",
      "Net.connect(host, port, secure).port is passed its default 80 explicitly at 1 calls"
    )
    assertFinding(found, "redundant-default", "secure is passed its default False explicitly at 1 calls")
    assertFinding(found, "redundant-default", "Config.retries is passed its default 3 explicitly at 1 calls")
    assertNoFinding(found)(_.detail.contains("default 443"))
  }

  test("defaults the compiler fills in, as inside a quote, and arguments differing inside Some are not redundant") {
    val found = findings(
      "Codec.scala" -> """import scala.quoted.*
                         |object Codec {
                         |  def group(name: String, minimum: Option[String] = Some("1.0")): String = name
                         |}
                         |object Wire {
                         |  object Codecs {
                         |    @scala.annotation.publicInBinary
                         |    private[Wire] def instance[A](name: String, nullary: Option[A] = None): String = name
                         |  }
                         |  object Macro {
                         |    def impl[A: Type](using Quotes): Expr[String] = '{ Codecs.instance[A]("x") }
                         |    def matched[A: Type](using Quotes): Expr[String] = Type.of[A] match {
                         |      case '[answer] => '{ Codecs.instance[Option[answer]]("y") }
                         |    }
                         |  }
                         |}""".stripMargin,
      "Main.scala" -> """object Main {
                        |  val newer = Codec.group("a", minimum = Some("1.1"))
                        |  val older = Codec.group("b", minimum = Some("1.0"))
                        |}""".stripMargin
    )
    assertNoFinding(found)(finding =>
      finding.rule == Finding.Rule.RedundantDefault && finding.detail.contains("nullary")
    )
    assertFinding(found, "redundant-default", "Codec.group(name, minimum).minimum is passed its default Some(\"1.0\")")
  }

  // Model-level cases pin the thresholds and exclusions directly, without compiling sources.
  private def node(key: String): Node =
    Node(key, "m", Output.Scope.Main, s"$key.scala", line = 1, generated = false, exported = false, outer = None)

  private def method(name: String, owner: String, fields: Vector[Algebra.Field]): Smells.Method =
    Smells.Method(name, name, owner, fields, overriding = false, exported = false, generated = false, test = false)

  private def field(name: String, kind: Algebra.Kind = Algebra.Kind.Number): Algebra.Field =
    Algebra.Field(name, Polynomial.atom("Int"), kind)

  private def site(arguments: Algebra.Argument*): Algebra.Site =
    Algebra.Site(arguments.toVector, test = false, own = false, location = "Main.scala:1")

  private def smells(model: Smells.Model): Vector[Finding] = {
    val keys = model.methods.map(_.node) ++ model.usages.map(_.method.node) ++ model.abstractions.map(_.node) ++
      model.instantiations.map(_.node)
    Smells.findings(model, Vector.empty, keys.map(key => key -> node(key)).toMap)
  }

  private def model(
      methods: Vector[Smells.Method] = Vector.empty,
      sites: Map[String, Vector[Algebra.Site]] = Map.empty,
      usages: Vector[Smells.Usage] = Vector.empty,
      abstractions: Vector[Smells.Abstraction] = Vector.empty,
      instantiations: Vector[Smells.Instantiation] = Vector.empty
  ): Smells.Model = Smells.Model(methods, sites, usages, abstractions, instantiations, Vector.empty)

  test("call thresholds are minimums, so more calls than the minimum still count") {
    val run   = method("lib.Api.run", "lib.Api", Vector(field("width")))
    val found = smells(model(
      methods = Vector(run),
      sites = Map(run.name -> Vector.fill(3)(site(Algebra.Argument.Number(80)))),
      usages = Vector(Smells.Usage(run, "Int", calls = 3, discarded = 3, testCalls = 0)),
      instantiations = Vector(Smells.Instantiation("lib.Api.wrap", "lib.Api", "A", Map("Int" -> 3), false, 0))
    ))
    assertFinding(found, "constant-argument", "Api.run.width is always 80 across 3 calls")
    assertFinding(found, "ignored-result", "discarded at all 3 calls")
    assertFinding(found, "fixed-type-parameter", "always Int across 3 uses")
  }

  test("results of methods a supertype, the compiler or another build fixes are not ignored") {
    val plain  = method("lib.Api.plain", "lib.Api", Vector.empty)
    val usages = Vector(
      plain.copy(name = "lib.Api.overriding", overriding = true),
      plain.copy(name = "lib.Api.generated", generated = true),
      plain.copy(name = "lib.Api.exported", exported = true)
    ).map(Smells.Usage(_, "Int", calls = 2, discarded = 2, testCalls = 0))
    assertNoFinding(smells(model(usages = usages)))(_.rule == Finding.Rule.IgnoredResult)
  }

  test("an implementation in another module than its abstraction is information") {
    val found = smells(model(abstractions =
      Vector(
        Smells.Abstraction("lib.Store", "lib.Store", "core", Vector("app.Memory" -> "app")),
        Smells.Abstraction("lib.Codec", "lib.Codec", "core", Vector("lib.Hex" -> "core"))
      )
    ))
    assertFinding(found, "single-implementation", "lib.Store has a single implementation")(using Finding.Level.Info)
    assertFinding(found, "single-implementation", "lib.Codec has a single implementation")
  }

  test("constant and flag arguments need every value known, and flags count both values") {
    val known   = method("lib.Api.known", "lib.Api", Vector(field("verbose", Algebra.Kind.Flag)))
    val unknown = method("lib.Api.unknown", "lib.Api", Vector(field("verbose", Algebra.Kind.Flag)))
    val found   = smells(model(
      methods = Vector(known, unknown),
      sites = Map(
        known.name   -> Vector(site(Algebra.Argument.True), site(Algebra.Argument.True), site(Algebra.Argument.False)),
        unknown.name -> Vector(site(Algebra.Argument.True), site(Algebra.Argument.Unknown))
      )
    ))
    assertFinding(found, "flag-argument", "Api.known.verbose is a literal at all 3 calls (2 true, 1 false)")
    assertNoFinding(found)(_.detail.contains("unknown"))
  }

  private def group(prefix: String, names: String*)(owners: String*): Vector[Smells.Method] =
    owners.toVector.map(owner => method(s"$owner.$prefix", owner, names.toVector.map(field(_))))

  private def clumps(methods: Vector[Smells.Method]): Vector[Finding] =
    smells(model(methods = methods)).filter(_.rule == Finding.Rule.DataClump)

  test("data clumps need three methods across more than one type") {
    val found = clumps(
      group("same", "a", "b", "c")("one.Same", "one.Same", "one.Same").zipWithIndex.map((method, index) =>
        method.copy(name = s"${method.name}$index", node = s"${method.node}$index")
      ) ++ group("pair", "d", "e", "f")("two.X", "two.Y") ++
        group("many", "g", "h", "i")("four.S", "four.R", "four.Q", "four.P")
    )
    assertEquals(found.size, 1, found.mkString("\n"))
    assert(
      found.head.detail.startsWith("(g: Int, h: Int, i: Int) recur in 4 methods across 4 types"),
      found.head.detail
    )
    assertEquals(found.map(_.node.name), Vector("four.P.many"))
  }

  test("overlapping data clumps over mostly the same methods are reported once, and disjoint ones each") {
    val shared = Vector("a", "b", "c", "x", "y", "z")
    val found  = clumps(
      group("first", "a", "b", "c")("one.A") ++ group("both", shared*)("one.B", "one.C") ++
        group("last", "x", "y", "z")("one.D") ++ group("other", "p", "q", "r")("two.E", "two.F", "two.G")
    )
    assertEquals(
      found.map(_.detail.takeWhile(_ != ')')).sorted,
      Vector("(a: Int, b: Int, c: Int", "(p: Int, q: Int, r: Int")
    )
  }

  test("at most twenty data clumps are reported") {
    val methods = (1 to 21).toVector.flatMap(index =>
      group("run", s"a$index", s"b$index", s"c$index")(s"g$index.X", s"g$index.Y", s"g$index.Z")
    )
    assertEquals(clumps(methods).size, 20)
  }

  test("long signatures only contribute their smallest groups to data clumps") {
    def signatures(size: Int) = Vector("p.A", "p.B", "p.C").map(owner =>
      method(
        s"$owner.run",
        owner,
        (Vector("a", "b", "c", "d") ++ (5 to size).map(index => s"$owner$index")).map(field(_))
      )
    )
    def widest(size: Int) = clumps(signatures(size)).map(_.detail.takeWhile(_ != ')').count(_ == ':'))
    assertEquals(widest(10), Vector(4))
    assertEquals(widest(11), Vector(3))
  }
}
