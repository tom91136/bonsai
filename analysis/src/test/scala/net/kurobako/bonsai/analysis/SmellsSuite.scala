package net.kurobako.bonsai.analysis

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

  test("a constant passed to an abstract method with several implementations picks a layout they share") {
    val found = findings(
      "Machines.scala" ->
        """trait Machine { def sample(slot: Int, into: Int): Int }
          |class Execute extends Machine { def sample(slot: Int, into: Int): Int = slot + into }
          |class Render extends Machine { def sample(slot: Int, into: Int): Int = slot - into }
          |trait Single { def read(slot: Int, into: Int): Int }
          |class Only extends Single { def read(slot: Int, into: Int): Int = slot + into }
          |object Main {
          |  def run(machine: Machine, single: Single, other: Int): Int =
          |    machine.sample(other, 56) + machine.sample(other + 1, 56) + single.read(other, 30) + single.read(other + 1, 30)
          |}""".stripMargin
    )
    assertEquals(
      found.filter(_.rule == Finding.Rule.ConstantArgument).map(_.detail),
      Vector("Single.read(slot, into).into is always 30 across 2 calls")
    )
  }

  test("a constant passed to a method that only one class implements, or a concrete one, is still constant") {
    val methods = Vector(
      method("lib.Api.run", "lib.Api", Vector(field("width"))),
      method("lib.Api.shared", "lib.Api", Vector(field("width"))).copy(shared = true)
    )
    val found = smells(model(
      methods = methods,
      sites = methods.map(_.name -> Vector.fill(2)(site(Algebra.Argument.Number(80)))).toMap
    ))
    assertEquals(found.map(_.detail), Vector("Api.run.width is always 80 across 2 calls"))
  }

  test("a string default passed explicitly is redundant") {
    assertFinding(
      findings(
        "Render.scala" -> "object Render { def fill(colour: String = \"red\"): Int = colour.length }",
        "Main.scala"   -> "object Main { val first = Render.fill(\"red\") }"
      ),
      "redundant-default",
      "Render.fill(colour).colour is passed its default \"red\" explicitly at 1 calls"
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

  test("a flag a method only passes to a constructor is data the value stores, not a choice between methods") {
    val found = findings(
      "Mesh.scala" ->
        """case class Vertex(x: Int, bgra: Boolean)
          |object Mesh { def uniform(x: Int, bgra: Boolean): Vertex = Vertex(x * 2, bgra) }""".stripMargin,
      "Main.scala" -> "object Main { val bgra = Mesh.uniform(1, true); val rgba = Mesh.uniform(2, false) }"
    )
    assertNoFinding(found)(_.rule == Finding.Rule.FlagArgument)
  }

  test("an inline method is specialised on the literal flags it is passed, so they select nothing at run time") {
    val found = findings(
      "Draw.scala" ->
        "object Draw { inline def draw(shape: String, filled: Boolean): String = if (filled) shape else shape.toLowerCase }",
      "Main.scala" -> "object Main { val solid = Draw.draw(\"a\", true); val hollow = Draw.draw(\"b\", false) }"
    )
    assertNoFinding(found)(_.rule == Finding.Rule.FlagArgument)
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

  test("a boolean method every caller negates is inverted") {
    assertFinding(
      findings(
        "Deadline.scala" -> "object Deadline { def isExpired(): Boolean = true }",
        "Main.scala"     ->
          """object Main {
            |  def first: Boolean = !Deadline.isExpired()
            |  def second: Boolean = !Deadline.isExpired()
            |}""".stripMargin
      ),
      "inverted-boolean",
      "Deadline.isExpired(): its Boolean result is inverted at all 2 calls"
    )
  }

  test("a boolean method that some caller reads directly is not inverted") {
    val found = findings(
      "Deadline.scala" -> "object Deadline { def isExpired(): Boolean = true }",
      "Main.scala"     ->
        """object Main {
          |  def first: Boolean = !Deadline.isExpired()
          |  def second: Boolean = Deadline.isExpired()
          |}""".stripMargin
    )
    assertNoFinding(found)(_.rule.id == "inverted-boolean")
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

  test("inline parameters, which each expansion specialises, are no data clump") {
    val found = findings(
      "Semantics.scala" ->
        """trait Algebra { def add(left: Long, right: Long): Long }
          |object Integer {
          |  inline def sum(inline algebra: Algebra, inline width: Int, inline signed: Boolean, value: Long): Long = algebra.add(value, width)
          |}
          |object Float {
          |  inline def sum(inline algebra: Algebra, inline width: Int, inline signed: Boolean, value: Long): Long = algebra.add(value, 1L)
          |  inline def max(inline algebra: Algebra, inline width: Int, inline signed: Boolean, value: Long): Long = algebra.add(value, 2L)
          |}""".stripMargin
    )
    assertNoFinding(found)(_.rule == Finding.Rule.DataClump)
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

  test(
    "an abstraction whose one implementation fixes its type parameter to a primitive keeps the implementation fast"
  ) {
    val found = findings(
      "Operands.scala" ->
        """abstract class Operands[A] { def first: A; def both: (A, A) = (first, first) }
          |class Longs(values: Array[Long]) extends Operands[Long] { def first: Long = values(0) }
          |abstract class Labels[A] { def first: A }
          |class Names extends Labels[String] { def first: String = "" }""".stripMargin
    )
    assertFinding(found, "single-implementation", "Labels has a single implementation, Names")
    assertNoFinding(found)(_.detail.contains("Operands"))
  }

  test("a trait only lambdas implement stands in for a function type, and is not speculative") {
    val found = findings(
      "Kernels.scala" ->
        """trait Kernel { def apply(left: Long, right: Long): Long }
          |object Kernels {
          |  def run(kernel: Kernel): Long = kernel(1L, 2L)
          |  val sum: Long  = run((left, right) => left + right)
          |  val diff: Long = run((left, right) => left - right)
          |}""".stripMargin
    )
    assertNoFinding(found)(_.rule == Finding.Rule.SingleImplementation)
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

  test("fixed type parameters of the definitions of one type are one finding that names them") {
    val found = findings(
      "Machines.scala" ->
        """trait Machine {
          |  def bind[A](value: A): A
          |  def leave[A](value: A): A
          |  def keep[A](value: A): A
          |}
          |trait Other { def pick[A](value: A): A }
          |object Run {
          |  def go(machine: Machine, other: Other): Int = {
          |    machine.bind(1) + machine.bind(2) + machine.leave(3) + machine.leave(4) + machine.keep(5) + machine.keep(6) +
          |      other.pick(7) + other.pick(8)
          |  }
          |}""".stripMargin
    ).filter(_.rule == Finding.Rule.FixedTypeParameter).map(_.detail)
    assertEquals(
      found,
      Vector(
        "3 type parameters of Machine are always given one type each (bind[A] is Int, keep[A] is Int, leave[A] is Int)",
        "Other.pick: type parameter A is always Int across 2 uses"
      )
    )
  }

  test("literal types given as type arguments are told apart, not widened to their class") {
    val found = findings(
      "Floats.scala" ->
        """case class FloatingPoint[W <: Int](bits: Long)
          |object Floats {
          |  def negate[W <: Int](value: FloatingPoint[W]): FloatingPoint[W] = FloatingPoint[W](-value.bits)
          |  val half   = negate(FloatingPoint[16](1L))
          |  val single = negate(FloatingPoint[32](1L))
          |}""".stripMargin
    )
    assertNoFinding(found)(_.rule == Finding.Rule.FixedTypeParameter)
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
  private def node(key: String, generated: Boolean = false): Node =
    Node(key, "m", Output.Scope.Main, s"$key.scala", line = 1, generated = generated, exported = false, outer = None)

  private def method(name: String, owner: String, fields: Vector[Algebra.Field]): Smells.Method =
    Smells.Method(name, name, owner, fields, overriding = false, exported = false, test = false)

  private def field(name: String, kind: Algebra.Kind = Algebra.Kind.Number): Algebra.Field =
    Algebra.Field(name, Polynomial.atom("Int"), kind)

  private def site(arguments: Algebra.Argument*): Algebra.Site =
    Algebra.Site(arguments.toVector, test = false, own = false, location = "Main.scala:1")

  private def smells(model: Smells.Model, generated: Boolean = false): Vector[Finding] = {
    val keys = model.methods.map(_.node) ++ model.usages.map(_.method.node) ++ model.abstractions.map(_.node) ++
      model.instantiations.map(_.node) ++ model.moduleUses.map(_.caller)
    Smells.findings(model, Vector.empty, keys.map(key => key -> node(key, generated)).toMap)
  }

  private def model(
      methods: Vector[Smells.Method] = Vector.empty,
      sites: Map[String, Vector[Algebra.Site]] = Map.empty,
      usages: Vector[Smells.Usage] = Vector.empty,
      abstractions: Vector[Smells.Abstraction] = Vector.empty,
      instantiations: Vector[Smells.Instantiation] = Vector.empty,
      moduleUses: Vector[Modules.Use] = Vector.empty
  ): Smells.Model = Smells.Model(methods, sites, usages, abstractions, instantiations, Vector.empty, moduleUses)

  test("a text parameter every caller gives the same text is constant") {
    val run   = method("lib.Api.run", "lib.Api", Vector(field("name", Algebra.Kind.Text)))
    val found = smells(model(
      methods = Vector(run),
      sites = Map(run.name -> Vector(site(Algebra.Argument.Text("a")), site(Algebra.Argument.Text("a"))))
    ))
    assertFinding(found, "constant-argument", "Api.run.name is always \"a\" across 2 calls")
  }

  test("a method called once is not constant in what it is passed, and one called twice with one value is") {
    val once  = method("lib.Api.once", "lib.Api", Vector(field("width")))
    val twice = method("lib.Api.twice", "lib.Api", Vector(field("width")))
    val found = smells(model(
      methods = Vector(once, twice),
      sites = Map(
        once.name  -> Vector(site(Algebra.Argument.Number(80))),
        twice.name -> Vector(site(Algebra.Argument.Number(80)), site(Algebra.Argument.Number(80)))
      )
    ))
    assertEquals(found.map(_.detail), Vector("Api.twice.width is always 80 across 2 calls"))
  }

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
      plain.copy(name = "lib.Api.exported", exported = true)
    ).map(Smells.Usage(_, "Int", calls = 2, discarded = 2, testCalls = 0))
    assertNoFinding(smells(model(usages = usages)))(_.rule == Finding.Rule.IgnoredResult)
  }

  test("results of generated methods are ignored like those of handwritten ones") {
    val usages = Vector(method("lib.Api.generated", "lib.Api", Vector.empty))
      .map(Smells.Usage(_, "Int", calls = 2, discarded = 2, testCalls = 0))
    assertFinding(
      smells(model(usages = usages), generated = true),
      "ignored-result",
      "Api.generated: its Int result is discarded at all 2 calls"
    )
  }

  test("a library boolean method every caller inverts is reported, as the name still reads backwards") {
    val ready = method("lib.Api.ready", "lib.Api", Vector.empty).copy(exported = true)
    val found = smells(model(usages =
      Vector(
        Smells.Usage(ready, "Boolean", calls = 2, discarded = 0, testCalls = 0, negated = 2)
      )
    ))

    assertFinding(found, "inverted-boolean", "Api.ready: its Boolean result is inverted at all 2 calls")
  }

  test("a result of a method called once, or of the API of a library, that no caller uses is reported") {
    val single  = method("lib.Api.single", "lib.Api", Vector.empty)
    val library = method("lib.Api.library", "lib.Api", Vector.empty).copy(exported = true)
    val found   = smells(model(usages =
      Vector(
        Smells.Usage(single, "Int", calls = 1, discarded = 1, testCalls = 0),
        Smells.Usage(library, "Int", calls = 2, discarded = 2, testCalls = 0)
      )
    ))

    assertFinding(found, "unused-return-value", "Api.single: its Int result is never used")
    assertFinding(found, "unused-return-value", "Api.library: its Int result is never used")
    assertNoFinding(found)(finding => finding.rule == Finding.Rule.IgnoredResult)
  }

  test("a result that a caller uses is not reported as unused") {
    val single = method("lib.Api.single", "lib.Api", Vector.empty)
    val found  = smells(model(usages = Vector(Smells.Usage(single, "Int", calls = 1, discarded = 0, testCalls = 0))))

    assertNoFinding(found)(finding => finding.rule == Finding.Rule.UnusedReturnValue)
  }

  test(
    "a method with @SuppressWarnings(UnusedReturnValue) is not reported as unused-return-value while an unsuppressed sibling is reported"
  ) {
    val quiet = method("lib.Api.quiet", "lib.Api", Vector.empty).copy(suppressed = Set("UnusedReturnValue"))
    val noisy = method("lib.Api.noisy", "lib.Api", Vector.empty)
    val found = smells(model(usages =
      Vector(
        Smells.Usage(quiet, "Int", calls = 1, discarded = 1, testCalls = 0),
        Smells.Usage(noisy, "Int", calls = 1, discarded = 1, testCalls = 0)
      )
    ))

    assertFinding(found, "unused-return-value", "Api.noisy: its Int result is never used")
    assertNoFinding(found)(finding => finding.detail.contains("Api.quiet"))
  }

  test("a reflective lookup of a class of a module the caller does not require is reported") {
    val use   = Modules.Use("lib.App", "lib/App.java", 12, "app.module", "lib.module")
    val found = Smells.findings(
      Smells.Model(Vector.empty, Map.empty, Vector.empty, Vector.empty, Vector.empty, Vector.empty, Vector(use)),
      Vector.empty,
      Map("lib.App" -> node("lib.App"))
    )

    assertFinding(
      found,
      "module-requirement",
      "The module 'app.module' does not have the module 'lib.module' in requirements"
    )
    val finding = found.find(_.rule == Finding.Rule.ModuleRequirement).get
    assertEquals(finding.node.source, "lib/App.java")
    assertEquals(finding.node.line, 12)
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

  private def group(prefix: String, names: String*)(owners: String*): Vector[Smells.Method] = {
    val fields = names.toVector.map(field(_))
    owners.toVector.map(owner => method(s"$owner.$prefix", owner, fields))
  }

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

  test("primitive parameters in hot classes, and inline methods, are no data clump, since an object would allocate") {
    val primitive = (name: String) => Algebra.Field(name, Polynomial.atom("Int"), Algebra.Kind.Number, primitive = true)
    val methods   = Vector("one.A", "one.B", "two.C").map(owner =>
      method(s"$owner.run", owner, Vector("a", "b", "c").map(primitive(_)))
    )
    val keys                                             = methods.map(_.node)
    def found(hot: Option[Set[String]], inline: Boolean) =
      Smells.findings(
        model(methods = methods.map(_.copy(inline = inline))),
        Vector.empty,
        keys.map(key => key -> node(key)).toMap,
        hot
      ).filter(_.rule == Finding.Rule.DataClump)
    assertEquals(found(None, inline = false).size, 1)
    assertEquals(found(Some(Set("one.A.run")), inline = false).size, 0)
    assertEquals(found(None, inline = true).size, 0)
  }

  test("primitive parameters recurring in the methods of a hot class are no data clump, references still are") {
    val sources = Seq(
      "Kernels.scala" ->
        """package hot
          |class Operand(val id: Int)
          |object Kernels {
          |  def one(dest: Int, index: Int, size: Int, out: Array[Int]): Unit  = out(dest) = index + size
          |  def two(dest: Int, index: Int, size: Int, out: Array[Int]): Unit  = out(dest) = index - size
          |  def three(dest: Int, index: Int, size: Int, out: Array[Int]): Unit = out(dest) = index * size
          |}
          |object Others {
          |  def four(dest: Int, index: Int, size: Int, out: Array[Int]): Unit = out(dest) = index / size
          |}
          |object Operands {
          |  def one(left: Operand, right: Operand, third: Operand): Int  = left.id + right.id + third.id
          |  def two(left: Operand, right: Operand, third: Operand): Int  = left.id - right.id - third.id
          |  def three(left: Operand, right: Operand, third: Operand): Int = left.id * right.id * third.id
          |}
          |object Calls {
          |  def four(left: Operand, right: Operand, third: Operand): Int = left.id
          |}""".stripMargin
    )
    def clumps(hot: Vector[String]) = {
      val result = extract(sources, hot = hot)
      Smells.findings(result.smells, result.algebra.products, result.graph.nodes, result.hot)
        .filter(_.rule == Finding.Rule.DataClump).map(_.detail)
    }
    assertEquals(clumps(Vector.empty).size, 2, clumps(Vector.empty).mkString("\n"))
    val marked = clumps(Vector("hot.Kernels", "hot.Others", "hot.Operands"))
    assertEquals(marked.size, 1, marked.mkString("\n"))
    assert(marked.head.contains("left: Operand"), marked.head)
  }

  test("every data clump is reported, so a fix shows as one fewer") {
    val methods = (1 to 21).toVector.flatMap(index =>
      group("run", s"a$index", s"b$index", s"c$index")(s"g$index.X", s"g$index.Y", s"g$index.Z")
    )
    assertEquals(clumps(methods).size, 21)
  }

  test("long signatures only contribute their smallest groups to data clumps") {
    val owners                = Vector("p.A", "p.B", "p.C")
    val shared                = Vector("a", "b", "c", "d")
    def signatures(size: Int) = owners.map(owner =>
      method(s"$owner.run", owner, (shared ++ (5 to size).map(index => s"$owner$index")).map(field(_)))
    )
    def widest(size: Int) = clumps(signatures(size)).map(_.detail.takeWhile(_ != ')').count(_ == ':'))
    assertEquals(widest(10), Vector(4))
    assertEquals(widest(11), Vector(3))
  }

  private def linked(sources: (String, String)*): Vector[Finding] = {
    val result = extract(sources)
    Smells.findings(result.smells, result.algebra.products, result.graph.nodes, edges = result.graph.edges)
  }

  test("an abstraction is not reported when naming its one implementation would close a file cycle") {
    val found = linked(
      "Context.scala"  -> "package lib\ntrait Context { def emit(op: Int): Unit }",
      "Family.scala"   -> "package lib\nobject Family { def lower(context: Context): Unit = context.emit(1) }",
      "Compiler.scala" ->
        """package lib
          |class Compiler extends Context {
          |  def emit(op: Int): Unit = ()
          |  def run(): Unit = Family.lower(this)
          |}""".stripMargin
    )
    assertNoFinding(found)(_.detail.contains("Context"))
  }

  test("an abstraction whose one implementation is apart from its users is still reported") {
    val found = linked(
      "Store.scala"    -> "package lib\ntrait Store { def put(key: Int): Unit }",
      "MapStore.scala" -> "package lib\nclass MapStore extends Store { def put(key: Int): Unit = () }",
      "User.scala"     -> "package lib\nobject User { def save(store: Store): Unit = store.put(1) }"
    )
    assertFinding(found, "single-implementation", "lib.Store has a single implementation, lib.MapStore")
  }

  test("a concrete mixin trait is not an abstraction, while one declaring an abstract member is") {
    val mixin = linked(
      "Machine.scala" -> "package lib\ntrait Machine { def park(): Int; def jump(): Int }",
      "Pending.scala" -> "package lib\ntrait Pending extends Machine { def park(): Int = -1 }",
      "Run.scala"     -> "package lib\nclass Run extends Machine with Pending { def jump(): Int = 0 }"
    )
    assertNoFinding(mixin)(_.detail.contains("Pending"))
    val declaring = linked(
      "Machine.scala" -> "package lib\ntrait Machine { def park(): Int; def jump(): Int }",
      "Pending.scala" -> "package lib\ntrait Pending extends Machine { def park(): Int = -1; def extra(): Int }",
      "Run.scala"     -> "package lib\nclass Run extends Pending { def jump(): Int = 0; def extra(): Int = 1 }"
    )
    assertFinding(declaring, "single-implementation", "lib.Pending has a single implementation, lib.Run")
  }

  test("a concrete mixin trait named as a type is an abstraction") {
    val found = linked(
      "Machine.scala" -> "package lib\ntrait Machine { def park(): Int; def jump(): Int }",
      "Pending.scala" -> "package lib\ntrait Pending extends Machine { def park(): Int = -1 }",
      "Run.scala"     -> "package lib\nclass Run extends Machine with Pending { def jump(): Int = 0 }",
      "Use.scala"     -> "package lib\nobject Use { def run(pending: Pending): Int = pending.park() }"
    )
    assertFinding(found, "single-implementation", "lib.Pending has a single implementation, lib.Run")
  }

  private def clumpsIn(sources: (String, String)*): Vector[String] = {
    val result = extract(sources)
    Smells.findings(result.smells, result.algebra.products, result.graph.nodes, result.hot)
      .filter(_.rule == Finding.Rule.DataClump).map(_.detail)
  }

  test("defs local to an inline method are no data clump, since each expansion specialises them") {
    val context =
      """class Ctx {
        |  def each(body: Int => Unit): Unit = ()
        |  def read(lane: Int, source: Int): Long = lane.toLong + source
        |  def set(lane: Int, target: Int, value: Long): Unit = ()
        |}""".stripMargin
    val loops = Seq(
      "Loops.scala" ->
        s"""package loops
           |$context
           |class Loop(val context: Ctx) {
           |  inline def unary(dst: Int, src: Int): Unit = {
           |    def lanes(context: Ctx, dst: Int, src: Int): Unit = context.each(l => context.set(l, dst, context.read(l, src)))
           |    lanes(context, dst, src)
           |  }
           |  inline def broadcast(dst: Int, src: Int): Unit = {
           |    def lanes(context: Ctx, dst: Int, src: Int): Unit = context.each(l => context.set(l, dst, context.read(0, src)))
           |    lanes(context, dst, src)
           |  }
           |  inline def reduce(dst: Int, src: Int): Unit = {
           |    def lanes(context: Ctx, dst: Int, src: Int): Unit = context.each(l => context.set(dst, l, context.read(l, src)))
           |    lanes(context, dst, src)
           |  }
           |}""".stripMargin
    )
    assertEquals(clumpsIn(loops*), Vector.empty)
    val methods = Seq(
      "Methods.scala" ->
        s"""package loops
           |$context
           |object A {
           |  def unary(context: Ctx, dst: Int, src: Int): Unit = context.each(l => context.set(l, dst, context.read(l, src)))
           |  def move(context: Ctx, dst: Int, src: Int): Unit = context.each(l => context.set(l, src, context.read(l, dst)))
           |}
           |object B {
           |  def broadcast(context: Ctx, dst: Int, src: Int): Unit = context.each(l => context.set(l, dst, context.read(0, src)))
           |}""".stripMargin
    )
    assertEquals(clumpsIn(methods*).size, 1, clumpsIn(methods*).mkString("\n"))
  }

  test("any-typed parameters recurring across methods are no data clump, typed ones still are") {
    val untyped = Seq(
      "Fragments.scala" ->
        """package render
          |trait Fragments { def storeElement(pointer: Any, offset: Any, bytes: Any, value: Any): Unit = () }
          |trait Images { def storeLevel(offset: Any, bytes: Any, value: Any): Unit = () }
          |trait Tensors { def storeAbsolute(address: Any, offset: Any, bytes: Any, value: Any, pointee: Any): Unit = () }""".stripMargin
    )
    assertEquals(clumpsIn(untyped*), Vector.empty)
    val typed = Seq(
      "Fragments.scala" ->
        """package render
          |trait Fragments { def storeElement(pointer: Long, offset: Int, bytes: Array[Byte], value: Long): Unit = () }
          |trait Images { def storeLevel(offset: Int, bytes: Array[Byte], value: Long): Unit = () }
          |trait Tensors { def storeAbsolute(address: Long, offset: Int, bytes: Array[Byte], value: Long): Unit = () }""".stripMargin
    )
    assertEquals(clumpsIn(typed*).size, 1, clumpsIn(typed*).mkString("\n"))
  }

  test("methods every call of which passes literals for their parameters are row templates, not data clumps") {
    def sources(arguments: (String, String, String) => String) = Seq(
      "Forms.scala" ->
        """package forms
          |object Chains {
          |  def chain(opcode: String, first: String, second: String): Vector[String] = Vector(opcode, first, second)
          |}
          |object Loads {
          |  def load(opcode: String, first: String, second: String): Vector[String] = Vector(opcode, "pointer", first, second)
          |  def store(opcode: String, first: String, second: String): Vector[String] = Vector(opcode, "pointer", second, first)
          |}
          |object Rows {
          |  def table(opcode: String, first: String, second: String): Vector[Vector[String]] = Vector(
          |    Chains.chain(%s),
          |    Loads.load(%s),
          |    Loads.store(%s)
          |  )
          |}""".stripMargin.format(
          arguments("OpAccessChain", "member", "index"),
          arguments("OpLoadKHR", "layout", "stride"),
          arguments("OpStoreKHR", "layout", "stride")
        )
    )
    assertEquals(clumpsIn(sources((_, _, _) => "\"op\", \"x\", \"y\"")*), Vector.empty)
    val computed = (_: String, _: String, _: String) => "opcode, first, second"
    val fired    = clumpsIn(sources(computed)*)
    assertEquals(fired.size, 1, fired.mkString("\n"))
  }

  test("every parameter list of a method takes part in data clumps, not only the first") {
    val sources = Seq(
      "Contexts.scala" ->
        """package contexts
          |class Site
          |object A { def one(x: Int)(site: Site, y: Long, k: Int): Int = x + k }
          |object B { def two(z: Int)(site: Site, y: Long, k: Int): Int = z - k }
          |object C { def three(w: Int)(site: Site, y: Long, k: Int): Int = w * k }""".stripMargin
    )
    val found = clumpsIn(sources*)
    assertEquals(found.size, 1, found.mkString("\n"))
    assert(found.head.startsWith("(k: Int, site: Site, y: Long)"), found.head)
  }

  test("context parameters are no data clump, so passing a recurring value as one is no clump of its own") {
    val lowering = Seq(
      "Lowering.scala" ->
        """package lower
          |class Operand
          |class Site
          |object A { def lower(resultType: Operand, dest: Operand)(using site: Site): Int = 1 }
          |object B { def emit(resultType: Operand, dest: Operand)(using site: Site): Int = 2 }
          |object C { def fold(resultType: Operand, dest: Operand)(using site: Site): Int = 3 }""".stripMargin
    )
    assertEquals(clumpsIn(lowering*), Vector.empty)
    val widened = Seq(
      "Lowering.scala" ->
        """package lower
          |class Operand
          |class Site
          |object A { def lower(resultType: Operand, dest: Operand, width: Int)(using site: Site): Int = 1 }
          |object B { def emit(resultType: Operand, dest: Operand, width: Int)(using site: Site): Int = 2 }
          |object C { def fold(resultType: Operand, dest: Operand, width: Int)(using site: Site): Int = 3 }""".stripMargin
    )
    val found = clumpsIn(widened*)
    assertEquals(found.size, 1, found.mkString("\n"))
    assert(found.head.startsWith("(dest: "), found.head)
    assert(found.head.contains("width: Int"), found.head)
    assert(!found.head.contains("site"), found.head)
  }

  test("a recurring triple in an explicit later parameter list is a data clump") {
    val found = clumpsIn(
      "Later.scala" ->
        """package later
          |class Site
          |object A { def one(a: Int)(y: Long, z: Int, w: Int): Int = a }
          |object B { def two(b: Int)(y: Long, z: Int, w: Int): Int = b + 1 }
          |object C { def three(c: Int)(y: Long, z: Int, w: Int)(using site: Site): Int = c + 2 }""".stripMargin
    )
    assertEquals(found.size, 1, found.mkString("\n"))
    assert(found.head.startsWith("(w: Int, y: Long, z: Int)"), found.head)
  }
}
