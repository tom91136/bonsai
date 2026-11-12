package net.kurobako.bonsai.analysis

class AlgebraSuite extends AnalysisSuite {
  import Polynomial.*

  private val a = atom("A")
  private val b = atom("B")
  private val c = atom("C")

  private def nested(depth: Int): Polynomial = (1 to depth).foldLeft(a) { (inner, level) =>
    power(inner * atom(s"X$level") + inner * atom(s"Y$level"), atom(s"E$level"))
  }

  private def findings(sources: (String, String)*): Vector[Finding] = findingsWithTests(sources, Nil)

  private def findingsWithTests(main: Seq[(String, String)], test: Seq[(String, String)]): Vector[Finding] = {
    val result = extract(main, test, generated = GeneratedSources)
    Algebra.findings(result.algebra, result.graph.nodes)
  }

  test("semiring laws hold in the canonical form") {
    assertEquals(a * b, b * a)
    assertEquals((a + b) + c, a + (b + c))
    assertEquals(a * (b + c), a * b + a * c)
    assertEquals(a * One + Zero, a)
    assertNotEquals(a * a, a + a)
  }

  test("exponent laws relate curried, tupled and split functions") {
    assertEquals(power(c, a * b), power(power(c, b), a))
    assertEquals(power(c, a + b), power(c, a) * power(c, b))
    assertEquals(power(b * c, a), power(b, a) * power(c, a))
    assertEquals(power(a, constant(2)), a * a)
    assertEquals(power(a, One), a)
    assertEquals(power(a, Zero), One)
  }

  test("expansions beyond the budget stay deterministic") {
    val wide = sum((1 to 20).map(index => atom(s"X$index")))
    assertEquals(wide * wide * wide, wide * (wide * wide))
    assert((wide * wide * wide).key.contains("<"), (wide * wide * wide).key)
  }

  test("keys of many-field products stay bounded as the fields grow") {
    val field = sum((1 to 3).map(index => atom(s"scala.collection.immutable.Option$index")))

    def keyLength(fields: Int): Int = product(Vector.fill(fields)(field)).key.length
    assert(keyLength(28) < 4 * keyLength(20), (keyLength(20), keyLength(28)))
  }

  test("keys of nested powers over sums stay bounded as the nesting grows") {
    assert(nested(16).key.length < 4 * nested(8).key.length, (nested(8).key.length, nested(16).key.length))
  }

  test("messages of nested powers over sums stay bounded as the nesting grows") {
    assert(
      nested(16).toString.length < 4 * nested(8).toString.length,
      (nested(8).toString.length, nested(16).toString.length)
    )
  }

  test("a power over a sum of atoms shows the sum as its base") {
    assertEquals(power(a + b, c).toString, "(A + B)^(C)")
  }

  test("an opaque atom shows its first eight digits in messages and its full digest in keys") {
    val wide      = sum((1 to 17).map(index => atom(s"pkg.X$index")))
    val collapsed = wide * wide
    assert(collapsed.toString.matches("<[0-9a-f]{8}>"), collapsed.toString)
    assert(collapsed.key.matches("<[0-9a-f]{64}>"), collapsed.key)
  }

  test("distinct opaque atoms in one message keep distinct short forms") {
    val wide  = sum((1 to 17).map(index => atom(s"pkg.X$index")))
    val other = sum((1 to 17).map(index => atom(s"pkg.Y$index")))
    val parts = (wide * wide + wide * other).toString.split(" \\+ ").toVector
    assertEquals(parts.size, 2)
    assert(parts.forall(_.matches("<[0-9a-f]{8}>")), parts)
    assertNotEquals(parts(0), parts(1))
  }

  test("equal operands built in different orders give equal keys") {
    val fields   = (1 to 4).map(index => sum((1 to 3).map(j => atom(s"pkg$index.Option$j"))))
    val forward  = product(fields)
    val backward = product(fields.reverse)
    assertEquals(forward, backward)
    assertEquals((forward * forward).key, (backward * backward).key)
    assertEquals((forward * forward * forward).key, (backward * backward * backward).key)
  }

  test("products with the same fields and shape are duplicates regardless of order, tupling and currying") {
    val found = findings(
      "P.scala" -> "case class P(x: Int, y: String, f: (Long, Char) => Byte)",
      "Q.scala" -> "case class Q(y: String, f: Long => Char => Byte, x: Int)",
      "R.scala" -> "case class R(a: Option[Int], b: String, c: Long)",
      "S.scala" -> "case class S(a: Either[Unit, Int], c: Long, b: String)",
      "T.scala" -> "case class T(a: Int, b: String, c: Long)"
    )
    assertFinding(found, "duplicate-shape", "P, Q")
    assertFinding(found, "duplicate-shape", "R, S")
    assertNoFinding(found)(_.detail.contains("T"))
  }

  test("types of one shape that are an intended pair are not duplicates, while the rest of a group still are") {
    val result = extract(Seq(
      "Layouts.scala" ->
        """package bits
          |case class Image(x: Int, y: String, f: (Long, Char) => Byte)
          |case class Buffer(y: String, f: Long => Char => Byte, x: Int)
          |case class Sampler(x: Int, f: (Long, Char) => Byte, y: String)
          |case class Left(a: Int, b: String, c: Long)
          |case class Right(a: Int, b: String, c: Long)""".stripMargin
    ))
    def shapes(intended: Intended) =
      Algebra.findings(result.algebra, result.graph.nodes, intended).collect {
        case finding if finding.rule == Finding.Rule.DuplicateShape => finding.detail.takeWhile(_ != ':')
      }
    assertEquals(shapes(Intended()).size, 2)
    assertEquals(shapes(Intended(Vector("bits.Left" -> "bits.Right"))).size, 1)
    assertEquals(shapes(Intended(Vector("bits.Right" -> "bits.Left"))).size, 1)
    assertEquals(shapes(Intended(Vector("bits.Image" -> "bits.Buffer"))).size, 2)
    assertEquals(shapes(Intended(Vector("bits.Image" -> "bits.*", "bits.Buffer" -> "bits.Sampler"))).size, 1)
    assertEquals(shapes(Intended(Vector("bits.*" -> "bits.*"))).size, 0)
  }

  test("types of one shape that generic derivation builds mirror an outside format and are not duplicates") {
    val found = findings(
      "Support.scala" ->
        """trait Codec[A]
          |object Codec { def derived[A](using scala.deriving.Mirror.Of[A]): Codec[A] = new Codec[A] {} }
          |case class FirstWire(id: Int, label: String) derives Codec
          |case class SecondWire(id: Int, label: String) derives Codec
          |case class FirstPlain(count: Long, tag: Char)
          |case class SecondPlain(count: Long, tag: Char)
          |case class MixedWire(size: Byte, name: Short) derives Codec
          |case class MixedPlain(size: Byte, name: Short)""".stripMargin
    )
    assertNoFinding(found)(finding =>
      finding.rule == Finding.Rule.DuplicateShape && finding.detail.contains("Wire") &&
        !finding.detail.contains("Mixed")
    )
    assertFinding(found, "duplicate-shape", "FirstPlain, SecondPlain")
    assertFinding(found, "duplicate-shape", "MixedPlain, MixedWire")
  }

  test("public types that generic derivation builds are not duplicates") {
    val result = extract(
      main = Seq(
        "Support.scala" ->
          """package support
            |trait Codec[A]
            |object Codec { def derived[A](using scala.deriving.Mirror.Of[A]): Codec[A] = new Codec[A] {} }
            |""".stripMargin,
        "A.scala" ->
          """package foo
            |import support.Codec
            |case class WireA(offset: Int, width: Int) derives Codec
            |case class PlainA(count: Long, tag: Char)
            |""".stripMargin,
        "B.scala" ->
          """package bar
            |import support.Codec
            |case class WireB(offset: Int, width: Int) derives Codec
            |case class PlainB(count: Long, tag: Char)
            |""".stripMargin
      ),
      libraries = Set("m")
    )
    val found = Algebra.findings(result.algebra, result.graph.nodes)
    assertFinding(found, "duplicate-shape", "PlainA")
    assertNoFinding(found)(finding => finding.rule == Finding.Rule.DuplicateShape && finding.detail.contains("Wire"))
  }

  test("function types with named parameters are plain functions") {
    val found = findings(
      "P.scala" -> "case class P(x: Int, y: String, f: (Long, Char) => Byte)",
      "Q.scala" -> "case class Q(y: String, f: (count: Long, symbol: Char) => Byte, x: Int)"
    )
    assertFinding(found, "duplicate-shape", "P, Q")
  }

  test("companion fields are not fields of the case classes they belong to") {
    val found = findings(
      "P.scala" -> """case class P(x: Int, y: Int, z: Int, w: Int, v: Int)
        |object P { val ALPHA = "a"; val BETA = "b"; val GAMMA = "c" }""".stripMargin,
      "Q.scala" -> "case class Q(a: Int, b: Int, c: Int, d: Int, e: Int)"
    )
    assertFinding(found, "duplicate-shape", "P, Q")(using Finding.Level.Info)
  }

  test("context functions are not curried plain functions") {
    val found = findings(
      "Contexts.scala" ->
        """type Contextual = Int ?=> String
          |type PlainFunction = Int => String
          |case class Context(value: (Contextual, Int))
          |case class Plain(value: (PlainFunction, Int))""".stripMargin
    )
    assertNoFinding(found)(_.rule == Finding.Rule.DuplicateShape)
  }

  test("duplicate-shape is silent for private types in independent modules") {
    val result = extract(
      main = Seq(
        "Amd.scala" ->
          """package amdgpu
            |private[amdgpu] case class BitField(offset: Int, width: Int)
            |""".stripMargin,
        "Nvidia.scala" ->
          """package nvidia
            |private[nvidia] case class Field(offset: Int, width: Int)
            |""".stripMargin
      )
    )
    val found = Algebra.findings(result.algebra, result.graph.nodes).filter(_.rule == Finding.Rule.DuplicateShape)
    assertEquals(found, Vector.empty)
  }

  test("duplicate-shape keeps firing for public types across packages or private types in one package") {
    val publicResult = extract(
      main = Seq(
        "A.scala" -> "package foo\ncase class PublicA(offset: Int, width: Int)",
        "B.scala" -> "package bar\ncase class PublicB(offset: Int, width: Int)"
      ),
      libraries = Set("m")
    )
    val publicFound =
      Algebra.findings(publicResult.algebra, publicResult.graph.nodes).filter(_.rule == Finding.Rule.DuplicateShape)
    assert(publicFound.nonEmpty, "public types across packages must keep firing")

    val localResult = extract(
      main = Seq(
        "Local.scala" ->
          """package samemod
            |private[samemod] case class LocalA(offset: Int, width: Int)
            |private[samemod] case class LocalB(offset: Int, width: Int)
            |""".stripMargin
      )
    )
    val localFound =
      Algebra.findings(localResult.algebra, localResult.graph.nodes).filter(_.rule == Finding.Rule.DuplicateShape)
    assert(localFound.nonEmpty, "private types in the same module must keep firing")
  }

  test("alternatives of the same shape are reported together as information") {
    given Finding.Level = Finding.Level.Info
    assertFinding(
      findings("Op.scala" ->
        "enum Op { case Add(l: Int, r: Int); case Sub(a: Int, b: Int); case Neg(v: Int); case Nop }"),
      "identical-cases",
      "2 cases",
      "Add, Sub"
    )
  }

  test("fields shared by several alternatives are reported as a common factor") {
    given Finding.Level = Finding.Level.Info
    assertFinding(
      findings(
        "Access.scala" -> """sealed trait Access
                            |case class Load(address: Long, scalar: Int, width: Int) extends Access
                            |case class Store(address: Long, scalar: Int, value: Long) extends Access
                            |case class Swap(address: Long, scalar: Int, value: Long, expected: Long) extends Access
                            |case class Gather(address: Long, scalar: Int, lanes: Int) extends Access
                            |case class Scatter(address: Long, scalar: Int, lanes: Int) extends Access
                            |case class Push(address: Long, scalar: Int) extends Access
                            |case class Pop(address: Long, scalar: Int) extends Access
                            |case class Fence(scope: String) extends Access""".stripMargin
      ),
      "common-factor",
      "7 of 8 cases",
      "address: Long",
      "scalar: Int"
    )
  }

  test("a common factor shared by fewer than eight cases is not reported") {
    assertNoFinding(
      findings(
        "Access.scala" -> """sealed trait Access
                            |case class Load(address: Long, scalar: Int, width: Int) extends Access
                            |case class Store(address: Long, scalar: Int, value: Long) extends Access
                            |case class Swap(address: Long, scalar: Int, value: Long, expected: Long) extends Access
                            |case class Gather(address: Long, scalar: Int, lanes: Int) extends Access
                            |case class Scatter(address: Long, scalar: Int, lanes: Int) extends Access
                            |case class Push(address: Long, scalar: Int) extends Access
                            |case class Fence(scope: String) extends Access""".stripMargin
      )
    )(_.rule == Finding.Rule.CommonFactor)
  }

  test("mutually exclusive options are reported, including named and default arguments") {
    assertFinding(
      findings(
        "Hit.scala"  -> "case class Hit(first: Option[Int] = None, second: Option[String] = None)",
        "Main.scala" -> "object Main { val hits = List(Hit(Some(1), None), Hit(second = Some(\"x\")), Hit()) }"
      ),
      "exclusive-options",
      "at most one of (first, second)",
      "never Int*String"
    )
  }

  test("a flag that mirrors an option is redundant") {
    assertFinding(
      findings(
        "Load.scala" -> "case class Load(done: Boolean, value: Option[Int])",
        "Main.scala" -> "object Main { val loads = List(Load(true, Some(1)), Load(false, None), Load(true, Some(2))) }"
      ),
      "redundant-flag",
      "Load.done is determined by Load.value"
    )
  }

  test("fields passed the same value at every construction are constant") {
    val found = findings(
      "Config.scala" -> "case class Config(verbose: Boolean, label: Option[String], count: Int)",
      "Main.scala"   -> "object Main { val configs = List(Config(false, None, 1), Config(false, None, 2)) }"
    )
    assertFinding(found, "constant-field", "Config.verbose is always False")
    assertFinding(found, "constant-field", "Config.label is always Absent")
  }

  test("fields of a class that generic derivation builds are not constant across the constructions seen") {
    val found = findings(
      "Config.scala" ->
        """trait Reader[A]
          |object Reader { def derived[A](using scala.deriving.Mirror.Of[A]): Reader[A] = new Reader[A] {} }
          |case class Loaded(verify: Boolean, label: Option[String], count: Int) derives Reader
          |case class Built(verify: Boolean, label: Option[String], count: Int)
          |object Decoder {
          |  inline def decode[A]: Option[A] = scala.compiletime.summonFrom {
          |    case _: scala.deriving.Mirror.Of[A] => None
          |    case _                               => None
          |  }
          |}
          |case class Decoded(verify: Boolean, count: Int)""".stripMargin,
      "Main.scala" ->
        """object Main {
          |  val loaded  = List(Loaded(false, None, 1), Loaded(false, None, 2))
          |  val built   = List(Built(false, None, 1), Built(false, None, 2))
          |  val decoded = List(Decoded(false, 1), Decoded(false, 2)) ++ Decoder.decode[Decoded]
          |}""".stripMargin
    )
    assertFinding(found, "constant-field", "Built.verify is always False")
    assertFinding(found, "constant-field", "Built.label is always Absent")
    assertNoFinding(found)(finding => finding.rule.id == "constant-field" && finding.detail.contains("Loaded"))
    assertNoFinding(found)(finding => finding.rule.id == "constant-field" && finding.detail.contains("Decoded"))
  }

  test("a field computed by calling a polymorphic function value is not derived from the other fields") {
    val found = findings(
      "Pair.scala" -> "case class Pair(n: Int, value: Int)",
      "Main.scala" ->
        """object Main {
          |  def make(f: [A] => A => A, n: Int): Pair = {
          |    val value = f[Int](n)
          |    Pair(n, value)
          |  }
          |  val pair = make([A] => (a: A) => a, 1)
          |}""".stripMargin
    )
    assertNoFinding(found)(finding => finding.rule.id == "derived-field" && finding.detail.contains("Pair.value"))
  }

  test("a field computed from another field by a method is derived from it") {
    val found = findings(
      "Pair.scala" -> "case class Pair(n: Int, text: String)",
      "Main.scala" ->
        """object Main {
          |  def make(n: Int): Pair = {
          |    val text = n.toString
          |    Pair(n, text)
          |  }
          |  val pair = make(1)
          |}""".stripMargin
    )
    assertFinding(found, "derived-field", "Pair.text is always n.toString", "across 1 construction")(using
      Finding.Level.Info
    )
  }

  test("constructions inside lambdas count") {
    val found = findings(
      "Instruction.scala" -> "enum Instruction { case Load(bytes: Int, signed: Boolean, coherent: Boolean = false) }",
      "Main.scala"        ->
        """object Main {
                               |  val direct = Instruction.Load(1, false)
                               |  def decode(codes: List[Int]) = codes.map(code => if (code > 0) Instruction.Load(code, true) else direct)
                               |}""".stripMargin
    )
    assertNoFinding(found)(_.detail.contains("signed"))
    assertFinding(found, "constant-field", "Load.coherent is always False across 2 constructions")
  }

  test("an unknown construction, which may set both options, leaves their exclusion unsettled") {
    val found = findings(
      "Hit.scala"  -> "case class Hit(first: Option[Int], second: Option[Int])",
      "Main.scala" ->
        "object Main { def make(value: Option[Int]) = List(Hit(value, value), Hit(Some(1), None), Hit(None, Some(2))) }"
    )
    assertNoFinding(found)(_.rule == Finding.Rule.ExclusiveOptions)
  }

  test("fields shared by every alternative are a warning") {
    assertFinding(
      findings(
        "Request.scala" -> """sealed trait Request
                             |case class Build(root: String, target: String) extends Request
                             |case class Check(root: String) extends Request
                             |case class Clean(root: String, all: Boolean) extends Request
                             |case class Format(root: String) extends Request
                             |case class Run(root: String, filter: String) extends Request
                             |case class Publish(root: String, version: String) extends Request
                             |case class Lint(root: String, fix: Boolean) extends Request
                             |case class Docs(root: String) extends Request""".stripMargin
      ),
      "common-factor",
      "8 of 8 cases share (root: String)"
    )
  }

  test("same-named duplicates are told apart by their owners") {
    assertFinding(
      findings(
        "Parser.scala"    -> "object Parser { case class Error(line: Int, column: Int, message: String) }",
        "Generator.scala" -> "object Generator { case class Error(line: Int, column: Int, message: String) }"
      ),
      "duplicate-shape",
      "Generator.Error, Parser.Error"
    )
  }

  test("duplicate-shape exempts a nested builder that constructs its outer type") {
    val found = findings(
      "Audio.scala" ->
        """case class Audio(rate: Int, channels: Int, depth: Int)
          |object Audio {
          |  case class Builder(rate: Int, channels: Int, depth: Int) {
          |    def build(): Audio = Audio(rate, channels, depth)
          |  }
          |}
          |""".stripMargin
    )
    assertNoFinding(found)(_.rule == Finding.Rule.DuplicateShape)
  }

  test("duplicate-shape keeps reporting unrelated types with the same fields") {
    val found = findings(
      "First.scala"  -> "case class First(rate: Int, channels: Int, depth: Int)",
      "Second.scala" -> "case class Second(rate: Int, channels: Int, depth: Int)"
    )
    assertFinding(found, "duplicate-shape", "First, Second")
  }

  test("duplicate-shape keeps reporting a nested type with the same fields that does not build its outer type") {
    val found = findings(
      "Audio.scala" ->
        """case class Audio(rate: Int, channels: Int, depth: Int)
          |object Audio {
          |  case class Snapshot(rate: Int, channels: Int, depth: Int)
          |}
          |""".stripMargin
    )
    assertFinding(found, "duplicate-shape", "Audio, Snapshot")
  }

  test("duplicate-shape keeps reporting a builder whose shape differs from its outer type") {
    val found = findings(
      "Audio.scala" ->
        """case class Audio(rate: Int, channels: Int, depth: Int)
          |object Audio {
          |  case class Builder(rate: Int, channels: Int) {
          |    def build(): Audio = Audio(rate, channels, 0)
          |  }
          |}
          |""".stripMargin,
      "First.scala" -> "case class First(rate: Int, channels: Int)"
    )
    assertFinding(found, "duplicate-shape", "Audio.Builder, First")
  }

  test("generated sums are reported like handwritten ones, and labelled as generated") {
    val source =
      """enum Opcode {
        |  case AddI(dest: Int, a: Int, b: Int); case SubI(dest: Int, a: Int, b: Int); case MulI(dest: Int, a: Int, b: Int)
        |  case AddF(dest: Int, a: Double, b: Double); case SubF(dest: Int, a: Double, b: Double)
        |  case Move(dest: Int, source: Int)
        |}""".stripMargin
    val found       = findings("src/generated/main/scala/Opcode.scala" -> source)
    val handwritten = findings("src/main/scala/Opcode.scala" -> source)
    assertEquals(
      found.map(finding => (finding.rule, finding.detail)),
      handwritten.map(finding => (finding.rule, finding.detail))
    )
    assert(found.nonEmpty)
    assert(found.forall(_.node.generated), found.mkString("\n"))
  }

  test("findings that only tests support are information") {
    val found = findingsWithTests(
      Seq("Config.scala"      -> "case class Config(verbose: Boolean, count: Int)"),
      Seq("ConfigCheck.scala" -> "class ConfigCheck { val configs = List(Config(false, 1), Config(false, 2)) }")
    )
    assertFinding(found, "constant-field", "Config.verbose is always False across 2 constructions (all in tests)")(using
      Finding.Level.Info
    )
  }

  test("constant arguments in main code remain warnings") {
    val found = findingsWithTests(
      Seq(
        "Config.scala" -> "case class Config(verbose: Boolean, count: Int)",
        "Main.scala"   -> "object Main { val config = Config(false, 1) }"
      ),
      Seq("ConfigCheck.scala" -> "class ConfigCheck { val config = Config(false, 2) }")
    )
    assertFinding(found, "constant-field", "Config.verbose is always False across 2 constructions")
    assertNoFinding(found)(_.detail.contains("all in tests"))
  }

  test("an unvalidated twin in tests is a fixture for the validator") {
    val found = findingsWithTests(
      Seq("Name.scala"      -> "case class Name(value: String, size: Int, kind: Int) { require(size >= 0) }"),
      Seq("NameCheck.scala" -> "class NameCheck { case class Name(value: String, size: Int, kind: Int) }")
    )
    assertFinding(found, "duplicate-shape", "NameCheck.Name skip the validation of Name")(using Finding.Level.Info)
  }

  test("sealed hierarchies of plain cases are enums written longhand") {
    val found = findings(
      "Colour.scala" ->
        """sealed trait Colour
          |object Colour {
          |  case object Red extends Colour
          |  case object Green extends Colour
          |  case class Grey(level: Int) extends Colour
          |}""".stripMargin,
      "Shape.scala" ->
        """sealed trait Shape
          |case class Circle(radius: Int) extends Shape
          |case class Square(side: Int) extends Shape""".stripMargin
    )
    assertFinding(found, "enum-candidate", "Colour: 3 cases without bodies; an enum is equivalent")
    assertFinding(found, "enum-candidate", "Shape: 2 cases without bodies; as an enum they move into Shape")(using
      Finding.Level.Info
    )
  }

  test("a sealed hierarchy whose companion is written before the trait is an enum candidate") {
    val found = findings(
      "CCallArg.scala" ->
        """object CCallArg {
          |  case class Explicit(fieldName: String) extends CCallArg
          |  case class BytesSize(bytesField: String) extends CCallArg
          |}
          |sealed trait CCallArg""".stripMargin
    )

    assertFinding(found, "enum-candidate", "CCallArg: 2 cases without bodies")
  }

  test("cases with bodies, further extension or other parents are not enum cases") {
    val found = findings(
      "Hierarchies.scala" ->
        """sealed trait Body
          |object Body { case class A(x: Int) extends Body { def twice: Int = x * 2 }; case object B extends Body }
          |sealed trait Checked
          |object Checked { case class A(x: Int) extends Checked { require(x > 0) }; case object B extends Checked }
          |sealed trait Nested
          |object Nested { sealed trait Inner extends Nested; case object A extends Inner; case object B extends Nested }
          |sealed trait Open
          |object Open { case class A(x: Int) extends Open; case object B extends Open }
          |class Wider(x: Int) extends Open.A(x)
          |sealed trait Left
          |sealed trait Right
          |object Both { case object A extends Left, Right; case object B extends Left; case object C extends Right }
          |sealed trait Plain
          |object Plain { class A extends Plain; case object B extends Plain }
          |sealed trait Single
          |object Single { case object A extends Single }
          |enum Already { case A, B }
          |sealed trait Derived
          |object Derived { case class A(x: Int) extends Derived derives CanEqual; case object B extends Derived }
          |sealed trait Companion
          |object Companion { case class A(x: Int) extends Companion; object A { def zero: A = A(0) }; case object B extends Companion }
          |""".stripMargin
    )
    assertNoFinding(found)(_.rule == Finding.Rule.EnumCandidate)
  }

  private val int = atom("Int")

  private enum Origin {
    case Handwritten, Generated, Test
  }

  private def node(key: String, origin: Origin = Origin.Handwritten): Node =
    Node(
      key,
      "m",
      if (origin == Origin.Test) Output.Scope.Test else Output.Scope.Main,
      s"$key.scala",
      line = 1,
      generated = origin == Origin.Generated,
      exported = false,
      outer = None
    )

  private def field(name: String, shape: Polynomial = int, kind: Algebra.Kind = Algebra.Kind.Other): Algebra.Field =
    Algebra.Field(name, shape, kind)

  private def ints(names: String): Vector[Algebra.Field] = names.toVector.map(name => field(name.toString))

  private def caseClass(
      name: String,
      fields: Vector[Algebra.Field],
      parents: Set[String] = Set.empty,
      access: Algebra.Access = Algebra.Access.Open
  ): Algebra.Product =
    Algebra.Product(name, name, name, fields, parents, Algebra.Predicate.Known(true), access, Algebra.Origin.Written)

  private def model(
      products: Vector[Algebra.Product] = Vector.empty,
      sums: Vector[Algebra.Sum] = Vector.empty,
      sites: Map[String, Vector[Algebra.Site]] = Map.empty,
      enumerations: Vector[Algebra.Enumeration] = Vector.empty
  ): Algebra.Model = Algebra.Model(products, sums, sites, Map.empty, enumerations, Map.empty)

  private def analyse(model: Algebra.Model, overrides: Node*): Vector[Finding] = {
    val keys = model.products.map(_.node) ++ model.sums.map(_.node) ++ model.enumerations.map(_.node)
    Algebra.findings(model, keys.map(key => key -> node(key)).toMap ++ overrides.map(entry => entry.name -> entry))
  }

  private def shown(found: Vector[Finding]): Vector[(String, Finding.Level)] =
    found.map(finding => finding.detail -> finding.level)

  test("duplicate shapes start at two fields, and from five fields regardless of names") {
    def twins(name: String, size: Int, rightNames: String = "abcdef") = Vector(
      caseClass(s"left.$name", ints("abcdef".take(size))),
      caseClass(s"right.$name", ints(rightNames.take(size)))
    )
    val found = analyse(model(products =
      twins("One", 1) ++ twins("Two", 2) ++ twins("Four", 4, "uvwxyz") ++ twins("Five", 5, "uvwxyz") ++
        twins("Six", 6, "uvwxyz")
    ))
    assertEquals(
      shown(found),
      Vector(
        "2 types with the same field names of shape Int^2: left.Two, right.Two"    -> Finding.Level.Warning,
        "2 types with different field names of shape Int^6: left.Six, right.Six"   -> Finding.Level.Info,
        "2 types with different field names of shape Int^5: left.Five, right.Five" -> Finding.Level.Info
      )
    )
  }

  test("wide duplicates keep their same-named groups, and add the whole group only when names differ within it") {
    val found = analyse(model(products =
      Vector(
        caseClass("left.Same", ints("abcde")),
        caseClass("right.Same", ints("abcde")),
        caseClass("left.Mixed", ints("abcdef")),
        caseClass("right.Mixed", ints("abcdef")),
        caseClass("other.Mixed", ints("uvwxyz"))
      )
    ))
    assertEquals(
      shown(found),
      Vector(
        "2 types with the same field names of shape Int^6: left.Mixed, right.Mixed" -> Finding.Level.Warning,
        "2 types with the same field names of shape Int^5: left.Same, right.Same"   -> Finding.Level.Warning,
        "3 types with different field names of shape Int^6: left.Mixed, other.Mixed, right.Mixed" -> Finding.Level.Info
      )
    )
  }

  test("duplicates in one declaration or under a common sealed parent are not reported") {
    val found = analyse(model(products =
      Vector(
        caseClass("left.Same", ints("xy")).copy(node = "same"),
        caseClass("right.Same", ints("xy")).copy(node = "same"),
        caseClass("left.Sibling", ints("xyz"), parents = Set("Parent")),
        caseClass("right.Sibling", ints("xyz"), parents = Set("Parent", "Other"))
      )
    ))
    assertEquals(found, Vector.empty)
  }

  test("an unvalidated twin outside tests is a warning that names the validation it skips") {
    val found = analyse(
      model(products =
        Vector(
          caseClass("a.Name", ints("xy"), access = Algebra.Access.Private),
          caseClass("b.Name", ints("xy")),
          caseClass("c.Name", ints("xy"))
        )
      ),
      node("c.Name", Origin.Test)
    )
    assertEquals(
      shown(found),
      Vector(
        "3 types with the same field names of shape Int^2: a.Name, b.Name, c.Name; b.Name, c.Name skip the " +
          "validation of a.Name" -> Finding.Level.Warning
      )
    )
  }

  test("exported hierarchies are enum candidates only as information") {
    val found = analyse(model(enumerations =
      Vector(Algebra.Enumeration("lib.Shape", "lib.Shape", 2, nested = true, exported = true))
    ))
    assertEquals(
      shown(found),
      Vector("Shape: 2 cases without bodies; as an enum it changes for the builds that use it" -> Finding.Level.Info)
    )
  }

  test("a generated sum lists its groups of identical cases like a handwritten one") {
    val variants = Vector("A" -> 3, "B" -> 2, "C" -> 2, "D" -> 2, "E" -> 1).flatMap((shape, count) =>
      (1 to count).map(index => Algebra.Variant(s"$shape$index", Vector(field("value", atom(shape)))))
    )
    val sum = Algebra.Sum("Op", "Op", variants)
    assertEquals(
      shown(analyse(model(sums = Vector(sum)), node("Op", Origin.Generated))),
      shown(analyse(model(sums = Vector(sum)), node("Op")))
    )
  }

  test("at most three common factors are reported, generated or not") {
    val variants = for {
      group <- Vector("p", "q", "r", "s")
      index <- 1 to 3
    } yield Algebra.Variant(
      s"$group$index",
      Vector(field(s"${group}x"), field(s"${group}y"), field("own", atom(s"$group$index")))
    )
    def factors(origin: Origin) =
      analyse(model(sums = Vector(Algebra.Sum("S", "S", variants))), node("S", origin)).map(_.detail)
    assertEquals(
      factors(Origin.Generated),
      Vector("p", "q", "r").map(group =>
        s"S: 3 of 12 cases share (${group}x: Int, ${group}y: Int): ${group}1, ${group}2, ${group}3"
      )
    )
    assertEquals(factors(Origin.Generated), factors(Origin.Handwritten))
  }

  test("common factors over mostly the same alternatives are reported once") {
    def variant(name: String, shared: String) =
      Algebra.Variant(
        name,
        shared.toVector.map(letter => field(letter.toString, atom(letter.toUpper.toString))) :+
          field("own", atom(name))
      )
    val found = analyse(model(sums =
      Vector(Algebra.Sum(
        "S",
        "S",
        Vector(
          variant("A", "xy"),
          variant("B", "wxyz"),
          variant("C", "wxyz"),
          variant("D", "wz"),
          variant("E", "pq"),
          variant("F", "rs"),
          variant("G", "tu"),
          variant("H", "vm")
        )
      ))
    ))
    assertEquals(shown(found), Vector("S: 3 of 8 cases share (w: W, z: Z): B, C, D" -> Finding.Level.Info))
  }

  test("a factor every case of a generated sum shares is reported, as for a handwritten sum") {
    val cases =
      Vector("V1", "V2", "V3", "V4", "V5", "V6", "V7").map(name =>
        Algebra.Variant(name, ints("mpabc") :+ field("own", atom(name)))
      ) :+ Algebra.Variant("V8", ints("mp"))
    val sum                     = Algebra.Sum("S", "S", cases)
    def factors(origin: Origin) = analyse(model(sums = Vector(sum)), node("S", origin))
      .collect { case finding if finding.rule == Finding.Rule.CommonFactor => finding.detail }
    assertEquals(factors(Origin.Generated), factors(Origin.Handwritten))
    assert(factors(Origin.Generated).contains("S: 8 of 8 cases share (m: Int, p: Int): V1, V2, V3, V4, V5, V6, V7, V8"))
  }

  test("the types of different newtypes, and cases of sums told apart by name, are no duplicate shape") {
    val found = findings(
      "Types.scala" ->
        """trait Newtype[A] { opaque type Type = A; def apply(value: A): Type = value }
          |object Register extends Newtype[Int]
          |object Predicate extends Newtype[Int]
          |case class Move(target: Register.Type, source: Register.Type)
          |case class Test(target: Predicate.Type, source: Predicate.Type)
          |enum Error { case Ambiguous(name: String, line: Int); case Unsupported(name: String, line: Int) }
          |case class Located(name: String, line: Int)
          |object Ids { object Region extends Newtype[Long] }
          |case class Range(first: Ids.Region.Type, last: Ids.Region.Type, step: Ids.Region.Type)
          |case class Span(first: Ids.Region.Type, last: Ids.Region.Type, step: Ids.Region.Type)""".stripMargin
    )
    assertEquals(
      found.collect { case finding if finding.rule == Finding.Rule.DuplicateShape => finding.detail },
      Vector("2 types with the same field names of shape Ids.Region.Type^3: Range, Span")
    )
  }

  test("alternatives with more than twelve fields only contribute pairs to common factors") {
    def factors(size: Int) = {
      val wide = (1 to 8).toVector.map(index =>
        Algebra.Variant(s"V$index", ints("abc") ++ (4 to size).map(extra => field(s"f$extra", atom(s"V$index"))))
      )
      analyse(model(sums = Vector(Algebra.Sum("S", "S", wide :+ Algebra.Variant("Other", ints("z")))))).map(_.detail)
    }
    assertEquals(
      factors(12),
      Vector("S: 8 of 9 cases share (a: Int, b: Int, c: Int): V1, V2, V3, V4, V5, V6, V7, V8")
    )
    assertEquals(factors(13), Vector("S: 8 of 9 cases share (a: Int, b: Int): V1, V2, V3, V4, V5, V6, V7, V8"))
  }

  private def constructed(fields: Algebra.Field*)(sites: Vector[Algebra.Argument]*): Vector[String] = {
    val load = caseClass("lib.Load", fields.toVector)
    val all  = sites.toVector.map(Algebra.Site(_, test = false, own = true, location = "Main.scala:1"))
    analyse(model(products = Vector(load), sites = Map(load.name -> all))).map(_.detail)
  }

  private val first  = field("first", One + a, Algebra.Kind.Optional)
  private val second = field("second", One + b, Algebra.Kind.Optional)

  test("options present one at a time across fully known constructions are exactly exclusive") {
    import Algebra.Argument.*
    assertEquals(
      constructed(first, second)(Vector(Present, Absent), Vector(Absent, Present), Vector(Absent, Present)),
      Vector(
        "Load: exactly one of (first, second) is present; 3 of 3 constructions determined; " +
          "(1 + A)*(1 + B) used as 1 + A + B, never A*B"
      )
    )
    assertEquals(
      constructed(first, second)(Vector(Present, Absent), Vector(Absent, Present), Vector(Unknown, Present)),
      Vector.empty
    )
  }

  test("options are not exclusive when one is never present or both are present at once") {
    import Algebra.Argument.*
    assertEquals(
      constructed(first, second)(Vector(Present, Absent), Vector(Absent, Absent)),
      Vector("Load.second is always Absent across 2 constructions")
    )
    assertEquals(
      constructed(first, second)(Vector(Present, Present), Vector(Present, Absent), Vector(Absent, Present)),
      Vector.empty
    )
  }

  test("a single construction does not make a field constant") {
    assertEquals(constructed(field("done", kind = Algebra.Kind.Flag))(Vector(Algebra.Argument.True)), Vector.empty)
  }

  test("a flag is redundant when it negates an option, but not when it stays fixed while the option varies") {
    import Algebra.Argument.*
    val done  = field("done", constant(2), Algebra.Kind.Flag)
    val value = field("value", One + a, Algebra.Kind.Optional)
    assertEquals(
      constructed(done, value)(Vector(True, Absent), Vector(False, Present)),
      Vector("Load.done is determined by Load.value; 2 of 2 constructions determined")
    )
    assertEquals(
      constructed(done, value)(Vector(True, Present), Vector(True, Absent)),
      Vector("Load.done is always True across 2 constructions")
    )
  }

  test("a flag is not redundant while a construction leaves it or the option unknown") {
    import Algebra.Argument.*
    val done  = field("done", constant(2), Algebra.Kind.Flag)
    val value = field("value", One + a, Algebra.Kind.Optional)
    assertEquals(
      constructed(done, value)(Vector(True, Absent), Vector(False, Present), Vector(Unknown, Present)),
      Vector.empty
    )
    assertEquals(
      constructed(done, value)(Vector(True, Absent), Vector(False, Present), Vector(True, Unknown)),
      Vector.empty
    )
  }

  test("a flag is determined by every option that matches or negates it, in field order") {
    import Algebra.Argument.*
    val done    = field("done", constant(2), Algebra.Kind.Flag)
    val same    = field("same", One + a, Algebra.Kind.Optional)
    val negated = field("negated", One + b, Algebra.Kind.Optional)
    val other   = field("other", One + a * b, Algebra.Kind.Optional)
    assertEquals(
      constructed(done, same, negated, other)(
        Vector(True, Present, Absent, Present),
        Vector(False, Absent, Present, Present),
        Vector(True, Present, Absent, Absent)
      ),
      Vector(
        "Load.done is determined by Load.same; 3 of 3 constructions determined",
        "Load.done is determined by Load.negated; 3 of 3 constructions determined"
      )
    )
  }

  test("a field a sum declares as an accessor is no common factor, which the cases implement") {
    val declared = findings(
      "Typed.scala" ->
        """enum Typed {
          |  case Undefined(id: Int)
          |  case Opaque(id: Int, typeId: Int)
          |  case Data(id: Int, typeId: Int)
          |  case Ordinal(id: Int, rank: Int)
          |  case Length(id: Int, rank: Int)
          |  case Span(id: Int, rank: Int)
          |  case Mark(id: Int, rank: Int)
          |  case Frame(id: Int, rank: Int)
          |  def id: Int
          |}""".stripMargin
    )
    assertEquals(
      declared.collect { case finding if finding.rule.id == "common-factor" => finding.detail }.filter(_.contains(
        "id: Int"
      )),
      Vector.empty
    )
    val undeclared = findings(
      "Typed.scala" ->
        """enum Typed {
          |  case Undefined(id: Int)
          |  case Opaque(id: Int, typeId: Int)
          |  case Data(id: Int, typeId: Int)
          |  case Ordinal(id: Int, rank: Int)
          |  case Length(id: Int, rank: Int)
          |  case Span(id: Int, rank: Int)
          |  case Mark(id: Int, rank: Int)
          |  case Frame(id: Int, rank: Int)
          |}""".stripMargin
    )
    assert(undeclared.exists(_.rule.id == "common-factor"), undeclared.mkString("\n"))
  }

  private val Archived = "Archive.scala" -> "case class Archive(path: String, isRar: Boolean, size: Int)"

  private def derived(main: String): Vector[Finding] =
    findings(Archived, "Main.scala" -> main).filter(_.rule.id == "derived-field")

  test("a field every construction computes from another is derived, also through a local") {
    val found = findings(
      Archived,
      "Main.scala" ->
        """object Main {
          |  def open(path: String, size: Int) = { val isRar = path.endsWith(".rar"); Archive(path, isRar, size) }
          |  def named(name: String) = Archive(name, name.endsWith(".rar"), 1)
          |}""".stripMargin
    )
    assertFinding(
      found,
      "derived-field",
      "Archive.isRar is always path.endsWith(\".rar\") across 2 constructions",
      "follows from path"
    )(using
      Finding.Level.Info
    )
  }

  test("a single construction that computes a field from another is derived") {
    assertFinding(
      findings(
        Archived,
        "Main.scala" -> "object Main { def one(name: String) = Archive(name, name.endsWith(\".rar\"), 1) }"
      ),
      "derived-field",
      "Archive.isRar"
    )(using Finding.Level.Info)
  }

  test("a field computed from another with varargs is derived") {
    assertFinding(
      findings(
        Archived,
        "Main.scala" ->
          """object Ext { def has(name: String, exts: String*): Boolean = exts.exists(name.endsWith) }
            |object Main { def one(name: String) = Archive(name, Ext.has(name, "rar", "7z"), 1) }""".stripMargin
      ),
      "derived-field",
      "Archive.isRar is always Ext.has(path, \"rar\", \"7z\")"
    )(using Finding.Level.Info)
  }

  test("a field read from the file system at construction is no derived field") {
    assertEquals(
      findings(
        "Sized.scala" -> "case class Sized(path: java.nio.file.Path, size: Long)",
        "Main.scala"  ->
          """import java.nio.file.{Files, Path}
            |object Main {
            |  def a(p: Path) = Sized(p, Files.size(p))
            |  def b(q: Path) = Sized(q, Files.size(q))
            |}""".stripMargin
      ).filter(_.rule.id == "derived-field"),
      Vector.empty
    )
  }

  test("a field some construction gives another value is no derived field") {
    assertEquals(
      derived(
        """object Main {
          |  def a(name: String) = Archive(name, name.endsWith(".rar"), 1)
          |  def b(name: String) = Archive(name, true, 2)
          |}""".stripMargin
      ),
      Vector.empty
    )
  }

  test("a field computed differently at two constructions is no derived field") {
    assertEquals(
      derived(
        """object Main {
          |  def a(name: String) = Archive(name, name.endsWith(".rar"), 1)
          |  def b(name: String) = Archive(name, name.endsWith(".zip"), 2)
          |}""".stripMargin
      ),
      Vector.empty
    )
  }

  test("a field computed from something that is no other field, or by a branch, is no derived field") {
    val twice = Seq(
      "def a(name: String, other: String) = Archive(name, other.endsWith(\".rar\"), 1); def b(name: String, other: String) = Archive(name, other.endsWith(\".rar\"), 2)",
      "def a(name: String) = Archive(name, if (name.isEmpty) true else false, 1); def b(name: String) = Archive(name, if (name.isEmpty) true else false, 2)",
      "def a(name: String) = Archive(name, name.endsWith(\".rar\"), 1); def b(a: Archive, name: String) = a.copy(path = name)"
    )
    twice.foreach(body => assertEquals(derived(s"object Main { $body }"), Vector.empty, body))
  }

  test("a field passed the same value as another is not derived from it") {
    assertEquals(
      findings(
        "Pair.scala" -> "case class Pair(left: String, right: String)",
        "Main.scala" -> "object Main { def a(x: String) = Pair(x, x); def b(y: String) = Pair(y, y) }"
      ).filter(_.rule.id == "derived-field"),
      Vector.empty
    )
  }

  test("local case classes of one name and different arities in different bodies are told apart") {
    val found = findings(
      "Main.scala" -> """class Suite {
        |  def check(name: String)(body: => Any): Any = body
        |  check("a") {
        |    val name = "a"
        |    case class Person(name: String, age: Int, active: Boolean, email: Option[String])
        |    List(new Person(name, 1, true, Some(name)), new Person(name, 2, false, None))
        |  }
        |  check("b") {
        |    val name = "b"
        |    case class Person(name: String, age: Int, active: Boolean)
        |    new Person(name, 3, true)
        |  }
        |}
        |object Main""".stripMargin
    )
    assertFinding(found, "redundant-flag", "Person.active", "Person.email")
  }
}
