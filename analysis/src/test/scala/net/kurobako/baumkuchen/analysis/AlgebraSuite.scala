package net.kurobako.baumkuchen.analysis

class AlgebraSuite extends AnalysisSuite {
  import Polynomial.*

  private val a = atom("A")
  private val b = atom("B")
  private val c = atom("C")

  private def findings(sources: (String, String)*): Vector[Finding] = findingsWithTests(sources, Nil)

  private def findingsWithTests(main: Seq[(String, String)], test: Seq[(String, String)]): Vector[Finding] = {
    val result = extract(main, test)
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
                            |case class Fence(scope: String) extends Access""".stripMargin
      ),
      "common-factor",
      "3 of 4 cases",
      "address: Long",
      "scalar: Int"
    )
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

  test("unknown arguments keep conclusions open") {
    val found = findings(
      "Hit.scala"  -> "case class Hit(first: Option[Int], second: Option[Int])",
      "Main.scala" ->
        "object Main { def make(value: Option[Int]) = List(Hit(value, value), Hit(Some(1), None), Hit(None, Some(2))) }"
    )
    assertFinding(found, "exclusive-options", "2 of 3 constructions determined, 1 unknown")
  }

  test("fields shared by every alternative are a warning") {
    assertFinding(
      findings(
        "Request.scala" -> """sealed trait Request
                             |case class Build(root: String, target: String) extends Request
                             |case class Check(root: String) extends Request
                             |case class Clean(root: String, all: Boolean) extends Request""".stripMargin
      ),
      "common-factor",
      "3 of 3 cases share (root: String)"
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

  test("generated sums are summarised once") {
    val found = findings(
      "src/generated/main/scala/Opcode.scala" ->
        """enum Opcode {
          |  case AddI(dest: Int, a: Int, b: Int); case SubI(dest: Int, a: Int, b: Int); case MulI(dest: Int, a: Int, b: Int)
          |  case AddF(dest: Int, a: Double, b: Double); case SubF(dest: Int, a: Double, b: Double)
          |  case Move(dest: Int, source: Int)
          |}""".stripMargin
    )
    assertEquals(found.count(_.rule == Finding.Rule.IdenticalCases), 1, found.mkString("\n"))
    assertFinding(found, "identical-cases", "5 of 6 cases repeat another case's shape (3 distinct shapes)")(using
      Finding.Level.Info
    )
    assertEquals(found.count(_.rule == Finding.Rule.CommonFactor), 1, found.mkString("\n"))
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

  private def node(key: String, test: Boolean = false, generated: Boolean = false): Node =
    Node(
      key,
      "m",
      if (test) Output.Scope.Test else Output.Scope.Main,
      s"$key.scala",
      line = 1,
      generated = generated,
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
    Algebra.Product(name, name, fields, parents, Algebra.Predicate.Known(true), access, exported = false)

  private def model(
      products: Vector[Algebra.Product] = Vector.empty,
      sums: Vector[Algebra.Sum] = Vector.empty,
      sites: Map[String, Vector[Algebra.Site]] = Map.empty,
      enumerations: Vector[Algebra.Enumeration] = Vector.empty
  ): Algebra.Model = Algebra.Model(products, sums, sites, Vector.empty, enumerations)

  private def analyse(model: Algebra.Model, overrides: Node*): Vector[Finding] = {
    val keys = model.products.map(_.node) ++ model.sums.map(_.node) ++ model.enumerations.map(_.node)
    Algebra.findings(model, keys.map(key => key -> node(key)).toMap ++ overrides.map(entry => entry.name -> entry))
  }

  private def shown(found: Vector[Finding]): Vector[(String, Finding.Level)] =
    found.map(finding => finding.detail -> finding.level)

  test("duplicate shapes start at two fields, and from five fields regardless of names") {
    def twins(name: String, size: Int, renamed: Boolean) = Vector(
      caseClass(s"left.$name", ints("abcdef".take(size))),
      caseClass(s"right.$name", ints((if (renamed) "uvwxyz" else "abcdef").take(size)))
    )
    val found = analyse(model(products =
      twins("One", 1, renamed = false) ++ twins("Two", 2, renamed = false) ++ twins("Four", 4, renamed = true) ++
        twins("Five", 5, renamed = true) ++ twins("Six", 6, renamed = true)
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
      node("c.Name", test = true)
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

  test("a generated sum lists its three largest groups of identical cases") {
    val variants = Vector("A" -> 3, "B" -> 2, "C" -> 2, "D" -> 2, "E" -> 1).flatMap((shape, count) =>
      (1 to count).map(index => Algebra.Variant(s"$shape$index", Vector(field("value", atom(shape)))))
    )
    val found = analyse(model(sums = Vector(Algebra.Sum("Op", "Op", variants))), node("Op", generated = true))
    assertEquals(
      shown(found),
      Vector(
        "Op: 9 of 10 cases repeat another case's shape (5 distinct shapes); largest: 3 of A; 2 of B; 2 of C" ->
          Finding.Level.Info
      )
    )
  }

  test("at most three common factors are reported, and one for a generated sum") {
    val variants = for {
      group <- Vector("p", "q", "r", "s")
      index <- 1 to 3
    } yield Algebra.Variant(
      s"$group$index",
      Vector(field(s"${group}x"), field(s"${group}y"), field("own", atom(s"$group$index")))
    )
    def factors(generated: Boolean) =
      analyse(model(sums = Vector(Algebra.Sum("S", "S", variants))), node("S", generated = generated)).map(_.detail)
    assertEquals(
      factors(generated = false),
      Vector("p", "q", "r").map(group =>
        s"S: 3 of 12 cases share (${group}x: Int, ${group}y: Int): ${group}1, ${group}2, ${group}3"
      )
    )
    assertEquals(factors(generated = true), Vector("S: 3 of 12 cases share (px: Int, py: Int): p1, p2, p3"))
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
        Vector(variant("A", "xy"), variant("B", "wxyz"), variant("C", "wxyz"), variant("D", "wz"))
      ))
    ))
    assertEquals(shown(found), Vector("S: 3 of 4 cases share (w: W, z: Z): B, C, D" -> Finding.Level.Info))
  }

  test("alternatives with more than twelve fields only contribute pairs to common factors") {
    def factors(size: Int) = {
      val wide = (1 to 4).toVector.map(index =>
        Algebra.Variant(s"V$index", ints("abc") ++ (4 to size).map(extra => field(s"f$extra", atom(s"V$index"))))
      )
      analyse(model(sums = Vector(Algebra.Sum("S", "S", wide :+ Algebra.Variant("Other", ints("z")))))).map(_.detail)
    }
    assertEquals(factors(12), Vector("S: 4 of 5 cases share (a: Int, b: Int, c: Int): V1, V2, V3, V4"))
    assertEquals(factors(13), Vector("S: 4 of 5 cases share (a: Int, b: Int): V1, V2, V3, V4"))
  }

  private def constructed(fields: Algebra.Field*)(sites: Vector[Algebra.Argument]*): Vector[String] = {
    val load = caseClass("lib.Load", fields.toVector)
    val all  = sites.toVector.map(Algebra.Site(_, test = false, own = true, location = "Main.scala:1"))
    analyse(model(products = Vector(load), sites = Map(load.name -> all))).map(_.detail)
  }

  private val first  = field("first", One + a, Algebra.Kind.Optional)
  private val second = field("second", One + b, Algebra.Kind.Optional)

  test("options present one at a time are exactly exclusive, counting only fully known constructions") {
    import Algebra.Argument.*
    assertEquals(
      constructed(first, second)(Vector(Present, Absent), Vector(Absent, Present), Vector(Unknown, Present)),
      Vector(
        "Load: exactly one of (first, second) is present; 2 of 3 constructions determined, 1 unknown; " +
          "(1 + A)*(1 + B) used as 1 + A + B, never A*B"
      )
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
}
