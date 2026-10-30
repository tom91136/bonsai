package net.kurobako.bonsai.analysis

class DesignsSuite extends AnalysisSuite {
  private def findings(sources: (String, String)*): Vector[Finding] = {
    val result = extract(sources)
    Designs.findings(result.designs, result.graph.nodes)
  }

  private def of(found: Vector[Finding], rule: Finding.Rule): Vector[String] =
    found.filter(_.rule == rule).map(_.detail.takeWhile(_ != ';')).sorted

  test("an object named for a type whose methods all take it first belongs in the type's companion") {
    val found = findings(
      "Terms.scala" ->
        """enum Term { case Leaf(value: Int); case Pair(left: Term, right: Term) }
          |object Terms {
          |  def size(term: Term): Int = term match { case Term.Leaf(_) => 1; case Term.Pair(l, r) => size(l) + size(r) }
          |  def leaves(term: Term): Int = size(term)
          |}
          |case class Item(count: Int)
          |object Items {
          |  def counted(item: Item): Int = item.count
          |  def doubled(count: Int): Int = count * 2
          |}
          |trait Show[A] { def show(value: A): String }
          |case class Label(text: String)
          |object Labels extends Show[Label] {
          |  def show(value: Label): String = value.text
          |  def shout(value: Label): String = value.text.toUpperCase
          |}
          |object Tools { def first(term: Term): Int = 1; def second(term: Term): Int = 2 }""".stripMargin
    )
    assertEquals(
      of(found, Finding.Rule.DetachedCompanion),
      Vector("object Terms holds 2 methods that take a Term first (leaves, size)")
    )
  }

  test("an alias with an object of operations beside it is a type in all but name") {
    val found = findings(
      "Program.scala" ->
        """object Program {
          |  type Steps = Vector[(String, Int)]
          |  object Steps { def names(steps: Steps): Vector[String] = steps.map(_._1); val empty: Steps = Vector.empty }
          |  type Plain = Vector[Int]
          |  object Plain { def total(values: Plain): Int = values.sum }
          |  type Lone = Vector[Int]
          |  type Binary16 = Binary16.Type
          |  object Binary16 {
          |    opaque type Type = Short
          |    def bits(value: Type): Short = value
          |  }
          |}""".stripMargin,
      "Plan.scala" ->
        "package app\ntype Plan = List[(String, Int)]\nobject Plan { def first(plan: Plan): String = plan.head._1 }",
      "Code.scala" ->
        "package app\ntype Code = Code.Type\nobject Code {\n  opaque type Type = Short\n  def bits(value: Type): Short = value\n}",
      "Word.scala" ->
        """package app
          |trait Newtype[A] { opaque type Type = A; def wrap(value: A): Type = value }
          |type Word = Word.Type
          |object Word extends Newtype[Int] { def zero: Type = wrap(0) }""".stripMargin
    )
    val aliased = found.filter(_.rule == Finding.Rule.AliasWithOperations).map(_.detail).sorted
    assertEquals(aliased.size, 3, aliased.mkString("\n"))
    assert(
      aliased.exists(detail => detail.startsWith("type Plain") && !detail.contains("case class")),
      aliased.mkString
    )
    assert(aliased.exists(detail => detail.startsWith("type Steps") && detail.contains("case class")), aliased.mkString)
    assert(aliased.exists(detail => detail.startsWith("type Plan") && detail.contains("case class")), aliased.mkString)
  }

  test("a family of functions that only call the parameter they take first are extension methods of it") {
    val found = findings(
      "Integers.scala" ->
        """trait Algebra { def add(left: Long, right: Long): Long; def mul(left: Long, right: Long): Long }
          |object Integers {
          |  def twice(algebra: Algebra, value: Long): Long  = algebra.add(value, value)
          |  def square(algebra: Algebra, value: Long): Long = algebra.mul(value, value)
          |  def cube(algebra: Algebra, value: Long): Long   = algebra.mul(algebra.mul(value, value), value)
          |  def pick(algebra: Algebra, value: Option[Long]): Long =
          |    value match { case Some(_) => algebra.add(1L, 2L); case None => algebra.mul(1L, 2L) }
          |  def mixed(algebra: Algebra, value: Long): Long  = algebra.add(value, 1L) + value
          |  def kept(algebra: Algebra, values: List[Long]): List[Long] =
          |    values.collect { case value if value > 0 => algebra.add(value, value) }
          |  def helped(algebra: Algebra, value: Long): Long = twice(algebra, value)
          |}
          |trait Arithmetic { def plus(left: Long, right: Long): Long; def minus(left: Long, right: Long): Long; def times(left: Long, right: Long): Long }
          |object Executed extends Arithmetic {
          |  def plus(left: Long, right: Long): Long  = left + right
          |  def minus(left: Long, right: Long): Long = left - right
          |  def times(left: Long, right: Long): Long = left * right
          |}
          |object Widths {
          |  extension (algebra: Algebra) {
          |    def double(value: Long): Long = algebra.add(value, value)
          |    def squared(value: Long): Long = algebra.mul(value, value)
          |    def cubed(value: Long): Long = algebra.mul(algebra.mul(value, value), value)
          |  }
          |}
          |object Pairs {
          |  def one(algebra: Algebra, value: Long): Long = algebra.add(value, value)
          |  def two(algebra: Algebra, value: Long): Long = algebra.mul(value, value)
          |}""".stripMargin
    )
    assertEquals(
      of(found, Finding.Rule.ReceiverParameter),
      Vector("4 methods of Integers take an Algebra first and only call it (cube, pick, square, twice)")
    )
  }

  test("receivers that already have a member of the name, and inline algebra instances, are not extension candidates") {
    val found = findings(
      "Semantics.scala" ->
        """trait Machine[V] {
          |  def fence(value: V): V
          |  def call(value: V): V
          |  def fork(value: V): V
          |  def twice(value: V): V
          |  def plain(value: V): V
          |}
          |trait Plain {
          |  def fence(value: Long): Long
          |  def call(value: Long): Long
          |  def fork(value: Long): Long
          |  def twice(value: Long): Long
          |}
          |object Named {
          |  def fence(machine: Plain, value: Long): Long = machine.fence(value)
          |  def call(machine: Plain, value: Long): Long  = machine.call(value)
          |  def fork(machine: Plain, value: Long): Long  = machine.fork(value)
          |}
          |object Inlined {
          |  inline def one(inline machine: Machine[Long], value: Long): Long   = machine.plain(value)
          |  inline def two(inline machine: Machine[Long], value: Long): Long   = machine.twice(value)
          |  inline def three(inline machine: Machine[Long], value: Long): Long = machine.plain(machine.twice(value))
          |}
          |object Kept {
          |  def one(machine: Plain, value: Long): Long   = machine.twice(value)
          |  def two(machine: Plain, value: Long): Long   = machine.twice(machine.twice(value))
          |  def three(machine: Plain, value: Long): Long = machine.twice(machine.call(value))
          |  def fence(machine: Plain, value: Long): Long = machine.fence(value)
          |}
          |object Mixed {
          |  def one(machine: Machine[Long], value: Long): Long   = machine.twice(value)
          |  def two(machine: Machine[Long], value: Long): Long   = machine.plain(value)
          |  def three(machine: Machine[Long], value: Long): Long = machine.plain(machine.twice(value))
          |}""".stripMargin
    )
    assertEquals(
      of(found, Finding.Rule.ReceiverParameter),
      Vector(
        "3 methods of Kept take a Plain first and only call it (one, three, two)",
        "3 methods of Mixed take a Machine first and only call it (one, three, two)"
      )
    )
  }

  test("sibling methods typed alike but for a width are one generic method") {
    val found = findings(
      "Floats.scala" ->
        """case class Float16(bits: Short)
          |case class Float32(bits: Int)
          |case class Float64(bits: Long)
          |object FloatOps {
          |  def add16(left: Float16, right: Float16): Float16 = Float16((left.bits + right.bits).toShort)
          |  def add32(left: Float32, right: Float32): Float32 = Float32(left.bits + right.bits)
          |  def add64(left: Float64, right: Float64): Float64 = Float64(left.bits + right.bits)
          |  def neg16(value: Float16): Float16 = value
          |  def neg32(value: Float32): Float32 = value
          |  def round8(value: Int): Int       = value
          |  def round16(value: Long): Long    = value
          |  def round32(value: Double): Double = value
          |}
          |class Bits[N <: Int](val value: Long)
          |object Converts {
          |  def toInt8(value: Long): Bits[8]   = Bits(value)
          |  def toInt16(value: Long): Bits[16] = Bits(value)
          |  def toInt32(value: Long): Bits[32] = Bits(value)
          |  def wide8(value: Bits[8]): Long    = value.value
          |  def wide16(value: Bits[16]): Long  = value.value
          |  def wide32(value: Bits[32]): Long  = value.value
          |}
          |case class Int8(value: Long)
          |case class Int16(value: Long)
          |case class Int32(value: Long)
          |object Narrows {
          |  def narrow8(value: Long): Int8   = Int8(value)
          |  def narrow16(value: Long): Int16 = Int16(value)
          |  def narrow32(value: Long): Int32 = Int32(value)
          |}""".stripMargin
    )
    assertEquals(
      of(found, Finding.Rule.NumberedFamily),
      Vector("add16, add32, add64 differ only in a width and share one signature")
    )
  }

  test("an alias of a type its object only builds is not reported, and operations on it are counted") {
    val factories = findings(
      "Lane.scala" ->
        """object Lanes {
          |  type Lane = Int
          |  object Lane {
          |    def fromIndex(index: Int): Lane = index
          |    def inWarp(base: Int, lane: Int): Lane = base + lane
          |  }
          |}""".stripMargin
    )
    assertEquals(of(factories, Finding.Rule.AliasWithOperations), Vector.empty)
    val operations = findings(
      "Celsius.scala" ->
        """object Temperatures {
          |  type Celsius = Double
          |  object Celsius { extension (c: Celsius) def toKelvin: Double = c + 273.15 }
          |}""".stripMargin
    )
    assertEquals(of(operations, Finding.Rule.AliasWithOperations).size, 1)
  }

  test("a type constructor that passes its parameters to a library class is a rename, not a value type") {
    val forwarding = findings(
      "Vec.scala" ->
        """object Vectors {
          |  type Vec[A] = List[A]
          |  object Vec { def of[A](value: A): Vec[A] = List(value); extension [A](v: Vec[A]) def nonEmptyVec: Boolean = v.nonEmpty }
          |}""".stripMargin
    )
    assertEquals(of(forwarding, Finding.Rule.AliasWithOperations), Vector.empty)
    val fixed = findings(
      "Registry.scala" ->
        """object Registries {
          |  type Registry = Map[String, Int]
          |  object Registry { def lookup(registry: Registry, key: String): Option[Int] = registry.get(key) }
          |}""".stripMargin
    )
    assertEquals(of(fixed, Finding.Rule.AliasWithOperations).size, 1)
  }

  test("conversions between two widths are not a family of numbered methods") {
    val conversions = findings(
      "Conversions.scala" ->
        """case class Float16(bits: Short)
          |case class Float32(bits: Int)
          |case class Float64(bits: Long)
          |object Conversions {
          |  def cvtF16F32(value: Float32): Float16 = Float16(value.bits.toShort)
          |  def cvtF32F16(value: Float16): Float32 = Float32(value.bits)
          |  def cvtF32F64(value: Float64): Float32 = Float32(value.bits.toInt)
          |  def cvtF64F32(value: Float32): Float64 = Float64(value.bits)
          |  def neg16(value: Float16): Float16 = value
          |  def neg32(value: Float32): Float32 = value
          |  def neg64(value: Float64): Float64 = value
          |}""".stripMargin
    )
    assertEquals(
      of(conversions, Finding.Rule.NumberedFamily),
      Vector("neg16, neg32, neg64 differ only in a width and share one signature")
    )
  }
}
