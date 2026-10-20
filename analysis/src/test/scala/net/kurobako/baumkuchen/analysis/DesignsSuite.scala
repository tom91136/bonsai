package net.kurobako.baumkuchen.analysis

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
}
