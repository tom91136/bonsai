package net.kurobako.bonsai.analysis

import scala.annotation.targetName

/** Exponential polynomials with natural coefficients over uninterpreted atoms, kept in a canonical form.
  *
  * Equal values denote isomorphic types under the semiring laws plus the exponent laws `B^(X+Y) = B^X * B^Y`,
  * `(B*C)^X = B^X * C^X` and `(B^X)^Y = B^(X*Y)`. Expansions beyond the term budget collapse into an opaque atom, which
  * keeps every reported isomorphism sound at the cost of completeness.
  */
object Polynomial {
  enum Naming {
    case Atom, Variable
  }

  enum Factor {
    case Named(naming: Naming, name: String)
    case Power(base: Polynomial, exponent: Polynomial)
  }

  type Monomial = Map[Factor, Int]

  private val MaxTerms = 256

  val Zero: Polynomial = Polynomial(Map.empty)
  val One: Polynomial  = constant(1)

  def constant(value: BigInt): Polynomial = if (value == 0) Zero else Polynomial(Map(Map.empty -> value))

  def atom(name: String): Polynomial     = factor(Factor.Named(Naming.Atom, name))
  def variable(name: String): Polynomial = factor(Factor.Named(Naming.Variable, name))

  def sum(terms: Iterable[Polynomial]): Polynomial       = terms.foldLeft(Zero)(_ + _)
  def product(factors: Iterable[Polynomial]): Polynomial = factors.foldLeft(One)(_ * _)

  def power(base: Polynomial, exponent: Polynomial): Polynomial =
    if (exponent == Zero || base == One) One
    else if (exponent == One) base
    else if (exponent.terms.size > 1)
      product(exponent.terms.map((monomial, count) => power(base, Polynomial(Map(monomial -> count)))))
    else {
      val (monomial, count) = exponent.terms.head
      if (monomial.isEmpty) repeat(base, count)
      else if (count > 1) repeat(power(base, Polynomial(Map(monomial -> BigInt(1)))), count)
      else if (base.terms.size == 1) {
        val (baseMonomial, coefficient) = base.terms.head
        val scaled = if (coefficient == 1) One else factor(Factor.Power(constant(coefficient), exponent))
        product(scaled +: baseMonomial.toVector.map {
          case (Factor.Power(inner, innerExponent), times) => power(inner, innerExponent * exponent * constant(times))
          case (single, times) => repeat(factor(Factor.Power(factor(single), exponent)), times)
        })
      } else factor(Factor.Power(base, exponent))
    }

  private def factor(value: Factor): Polynomial = Polynomial(Map(Map(value -> 1) -> BigInt(1)))

  private def repeat(base: Polynomial, count: BigInt): Polynomial =
    if (count > MaxTerms) opaque(s"(${base.key})^$count") else product(Vector.fill(count.toInt)(base))

  private def opaque(name: String): Polynomial = atom(s"<$name>")

  private def show(factor: Factor, full: Boolean): String = factor match {
    case Factor.Named(Naming.Atom, name)     => if (full) name else brief(name)
    case Factor.Named(Naming.Variable, name) => name
    case Factor.Power(base, exponent)        => s"(${base.render(full)})^(${exponent.render(full)})"
  }

  // Strips package prefixes from every qualified name inside a possibly applied type, keeping the types that hold it.
  private val Qualifier = java.util.regex.Pattern.compile("""\b[a-z][\w$]*\.""")

  private def brief(name: String): String = Qualifier.matcher(name).replaceAll("")

  private def show(monomial: Monomial, full: Boolean): String =
    monomial.toVector.map { (factor, times) =>
      val shown = show(factor, full)
      if (times == 1) shown else s"$shown^$times"
    }.sorted.mkString("*")
}

case class Polynomial(terms: Map[Polynomial.Monomial, BigInt]) {
  import Polynomial.*

  @targetName("plus")
  def +(that: Polynomial): Polynomial =
    Polynomial(that.terms.foldLeft(terms) { case (sum, (monomial, count)) =>
      sum.updated(monomial, sum.getOrElse(monomial, BigInt(0)) + count)
    })

  /** Truncated subtraction: removes the terms of `that` and drops terms that reach zero. */
  @targetName("minus")
  def -(that: Polynomial): Polynomial =
    Polynomial(that.terms.foldLeft(terms) { case (remaining, (monomial, count)) =>
      remaining.updatedWith(monomial)(_.map(_ - count).filter(_ > 0))
    })

  @targetName("times")
  def *(that: Polynomial): Polynomial =
    if (terms.size * that.terms.size > MaxTerms) opaque(Vector(key, that.key).sorted.mkString("(", ")*(", ")"))
    else {
      val rights   = that.terms.toVector
      val products = for {
        (left, leftCount)   <- terms.toVector
        (right, rightCount) <- rights
      } yield right.foldLeft(left) { case (monomial, (factor, times)) =>
        monomial.updated(factor, monomial.getOrElse(factor, 0) + times)
      } -> leftCount * rightCount
      Polynomial(products.groupMapReduce(_._1)(_._2)(_ + _))
    }

  /** Number of atom and variable occurrences in the largest term, a rough measure of structural weight. */
  def weight: Int = terms.keys.map(_.values.sum).maxOption.getOrElse(0)

  lazy val key: String = render(full = true)

  private def render(full: Boolean): String =
    if (terms.isEmpty) "0"
    else terms.toVector.map { (monomial, count) =>
      val shown = show(monomial, full)
      if (monomial.isEmpty) count.toString else if (count == 1) shown else s"$count*$shown"
    }.sorted.mkString(" + ")

  override def toString: String = render(full = false)
}
