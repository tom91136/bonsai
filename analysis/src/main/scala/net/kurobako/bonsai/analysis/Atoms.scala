package net.kurobako.bonsai.analysis

import Polynomial.*
import Polynomial.Factor.*

/** The atoms of a bound: sizes of collections by access path, logarithms of those, and the unknown `?`.
  *
  * A path is rooted at a parameter (`name#id`), at `this`, or at a module's qualified name, and continues with the
  * fields read from the root, so equal paths are equal atoms and bounds can be rebound from a callee to its caller.
  */
private object Atoms {
  val Unknown: Polynomial = Polynomial.atom("?")

  private val SizePrefix = "s:"
  private val LogPrefix  = "l:"
  private val Ids        = "#\\d+".r

  case class Binding(path: Option[String], size: Option[Polynomial], static: Boolean = false)

  def size(path: String): Polynomial = Polynomial.atom(SizePrefix + path)

  def log(size: Polynomial): Polynomial =
    if (degree(size) == 0) Polynomial.One
    else single(size).fold(Unknown)(path => Polynomial.atom(LogPrefix + path))

  /** The path of a polynomial that is the size of exactly one path. */
  private def single(size: Polynomial): Option[String] = size.terms.toVector match {
    case Vector((monomial, count)) if count == 1 =>
      monomial.toVector match {
        case Vector((Named(Naming.Atom, atom), 1)) if atom.startsWith(SizePrefix) => Some(atom.drop(2))
        case _                                                                    => None
      }
    case _ => None
  }

  private def display(path: String): String = Ids.replaceAllIn(path, "").split('.').takeRight(3).mkString(".")

  def show(size: Polynomial): String = {
    val shown = terms(size).view.collect { case term if term.sizes.nonEmpty => term.show }.toVector.sorted
    if (shown.nonEmpty) shown.mkString(" + ") else if (terms(size).nonEmpty) "1" else "?"
  }

  private case class Part(size: Boolean, name: String, power: Int)

  /** The terms of a bound made of sizes alone, without unknown or collapsed atoms. */
  def terms(bound: Polynomial): Vector[Complexity.Term] = bound.terms.keys.toVector.flatMap { monomial =>
    val parts = monomial.toVector.map {
      case (Named(Naming.Atom, atom), power) if atom.startsWith(SizePrefix) =>
        Some(Part(size = true, display(atom.drop(2)), power))
      case (Named(Naming.Atom, atom), power) if atom.startsWith(LogPrefix) =>
        Some(Part(size = false, display(atom.drop(2)), power))
      case _ => None
    }
    Option.when(parts.forall(_.isDefined)) {
      val known         = parts.flatten
      val (sizes, logs) = known.partition(_.size)
      Complexity.Term(sizes.map(part => part.name -> part.power).sortBy(_._1), logs.map(_.name).sorted)
    }
  }

  /** The number of size atoms and unknowns multiplied together in the largest term, not counting logarithms. */
  def degree(bound: Polynomial): Int = bound.terms.keys.map(degreeOf(_)).maxOption.getOrElse(0)

  def degreeOf(monomial: Polynomial.Monomial): Int = monomial.collect {
    case (Named(Naming.Atom, atom), power) if !atom.startsWith(LogPrefix) => power
  }.sum

  /** The bound with every path rooted at a bound name replaced by what the binding gives. */
  def rebind(bound: Polynomial, bindings: Map[String, Binding]): Polynomial =
    Polynomial.sum(bound.terms.map { (monomial, count) =>
      Polynomial.constant(count) * Polynomial.product(monomial.toVector.map { (factor, power) =>
        Polynomial.product(Vector.fill(power)(factor match {
          case Named(Naming.Atom, atom) => rebound(atom, bindings)
          case _                        => Unknown
        }))
      })
    })

  private def rebound(atom: String, bindings: Map[String, Binding]): Polynomial =
    if (!atom.startsWith(SizePrefix) && !atom.startsWith(LogPrefix)) Polynomial.atom(atom)
    else {
      val prefix = atom.take(2)
      val path   = atom.drop(2)
      bindings.collectFirst { case (root, binding) if path == root || path.startsWith(s"$root.") => (root, binding) }
        .fold(Polynomial.atom(atom)) { (root, binding) =>
          val rest = path.drop(root.length)
          if (rest.nonEmpty && binding.static) Polynomial.One
          else if (rest.nonEmpty) binding.path.fold(Unknown)(base => Polynomial.atom(prefix + base + rest))
          else if (prefix == SizePrefix) binding.size.getOrElse(Unknown)
          else binding.size.fold(Unknown)(log(_))
        }
    }
}
