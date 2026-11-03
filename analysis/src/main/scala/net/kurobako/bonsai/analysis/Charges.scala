package net.kurobako.bonsai.analysis

import net.kurobako.bonsai.collections.Sink
import scala.collection.mutable

/** The bounds and charges the cost model of either language is made of. */
private object Charges {

  object Bound {
    val Zero: Bound    = Bound(Polynomial.Zero, Polynomial.Zero)
    val Unknown: Bound = Bound(Atoms.Unknown, Atoms.Unknown)
  }

  /** Time and space bounds; space counts the copies of collections an operation makes that outlive the call. */
  case class Bound(time: Polynomial, space: Polynomial) {
    def +(that: Bound): Bound = Bound(time + that.time, space + that.space)

    def *(factor: Polynomial): Bound = Bound(time * factor, space * factor)

    /** The terms that are not above linear in sizes, which is what a caller inherits from a callee. */
    def linear: Bound = Bound(kept(time), kept(space))

    private def kept(bound: Polynomial): Polynomial =
      Polynomial(bound.terms.filter((monomial, _) => Atoms.degreeOf(monomial) <= 1))
  }

  /** What an operation costs: a description of it, its time and allocation, and whether it is linear in a size. */
  case class Work(what: String, time: Polynomial, space: Polynomial, linear: Boolean)

  enum Operation {
    case Search, Shift, Copy, Sort

    def work(what: String, size: Polynomial, linear: Boolean): Work = this match {
      case Search | Shift => Work(what, size, Polynomial.Zero, linear)
      case Copy           => Work(what, size, size, linear)
      case Sort           => Work(what, size * Atoms.log(size), size, linear)
    }
  }

  def growsWith(count: Option[Polynomial]): Boolean = count.exists(Atoms.degree(_) > 0)

  def linearIn(time: Polynomial): Boolean = Atoms.terms(time).exists(_.degree == 1)

  private def sum(charges: Iterable[Charge]): Bound =
    charges.foldLeft(Bound.Zero)((total, charge) => total + Bound(charge.time, charge.escaping))

  final class Memo[K] {
    private val summaries = mutable.Map.empty[K, Bound]
    private val pending   = mutable.Set.empty[K]

    def apply(key: K)(compute: => Bound): Bound = summaries.get(key) match {
      case Some(known)          => known
      case None if pending(key) => Bound.Unknown
      case None                 =>
        pending += key
        val result =
          try compute
          finally pending -= key
        summaries(key) = result
        result
    }
  }

  /** A callee that is not a body of the build to walk: its bound, and the names its parameters are bound by. */
  case class Callee(bound: Bound, roots: Seq[String])

  /** How a method calls itself: not at all, once per element of a parameter that shrinks, or unboundedly. */
  enum Recursion {
    case Absent
    case Bounded(factor: Polynomial)
    case Unbounded

    /** The bound of a method from the work of one frame and of the branches that end the recursion. */
    def total(frame: Bound, exits: Bound): Bound = this match {
      case Absent          => frame + exits
      case Bounded(factor) => (frame + Bound(Polynomial.One, Polynomial.Zero)) * factor + exits
      case Unbounded       => Bound.Unknown
    }

    def scaled(charges: Vector[Charge]): Vector[Charge] = this match {
      case Bounded(factor) => charges.map(_.scaled(factor))
      case _               => charges
    }
  }

  /** The bound of a method from its charges: the frames of its recursion, and the exits that end it. */
  def total(recursion: Recursion, charges: Iterable[Charge]): Bound = {
    val (exits, frames) = charges.partition(_.terminal)
    recursion.total(sum(frames), sum(exits))
  }

  /** The hits of the charges of a method: the time and allocation above linear, and linear work done by callbacks. */
  def bounds(recursion: Recursion, charges: Vector[Charge]): Complexity.Bounds = {
    val scaled = recursion.scaled(charges)
    Complexity.Bounds(
      hit(scaled, _.time),
      hit(scaled, _.space),
      scaled.find(charge => charge.callback && charge.linear).map(_.hit(Vector.empty))
    )
  }

  private def hit(charges: Vector[Charge], select: Charge => Polynomial): Option[Complexity.Hit] = {
    val candidates = charges.flatMap(charge => Atoms.terms(select(charge)).map(_ -> charge))
    val top        = candidates.map(_._1.degree).maxOption.getOrElse(0)
    Option.when(top >= 2) {
      val best = candidates.filter(_._1.degree == top)
      best.head._2.hit(best.map(_._1).distinct)
    }
  }

  /** Where a charge is made: its line, the scope around it, and what becomes of the value it produces. */
  case class Site(line: Int, scope: Scope, sink: Sink)

  /** One costly operation, its cost already multiplied by the turns of the loops around it. Its space is what is live
    * at once, which only the loops it is kept across multiply; `escaping` is the space a caller inherits, which is the
    * part the method returns or stores.
    */
  case class Charge(
      what: String,
      within: Option[String],
      time: Polynomial,
      space: Polynomial,
      escaping: Polynomial,
      linear: Boolean,
      callback: Boolean,
      terminal: Boolean,
      line: Int
  ) {
    def scaled(factor: Polynomial): Charge =
      if (terminal) this else copy(time = time * factor, space = space * factor, escaping = escaping * factor)

    def hit(terms: Vector[Complexity.Term]): Complexity.Hit = Complexity.Hit(terms, what, within, line)
  }

  object Charge {
    def at(site: Site, counted: Scope, work: Work): Charge =
      Charge(
        work.what,
        site.scope.loops.findLast(!_.hidden).map(_.label),
        counted.factor * work.time,
        counted.factorFrom(site.sink.level) * work.space,
        if (site.sink.escapes) counted.factor * work.space else Polynomial.Zero,
        work.linear,
        site.scope.callback,
        site.scope.terminal,
        site.line
      )
  }
}
