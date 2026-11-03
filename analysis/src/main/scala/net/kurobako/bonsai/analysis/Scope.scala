package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Symbols.Symbol

private object Scope {

  /** A loop the code runs inside, with the number of turns it takes and a description of it. A loop driven by a counter
    * or iterator that survives the loops around it takes its turns across all their turns, so `absorbs` of them do not
    * multiply it.
    */
  case class Loop(trips: Polynomial, label: String, absorbs: Int = 0, hidden: Boolean = false)

  /** A local variable: the number of loops enclosing its declaration, its size at declaration, and whether every
    * assignment to it takes a part of the previous value, so that it never outgrows its first size, and how much a turn
    * of a loop adds to it.
    */
  case class Variable(
      level: Int,
      initial: Option[Polynomial],
      shrinking: Boolean,
      growth: Polynomial = Polynomial.One
  )
}

/** What is known at a point of a method body. */
private case class Scope(
    sizes: Map[Symbol, Polynomial] = Map.empty,
    paths: Map[Symbol, String] = Map.empty,
    counts: Map[Symbol, Polynomial] = Map.empty,
    variables: Map[Symbol, Scope.Variable] = Map.empty,
    smaller: Map[Symbol, Symbol] = Map.empty,
    built: Map[Symbol, tpd.Apply] = Map.empty,
    levels: Map[Symbol, Int] = Map.empty,
    resets: Set[Symbol] = Set.empty,
    loops: Vector[Scope.Loop] = Vector.empty,
    callback: Boolean = false,
    terminal: Boolean = false,
    inFallback: Boolean = false,
    amortized: Option[Int] = None
) {
  def factor: Polynomial = factorOf(loops)

  /** The turns of the loops from the `level`-th one inwards, which is how often a value kept across them is built. */
  def factorFrom(level: Int): Polynomial =
    if (level <= 0) factor
    else
      factorOf(loops.drop(loops.zipWithIndex.filterNot(_._1.hidden).lift(level).fold(loops.size)(_._2)))

  private def factorOf(inner: Vector[Scope.Loop]): Polynomial =
    Polynomial.product(inner.foldLeft(Vector.empty[Polynomial])((turns, loop) =>
      turns.dropRight(loop.absorbs) :+ loop.trips
    ))

  def tracking(symbol: Symbol, variable: Scope.Variable): Scope = copy(variables = variables.updated(symbol, variable))

  def pushed(loop: Scope.Loop): Scope =
    copy(loops = loops :+ (if amortized.contains(loops.size) then loop.copy(trips = Polynomial.One) else loop))
}
