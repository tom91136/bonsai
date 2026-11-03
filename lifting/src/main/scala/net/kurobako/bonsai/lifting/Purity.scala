package net.kurobako.bonsai.lifting

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.ExprType
import dotty.tools.dotc.core.Types.Type
import net.kurobako.bonsai.effects.*

/** What a front end knows of the effects of the functions a chain passes. `lambda` and `method` hold when calling the
  * function has no effect a rewrite could reorder, repeat or drop, and `value` when evaluating a plain expression has
  * none.
  */
trait Purity {
  def lambda(owner: Symbol, definition: tpd.DefDef)(using Context): Boolean
  def method(symbol: Symbol)(using Context): Boolean
  def value(tree: tpd.Tree)(using Context): Boolean = Purity.Syntactic.body(tree, Set.empty)
}

object Purity {

  private val Stateful = Vector(
    "scala.collection.mutable.Growable",
    "scala.collection.mutable.Shrinkable",
    "scala.collection.mutable.Iterable",
    "scala.collection.Iterator"
  ) ++ Axioms.Randoms

  def stateful(tpe: Type)(using Context): Boolean =
    tpe.derivesFrom(defn.ArrayClass) || Classes.derives(tpe, Stateful*)

  /** Purity from the syntax alone: literals, references to values, operators of the standard library on those, field
    * selections, tuples and matches that destructure a tuple or case class are pure, and any other call is not.
    */
  object Syntactic extends Purity {
    private val operators =
      Set("+", "-", "*", "/", "%", "<", ">", "<=", ">=", "==", "!=", "&&", "||", "&", "|", "^", "<<", ">>", ">>>")
    private val unary = Set("unary_!", "unary_-", "unary_+", "unary_~")
    private val Tuple = "_[1-9][0-9]*".r

    def lambda(owner: Symbol, definition: tpd.DefDef)(using Context): Boolean =
      body(definition.rhs, definition.termParamss.flatten.map(_.symbol).toSet)

    def method(symbol: Symbol)(using Context): Boolean = false

    def body(tree: tpd.Tree, bound: Set[Symbol])(using Context): Boolean = Calls.peel(tree) match {
      case _: (tpd.Literal | tpd.This)          => true
      case reference: tpd.Ident                 => bound(reference.symbol) || value(reference.symbol)
      case select @ tpd.Select(qualifier, name) =>
        if (unary(name.toString) && standard(select.symbol)) body(qualifier, bound)
        else
          (Tuple.matches(name.toString) || value(select.symbol)) && body(qualifier, bound)
      case tpd.Apply(select @ tpd.Select(left, name), List(right)) =>
        operators(name.toString) && standard(select.symbol) && body(left, bound) && body(right, bound) &&
        safeDivision(name.toString, right)
      case tpd.Match(selector, List(tpd.CaseDef(pattern, tpd.EmptyTree, result))) =>
        PatternShapes.irrefutable(pattern, selector.tpe) && body(selector, bound) && body(result, bound)
      case tpd.Apply(function, arguments) if Calls.tuple(function.symbol)         => arguments.forall(body(_, bound))
      case tpd.Apply(function, List(argument)) if Calls.identity(function.symbol) => body(argument, bound)
      case _                                                                      => false
    }

    private def standard(symbol: Symbol)(using Context): Boolean = symbol.exists && {
      val owner = symbol.owner.fullName.toString
      owner.startsWith("scala.") || owner == "java.lang.String"
    }

    private def safeDivision(operator: String, right: tpd.Tree): Boolean = operator match {
      case "/" | "%" =>
        right match {
          case tpd.Literal(constant) =>
            constant.value match {
              case n: Int    => n != 0
              case n: Long   => n != 0
              case n: Double => n != 0
              case n: Float  => n != 0
              case _         => false
            }
          case _ => false
        }
      case _ => true
    }

    /** A value, parameter or object, which reading has no effect, unlike a method, a variable, a lazy value or a
      * by-name parameter.
      */
    def value(symbol: Symbol)(using Context): Boolean =
      symbol.exists && symbol.isTerm &&
        (symbol.isOneOf(Flags.Module | Flags.Package) ||
          !symbol.isOneOf(Flags.Method | Flags.Lazy | Flags.Mutable) && !symbol.info.isInstanceOf[ExprType])
  }
}
