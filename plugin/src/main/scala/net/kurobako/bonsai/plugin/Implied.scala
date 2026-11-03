package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.NullTag
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.ExprType
import net.kurobako.bonsai.lifting.*

private object Implied {
  val Checks: Set[Check] = Set(Check.ImpliedCondition)
}

/** Reports an `if` whose condition an enclosing `if` of the same method already decided, so one of its branches never
  * runs. Conditions are the same when written the same: comparisons and sums of literals, immutable values and
  * parameters, and emptiness tests of options, strings and immutable collections. Anything a call or an assignment
  * could change, and any call but those, is never taken to be the same twice.
  */
private class Implied(checks: Checks)(using Context) {
  private val Comparisons = Set("==", "!=", "<", "<=", ">", ">=")
  private val Arithmetic  = Set("+", "-", "*")
  private val Emptiness   = Set("isEmpty", "nonEmpty", "isDefined")
  private val OptionClass = Classes.required("scala.Option")
  private val StringClass = Classes.required("java.lang.String")
  private val Immutable   = List(OptionClass, StringClass, Classes.required("scala.collection.immutable.Iterable"))

  /** A condition as a fact: the expression compared, and whether the condition holds when the expression does. */
  private case class Atom(key: String, holds: Boolean)

  /** A binary operator on booleans, where an operand that is `stops` decides the result. */
  private class Operator(name: String, val stops: Boolean) {
    def unapply(tree: tpd.Tree): Option[(tpd.Tree, tpd.Tree)] = tree match {
      case tpd.Apply(tpd.Select(left, method), List(right)) if method.toString == name => Some((left, right))
      case _                                                                           => None
    }
  }

  private val Conjunction = Operator("&&", stops = false)
  private val Disjunction = Operator("||", stops = true)

  private object Negation {
    def unapply(tree: tpd.Tree): Option[tpd.Tree] = tree match {
      case tpd.Select(inner, name) if name.toString == "unary_!"                 => Some(inner)
      case tpd.Apply(tpd.Select(inner, name), Nil) if name.toString == "unary_!" => Some(inner)
      case _                                                                     => None
    }
  }

  def run(tree: tpd.Tree): Unit = Traversal().traverse(tree)

  private class Traversal extends tpd.TreeTraverser {
    // The facts in force at the tree being visited, replaced for the span of a branch.
    private var known = Map.empty[String, Boolean] // scalafix:ok DisableSyntax.var

    def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
      case definition: tpd.DefDef if !definition.symbol.isAnonymousFunction =>
        reset(traverseChildren(tree))
      case branch: tpd.If =>
        if (!branch.isInline && Trees.real(branch)) decided(branch.cond).foreach(value =>
          checks(
            Check.ImpliedCondition,
            s"this condition is always $value here; an enclosing if already decided it",
            branch.cond.srcPos
          )
        )
        traverse(branch.cond)
        within(assumed(branch.cond, true))(traverse(branch.thenp))
        within(assumed(branch.cond, false))(traverse(branch.elsep))
      case _ => traverseChildren(tree)
    }

    private def within(facts: Map[String, Boolean])(body: => Unit): Unit = scoped(known ++ facts)(body)

    private def reset(body: => Unit): Unit = scoped(Map.empty)(body)

    private def scoped(facts: Map[String, Boolean])(body: => Unit): Unit = {
      val before = known
      known = facts
      try body
      finally known = before
    }

    def decided(condition: tpd.Tree)(using Context): Option[Boolean] = Calls.peelInlined(condition) match {
      case Negation(inner)   => decided(inner).map(!_)
      case Conjunction(l, r) => joined(l, r, Conjunction)
      case Disjunction(l, r) => joined(l, r, Disjunction)
      case other             => atom(other).flatMap(atom => known.get(atom.key).map(_ == atom.holds))
    }

    // An operand that stops the operator decides the whole; otherwise both must be known.
    private def joined(left: tpd.Tree, right: tpd.Tree, operator: Operator)(using Context): Option[Boolean] =
      (decided(left), decided(right)) match {
        case (Some(operator.stops), _) | (_, Some(operator.stops)) => Some(operator.stops)
        case (Some(_), Some(_))                                    => Some(!operator.stops)
        case _                                                     => None
      }
  }

  private def assumed(condition: tpd.Tree, value: Boolean)(using Context): Map[String, Boolean] =
    Calls.peelInlined(condition) match {
      case Negation(inner)                       => assumed(inner, !value)
      case Conjunction(l, r) if value            => assumed(l, true) ++ assumed(r, true)
      case Disjunction(l, r) if !value           => assumed(l, false) ++ assumed(r, false)
      case Conjunction(_, _) | Disjunction(_, _) => Map.empty
      case other                                 => atom(other).map(atom => atom.key -> (value == atom.holds)).toMap
    }

  private def atom(tree: tpd.Tree)(using Context): Option[Atom] = tree match {
    case tpd.Apply(tpd.Select(left, name), List(right)) if Comparisons(name.toString) && comparable(left, right) =>
      for {
        l <- key(left)
        r <- key(right)
      } yield
        if (name.toString == "!=") Atom(s"($l == $r)", holds = false) else Atom(s"($l $name $r)", holds = true)
    case tpd.Select(receiver, name) if Emptiness(name.toString)                 => emptiness(receiver, name.toString)
    case tpd.Apply(tpd.Select(receiver, name), Nil) if Emptiness(name.toString) =>
      emptiness(receiver, name.toString)
    case reference: (tpd.Ident | tpd.Select) => key(reference).map(Atom(_, holds = true))
    case _                                   => None
  }

  private def emptiness(receiver: tpd.Tree, name: String)(using Context): Option[Atom] = {
    val value = Calls.unconverted(receiver)
    if (Immutable.exists(value.tpe.widen.derivesFrom(_)))
      key(value).map(inner => Atom(s"$inner.isEmpty", holds = name == "isEmpty"))
    else None
  }

  // Equality of primitives, strings and options does not run code of the program, and reading an immutable value
  // twice finds the same value.
  private def comparable(left: tpd.Tree, right: tpd.Tree)(using Context): Boolean = {
    val tpe = left.tpe.widen
    Trees.primitive(tpe) || tpe.derivesFrom(StringClass) || tpe.derivesFrom(OptionClass) || nothing(right)
  }

  private def nothing(tree: tpd.Tree)(using Context): Boolean = Calls.peelInlined(tree) match {
    case tpd.Literal(constant) => constant.tag == NullTag
    case _                     => false
  }

  private def key(tree: tpd.Tree)(using Context): Option[String] = Calls.peelInlined(tree) match {
    case tpd.Literal(constant)                                      => Some(s"'${constant.value}")
    case tpd.This(_)                                                => Some("this")
    case select @ tpd.Select(qualifier, _) if stable(select.symbol) =>
      key(qualifier).map(inner => s"$inner.${select.symbol.id}")
    case reference: tpd.Ident if stable(reference.symbol) => Some(s"#${reference.symbol.id}")
    case tpd.Apply(tpd.Select(left, name), List(right))
        if Arithmetic(name.toString) && Trees.primitive(left.tpe.widen) =>
      for {
        l <- key(left)
        r <- key(right)
      } yield s"($l $name $r)"
    case _ => None
  }

  private def stable(symbol: Symbol)(using Context): Boolean =
    symbol.exists && symbol.isTerm && !symbol.is(Flags.Mutable) && !symbol.is(Flags.Method) &&
      !symbol.info.isInstanceOf[ExprType]
}
