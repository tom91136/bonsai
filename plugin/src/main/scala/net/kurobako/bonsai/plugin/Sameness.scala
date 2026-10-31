package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.Trees as ast
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import scala.collection.mutable

/** Whether two typed trees are the same expression, the symbols they resolve to equal, up to the names of the values
  * they bind: the binders of one tree stand for those of the other in the same position. Definitions of classes and
  * types are never the same, and an inferred type must be equal, so overloads and instantiations that differ keep two
  * trees apart.
  */
private final class Sameness(using Context) {
  private val renamed = mutable.Map.empty[Symbol, Symbol]

  def apply(left: tpd.Tree, right: tpd.Tree): Boolean = same(left, right)

  /** Takes the value `left` for the value `right`, as when two patterns bind them. */
  def rename(left: Symbol, right: Symbol): Sameness = {
    if (left.exists && right.exists) renamed(left) = right
    this
  }

  private def same(left: tpd.Tree, right: tpd.Tree): Boolean = (left eq right) ||
    ((left, right) match {
      case (a: tpd.Ident, b: tpd.Ident)   => reference(a.symbol, b.symbol, a.name == b.name)
      case (a: tpd.Select, b: tpd.Select) => reference(a.symbol, b.symbol, a.name == b.name) &&
        same(a.qualifier, b.qualifier)
      case (a: tpd.This, b: tpd.This)         => a.symbol == b.symbol
      case (a: tpd.Literal, b: tpd.Literal)   => a.const == b.const
      case (a: tpd.TypeTree, b: tpd.TypeTree) => a.tpe =:= b.tpe
      case (a: tpd.ValDef, b: tpd.ValDef)     =>
        bind(a.symbol, b.symbol) && same(a.tpt, b.tpt) && same(a.rhs, b.rhs)
      case (a: tpd.DefDef, b: tpd.DefDef) =>
        bind(a.symbol, b.symbol) && values(a.paramss, b.paramss) && same(a.tpt, b.tpt) && same(a.rhs, b.rhs)
      case (a: tpd.Bind, b: tpd.Bind)           => bind(a.symbol, b.symbol) && same(a.body, b.body)
      case (_: (tpd.TypeDef | tpd.Template), _) => false
      case _                                    =>
        left.getClass == right.getClass && values(left.productIterator.toList, right.productIterator.toList)
    })

  private def values(left: List[Any], right: List[Any]): Boolean =
    left.size == right.size && left.lazyZip(right).forall(value)

  private def value(left: Any, right: Any): Boolean = (left, right) match {
    case (a: ast.Tree[?], b: ast.Tree[?]) => same(a.asInstanceOf[tpd.Tree], b.asInstanceOf[tpd.Tree])
    case (a: List[?], b: List[?])         => values(a, b)
    case _                                => left == right
  }

  private def reference(left: Symbol, right: Symbol, named: Boolean): Boolean =
    if (left.exists && right.exists) renamed.getOrElse(left, left) == right else named && !left.exists && !right.exists

  private def bind(left: Symbol, right: Symbol): Boolean = {
    val ok = left.exists && right.exists && left.is(Flags.Mutable) == right.is(Flags.Mutable) &&
      left.is(Flags.Lazy) == right.is(Flags.Lazy)
    if (ok) renamed(left) = right
    ok
  }
}
