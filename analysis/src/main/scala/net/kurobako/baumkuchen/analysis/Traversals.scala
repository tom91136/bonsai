package net.kurobako.baumkuchen.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.Type

/** Collection traversals, whose function arguments run once per element. */
private object Traversals {

  /** Whether values of a type can be traversed: collections and arrays, but not options. */
  def collection(tpe: Type)(using Context): Boolean = {
    val widened = tpe.widen
    !widened.derivesFrom(defn.OptionClass) &&
    (widened.derivesFrom(requiredClass("scala.collection.IterableOnce")) || widened.derivesFrom(defn.ArrayClass) ||
      widened.derivesFrom(requiredClass("scala.collection.ArrayOps")) ||
      widened.derivesFrom(requiredClass("scala.collection.StringOps")))
  }

  /** Whether `function` selects a method of a collection. */
  def traversal(function: tpd.Tree)(using Context): Boolean = selected(function).exists(select =>
    collection(select.qualifier.tpe)
  )

  /** The method selection an application applies, through type applications and earlier argument lists. */
  @annotation.tailrec
  def selected(function: tpd.Tree): Option[tpd.Select] = function match {
    case select: tpd.Select      => Some(select)
    case tpd.TypeApply(inner, _) => selected(inner)
    case tpd.Apply(inner, _)     => selected(inner)
    case _                       => None
  }

  /** The definition of an anonymous function argument. */
  @annotation.tailrec
  def lambda(tree: tpd.Tree): Option[tpd.DefDef] = tree match {
    case tpd.Block(List(definition: tpd.DefDef), tpd.Closure(_, _, _)) => Some(definition)
    case tpd.Block(Nil, expression)                                    => lambda(expression)
    case tpd.Typed(expression, _)                                      => lambda(expression)
    case tpd.Inlined(_, _, expansion)                                  => lambda(expansion)
    case _                                                             => None
  }
}
