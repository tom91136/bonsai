package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.Type
import net.kurobako.bonsai.lifting.Calls

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
  def traversal(function: tpd.Tree)(using Context): Boolean = Calls.selected(function).exists(select =>
    collection(select.qualifier.tpe)
  )
}
