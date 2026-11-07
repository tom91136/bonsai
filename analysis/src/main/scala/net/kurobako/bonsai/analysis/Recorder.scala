package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol

/** What the tree traversal records of the declarations and uses it meets, beyond what the components own. */
private[analysis] trait Recorder {
  def initialiser(symbol: Symbol, definition: tpd.ValDef)(using Context): Unit

  def body(symbol: Symbol, definition: tpd.DefDef)(using Context): Unit

  def constructor(symbol: Symbol, definition: tpd.DefDef, template: Option[tpd.Template])(using Context): Unit

  def define(cls: Symbol, template: tpd.Template)(using Context): Unit

  def signature(method: Symbol, body: tpd.Tree)(using Context): Unit

  def design(method: Symbol, body: tpd.Tree)(using Context): Unit

  def alias(symbol: Symbol)(using Context): Unit

  def precondition(method: Symbol, body: tpd.Tree)(using Context): Unit

  def member(symbol: Symbol)(using Context): Unit

  def matched(unapply: tpd.UnApply, from: Option[Use])(using Context): Unit

  def assign(symbol: Symbol): Unit

  def enclosed(symbol: Symbol)(using Context): Unit

  def expose(symbol: Symbol)(using Context): Unit

  def instance(symbol: Symbol)(using Context): Unit

  def mention(tree: tpd.Tree)(using Context): Unit

  def mention(symbols: List[Symbol])(using Context): Unit

  def use(symbol: Symbol, from: Option[Use])(using Context): Unit
}
