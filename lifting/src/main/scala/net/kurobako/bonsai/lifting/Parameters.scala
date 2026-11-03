package net.kurobako.bonsai.lifting

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol

object Parameters {
  def sole(definition: tpd.DefDef)(using Context): Option[Symbol] = definition.termParamss.flatten match {
    case List(only) => Some(only.symbol)
    case _          => None
  }
}
