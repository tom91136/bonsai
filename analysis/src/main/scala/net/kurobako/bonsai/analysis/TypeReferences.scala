package net.kurobako.bonsai.analysis

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.NamedType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeTraverser

private[analysis] class TypeReferences(refer: Symbol => Unit)(using Context) extends TypeTraverser {
  def traverse(tpe: Type): Unit = {
    refer(tpe.typeSymbol)
    refer(tpe.termSymbol)
    tpe match {
      case named: NamedType => traverse(named.prefix)
      case _                => traverseChildren(tpe)
    }
  }
}
