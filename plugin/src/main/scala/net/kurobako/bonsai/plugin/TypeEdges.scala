package net.kurobako.bonsai.plugin

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.NamedType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeTraverser

/** Reports the type symbol of a type and of every type it is built from. */
private class TypeEdges(add: Symbol => Unit)(using Context) extends TypeTraverser {
  def traverse(tpe: Type): Unit = {
    add(tpe.typeSymbol)
    tpe match {
      case named: NamedType => traverse(named.prefix)
      case _                => traverseChildren(tpe)
    }
  }
}
