package net.kurobako.bonsai.analysis

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.NamedType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeTraverser
import scala.collection.mutable

/** The classes a type names, with their parents, not those only its prefixes name. */
private[analysis] object ClassesNamed {

  def of(tpe: Type)(using Context): Vector[Symbol] = {
    val found = mutable.LinkedHashSet.empty[Symbol]
    Traverser(found).traverse(tpe)
    found.toVector
  }

  private class Traverser(found: mutable.Set[Symbol])(using Context) extends TypeTraverser {
    def traverse(tpe: Type): Unit = tpe.dealias match {
      case named: NamedType => if (named.symbol.isClass) found ++= named.symbol.asClass.baseClasses
      case other            => traverseChildren(other)
    }
  }
}
