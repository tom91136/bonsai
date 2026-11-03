package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.NamedType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeTraverser
import scala.collection.mutable

/** Records the build files a unit references, marking references made inside the splices of macro implementations.
  * `sourceOf` gives the path of the build file that defines a symbol, or nothing where it is not one.
  */
private class Edges(path: String, sourceOf: Symbol => Option[String], analysis: Analysis)(using Context)
    extends tpd.TreeTraverser {
  // (inside a macro implementation, inside one of its splices) for the tree being traversed
  private val scopes = mutable.Stack((false, false))

  // The files and the types already reported from the code outside and inside splices, which report them again alike.
  private val files = Array.fill(2)(mutable.HashSet.empty[String])
  private val types =
    Array.fill(2)(java.util.Collections.newSetFromMap(java.util.IdentityHashMap[Type, java.lang.Boolean]()))
  private var from: tpd.Tree = tpd.EmptyTree

  private val typeEdges = new TypeTraverser {
    def traverse(tpe: Type): Unit = if (types(spliced).add(tpe)) {
      add(tpe.typeSymbol, from)
      tpe match {
        case named: NamedType => traverse(named.prefix)
        case _                => traverseChildren(tpe)
      }
    }
  }

  private def spliced: Int = if (scopes.top._2) 1 else 0

  override def traverse(tree: tpd.Tree)(using Context): Unit = if (!tree.isEmpty) {
    val (declaration, call) = scopes.top
    scopes.push(tree match {
      case definition: tpd.DefDef       => (definition.symbol.is(Flags.Macro), call)
      case _: tpd.Splice if declaration => (declaration, true)
      case _: tpd.Quote                 => (declaration, false)
      case _                            => (declaration, call)
    })
    try visit(tree)
    finally scopes.pop(): Unit
  }

  private def visit(tree: tpd.Tree)(using Context): Unit = tree match {
    case _: tpd.Import                                              => ()
    case definition: tpd.MemberDef if definition.symbol == NoSymbol => traverseChildren(tree)
    case definition: tpd.MemberDef                                  =>
      definition.symbol.annotations.foreach(annotation => traverse(annotation.tree))
      traverseChildren(tree)
    case template: tpd.Template =>
      traverse(template.constr)
      template.parents.foreach { parent =>
        if (parent.hasType) add(parent.tpe.typeSymbol, parent)
        traverse(parent)
      }
      traverse(template.self)
      template.body.foreach(traverse(_))
    case _ =>
      add(tree.symbol, tree)
      addType(tree)
      traverseChildren(tree)
  }

  private def add(symbol: Symbol, tree: tpd.Tree)(using Context): Unit =
    sourceOf(symbol).foreach { source =>
      if (source != path && files(spliced).add(source)) analysis.reference(path, source, spliced == 1, tree.sourcePos)
    }

  private def addType(tree: tpd.Tree): Unit = if (tree.hasType) {
    from = tree
    typeEdges.traverse(tree.tpe)
  }
}
