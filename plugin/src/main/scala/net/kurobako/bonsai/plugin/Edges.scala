package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.Symbols.Symbol
import scala.collection.mutable

/** Records the build files a unit references, marking references made inside the splices of macro implementations. */
private class Edges(path: String, valid: Symbol => Boolean, analysis: Analysis) extends tpd.TreeTraverser {
  // (inside a macro implementation, inside one of its splices) for the tree being traversed
  private val scopes = mutable.Stack((false, false))

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
    case select: tpd.Select =>
      add(select.symbol, tree)
      addType(tree)
      traverse(select.qualifier)
    case _ =>
      add(tree.symbol, tree)
      addType(tree)
      traverseChildren(tree)
  }

  private def add(symbol: Symbol, tree: tpd.Tree)(using Context): Unit =
    if (valid(symbol) && symbol.source.path != path)
      analysis.reference(path, symbol.source.path, scopes.top._2, tree.sourcePos)

  private def addType(tree: tpd.Tree)(using Context): Unit =
    if (tree.hasType) TypeEdges(add(_, tree)).traverse(tree.tpe)
}
