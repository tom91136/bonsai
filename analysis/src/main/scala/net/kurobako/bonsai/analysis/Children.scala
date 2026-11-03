package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx

/** The immediate subtrees of a tree, in source order. */
private class Children(using Context) extends tpd.TreeAccumulator[List[tpd.Tree]] {
  // The compiler looks up the context of a tree's source on every visit that arrives from another source's context.
  private var sourced = ctx

  def apply(children: List[tpd.Tree], tree: tpd.Tree)(using Context): List[tpd.Tree] = tree :: children

  def of(tree: tpd.Tree): List[tpd.Tree] = {
    val source = tree.source
    if (source.exists && !(source eq sourced.source)) sourced = ctx.withSource(source)
    foldOver(Nil, tree)(using sourced).reverse
  }
}
