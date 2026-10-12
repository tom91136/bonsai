package net.kurobako.baumkuchen.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

/** The immediate subtrees of a tree, in source order. */
private class Children(using Context) extends tpd.TreeAccumulator[List[tpd.Tree]] {
  def apply(children: List[tpd.Tree], tree: tpd.Tree)(using Context): List[tpd.Tree] = tree :: children

  def of(tree: tpd.Tree): List[tpd.Tree] = foldOver(Nil, tree).reverse
}
