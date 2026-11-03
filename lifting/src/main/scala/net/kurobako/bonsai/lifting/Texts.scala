package net.kurobako.bonsai.lifting

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

/** The source text of trees. */
private[lifting] object Texts {
  def of(tree: tpd.Tree)(using Context): String = {
    val position = tree.sourcePos
    if (position.exists) {
      val content = scala.util.Try(position.source.content()).getOrElse(Array.empty[Char])
      if (position.end <= content.length) String(content, position.start, position.end - position.start)
      else tree.show
    } else tree.show
  }

  def real(tree: tpd.Tree): Boolean = tree.span.exists && tree.span.end > tree.span.start
}
