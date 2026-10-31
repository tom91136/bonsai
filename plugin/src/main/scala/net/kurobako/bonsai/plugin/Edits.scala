package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.rewrites.Rewrites.ActionPatch
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans.Span

/** Fixes that edit the source of the unit being checked, by character offsets. */
private object Edits {

  def source(using Context): String = ctx.source.content().mkString

  def replace(start: Int, end: Int, text: String)(using Context): ActionPatch =
    ActionPatch(SourcePosition(ctx.source, Span(start, end)), text)

  def insert(at: Int, text: String)(using Context): ActionPatch = replace(at, at, text)

  /** Removes a statement, and the line it leaves empty. */
  def remove(content: String, statement: tpd.Tree)(using Context): ActionPatch = {
    val start     = statement.span.start
    val end       = statement.span.end
    val before    = content.lastIndexWhere(character => character == '\n' || visible(character), start - 1)
    val lineStart = before + 1
    val alone     = lineStart == 0 || content.charAt(before) == '\n'
    val after     = content.indexWhere(character => character == '\n' || visible(character), end)
    if (alone && (after < 0 || content.charAt(after) == '\n'))
      replace(lineStart, if (after < 0) content.length else after + 1, "")
    else {
      val next = content.indexWhere(visible, end)
      replace(start, if (next >= 0 && content.charAt(next) == ';') next + 1 else end, "")
    }
  }

  private def visible(character: Char): Boolean = character != ' ' && character != '\t' && character != '\r'
}
