package net.kurobako.bonsai.lifting

import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span
import net.kurobako.bonsai.collections.*

/** A chain of calls as the laws see it, with where its steps are written: `ends(i)` is the extent of the expression
  * that runs the steps up to and including `i`, so a rewrite of steps `from` until `until` replaces `span(until)` and
  * builds from the receiver before `from`. `standalone` when the chain is a statement, a definition or an argument,
  * where a receiver needs no parentheses once nothing follows it. `defines` names the local value the chain is the
  * definition of, and `minuend` the collection whose size or length the value of the chain is subtracted from.
  */
case class Lifted(
    chain: Chain,
    file: SourceFile,
    source: Span,
    ends: Vector[Span],
    standalone: Boolean,
    defines: Option[String] = None,
    minuend: Option[String] = None
) {

  /** The extent a rewrite of the steps up to `until` replaces. */
  def span(until: Int): Span = ends(until - 1)

  /** The extent of the expression that runs the steps before `from`. */
  def bare(from: Int): Span = if (from == 0) source else ends(from - 1)

  def text(span: Span): String = String(file.content(), span.start, span.end - span.start)

  /** The source text of the receiver of step `from`, written so a call can follow it. */
  def receiver(from: Int): String = Text.operand(text(bare(from)))
}
