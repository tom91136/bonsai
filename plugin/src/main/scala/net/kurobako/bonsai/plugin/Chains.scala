package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.rewrites.Rewrites.ActionPatch
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans.Span
import net.kurobako.bonsai.collections.*
import net.kurobako.bonsai.lifting.*
import scala.collection.mutable

private object Chains {
  val Checks: Set[Check] = Check.laws.toSet
}

/** Chains of collection calls that the laws of the collections module find a cheaper or clearer form of. The chains are
  * lifted from the typed trees, the laws decide what to do with them, and an exact rewrite becomes the fix of its
  * warning. A run that a view fuses is reported here only where its functions are pure and the plugin can write the
  * view and the conversion back to the collection the chain gives.
  */
private class Chains(checks: Checks)(using Context) {
  private val lifter = Lifter(Purity.Syntactic)

  def run(tree: tpd.Tree): Unit = {
    val reported = mutable.Set.empty[(String, Span)]
    lifter.chains(tree, ctx.owner).filter(_.file == ctx.source).foreach { lifted =>
      Laws.outcomes(lifted.chain).foreach { outcome =>
        val fix = fixes(lifted, outcome)
        if (outcome.lint || fix.nonEmpty) {
          val span = lifted.span(outcome.until)
          if (reported.add((outcome.law, span)))
            checks(Check.law(outcome.law), outcome.message, SourcePosition(lifted.file, span), fix)
        }
      }
    }
  }

  private def fixes(lifted: Lifted, outcome: Outcome): List[ActionPatch] = {
    val span = lifted.span(outcome.until)
    outcome match {
      case rewrite: Outcome.Rewrite =>
        replacement(lifted, rewrite).map(ActionPatch(SourcePosition(lifted.file, span), _)).toList
      case report: Outcome.Report if report.viewable && report.pure => viewing(lifted, report)
      case _: Outcome.Report                                        => Nil
    }
  }

  /** The text that replaces the steps of an exact rewrite; a rewrite that leaves the receiver as it is replaces a chain
    * that stands alone by the receiver as it was written.
    */
  private def replacement(lifted: Lifted, rewrite: Outcome.Rewrite): Option[String] = {
    val receiver = lifted.receiver(rewrite.from)
    val text     = rewrite.render(receiver)
    Some(
      if (text == receiver && lifted.standalone && rewrite.until == lifted.chain.steps.size)
        lifted.text(lifted.bare(rewrite.from))
      else text
    )
  }

  private val Conversions = Map(
    "immutable.List"       -> "toList",
    "immutable.Vector"     -> "toVector",
    "immutable.Set"        -> "toSet",
    "immutable.Map"        -> "toMap",
    "immutable.Seq"        -> "toSeq",
    "Seq"                  -> "toSeq",
    "immutable.IndexedSeq" -> "toIndexedSeq"
  )

  /** The insertions that run a fused run on a view: `.view` after its receiver, and the conversion to the collection
    * the chain gives after its last step, which the chain's next step makes unnecessary if it converts or ends the
    * chain.
    */
  private def viewing(lifted: Lifted, report: Outcome.Report): List[ActionPatch] = {
    val steps  = lifted.chain.steps
    val bare   = lifted.bare(report.from)
    val ending = steps.lift(report.until) match {
      case None       => Conversions.get(steps(report.until - 1).result.name).map("." + _)
      case Some(next) =>
        Option.when(Methods.Conversions(next.method) || Ops(next.method).exists(_.shape == Shape.Terminal))("")
    }
    val dotted = (report.from until report.until).forall(k => dot(lifted, lifted.bare(k).end))
    if (ending.isEmpty || !dotted || lifted.text(bare) != lifted.receiver(report.from)) Nil
    else {
      val insertion = (at: Int, text: String) => ActionPatch(SourcePosition(lifted.file, Span(at, at)), text)
      insertion(bare.end, ".view") :: ending.filter(_.nonEmpty).map(insertion(lifted.span(report.until).end, _)).toList
    }
  }

  /** Whether the call after `end` is written with a dot rather than infix. */
  private def dot(lifted: Lifted, end: Int): Boolean = {
    val content = lifted.file.content()
    Iterator.range(end, content.length).map(content(_)).dropWhile(_.isWhitespace).nextOption().contains('.')
  }
}
