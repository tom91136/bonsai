package net.kurobako.baumkuchen.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.CodeAction
import dotty.tools.dotc.reporting.Diagnostic.OriginWarning
import dotty.tools.dotc.reporting.NoExplanation
import dotty.tools.dotc.rewrites.Rewrites.ActionPatch
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.util.SrcPos
import java.nio.file.Path
import java.nio.file.PathMatcher
import scala.collection.mutable

/** Reports each check at its severity unless it is disabled, escalated to an error, kept as a warning by `lenient`, or
  * found in a `generated` source. Messages end with the check id, so `@nowarn("msg=baumkuchen:<id>")` suppresses one
  * check's warnings. As for the analysis, `@SuppressWarnings("baumkuchen:<id>")`, or `"baumkuchen"` for every check, on
  * an enclosing definition suppresses its warnings, errors and information. A warning carries its `fix` as a code
  * action, which `-rewrite` applies and editors offer.
  */
private class Checks(disabled: Set[Check], errors: Set[Check], lenient: Set[Check], generated: List[PathMatcher]) {
  def apply(check: Check, message: String, position: SrcPos, fix: List[ActionPatch] = Nil)(using Context): Unit =
    if (
      !disabled(check) && !generated.exists(_.matches(Path.of(position.sourcePos.source.path))) &&
      !suppressed(check, position)
    ) {
      val tagged = s"$message [baumkuchen:${check.id}]"
      val shown  = start(position)
      if (errors(check) || check.severity == Severity.Error && !lenient(check)) report.error(tagged, shown)
      else if (check.severity == Severity.Info && !lenient(check)) report.echo(tagged, shown)
      else
        report.warning(
          NoExplanation(tagged).withActions(Option.when(fix.nonEmpty)(CodeAction(
            s"fix ${check.id}",
            None,
            fix
          )).toList*),
          shown,
          OriginWarning.NoOrigin
        )
    }

  // The compiler shows every line a position spans, which for a table of thousands of lines exhausts the heap.
  private def start(position: SrcPos)(using Context): SourcePosition = {
    val source = position.sourcePos
    if (!source.exists || source.startLine == source.endLine) source else source.withSpan(source.span.startPos)
  }

  // The definitions of each source annotated with the checks they suppress, read from the source's own unit.
  private val suppressions = mutable.Map.empty[SourceFile, List[(Span, Set[String])]]

  private def suppressed(check: Check, position: SrcPos)(using Context): Boolean = {
    val source = position.sourcePos.source
    suppressions.getOrElseUpdate(
      source,
      ctx.run.nn.units.find(_.source == source).fold(Nil)(unit => annotated(unit.tpdTree))
    ).exists((span, ids) => span.contains(position.span) && (ids("*") || ids(check.id)))
  }

  private def annotated(tree: tpd.Tree)(using Context): List[(Span, Set[String])] = {
    val Suppress = requiredClass("java.lang.SuppressWarnings")
    tree.filterSubTrees {
      case definition: tpd.MemberDef => definition.symbol.exists && definition.symbol.hasAnnotation(Suppress)
      case _                         => false
    }.map { definition =>
      // The annotation takes one array of names, written `Array(...)`, whose second argument list is its ClassTag.
      @annotation.tailrec
      def elements(tree: tpd.Tree): List[tpd.Tree] = tree match {
        case tpd.NamedArg(_, argument)         => elements(argument)
        case tpd.Apply(function: tpd.Apply, _) => elements(function)
        case tpd.Apply(_, List(argument))      => elements(argument)
        case tpd.Typed(expression, _)          => elements(expression)
        case tpd.SeqLiteral(values, _)         => values
        case other                             => List(other)
      }
      val names = definition.symbol.getAnnotation(Suppress).flatMap(_.argument(0)).toList.flatMap(elements(_))
      definition.span -> names.collect {
        case tpd.Literal(Constant("baumkuchen"))              => "*"
        case tpd.Literal(Constant(s"baumkuchen:$id": String)) => id
      }.toSet
    }
  }
}
