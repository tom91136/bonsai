package net.kurobako.bonsai.plugin

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Types.AnnotatedType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.CodeAction
import dotty.tools.dotc.rewrites.Rewrites
import dotty.tools.dotc.rewrites.Rewrites.ActionPatch
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.util.SrcPos
import java.nio.file.Path
import java.nio.file.Paths
import net.kurobako.bonsai.collections.*
import net.kurobako.bonsai.lifting.*
import scala.collection.mutable

/** Reports each check at its severity unless it is disabled, escalated to an error, kept as a warning by `lenient`, or
  * found in a mutation-testing copy of a source. The checks `overGenerated` holds skip generated code, which
  * `Generated` marks or a `generated` glob names. `@SuppressWarnings(Array("bonsai:<id>"))`, or `"bonsai"` for every
  * check, on an enclosing definition, as for the analysis, or ascribed to an expression, as
  * `expr: @SuppressWarnings(...)`, suppresses its warnings, errors and information. `@nowarn("msg=bonsai:<id>")`
  * suppresses a check's warnings too, but a build without bonsai reports it as unused. A report that `escalate`s is an
  * error, which `@nowarn` cannot suppress. A warning carries its `fix` as a code action, which `-rewrite` applies and
  * editors offer.
  */
private class Checks(
    disabled: Set[Check],
    errors: Set[Check],
    lenient: Set[Check],
    instrumented: Path => Boolean,
    generatedPath: Path => Boolean,
    overGenerated: Set[Check],
    suppressions: mutable.Map[SourceFile, List[(Span, Set[String])]],
    patched: mutable.Map[SourceFile, List[Span]] = mutable.Map.empty,
    generatedSpans: mutable.Map[SourceFile, List[Span]] = mutable.Map.empty,
    reported: Option[Set[Check]] = None,
    records: Option[Records] = None
) {

  /** A view that reports only `owned`, and says whether any of them is enabled so that the code reporting them can be
    * skipped. Reporting any other check through it fails, so the set cannot drift from the code.
    */
  def owning(owned: Iterable[Check]): Checks =
    Checks(
      disabled,
      errors,
      lenient,
      instrumented,
      generatedPath,
      overGenerated,
      suppressions,
      patched,
      generatedSpans,
      Some(owned.toSet),
      records
    )

  /** Whether some check this view owns is enabled. */
  def active: Boolean = reported.forall(_.exists(!disabled(_)))

  def apply(
      check: Check,
      message: String,
      position: SrcPos,
      fix: List[ActionPatch] = Nil,
      escalate: Boolean = false
  )(using Context): Unit = {
    require(reported.forall(_(check)), s"${check.id} is not among the checks this pass reports")
    if (
      !disabled(check) && !instrumented(Paths.get(position.sourcePos.source.path)) &&
      !suppressed(check, position) && !generated(check, position)
    ) {
      val tagged      = s"$message [bonsai:${check.id}]"
      val shown       = start(position)
      val failing     = escalate || errors(check) || check.severity == Check.Severity.Error && !lenient(check)
      val informative = !failing && check.severity == Check.Severity.Info && !lenient(check)
      records.foreach(
        _.add(shown.source, Recorded.Entry(check.id, level(check, failing, informative), shown.line + 1, message))
      )
      if (failing && records.isEmpty) report.error(tagged, shown)
      else if (informative) report.echo(tagged, shown)
      else {
        val actions = Option.when(fix.nonEmpty && disjoint(fix))(CodeAction(s"fix ${check.id}", None, fix)).toList
        actions.foreach(Rewrites.applyAction)
        report.warning(Fixable(tagged, actions), shown)
      }
    }
  }

  // what the strict mode makes of the finding: it fails on all but information and the checks kept as warnings
  private def level(check: Check, failing: Boolean, informative: Boolean): Recorded.Level =
    if (informative) Recorded.Level.Info
    else if (failing || !lenient(check)) Recorded.Level.Error
    else Recorded.Level.Warning

  // Patches that overlap make the compiler fail, so the fix of the check that reports second is left out.
  private def disjoint(fix: List[ActionPatch]): Boolean = {
    val spans = fix.map(patch => patch.srcPos.source -> patch.srcPos.span)
    val clear = spans.forall((source, span) =>
      !patched.getOrElse(source, Nil).exists(other => other.start < span.end && span.start < other.end)
    )
    if (clear) spans.foreach((source, span) => patched.update(source, span :: patched.getOrElse(source, Nil)))
    clear
  }

  // The compiler shows every line a position spans, which for a table of thousands of lines exhausts the heap.
  private def start(position: SrcPos)(using Context): SourcePosition = {
    val source = position.sourcePos
    if (!source.exists || source.startLine == source.endLine) source else source.withSpan(source.span.startPos)
  }

  // later runs of a suspended compilation report at positions of units they no longer hold
  def remember(unit: CompilationUnit)(using Context): List[(Span, Set[String])] = {
    val spans = annotated(unit.tpdTree)
    suppressions.update(unit.source, spans)
    spans
  }

  private def suppressed(check: Check, position: SrcPos)(using Context): Boolean = {
    val source = position.sourcePos.source
    val spans  = suppressions.get(source).orElse(ctx.run.nn.units.find(_.source == source).map(remember(_)))
    spans.getOrElse(Nil).exists((span, ids) => span.contains(position.span) && (ids("*") || ids(check.id)))
  }

  private def generated(check: Check, position: SrcPos)(using Context): Boolean = {
    val source = position.sourcePos.source
    val spans  = generatedSpans.get(source).orElse(ctx.run.nn.units.find(_.source == source).map(generatedBy(_)))
    overGenerated(check) && spans.getOrElse(Nil).exists(_.contains(position.span))
  }

  private def generatedBy(unit: CompilationUnit)(using Context): List[Span] = {
    val content = unit.source.content
    val spans   =
      if (generatedPath(Paths.get(unit.source.path)) || Generated.marked(String(content)))
        List(Span(0, content.length))
      else annotatedGenerated(unit.tpdTree)
    generatedSpans.update(unit.source, spans)
    spans
  }

  private def annotatedGenerated(tree: tpd.Tree)(using Context): List[Span] =
    tree.filterSubTrees {
      case definition: tpd.MemberDef =>
        definition.symbol.exists && definition.symbol.annotations.exists(annotation =>
          Generated.Annotations(annotation.symbol.fullName.toString)
        )
      case _ => false
    }.map(_.span)

  private def annotated(tree: tpd.Tree)(using Context): List[(Span, Set[String])] = {
    val Suppress = Classes.required("java.lang.SuppressWarnings")
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
    def ids(annotation: Annotation): Set[String] =
      annotation.argument(0).toList.flatMap(elements(_)).collect {
        case tpd.Literal(Constant("bonsai"))                                   => "*"
        case tpd.Literal(Constant(text: String)) if text.startsWith("bonsai:") => text.stripPrefix("bonsai:")
      }.toSet
    @annotation.tailrec
    def ascribed(tpe: Type, found: List[Annotation]): List[Annotation] = tpe match {
      case AnnotatedType(parent, annotation) =>
        ascribed(parent, if (annotation.matches(Suppress)) annotation :: found else found)
      case _ => found
    }
    tree.filterSubTrees {
      case definition: tpd.MemberDef => definition.symbol.exists && definition.symbol.hasAnnotation(Suppress)
      case tpd.Typed(_, tpt)         => ascribed(tpt.tpe, Nil).nonEmpty
      case _                         => false
    }.flatMap {
      case definition: tpd.MemberDef =>
        definition.symbol.getAnnotation(Suppress).map(annotation => definition.span -> ids(annotation))
      case typed @ tpd.Typed(_, tpt) => Some(typed.span -> ascribed(tpt.tpe, Nil).flatMap(ids(_)).toSet)
      case _                         => None
    }
  }
}
