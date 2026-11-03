package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.AnnotatedType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.rewrites.Rewrites.ActionPatch
import dotty.tools.dotc.util.Spans.Span
import net.kurobako.bonsai.lifting.Classes

private object Nowarns {
  val Checks: Set[Check] = Set(Check.NowarnSuppression)
}

/** Reports `@nowarn` annotations whose filters name only bonsai messages, on definitions and ascribed to expressions,
  * since a build without bonsai reports them as unused. `@SuppressWarnings` suppresses the same checks without it.
  */
private class Nowarns(checks: Checks)(using Context) {
  private val Nowarn  = Classes.required("scala.annotation.nowarn")
  private val Message = """msg=bonsai(?::([a-z][a-z0-9-]*))?""".r

  def run(tree: tpd.Tree): Unit = tree.foreachSubTree {
    case definition: tpd.MemberDef if definition.symbol.exists =>
      definition.symbol.annotations.filter(_.matches(Nowarn)).foreach(report)
    case tpd.Typed(_, tpt) => annotations(tpt.tpe).foreach(report)
    case _                 => ()
  }

  private def annotations(tpe: Type): List[Annotation] = tpe match {
    case AnnotatedType(parent, annotation) => annotations(parent) ++ Option.when(annotation.matches(Nowarn))(annotation)
    case _                                 => Nil
  }

  // The names the annotation suppresses, with `*` for every check, or nothing if any filter is not a bonsai one.
  private def names(annotation: Annotation): Option[List[String]] =
    annotation.argument(0).collect { case tpd.Literal(Constant(filters: String)) =>
      filters.split('&').toList.map { filter =>
        filter.trim match {
          case Message(id) => Some(Option(id).getOrElse("*"))
          case _           => None
        }
      }
    }.filter(filters => filters.nonEmpty && filters.forall(_.isDefined)).map(_.flatten.distinct)

  private def report(annotation: Annotation): Unit = names(annotation).foreach { ids =>
    val tree     = annotation.tree
    val source   = tree.source.content
    val start    = source.lastIndexWhere(!_.isWhitespace, tree.span.start - 1)
    val written  = if (start >= 0 && source(start) == '@') start else tree.span.start
    val span     = Span(written, tree.span.end)
    val literals = ids.map(id => if (id == "*") "\"bonsai\"" else s"\"bonsai:$id\"").mkString(", ")
    // A filter naming no id, or this check's own, matches the tag of this report, so only an error escapes it. The
    // message of the others must not name their id, or they would suppress it as well.
    checks(
      Check.NowarnSuppression,
      "use @SuppressWarnings(Array(\"bonsai:<id>\")) instead of @nowarn; builds without bonsai report " +
        "@nowarn as unused under -Wunused:nowarn",
      tree.srcPos,
      List(ActionPatch(tree.sourcePos.withSpan(span), s"@SuppressWarnings(Array($literals))")),
      escalate = ids.exists(id => id == "*" || id == Check.NowarnSuppression.id)
    )
  }
}
