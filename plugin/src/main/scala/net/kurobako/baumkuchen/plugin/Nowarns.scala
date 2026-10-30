package net.kurobako.baumkuchen.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.AnnotatedType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.rewrites.Rewrites.ActionPatch
import dotty.tools.dotc.util.Spans.Span

/** Reports `@nowarn` annotations whose filters name only baumkuchen messages, on definitions and ascribed to
  * expressions, since a build without baumkuchen reports them as unused. `@SuppressWarnings` suppresses the same checks
  * without it.
  */
private class Nowarns(checks: Checks)(using Context) {
  private val Nowarn  = requiredClass("scala.annotation.nowarn")
  private val Message = """msg=baumkuchen(?::([a-z][a-z0-9-]*))?""".r

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

  // The names the annotation suppresses, with `*` for every check, or nothing if any filter is not a baumkuchen one.
  private def names(annotation: Annotation): Option[List[String]] =
    annotation.argument(0).collect { case tpd.Literal(Constant(filters: String)) =>
      filters.split('&').toList.map(_.trim).map {
        case Message(id) => Some(Option(id).getOrElse("*"))
        case _           => None
      }
    }.filter(filters => filters.nonEmpty && filters.forall(_.isDefined)).map(_.flatten.distinct)

  private def report(annotation: Annotation): Unit = names(annotation).foreach { ids =>
    val tree   = annotation.tree
    val source = tree.source.content
    val start  = Iterator.iterate(tree.span.start - 1)(_ - 1).dropWhile(at => at >= 0 && source(at).isWhitespace).next()
    val written  = if (start >= 0 && source(start) == '@') start else tree.span.start
    val span     = Span(written, tree.span.end)
    val literals = ids.map(id => if (id == "*") "\"baumkuchen\"" else s"\"baumkuchen:$id\"").mkString(", ")
    // A filter naming no id, or this check's own, matches the tag of this report, so only an error escapes it. The
    // message of the others must not name their id, or they would suppress it as well.
    checks(
      Check.NowarnSuppression,
      "use @SuppressWarnings(Array(\"baumkuchen:<id>\")) instead of @nowarn; builds without baumkuchen report " +
        "@nowarn as unused under -Wunused:nowarn",
      tree.srcPos,
      List(ActionPatch(tree.sourcePos.withSpan(span), s"@SuppressWarnings(Array($literals))")),
      escalate = ids.exists(id => id == "*" || id == Check.NowarnSuppression.id)
    )
  }
}
