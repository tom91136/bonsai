package net.kurobako.baumkuchen.scalafix

import java.util.regex.Pattern
import scala.meta.*
import scala.meta.tokens.Token
import scalafix.lint.LintSeverity
import scalafix.v1.*

/** Comments worded for the moment they were written, such as `not ... yet`, `for now`, `at the moment`, `currently`,
  * and `these days`. They go stale without a change to the code beside them; a comment says what the code does and why.
  * Comments with a `TODO`, `FIXME`, or `XXX` marker are left alone, since a marker is where a time-bound note belongs,
  * as are scalafix directives and wordings that mean something else, such as `at the moment the lock is released` and
  * `the currently selected item`. `No longer` and `anymore` are left out, since they describe a change of state at run
  * time as often as a change of the code. Reports at warning severity.
  */
class TimeRelativeComments extends SyntacticRule("TimeRelativeComments") {
  private def compile(pattern: String): Pattern = Pattern.compile(pattern, Pattern.CASE_INSENSITIVE)

  private val Patterns = List(
    "\\b(?:not|n't)\\b[^.;:!?]{0,40}\\byet\\b",
    "\\bfor\\s+now\\b",
    "\\bat\\s+the\\s+moment\\b(?!\\s+(?:of|when|that|which|where|the|a|an|each|every|all|any)\\b)",
    "\\b(?:is|are|was|were|does|do)(?:\\s+not|n't)?\\s+currently\\b",
    "(?:^|[.!?]\\s+|//\\s*|/\\*+\\s*)currently\\b",
    "\\bcurrently\\s+(?:unsupported|unused|disabled|ignored|only|not|no)\\b",
    "\\bthese\\s+days\\b"
  ).map(compile(_))
  private val Marked    = compile("\\b(?:TODO|FIXME|XXX)\\b")
  private val Directive = "(?:scalafix|scalafmt|baumkuchen):\\s*[a-z{]".r

  override def fix(using document: SyntacticDocument): Patch = document.tokens.collect {
    case comment: Token.Comment
        if Directive.findFirstIn(comment.syntax).isEmpty && !Marked.matcher(comment.syntax).find() &&
          Patterns.exists(_.matcher(comment.syntax).find()) =>
      Patch.lint(
        Diagnostic(
          "time-relative-comment",
          "the comment states a point in time that the code outlives; say what the code does and why",
          comment.pos,
          "",
          LintSeverity.Warning
        )
      )
  }.asPatch
}
