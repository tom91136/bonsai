package net.kurobako.baumkuchen.scalafix

import java.util.regex.Pattern
import metaconfig.Configured
import scala.meta.*
import scala.meta.tokens.Token
import scalafix.v1.*

object ToolingComments {
  def compile(pattern: String): Pattern = Pattern.compile(pattern, Pattern.CASE_INSENSITIVE)
}

/** Comments that justify code by the tools that check it, naming a linter, a warning, or a dependency cycle. A comment
  * says why the code is the way it is for a reader; the tools say what they find. The cycle and `would otherwise`
  * wordings count only beside a word of tooling in the same comment, since loops and assemblers have cycles too. A
  * comment on a definition that suppresses a rule is where the reason belongs, and is left alone unless it only repeats
  * what the rule checks, naming a word of the rule and a repeated, restated or duplicated thing. Scalafix directives
  * are left alone.
  *
  * `ToolingComments.phrases` lists further case-insensitive regular expressions that mark such a comment, for the
  * wording a codebase falls into.
  */
class ToolingComments(extra: List[Pattern]) extends SyntacticRule("ToolingComments") {
  def this() = this(Nil)

  override def withConfiguration(config: Configuration): Configured[Rule] = Configured.Ok(
    ToolingComments(
      settings.strings(settings.section(config.conf, "ToolingComments"), "phrases").map(ToolingComments.compile(_))
    )
  )

  private val Tools    = "scalafix|scalafmt|baumkuchen|wartremover|scapegoat|linters?|lint(?:ing|ed|s)?"
  private val Verbs    = "silenc|suppress|appeas|placat|dodg|quieten"
  private val Patterns = List(
    s"\\b(?:$Tools)\\b",
    s"\\b(?:$Verbs)\\w*\\s+(?:the\\s+|a\\s+|an\\s+)?(?:\\w+\\s+)?(?:warnings?|rules?|checks?|diagnostics?)\\b"
  ).map(ToolingComments.compile(_))

  // Wordings that need a word of tooling beside them to be about a tool.
  private val Contextual = List(
    "\\b(?:dependency|import|package|circular)\\s+cycles?\\b",
    "\\b(?:cyclic|circular)\\s+(?:dependenc|import|reference)",
    "\\bbreak(?:s|ing)?\\s+(?:the\\s+|a\\s+)?cycles?\\b",
    "\\b(?:keeps?|makes?|ensures?)\\s+(?:\\w+\\s+){0,3}acyclic\\b",
    "\\bwould\\s+otherwise\\s+(?:warn|trigger|fire|be\\s+(?:flagged|reported|rejected))\\b"
  ).map(ToolingComments.compile(_))
  private val Context = ToolingComments.compile(
    "\\b(?:imports?|modules?|packages?|rules?|unused|compiler|diagnostics?|warn\\w*|lint\\w*|scalac|sbt)\\b"
  )
  private val Repeats    = ToolingComments.compile("\\b(?:repeat|restat|duplicat|echo)\\w*")
  private val Directive  = "(?:scalafix|scalafmt|baumkuchen):\\s*[a-z{]".r
  private val Word       = "[A-Za-z]+".r
  private val RuleWord   = "[A-Z]?[a-z]+".r
  private val Suppressed = "\"(?:scalafix|baumkuchen|wartremover):([A-Za-z-]+)\"".r

  override def fix(using document: SyntacticDocument): Patch = {
    val tokens = document.tokens
    tokens.zipWithIndex.collect {
      case (comment: Token.Comment, index) if Directive.findFirstIn(comment.syntax).isEmpty =>
        suppressed(tokens, index) match {
          case Some(rules) =>
            Option.when(restating(comment.syntax, rules))(
              lint(
                "tooling-comment",
                "the comment repeats what the suppressed rule checks; say why this code is the exception",
                comment.pos
              )
            )
          case None =>
            Option.when(about(comment.syntax))(
              lint(
                "tooling-comment",
                "describe what the code does and why, not the lint, rule, or cycle it answers; if the tool's " +
                  "finding is a false positive, suppress it where it occurs",
                comment.pos
              )
            )
        }
    }.flatten.asPatch
  }

  private def about(text: String): Boolean = (Patterns ++ extra).exists(_.matcher(text).find()) ||
    Context.matcher(text).find() && Contextual.exists(_.matcher(text).find())

  private def stems(words: Iterator[String]): Set[String] =
    words.map(_.toLowerCase(java.util.Locale.ROOT).stripSuffix("s")).filter(_.length >= 4).toSet

  private def restating(text: String, rules: List[String]): Boolean =
    Repeats.matcher(text).find() &&
      stems(Word.findAllIn(text)).intersect(stems(rules.iterator.flatMap(RuleWord.findAllIn(_)))).nonEmpty

  // The rules named by the annotation after a comment, past other comments and whitespace, if it suppresses.
  private def suppressed(tokens: scala.meta.tokens.Tokens, index: Int): Option[List[String]] = {
    val annotation = tokens.drop(index + 1).dropWhile {
      case _: Token.Comment | _: Token.Space | _: Token.LF | _: Token.CR | _: Token.Tab | _: Token.FF => true
      case _                                                                                          => false
    }
    annotation.take(2).toList match {
      case List(_: Token.At, name: Token.Ident) if name.value == "SuppressWarnings" =>
        val clause = annotation.drop(2).takeWhile(!_.isInstanceOf[Token.LF]).map(_.syntax).mkString
        Some(Suppressed.findAllMatchIn(clause).map(_.group(1)).toList)
      case List(_: Token.At, name: Token.Ident) if name.value == "nowarn" => Some(Nil)
      case _                                                              => None
    }
  }
}
