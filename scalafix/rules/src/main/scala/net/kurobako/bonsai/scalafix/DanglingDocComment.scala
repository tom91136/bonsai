package net.kurobako.bonsai.scalafix

import scala.meta.*
import scala.meta.tokens.Token
import scalafix.v1.*

/** Doc comments that document nothing, because another doc comment, a closing brace, or the end of the file follows
  * them. Plain comments between the two are skipped, as they document nothing either. The orphan is deleted.
  */
class DanglingDocComment extends SyntacticRule("DanglingDocComment") {

  override def fix(using document: SyntacticDocument): Patch = {
    val tokens = document.tokens.toVector
    tokens.indices.collect {
      case index if documentation(tokens(index)) && orphaned(tokens, index) =>
        lint("dangling-doc-comment", "the doc comment documents nothing", tokens(index).pos) +
          lines.remove(tokens, index, index)
    }.asPatch
  }

  private def documentation(token: Token): Boolean = token.is[Token.Comment] && {
    val text = token.syntax
    text.startsWith("/**") && text.length > "/**/".length
  }

  private def orphaned(tokens: Vector[Token], index: Int): Boolean =
    tokens.drop(index + 1).find(token =>
      !token.is[Token.Whitespace] && !(token.is[Token.Comment] && !documentation(token))
    ) match {
      case Some(next) => documentation(next) || next.is[Token.RightBrace] || next.is[Token.EOF]
      case None       => true
    }
}
