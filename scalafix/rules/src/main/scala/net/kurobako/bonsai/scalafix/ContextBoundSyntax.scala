package net.kurobako.bonsai.scalafix

import scala.meta.*
import scala.meta.tokens.Token
import scalafix.v1.*

/** Several context bounds of one type parameter chained as `A: X: Y` rather than grouped as `A: {X, Y}`, which Scala
  * 3.6 introduced and which later versions require.
  */
class ContextBoundSyntax extends SyntacticRule("ContextBoundSyntax") {
  override def fix(using document: SyntacticDocument): Patch = document.tree.collect {
    case parameter: Type.Param if parameter.bounds.context.size > 1 && !grouped(parameter) =>
      val first    = parameter.bounds.context.head.pos.start
      val last     = parameter.bounds.context.last.pos.end
      val chain    = parameter.tokens.filter(token => token.start >= first && token.end <= last)
      val grouping = parameter.bounds.context.map(_.syntax).mkString("{", ", ", "}")
      Patch.removeTokens(chain) + Patch.addLeft(chain.head, grouping) + lint(
        "chained-bounds",
        s"group the context bounds of ${parameter.name.value} as : $grouping",
        parameter.pos
      )
  }.asPatch

  // Grouped bounds open with a brace before the first of them.
  private def grouped(parameter: Type.Param): Boolean =
    parameter.tokens.takeWhile(_.end <= parameter.bounds.context.head.pos.start).reverseIterator
      .find(token => !token.is[Token.Whitespace]).exists(_.is[Token.LeftBrace])
}
