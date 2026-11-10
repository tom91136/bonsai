package net.kurobako.bonsai.scalafix

import scala.meta.*
import scala.meta.tokens.Token
import scalafix.v1.*

/** Bans `asInstanceOf` except to Scala.js types, where a cast at a facade boundary neither checks nor throws. */
class NoAsInstanceOf extends SemanticRule("NoAsInstanceOf") {
  private def facade(tpe: Type)(using SemanticDocument): Boolean =
    definitions.lineage(List(definitions.classSymbol(tpe))).exists(_.value == "scala/scalajs/js/Any#")

  override def fix(using document: SemanticDocument): Patch = {
    val exempt = document.tree.collect {
      case Term.ApplyType.After_4_6_0(Term.Select(_, name @ Term.Name("asInstanceOf")), targets)
          if targets.values.exists(facade(_)) =>
        name.pos.start
    }.toSet
    document.tree.tokens.collect {
      case token: Token.Ident if token.value == "asInstanceOf" && !exempt(token.pos.start) =>
        val matchable = document.tokenList.trailing(token).takeWhile(!_.is[Token.RightBracket]).exists {
          case Token.Ident("Matchable") => true
          case _                        => false
        }
        if (matchable)
          lint(
            "as-instance-of-matchable",
            "asInstanceOf[Matchable] is used here to enable pattern matching on Any, use the .asMatchable extension method instead",
            token.pos
          )
        else lint("as-instance-of", "asInstanceOf casts are disabled, use pattern matching instead", token.pos)
    }.asPatch
  }
}
