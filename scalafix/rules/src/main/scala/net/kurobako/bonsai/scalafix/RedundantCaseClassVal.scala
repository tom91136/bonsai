package net.kurobako.bonsai.scalafix

import scala.meta.*
import scala.meta.tokens.Token
import scalafix.v1.*

/** Case-class parameters already define public vals. */
class RedundantCaseClassVal extends SyntacticRule("RedundantCaseClassVal") {
  override def fix(using document: SyntacticDocument): Patch = document.tree.collect {
    case definition: Defn.Class if definition.mods.exists { case _: Mod.Case => true; case _ => false } =>
      definition.ctor.paramClauses.headOption.toList.flatMap(_.values).collect {
        case Term.Param(List(modifier: Mod.ValParam), _, _, _) =>
          val spacing = document.tokens.dropWhile(_.pos.start < modifier.pos.end).takeWhile {
            case _: Token.Space => true
            case _              => false
          }
          Patch.removeTokens(modifier.tokens ++ spacing) +
            lint("redundant-case-class-val", "case-class parameters already define public vals", modifier.pos)
      }.asPatch
  }.asPatch
}
