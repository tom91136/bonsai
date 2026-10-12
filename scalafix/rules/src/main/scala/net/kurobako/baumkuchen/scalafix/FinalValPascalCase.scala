package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Final values use PascalCase, including acronym and numeric segments. */
class FinalValPascalCase extends SyntacticRule("FinalValPascalCase") {
  override def fix(using document: SyntacticDocument): Patch = document.tree.collect {
    case definition: Defn.Val if definition.mods.exists(_.isInstanceOf[Mod.Final]) =>
      definition.pats.flatMap(_.collect { case Pat.Var(name) => check(name) }).asPatch
    case parameter: Term.Param
        if parameter.mods.exists(_.isInstanceOf[Mod.Final]) &&
          parameter.mods.exists(_.isInstanceOf[Mod.ValParam]) => check(parameter.name)
  }.asPatch

  private def check(name: Name): Patch =
    if (name.value.headOption.exists(_.isUpper) && name.value.forall(_.isLetterOrDigit)) Patch.empty
    else lint("final-val-name", "name final vals in PascalCase, for example MaximumBytes or HTTP2Port", name.pos)
}
