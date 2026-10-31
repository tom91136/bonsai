package net.kurobako.bonsai.scalafix

import scala.meta.*
import scalafix.v1.*

/** An annotation applied twice with the same arguments to one definition or parameter, whichever way its name is spelt.
  * The later occurrence is deleted. Annotations with different arguments are left alone, as the repeatable ones are
  * written that way.
  */
class DuplicateAnnotation extends SemanticRule("DuplicateAnnotation") {

  override def fix(using document: SemanticDocument): Patch = {
    val tokens  = document.tokens.toVector
    val indices = tokens.zipWithIndex.map((token, index) => token.pos.start -> index).toMap
    document.tree.collect { case tree =>
      val annotations = modifiers(tree).collect { case annotation: Mod.Annot => annotation }
      annotations.foldLeft((Set.empty[(String, Seq[String])], Patch.empty)) { case ((seen, patch), annotation) =>
        val key = (name(annotation.init.tpe), annotation.init.argClauses.map(_.syntax))
        if (seen(key)) {
          val first = indices(annotation.tokens.head.pos.start)
          val last  = indices(annotation.tokens.last.pos.start)
          (
            seen,
            patch + lint("duplicate-annotation", "the annotation is already applied", annotation.pos) +
              lines.remove(tokens, first, last)
          )
        } else (seen + key, patch)
      }._2
    }.asPatch
  }

  private def modifiers(tree: Tree): List[Mod] = tree match {
    case definition: Stat.WithMods => definition.mods
    case parameter: Term.Param     => parameter.mods
    case parameter: Type.Param     => parameter.mods
    case _                         => Nil
  }

  private def name(tpe: Type)(using SemanticDocument): String = {
    val symbol = definitions.symbol(tpe)
    if (symbol == Symbol.None) tpe.syntax else symbol.value
  }
}
