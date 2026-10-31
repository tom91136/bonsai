package net.kurobako.bonsai.scalafix

import scala.meta.*
import scala.meta.tokens.Token
import scalafix.v1.*

/** Field renames between a JSON codec and the schema documenting it: a upickle `@key` naming the field as it is already
  * named, and a `@key` rename on a type deriving a tapir `Schema` too, without the `@encodedName` that makes the schema
  * document the name on the wire.
  */
class CodecNaming extends SemanticRule("CodecNaming") {
  private val Key         = "upickle/implicits/key#"
  private val EncodedName = "sttp/tapir/Schema.annotations.encodedName#"

  override def fix(using document: SemanticDocument): Patch = document.tree.collect {
    case definition: Defn.Class if definitions.has[Mod.Case](definition.mods) =>
      val documented = schema(definition)
      definition.ctor.paramClauses.flatMap(_.values.map { parameter =>
        val keys    = renames(parameter.mods, Key)
        val encoded = renames(parameter.mods, EncodedName).map(_._2).toSet
        keys.map { (annotation, wire) =>
          if (wire == parameter.name.value)
            lint(
              "redundant-key",
              s"@key(\"$wire\") names ${parameter.name.value} as it is already named",
              annotation.pos
            ) + removal(annotation)
          else if (documented && !encoded(wire))
            lint(
              "schema-name",
              s"the codec reads and writes ${parameter.name.value} as $wire, but the schema documents " +
                s"${parameter.name.value}; add @encodedName(\"$wire\")",
              annotation.pos
            )
          else Patch.empty
        }.asPatch
      }).asPatch
  }.asPatch

  /** The annotations of `symbol` among `mods`, with the string each passes. */
  private def renames(mods: List[Mod], symbol: String)(using SemanticDocument): List[(Mod.Annot, String)] =
    mods.collect {
      case annotation @ Mod.Annot(init) if definitions.symbol(init.tpe).value == symbol =>
        init.argClauses.flatMap(_.values).collectFirst { case Lit.String(value) => annotation -> value }
    }.flatten

  private def schema(definition: Defn.Class): Boolean =
    definition.templ.derives.exists(derived => derived.syntax.split('.').last == "Schema")

  private def removal(annotation: Mod.Annot): Patch = {
    val spacing = annotation.parent.iterator.flatMap(_.tokens).dropWhile(_.start < annotation.pos.end)
      .takeWhile(_.is[Token.Whitespace]).toList
    Patch.removeTokens(annotation.tokens) + Patch.removeTokens(spacing)
  }
}
