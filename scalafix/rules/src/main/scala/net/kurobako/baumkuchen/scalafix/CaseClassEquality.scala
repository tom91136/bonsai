package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Case class fields whose types compare by reference or change after construction, so `equals` and `hashCode` stop
  * describing the value.
  */
class CaseClassEquality extends SemanticRule("CaseClassEquality") {
  override def fix(using document: SemanticDocument): Patch = document.tree.collect {
    case definition: Defn.Class if definitions.has[Mod.Case](definition.mods) =>
      definition.ctor.paramClauses.headOption.toList.flatMap(_.values).flatMap { parameter =>
        parameter.decltpe.flatMap { tpe =>
          val symbol = definitions.symbol(tpe)
          val reason =
            if (symbol.value == "scala/Array#") Some("arrays compare by reference")
            else if (definitions.function(tpe)) Some("functions compare by reference")
            else Option.when(definitions.mutableType(symbol) || definitions.has[Mod.VarParam](parameter.mods))(
              "mutable fields change the value after construction"
            )
          reason.map(why =>
            lint(
              "unstable-equality",
              s"${parameter.name.value}: $why, so ${definition.name.value} has unstable equality",
              parameter.pos
            )
          )
        }
      }.asPatch
  }.asPatch
}
