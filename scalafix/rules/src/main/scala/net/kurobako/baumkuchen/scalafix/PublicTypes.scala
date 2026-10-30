package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Public members that leave their type to inference, which the reader of the signature cannot see.
  *
  * A public method or field is part of the API of its type, and the type it gives is what callers read. The rule names
  * the members of the build that do not write it, including the ones that override a member of a supertype.
  */
class PublicTypes extends SemanticRule("PublicTypes") {

  override def fix(using document: SemanticDocument): Patch = document.tree.collect {
    case definition: Defn.Def
        if definition.decltpe.isEmpty && definitions.public(definition.mods) &&
          definitions.reachable(definition, definition.mods) && !definitions.has[Mod.Transparent](definition.mods) =>
      lint("missing-public-type", "a public member names its type", definition.name.pos)
    case definition: Defn.Val
        if definition.decltpe.isEmpty && !definition.pats.exists(_.is[Pat.Wildcard]) &&
          definitions.public(definition.mods) && definitions.reachable(definition, definition.mods) &&
          !constant(definition) =>
      lint("missing-public-type", "a public member names its type", definition.pos)
    case definition: Defn.Var
        if definition.decltpe.isEmpty && !definition.pats.exists(_.is[Pat.Wildcard]) &&
          definitions.public(definition.mods) && definitions.reachable(definition, definition.mods) =>
      lint("missing-public-type", "a public member names its type", definition.pos)
  }.asPatch

  // an explicit type widens a constant, and replaces the precise result of a transparent inline def
  private def constant(definition: Defn.Val)(using SemanticDocument): Boolean =
    (definitions.has[Mod.Final](definition.mods) || definitions.has[Mod.Inline](definition.mods)) &&
      definition.pats.forall {
        case pattern: Pat.Var =>
          definitions.info(pattern.name.symbol).fold(literal(definition.rhs)) { information =>
            information.signature match {
              case ValueSignature(_: ConstantType) => true
              case _                               => false
            }
          }
        case _ => false
      }

  private def literal(term: Term): Boolean = term match {
    case _: Lit.Int | _: Lit.Long | _: Lit.Double | _: Lit.Float | _: Lit.Char | _: Lit.String | _: Lit.Boolean => true
    case _                                                                                                      => false
  }
}
