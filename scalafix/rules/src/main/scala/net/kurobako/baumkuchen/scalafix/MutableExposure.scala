package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Mutable state reachable from outside its owner: public `var`s, mutable state in `object`s, which is global, and
  * public members exposing mutable collections or arrays. Members of a class nothing outside can reach, because it or
  * an enclosing definition is private or local, are not exposed.
  */
class MutableExposure extends SemanticRule("MutableExposure") {
  override def fix(using document: SemanticDocument): Patch = document.tree.collect {
    case owner: Defn.Object => definitions.members(owner.templ).collect {
        case variable: Defn.Var =>
          lint("global-state", s"${owner.name.value} holds a var, which is global mutable state", variable.pos)
        case value: Defn.Val if definitions.public(value.mods) && mutable(value.decltpe, value.pats) =>
          lint(
            "global-state",
            s"${owner.name.value} holds a mutable collection, which is global mutable state",
            value.pos
          )
      }.asPatch
    case owner: Defn.Class if definitions.reachable(owner, owner.mods) => exposed(definitions.members(owner.templ))
    case owner: Defn.Trait if definitions.reachable(owner, owner.mods) => exposed(definitions.members(owner.templ))
  }.asPatch

  private def exposed(stats: List[Stat])(using SemanticDocument): Patch = stats.collect {
    case variable: Defn.Var if definitions.public(variable.mods) =>
      lint("public-var", "a public var lets any caller change this state; expose an operation instead", variable.pos)
    case value: Defn.Val if definitions.public(value.mods) && mutable(value.decltpe, value.pats) =>
      lint("exposed-mutable", "this public value exposes a mutable collection; expose a read-only view", value.pos)
    case method: Defn.Def
        if definitions.public(method.mods) &&
          method.decltpe.exists(tpe => definitions.mutableType(definitions.symbol(tpe))) =>
      lint(
        "exposed-mutable",
        "this public method returns a mutable collection; return a read-only view",
        method.name.pos
      )
  }.asPatch

  // Inferred types are read from the definition's own symbol, which SemanticDB records within the same file.
  private def mutable(declared: Option[Type], patterns: List[Pat])(using SemanticDocument): Boolean =
    declared.map(tpe => definitions.mutableType(definitions.symbol(tpe))).getOrElse(patterns.exists {
      case Pat.Var(name) => name.symbol.info.exists(_.signature match {
          case ValueSignature(TypeRef(_, symbol, _)) => definitions.mutableType(symbol)
          case _                                     => false
        })
      case _ => false
    })
}
