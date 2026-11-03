package net.kurobako.bonsai.scalafix

import scala.meta.*
import scalafix.v1.*

/** Mutable state reachable from outside its owner: mutable collections held by `object`s, which are global, and public
  * members exposing mutable collections or arrays. Members of a class nothing outside can reach, because it or an
  * enclosing definition is private or local, are not exposed.
  */
class MutableExposure extends SemanticRule("MutableExposure") {
  override def fix(using document: SemanticDocument): Patch = document.tree.collect {
    case owner: Defn.Object => definitions.members(owner.templ).collect {
        case value: Defn.Val if definitions.public(value.mods) && mutable(value.decltpe, value.pats) =>
          lint(
            "global-state",
            s"${owner.name.value} holds a mutable collection, which is global mutable state",
            value.pos
          )
      }.asPatch
    case owner: (Defn.Class | Defn.Trait)
        if ancestry.reachable(owner, owner.mods) => exposed(definitions.members(owner.templ))
  }.asPatch

  private def exposed(stats: List[Stat])(using SemanticDocument): Patch = {
    val fields = stats.flatMap {
      case value: Defn.Val    => value.pats.collect { case Pat.Var(name) => name.value }
      case variable: Defn.Var => variable.pats.collect { case Pat.Var(name) => name.value }
      case _                  => Nil
    }.toSet
    stats.collect {
      case value: Defn.Val if definitions.public(value.mods) && mutable(value.decltpe, value.pats) =>
        lint("exposed-mutable", "this public value exposes a mutable collection; expose a read-only view", value.pos)
      case method: Defn.Def
          if definitions.public(method.mods) &&
            method.decltpe.exists(tpe => mutables.contains(definitions.symbol(tpe))) &&
            returnsField(method.body, fields) =>
        lint(
          "exposed-mutable",
          "this public method returns a mutable collection; return a read-only view",
          method.name.pos
        )
    }.asPatch
  }

  private def returnsField(body: Term, fields: Set[String]): Boolean = body match {
    case Term.Name(name)                 => fields(name)
    case Term.Select(_, Term.Name(name)) => fields(name)
    case Term.Block(stats)               => stats.lastOption.exists {
        case term: Term => returnsField(term, fields)
        case _          => false
      }
    case _ => false
  }

  // Inferred types are read from the definition's own symbol, which SemanticDB records within the same file.
  private def mutable(declared: Option[Type], patterns: List[Pat])(using SemanticDocument): Boolean =
    declared.map(tpe => mutables.contains(definitions.symbol(tpe))).getOrElse(patterns.exists {
      case Pat.Var(name) => definitions.info(name.symbol).exists(_.signature match {
          case ValueSignature(TypeRef(_, symbol, _)) => mutables.contains(symbol)
          case _                                     => false
        })
      case _ => false
    })
}
