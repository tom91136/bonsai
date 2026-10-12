package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Fields that start uninitialised, which means some method must run before others may use them. */
class TemporalCoupling extends SemanticRule("TemporalCoupling") {
  override def fix(using document: SemanticDocument): Patch = document.tree.collect {
    case template: Template => definitions.members(template).collect {
        case variable: Defn.Var if uninitialised(variable.body) && !variable.decltpe.exists(nullable(_)) =>
          lint(
            "uninitialised",
            "this field starts uninitialised, so callers must know the initialisation order; construct it complete",
            variable.pos
          )
      }.asPatch
  }.asPatch

  // A declared `| Null` makes absence part of the type, which callers must already handle.
  private def nullable(tpe: Type): Boolean = tpe match {
    case Type.ApplyInfix(left, Type.Name("|"), right) => nullable(left) || nullable(right)
    case Type.Name("Null")                            => true
    case _                                            => false
  }

  private def uninitialised(body: Term)(using SemanticDocument): Boolean = body match {
    case _: Term.Placeholder | Lit.Null() => true
    case reference => reference.symbol.value == "scala/compiletime/package$package.uninitialized()."
  }
}
