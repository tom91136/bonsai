package net.kurobako.bonsai.scalafix

import scala.meta.*
import scalafix.v1.*

/** Opaque types whose scope wraps and unwraps them without checks, which makes them an alias with extra steps. */
class TransparentOpaque extends SyntacticRule("TransparentOpaque") {
  override def fix(using document: SyntacticDocument): Patch = document.tree.collect {
    case template: Template => check(definitions.members(template))
    case pkg: Pkg           => check(pkg.body.stats)
  }.asPatch

  private def check(stats: List[Stat]): Patch = {
    val methods = stats.flatMap {
      case method: Defn.Def   => List(method)
      case owner: Defn.Object => definitions.members(owner.templ).collect { case method: Defn.Def => method }
      case _                  => Nil
    }
    val forwarded = methods.filter(forwards(_)).view.flatMap(_.decltpe).map(_.toString).toSet
    val unwrapped = stats.flatMap {
      case extension: Defn.ExtensionGroup => unwrapping(extension)
      case _                              => None
    }.toSet
    stats.collect {
      case alias: Defn.Type if definitions.has[Mod.Opaque](alias.mods) =>
        val name    = alias.name.value
        val wraps   = forwarded(name)
        val unwraps = forwarded.size > (if (wraps) 1 else 0) || unwrapped(name)
        Option.when(wraps && unwraps)(
          lint(
            "transparent-opaque",
            s"$name converts to and from its representation unchecked, so it hides nothing",
            alias.name.pos
          )
        ).asPatch
    }.asPatch
  }

  private def forwards(method: Defn.Def): Boolean = (definitions.parameters(method), method.body) match {
    case (List(parameter), Term.Name(name)) => name == parameter.name.value
    case _                                  => false
  }

  private def unwrapping(extension: Defn.ExtensionGroup): Option[String] =
    extension.paramClauseGroup.toList.flatMap(_.paramClauses.flatMap(_.values)) match {
      case List(receiver) =>
        val bodies = extension.body match {
          case method: Defn.Def  => List(method)
          case Term.Block(stats) => stats.collect { case method: Defn.Def => method }
          case _                 => Nil
        }
        receiver.decltpe.map(_.toString).filter(_ =>
          bodies.exists(method =>
            definitions.parameters(method).isEmpty &&
              (method.body match {
                case Term.Name(value) => value == receiver.name.value
                case _                => false
              })
          )
        )
      case _ => None
    }
}
