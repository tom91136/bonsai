package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Keep companion objects before their types. A type alias defined through the object of its name, as in
  * `type Id = Id.Type`, is no companion: the object defines it, so either may come first.
  */
class CompanionOrder extends SyntacticRule("CompanionOrder") {
  override def fix(using document: SyntacticDocument): Patch = document.tree.collect {
    case source: Source     => check(source.stats)
    case pkg: Pkg           => check(pkg.body.stats)
    case template: Template => check(template.body.stats)
    case block: Term.Block  => check(block.stats)
  }.asPatch

  private def check(stats: List[Stat]): Patch = {
    val types = stats.collect {
      case definition: Defn.Class                        => definition.name.value -> definition
      case definition: Defn.Trait                        => definition.name.value -> definition
      case definition: Defn.Enum                         => definition.name.value -> definition
      case definition: Defn.Type if !through(definition) => definition.name.value -> definition
    }.toMap
    stats.collect {
      case companion: Defn.Object if types.get(companion.name.value).exists(_.pos.start < companion.pos.start) =>
        lint(
          "companion-order",
          s"place object ${companion.name.value} before its companion type",
          companion.name.pos
        )
    }.asPatch
  }

  private def through(alias: Defn.Type): Boolean = alias.body.collect {
    case Type.Select(Term.Name(owner), _) if owner == alias.name.value => ()
  }.nonEmpty
}
