package net.kurobako.baumkuchen.scalafix

import metaconfig.Configured
import scala.meta.*
import scalafix.v1.*

/** Restrict implementation access through private declarations or namespaces, rather than package qualifiers.
  *
  * `NoPackagePrivate.tiers` lists packages, such as `org.example`, that span several modules; plain `private` cannot
  * express access across a module boundary, so qualifying with one of them is allowed.
  */
class NoPackagePrivate(tiers: Set[String]) extends SemanticRule("NoPackagePrivate") {
  def this() = this(Set.empty)

  override def withConfiguration(config: Configuration): Configured[Rule] = Configured.Ok(
    NoPackagePrivate(
      settings.strings(settings.section(config.conf, "NoPackagePrivate"), "tiers").map(_.replace('.', '/') + "/").toSet
    )
  )

  override def fix(using document: SemanticDocument): Patch = document.tree.collect {
    case modifier @ Mod.Private(within: Name)
        if within.value.nonEmpty && (qualifier(within.symbol) || constructorPackage(modifier)) =>
      lint(
        "package-private",
        "use plain private, no modifier, or a private namespace instead of package-qualified private",
        modifier.pos
      )
  }.asPatch

  private def qualifier(symbol: Symbol)(using SemanticDocument): Boolean =
    definitions.info(symbol).exists(_.isPackage) && !tiers(symbol.value)

  // Scala's SemanticDB may omit the qualifier occurrence on a primary constructor.
  private def constructorPackage(modifier: Mod.Private)(using SemanticDocument): Boolean =
    modifier.parent.flatMap {
      case constructor: Ctor.Primary => constructor.parent.collect {
          case owner: Defn.Class => owner.name.symbol
          case owner: Defn.Enum  => owner.name.symbol
        }
      case _ => None
    }.flatMap(definitions.info(_)).map(_.signature).exists {
      case ClassSignature(_, _, _, declarations) =>
        declarations.exists(member => member.isPrimary && member.within.exists(qualifier(_)))
      case _ => false
    }
}
