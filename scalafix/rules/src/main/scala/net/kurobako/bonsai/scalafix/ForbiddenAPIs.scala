package net.kurobako.bonsai.scalafix

import metaconfig.Configured
import scala.meta.*
import scalafix.v1.*

/** Configured API bans, matched by resolved SemanticDB symbols rather than source spellings.
  *
  * `ForbiddenAPIs.symbols` maps a symbol to the message shown at its uses. A class or object symbol (ending in `#` or
  * `.`) matches exactly, and a class symbol also matches the static members of the class; anything else, such as
  * `java/lang/String#toUpperCase(`, matches every overload it prefixes.
  */
class ForbiddenAPIs(banned: Map[String, String]) extends SemanticRule("ForbiddenAPIs") {
  def this() = this(Map.empty)

  override def withConfiguration(config: Configuration): Configured[Rule] =
    Configured.Ok(ForbiddenAPIs(settings.table(settings.section(config.conf, "ForbiddenAPIs"), "symbols")))

  private val (types, members) = banned.partition((symbol, _) => symbol.endsWith("#") || symbol.endsWith("."))

  override def fix(using document: SemanticDocument): Patch =
    if (banned.isEmpty) Patch.empty
    else document.tree.collect {
      case reference: Term.Ref => diagnostic(reference, reference.symbol.value)
      case reference: Type.Ref => diagnostic(reference, reference.symbol.value)
    }.flatten.asPatch

  private def diagnostic(tree: Tree, symbol: String)(using SemanticDocument): Option[Patch] =
    types.get(symbol)
      .orElse(
        members.collectFirst {
          case (prefix, message) if symbol.startsWith(prefix) && !references.isSelectionName(tree) => message
        }
      )
      .orElse(
        types.collectFirst {
          case (owner, message)
              if owner.endsWith("#") && symbol.startsWith(owner) && static(tree) && !references.isSelectionName(tree) =>
            message
        }
      )
      .map(lint("forbidden-api", _, tree.pos))

  private def static(tree: Tree)(using SemanticDocument): Boolean =
    definitions.info(tree.symbol).exists(_.isStatic)
}
