package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scala.meta.tokens.Token
import scalafix.v1.*

/** Syntax and member bans that need no semantic information. */
class BannedSyntax extends SyntacticRule("BannedSyntax") {
  private val messages = Map(
    "explicit-new"         -> "use Scala 3 creator application syntax instead of explicit new",
    "anonymous-allocation" -> "replace the anonymous allocation with a named implementation or a function value",
    "intrinsic-lock"       -> "use an explicit java.util.concurrent lock",
    "no-iterator"          -> "use .view for lazy collection traversal or adapt the Java collection boundary",
    "product-traversal"    ->
      "match the domain's typed cases explicitly; reserve Product traversal for generic derivation boundaries",
    "final-class" -> "remove final from the class declaration; use open when extension is intended",
    "no-then"     -> "omit `then`; use parentheses or braces for conditionals"
  )

  private val iteratorMembers = Set("iterator", "keysIterator", "valuesIterator")
  private val productTypes    = Set("Product", "scala.Product", "_root_.scala.Product")
  private val productMembers  =
    Set("productIterator", "productElement", "productElementNames", "productElementName", "productArity")

  private def ban(id: String, position: Position): Patch = lint(id, messages(id), position)

  // Inside a companion that defines apply, creator application calls that apply, so only new reaches the constructor.
  private def construction(term: Term.New): Boolean = {
    val created = term.init.tpe match {
      case application: Type.Apply => Some(application.tpe).collect { case Type.Name(name) => name }
      case Type.Name(name)         => Some(name)
      case _                       => None
    }
    created.exists(name =>
      definitions.ancestors(term).exists {
        case owner: Defn.Object => owner.name.value == name && definitions.members(owner.templ).exists {
            case method: Defn.Def => method.name.value == "apply"
            case _                => false
          }
        case _ => false
      }
    )
  }

  override def fix(using document: SyntacticDocument): Patch = {
    val trees = document.tree.collect {
      case term: Term.New if !construction(term) => ban("explicit-new", term.pos)
      case term: Term.NewAnonymous               => ban("anonymous-allocation", term.pos)
      case Term.Apply.After_4_6_0(select @ Term.Select(_, Term.Name("synchronized")), _) =>
        ban("intrinsic-lock", select.name.pos)
      case Term.Apply.After_4_6_0(name @ Term.Name("synchronized"), _) =>
        ban("intrinsic-lock", name.pos)
      case selection @ Term.Select(_, Term.Name(name)) if iteratorMembers(name) => ban("no-iterator", selection.pos)
      case selection @ Term.Select(_, Term.Name(name)) if productMembers(name)  =>
        ban("product-traversal", selection.pos)
      case pattern @ Pat.Typed(_, tpe) if productTypes(tpe.syntax) =>
        ban("product-traversal", pattern.pos)
      case definition: Defn.Class =>
        definition.mods.collect { case modifier: Mod.Final => ban("final-class", modifier.pos) }.asPatch
    }
    val keywords = document.tree.tokens.collect { case keyword: Token.KwThen => ban("no-then", keyword.pos) }
    (trees ++ keywords).asPatch
  }
}
