package net.kurobako.bonsai.scalafix

import scala.meta.*
import scala.meta.tokens.Token
import scalafix.v1.*

/** Syntax and member bans. Allocation exemptions read the symbol information of the constructed type. */
class BannedSyntax extends SemanticRule("BannedSyntax") {
  private val messages = Map(
    "explicit-new"         -> "use Scala 3 creator application syntax instead of explicit new",
    "explicit-new-array"   -> "use Array.ofDim[T](n)",
    "anonymous-allocation" -> "replace the anonymous allocation with a named implementation or a function value",
    "intrinsic-lock"       -> "use an explicit java.util.concurrent lock",
    "no-iterator"          -> "use .view for lazy collection traversal or adapt the Java collection boundary",
    "product-traversal"    ->
      "match the domain's typed cases explicitly; reserve Product traversal for generic derivation boundaries",
    "final-class" -> "remove final from the class declaration; use open when extension is intended",
    "no-then"     -> "omit `then`; use parentheses or braces for conditionals"
  )

  private val iteratorMembers  = Set("iterator", "keysIterator", "valuesIterator")
  private val advancingMembers = Set("next", "hasNext")
  private val productTypes     = Set("Product", "scala.Product", "_root_.scala.Product")
  private val productMembers   =
    Set("productIterator", "productElement", "productElementNames", "productElementName", "productArity")

  private def ban(id: String, position: Position): Patch = lint(id, messages(id), position)

  private def lineage(roots: List[Symbol])(using SemanticDocument): Set[Symbol] = {
    @annotation.tailrec
    def visit(frontier: List[Symbol], seen: Set[Symbol]): Set[Symbol] = frontier match {
      case Nil                            => seen
      case symbol :: rest if seen(symbol) => visit(rest, seen)
      case symbol :: rest                 =>
        val supertypes = definitions.info(symbol).map(_.signature).collect { case ClassSignature(_, parents, _, _) =>
          parents.collect { case TypeRef(_, parent, _) => parent }
        }
        visit(supertypes.getOrElse(Nil) ::: rest, seen + symbol)
    }
    visit(roots, Set.empty)
  }

  private def definesApply(companion: Symbol)(using SemanticDocument): Boolean =
    lineage(List(companion)).exists(symbol =>
      definitions.info(symbol).map(_.signature).exists {
        case ClassSignature(_, _, _, declarations) => declarations.exists(_.displayName == "apply")
        case _                                     => false
      }
    )

  // SemanticDB records no occurrence for the type after new when its type argument is inferred.
  private def constructed(term: Term.New)(using SemanticDocument): Symbol = {
    val symbol = definitions.symbol(term.init.tpe)
    if (!symbol.isNone) symbol
    else
      ancestry.ancestors(term).collectFirst {
        case owner: Defn.Object if owner.name.value == term.init.tpe.syntax =>
          Symbol(owner.name.symbol.value.stripSuffix(".") + "#")
      }.getOrElse(symbol)
  }

  // A case class's synthetic apply takes the constructor's arguments, so outside its companion creator application
  // reaches the same code, while inside a companion with a custom apply only new reaches the constructor.
  private def constructs(term: Term.New)(using SemanticDocument): Boolean = {
    val created   = constructed(term)
    val companion = Symbol(created.value.stripSuffix("#") + ".")
    val reachable = !definitions.info(created).exists(_.isCase) || ancestry.ancestors(term).exists {
      case owner: Defn.Object => owner.name.symbol == companion
      case _                  => false
    }
    reachable && definesApply(companion)
  }

  private def scalaJsAny(term: Term.NewAnonymous)(using SemanticDocument): Boolean =
    lineage(term.templ.inits.map(init => definitions.symbol(init.tpe))).exists(_.value == "scala/scalajs/js/Any#")

  private def arrayAllocation(term: Term.New): Boolean = term.init.tpe match {
    case Type.Apply.After_4_6_0(Type.Name("Array"), _) => true
    case _                                             => false
  }

  // A cursor advanced in its scope, or buffered, has no view equivalent.
  private def stateful(selection: Term.Select): Boolean = {
    val iterator = selection.parent match {
      case Some(call @ Term.Apply.After_4_6_0(`selection`, _)) => call
      case _                                                   => selection
    }
    iterator.parent match {
      case Some(Term.Select(qualifier, Term.Name("buffered"))) if qualifier eq iterator => true
      case Some(definition: Defn.Val) if definition.rhs eq iterator                     => bound(definition)
      case Some(definition: Defn.Var) if definition.body eq iterator                    => bound(definition)
      case _                                                                            => false
    }
  }

  private def bound(definition: Defn.Val | Defn.Var): Boolean = definition.pats match {
    case List(Pat.Var(Term.Name(name))) =>
      definition.parent.exists(_.collect {
        case Term.Select(Term.Name(`name`), Term.Name(member)) if advancingMembers(member) => member
      }.nonEmpty)
    case _ => false
  }

  override def fix(using document: SemanticDocument): Patch = {
    val trees = document.tree.collect {
      case term: Term.New if arrayAllocation(term)      => ban("explicit-new-array", term.pos)
      case term: Term.New if !constructs(term)          => ban("explicit-new", term.pos)
      case term: Term.NewAnonymous if !scalaJsAny(term) => ban("anonymous-allocation", term.pos)
      case Term.Apply.After_4_6_0(select @ Term.Select(_, Term.Name("synchronized")), _) =>
        ban("intrinsic-lock", select.name.pos)
      case Term.Apply.After_4_6_0(name @ Term.Name("synchronized"), _) =>
        ban("intrinsic-lock", name.pos)
      case selection @ Term.Select(_, Term.Name(name)) if iteratorMembers(name) && !stateful(selection) =>
        ban("no-iterator", selection.pos)
      case selection @ Term.Select(_, Term.Name(name)) if productMembers(name) =>
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
