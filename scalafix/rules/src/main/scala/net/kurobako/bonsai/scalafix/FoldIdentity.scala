package net.kurobako.bonsai.scalafix

import scala.meta.*
import scala.meta.tokens.Token
import scalafix.v1.*

/** Options folded or mapped through the identity function: `opt.fold(d)(x => x)`, `opt.fold(d)(identity)`, and
  * `opt.map(x => x).getOrElse(d)` are `opt.getOrElse(d)`.
  */
class FoldIdentity extends SemanticRule("FoldIdentity") {

  override def fix(using document: SemanticDocument): Patch = document.tree.collect {
    case application: Term.Apply => fold(application) + map(application)
  }.asPatch

  private def fold(outer: Term.Apply)(using SemanticDocument): Patch = outer match {
    case Term.Apply.After_4_6_0(
          inner @ Term.Apply.After_4_6_0(select @ Term.Select(_, Term.Name("fold")), first),
          second
        )
        if first.values.size == 1 && second.values.size == 1 && identityFunction(second.values.head) &&
          fromOption(select) =>
      lint("fold-identity", "the fold returns the value itself; use getOrElse", outer.pos) +
        Patch.replaceTree(select.name, "getOrElse") +
        Patch.removeTokens(outer.tokens.filter(_.pos.start >= inner.pos.end))
    case _ => Patch.empty
  }

  private def map(outer: Term.Apply)(using SemanticDocument): Patch = outer match {
    case Term.Apply.After_4_6_0(
          orElse @ Term.Select(
            mapped @ Term.Apply.After_4_6_0(select @ Term.Select(_, Term.Name("map")), arguments),
            Term.Name("getOrElse")
          ),
          _
        )
        if arguments.values.size == 1 && identityFunction(arguments.values.head) && fromOption(select) &&
          fromOption(orElse) =>
      val dot      = mapped.tokens.takeWhile(_.pos.start < select.name.pos.start).findLast(_.is[Token.Dot])
      val trailing = outer.tokens.view.dropWhile(_.pos.start < mapped.pos.end).takeWhile(_.is[Token.Whitespace]).toList
      lint("fold-identity", "the map returns the value itself; drop it", outer.pos) +
        Patch.removeTokens(dot.toList.flatMap(dot => mapped.tokens.filter(_.pos.start >= dot.pos.start)) ++ trailing)
    case _ => Patch.empty
  }

  private def identityFunction(term: Term)(using SemanticDocument): Boolean = term match {
    case function: Term.Function =>
      function.paramClause.values match {
        case List(parameter) =>
          function.body match {
            case Term.Name(value) => value == parameter.name.value
            case _                => false
          }
        case _ => false
      }
    case Term.Block(List(function: Term.Function)) => identityFunction(function)
    case reference: (Term.Name | Term.Select)      => reference.symbol.value == "scala/Predef.identity()."
    case _                                         => false
  }

  private def fromOption(select: Term.Select)(using SemanticDocument): Boolean =
    select.symbol.value.startsWith("scala/Option#")
}
