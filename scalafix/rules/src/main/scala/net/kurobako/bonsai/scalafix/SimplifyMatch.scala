package net.kurobako.bonsai.scalafix

import scala.meta.*
import scalafix.v1.*

/** Simplifies anonymous functions matching on their single parameter to pattern matching anonymous functions. */
class SimplifyMatch extends SyntacticRule("SimplifyMatch") {
  override def fix(using document: SyntacticDocument): Patch = document.tree.collect { case function: Term.Function =>
    simplify(function)
  }.asPatch

  private def simplify(function: Term.Function): Patch = function.paramClause.values match {
    case List(Term.Param(Nil, name: Term.Name, None, _)) =>
      extractMatch(function.body) match {
        case Some(matched)
            if matched.expr.syntax == name.value && matched.mods.isEmpty &&
              !matched.casesBlock.cases.exists(_.collect {
                case use: Term.Name if use.value == name.value => use
              }.nonEmpty) =>
          val cases       = matched.casesBlock.cases.map(_.syntax).mkString("\n")
          val replacement = function.parent match {
            case Some(clause: Term.ArgClause) if clause.values == List(function) =>
              clause.parent match {
                case Some(call: Term.Apply) => Patch.replaceTree(call, s"${call.fun.syntax} {\n$cases\n}")
                case _                      => Patch.replaceTree(function, s"{\n$cases\n}")
              }
            case _ => Patch.replaceTree(function, s"{\n$cases\n}")
          }
          replacement + lint(
            "simplify-match",
            s"anonymous function with pattern match `${name.value} => ${name.value} match { ... }` can be simplified to `{ case ... }`",
            function.pos
          )
        case _ => Patch.empty
      }
    case _ => Patch.empty
  }

  private def extractMatch(body: Term): Option[Term.Match] = body match {
    case matched: Term.Match                   => Some(matched)
    case Term.Block(List(matched: Term.Match)) => Some(matched)
    case _                                     => None
  }
}
