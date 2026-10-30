package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scala.meta.internal.semanticdb.Scala.*
import scala.meta.tokens.Token
import scala.util.control.NonFatal
import scalafix.v1.*

/** Requires method values to show their arity instead of relying on invisible eta expansion. */
class ExplicitEtaExpansion extends SemanticRule("ExplicitEtaExpansion") {
  override def fix(using document: SemanticDocument): Patch = {
    // Parameter refs are values, never methods needing eta expansion. Building this set once
    // avoids resolving SemanticDB signatures for the many generated constructor arguments.
    val parameterSymbols = document.tree.collect { case parameter: Term.Param => parameter.name.symbol }
      .filterNot(_ == Symbol.None)
      .toSet
    document.tree.collect {
      case reference: Term.Ref
          if isArgument(reference) &&
            !parameterSymbols(reference.symbol) &&
            !reference.symbol.value.startsWith("scala/Predef.identity") &&
            mayBeMethod(reference.symbol) =>
        explicitApplication(reference).map { application =>
          Patch.replaceTree(reference, if (bareInfixOperand(reference)) s"($application)" else application) + lint(
            "method-value",
            "write an explicit lambda or placeholder application so the method value's arity is visible",
            reference.pos
          )
        }
    }.flatten.asPatch
  }

  private def isArgument(reference: Term.Ref): Boolean = reference.parent match {
    case Some(arguments: Term.ArgClause)                                 => arguments.values.exists(_.eq(reference))
    case Some(assignment @ Term.Assign(_, value)) if value.eq(reference) =>
      assignment.parent.exists {
        case arguments: Term.ArgClause => arguments.values.exists(_.eq(assignment))
        case _                         => false
      }
    case _ => false
  }

  // a placeholder application without parentheses would scope over the whole infix expression
  private def bareInfixOperand(reference: Term.Ref): Boolean = reference.parent.exists {
    case arguments: Term.ArgClause =>
      arguments.parent.exists { case _: Term.ApplyInfix => true; case _ => false } &&
      !arguments.tokens.headOption.exists { case _: Token.LeftParen => true; case _ => false }
    case _ => false
  }

  // Resolving .info for a global term (for example, a generated enum case field
  // forwarded to its superclass) cannot identify a method needing eta expansion. In large
  // generated files, each such lookup scans SemanticDB documents, so restrict
  // lookups to method descriptors and local symbols (which may be local defs).
  private def mayBeMethod(symbol: Symbol): Boolean =
    symbol.isLocal || (symbol.isGlobal && symbol.value.desc.isMethod)

  private def explicitApplication(reference: Term.Ref)(using SemanticDocument): Option[String] =
    try
      reference.symbol.info.flatMap(info =>
        info.signature match {
          case signature: MethodSignature =>
            val parameterLists = reference match {
              case Term.Select(qualifier: Term.Ref, _)
                  if info.owner.value.endsWith(".") && qualifier.symbol != info.owner =>
                signature.parameterLists match {
                  case receiver :: rest => receiver.drop(1) :: rest
                  case Nil              => Nil
                }
              case _ => signature.parameterLists
            }
            // Contextual parameters are supplied at the reference, so they complete the call rather than await arguments.
            parameterLists.filterNot(_.exists(parameter => parameter.isImplicit || parameter.isGiven))
              .find(_.nonEmpty).map { parameters =>
                val placeholders = List.fill(parameters.size)("_").mkString(", ")
                s"${reference.syntax}($placeholders)"
              }
          case _ => None
        }
      )
    catch {
      case NonFatal(_) => None
    }
}
