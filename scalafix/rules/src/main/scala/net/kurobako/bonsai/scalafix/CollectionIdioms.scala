package net.kurobako.bonsai.scalafix

import scala.meta.*
import scala.meta.internal.semanticdb.Scala.*
import scala.meta.tokens.Token
import scala.util.control.NonFatal
import scalafix.v1.*

/** Collection rewrites with bounded arithmetic and unchanged evaluation of user expressions. */
class CollectionIdioms extends SemanticRule("CollectionIdioms") {
  override def fix(using document: SemanticDocument): Patch = document.tree.collect {
    case call: Term.Apply            => emptyCollection(call) + arrayPrint(call)
    case comparison: Term.ApplyInfix => tautology(comparison)
  }.asPatch

  private def sizeMethod(selection: Term.Select)(using SemanticDocument): Boolean = {
    val symbol = selection.symbol.value
    (symbol.startsWith("scala/collection/") && !symbol.startsWith("scala/collection/StringOps")) ||
    symbol.startsWith("scala/Array#")
  }

  private val Mirrored = Map("<" -> ">", ">" -> "<", "<=" -> ">=", ">=" -> "<=", "==" -> "==", "!=" -> "!=")

  /** The receiver, operator and literal of a comparison of a size with an integer, read with the size on the left. */
  private def sizeComparison(term: Term)(using SemanticDocument): Option[(Term, String, Int)] = term match {
    case comparison: Term.ApplyInfix =>
      (comparison.lhs, comparison.op.value, comparison.argClause.values) match {
        case (size @ Term.Select(receiver, Term.Name("size" | "length")), operator, List(Lit.Int(value)))
            if Mirrored.contains(operator) && sizeMethod(size) =>
          Some((receiver, operator, value))
        case (Lit.Int(value), operator, List(size @ Term.Select(receiver, Term.Name("size" | "length"))))
            if Mirrored.contains(operator) && sizeMethod(size) =>
          Some((receiver, Mirrored(operator), value))
        case _ => None
      }
    case _ => None
  }

  private def sizeComparison(term: Term, operator: String)(using SemanticDocument): Option[(Term, Int)] =
    sizeComparison(term).collect { case (receiver, `operator`, value) => receiver -> value }

  /** The receiver of a test for emptiness, and whether the test holds when it is empty. */
  private def emptinessTest(term: Term)(using SemanticDocument): Option[(Term, Boolean)] = term match {
    case test @ Term.Select(receiver, Term.Name(name @ ("isEmpty" | "nonEmpty"))) if sizeMethod(test) =>
      Some(receiver -> (name == "isEmpty"))
    case _ => sizeComparison(term).flatMap((receiver, operator, value) =>
        (operator, value) match {
          case ("==" | "<=", 0) | ("<", 1) => Some(receiver -> true)
          case ("!=" | ">", 0) | (">=", 1) => Some(receiver -> false)
          case _                           => None
        }
      )
  }

  private def tautology(comparison: Term.ApplyInfix)(using SemanticDocument): Patch =
    (comparison.op.value, sizeComparison(comparison)) match {
      case (_, Some((_, operator @ (">=" | "<"), 0))) if !impliedOperand(comparison) =>
        lint(
          "tautological-size-comparison",
          s"a size is never negative, so this comparison is always ${operator == ">="}",
          comparison.pos
        )
      case (operator @ ("||" | "&&"), _) if complementary(comparison, operator) =>
        lint(
          "tautological-size-comparison",
          s"a collection is either empty or not, so this ${if (operator == "||") "disjunction" else "conjunction"} " +
            s"is always ${operator == "||"}",
          comparison.pos
        )
      case ("&&", _) => impliedBound(comparison).fold(Patch.empty) { disjuncts =>
          val clause    = comparison.argClause
          val brackets  = clause.tokens.headOption.exists { case _: Token.LeftParen => true; case _ => false }
          val redundant = brackets && (disjuncts == 1 || !leadingConjunct(comparison))
          lint("tautological-size-comparison", "the size equalities imply the lower bound; remove it", comparison.pos) +
            Patch.removeTokens(comparison.tokens.takeWhile(_.start < clause.pos.start)) +
            (if (redundant) Patch.removeToken(clause.tokens.head) + Patch.removeToken(clause.tokens.last)
             else Patch.empty)
        }
      case _ => Patch.empty
    }

  private def impliedOperand(comparison: Term.ApplyInfix)(using SemanticDocument): Boolean =
    comparison.parent.exists {
      case conjunction: Term.ApplyInfix => conjunction.lhs == comparison && impliedBound(conjunction).isDefined
      case _                            => false
    }

  /** The number of disjuncts in `x.size >= n && (x.size == m || ...)` when every `m` is at least `n`. */
  private def impliedBound(conjunction: Term.ApplyInfix)(using SemanticDocument): Option[Int] =
    (conjunction.op.value, conjunction.argClause.values) match {
      case ("&&", List(rhs)) => sizeComparison(conjunction.lhs, ">=").flatMap { (receiver, lower) =>
          val equalities = operands(rhs, "||")
          Option.when(stable(receiver) && equalities.forall(sizeComparison(_, "==").exists { (other, value) =>
            value >= lower && other.structure == receiver.structure
          }))(equalities.size)
        }
      case _ => None
    }

  private def operands(term: Term, operator: String): List[Term] = term match {
    case chain: Term.ApplyInfix if chain.op.value == operator && chain.argClause.values.size == 1 =>
      operands(chain.lhs, operator) ++ operands(chain.argClause.values.head, operator)
    case other => List(other)
  }

  /** Whether a whole chain of `||` or `&&` tests one stable collection for emptiness and for the opposite. */
  private def complementary(chain: Term.ApplyInfix, operator: String)(using SemanticDocument): Boolean =
    !chain.parent.exists {
      case outer: Term.ApplyInfix => outer.op.value == operator
      case _                      => false
    } && operands(chain, operator).flatMap(operand => emptinessTest(operand).filter((receiver, _) => stable(receiver)))
      .groupBy((receiver, _) => receiver.structure).values.exists(_.map(_._2).distinct.size > 1)

  // dropping the bound drops one evaluation of the receiver
  @scala.annotation.tailrec
  private def stable(term: Term): Boolean = term match {
    case _: Term.Name | _: Term.This => true
    case Term.Select(qualifier, _)   => stable(qualifier)
    case _                           => false
  }

  // A disjunction left in place of a conjunction that leads a further `&&` or a tighter operator would regroup.
  private def leadingConjunct(conjunction: Term.ApplyInfix): Boolean = conjunction.parent.exists {
    case outer: Term.ApplyInfix =>
      outer.lhs == conjunction && outer.op.value != "||" && outer.tokens.head.start == conjunction.pos.start
    case _ => false
  }

  // An array printed directly renders its identity, not its elements.
  private def arrayPrint(call: Term.Apply)(using SemanticDocument): Patch = call.fun match {
    case print @ Term.Name("println" | "print")
        if printing(print) && call.argClause.values.exists(definitions.arrayType(_)) =>
      lint("array-to-string", "printing an array renders its identity; use mkString or print its elements", call.pos)
    case _ => Patch.empty
  }

  private def printing(name: Term.Name)(using SemanticDocument): Boolean = {
    val symbol = name.symbol.value
    symbol.startsWith("scala/Predef.print") || symbol.startsWith("scala/Console.print")
  }

  // An extractor pattern such as `case Seq() =>` is not an application, so only constructions are rewritten.
  private def emptyCollection(call: Term.Apply)(using SemanticDocument): Patch =
    if (call.argClause.values.nonEmpty) Patch.empty
    else {
      val companion = call.fun match {
        case application: Term.ApplyType => application.fun
        case other                       => other
      }
      companion match {
        case reference: (Term.Name | Term.Select) if hasEmpty(reference.symbol) =>
          lint("empty-collection", "use empty for an empty collection", call.pos) +
            Patch.addLeft(call.tokens(companion.tokens.length), ".empty") + Patch.removeTokens(call.argClause.tokens)
        case _ => Patch.empty
      }
    }

  // the symbol table has no information on Scala classes of the classpath, so those the rules run on stand in for them
  private def hasEmpty(companion: Symbol)(using SemanticDocument): Boolean =
    companion.isGlobal && companion.value.endsWith(".") &&
      (if (definitions.info(companion).isDefined) objectOf(companion).exists(definitions.declares(_, "empty"))
       else runtimeObject(companion.value).exists(_.getMethods.exists(_.getName == "empty")))

  // a companion reached through a value such as `val Vector = scala.collection.immutable.Vector` is its object
  @scala.annotation.tailrec
  private def objectOf(symbol: Symbol)(using SemanticDocument): Option[Symbol] =
    definitions.info(symbol).map(_.signature) match {
      case Some(_: ClassSignature)                => Some(symbol)
      case Some(ValueSignature(SingleType(_, s))) => objectOf(s)
      case _                                      => None
    }

  private def runtimeObject(symbol: String): Option[Class[?]] =
    load(binaryName(symbol) + "$").orElse(
      Option.when(!symbol.owner.isPackage)(symbol.owner).flatMap(owner => load(binaryName(owner) + "$")).flatMap(
        _.getMethods.find(method => method.getName == symbol.desc.value && method.getParameterCount == 0)
          .map(_.getReturnType)
      )
    )

  private def binaryName(symbol: String): String =
    if (symbol.owner.isPackage) symbol.owner.stripSuffix("/").replace('/', '.') + "." + symbol.desc.value
    else binaryName(symbol.owner) + "$" + symbol.desc.value

  private def load(name: String): Option[Class[?]] =
    try Some(Class.forName(name, false, getClass.getClassLoader))
    catch {
      case NonFatal(_) | _: LinkageError => None
    }
}
