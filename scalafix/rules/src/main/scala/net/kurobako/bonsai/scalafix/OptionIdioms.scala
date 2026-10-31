package net.kurobako.bonsai.scalafix

import scala.meta.*
import scalafix.v1.*

/** Rewrites of Option plumbing written as conditionals: emulated headOption and lastOption, bounds checks that are a
  * lift, and ifs that are a filter of an Option. The calls and comparisons that have a shorter form are left to the
  * laws of collection chains, which the compiler plugin applies.
  */
class OptionIdioms extends SemanticRule("OptionIdioms") {

  override def fix(using document: SemanticDocument): Patch = document.tree.collect {
    case conditional: Term.If => conditional_(conditional)
  }.asPatch

  /** Emulations of headOption, lastOption and lift, and if statements that are a filter of an Option. */
  private def conditional_(tree: Term.If)(using SemanticDocument): Patch =
    emptyOrLast(tree) + boundsToLift(tree) + simulatedFilter(tree) + filteredOption(tree)

  private def emptyOrLast(tree: Term.If)(using SemanticDocument): Patch = tree.cond match {
    case check @ Term.Select(receiver, Term.Name(emptiness @ ("nonEmpty" | "isEmpty")))
        if libraryMethod(check) && sequenceType(receiver) =>
      val (present, absent) = if (emptiness == "nonEmpty") (tree.thenp, tree.elsep) else (tree.elsep, tree.thenp)
      present match {
        case SomeOf(Term.Select(inner, Term.Name(tail @ ("head" | "last"))))
            if inner.syntax == receiver.syntax && isNone(absent) =>
          lint.replace(
            tree,
            s"${receiver.syntax}.${tail}Option",
            "head-or-last-option",
            s"use ${tail}Option for an emptiness check and $tail"
          )
        case _ => Patch.empty
      }
    case _ => Patch.empty
  }

  /** `if (i < xs.length) Some(xs(i)) else None` is `xs.lift(i)`. */
  private def boundsToLift(tree: Term.If)(using SemanticDocument): Patch = tree.cond match {
    case comparison: Term.ApplyInfix if comparison.op.value == "<" =>
      (comparison.argClause.values, tree.thenp) match {
        case (List(length @ Term.Select(receiver, Term.Name("length"))), SomeOf(indexed: Term.Apply))
            if indexed.fun.syntax == receiver.syntax &&
              indexed.argClause.values.map(_.syntax) == List(comparison.lhs.syntax) && libraryMethod(length) &&
              sequenceType(receiver) && isNone(tree.elsep) =>
          lint.replace(
            tree,
            s"${receiver.syntax}.lift(${comparison.lhs.syntax})",
            "bounds-to-lift",
            "use lift for an index bounds check"
          )
        case _ => Patch.empty
      }
    case _ => Patch.empty
  }

  /** `if (opt.isDefined && p(opt.get)) opt else None` is `opt.filter(p)`. */
  private def simulatedFilter(tree: Term.If)(using SemanticDocument): Patch = tree.cond match {
    case condition: Term.ApplyInfix if condition.op.value == "&&" && condition.argClause.values.size == 1 =>
      (condition.lhs, condition.argClause.values.head) match {
        case (defined @ Term.Select(option, Term.Name("isDefined")), applied: Term.Apply)
            if libraryMethod(defined) && optionType(option) && tree.thenp.syntax == option.syntax &&
              isNone(tree.elsep) =>
          (applied.fun, applied.argClause.values) match {
            case (predicate, List(Term.Select(inner, Term.Name("get")))) if inner.syntax == option.syntax =>
              lint.replace(
                tree,
                s"${option.syntax}.filter(${functionArgument(predicate)})",
                "simulated-filter",
                "use filter for an isDefined check and get"
              )
            case _ => Patch.empty
          }
        case _ => Patch.empty
      }
    case _ => Patch.empty
  }

  private def functionArgument(predicate: Term)(using SemanticDocument): String =
    if (definitions.methodReference(predicate)) s"${predicate.syntax}(_)" else predicate.syntax

  /** `if (p(v)) Some(v) else None` is `Some(v).filter(p)` for a stable `v`. */
  private def filteredOption(tree: Term.If)(using SemanticDocument): Patch = tree.cond match {
    case condition: Term.Apply if condition.argClause.values.size == 1 && isNone(tree.elsep) =>
      (condition.argClause.values.head, tree.thenp) match {
        case (name: Term.Name, SomeOf(value: Term.Name)) if value.value == name.value =>
          lint.replace(
            tree,
            s"Some(${value.syntax}).filter(${functionArgument(condition.fun)})",
            "if-else-to-filtered-option",
            "use filter for an if that wraps one value"
          )
        case _ => Patch.empty
      }
    case _ => Patch.empty
  }

  /** The value of `Some(value)`. */
  private object SomeOf {
    def unapply(tree: Term)(using SemanticDocument): Option[Term] = tree match {
      case application: Term.Apply
          if application.fun.symbol.value == "scala/Some." && application.argClause.values.size == 1 =>
        Some(application.argClause.values.head)
      case _ => None
    }
  }

  private def libraryMethod(selection: Term.Select)(using SemanticDocument): Boolean = {
    val symbol = selection.symbol.value
    List("scala/collection/", "scala/Option#", "scala/PartialFunction#").exists(symbol.startsWith(_))
  }

  private def isNone(tree: Term)(using SemanticDocument): Boolean = tree match {
    case Term.Name("None") | Term.Select(_, Term.Name("None")) => tree.symbol.value == "scala/None."
    case _                                                     => false
  }

  private val sequences = Set(
    "scala/collection/immutable/List#",
    "scala/collection/immutable/Vector#",
    "scala/collection/immutable/ArraySeq#",
    "scala/collection/immutable/Range#",
    "scala/collection/immutable/Range.Inclusive#",
    "scala/collection/immutable/Seq#",
    "scala/collection/Seq#",
    "scala/collection/IndexedSeq#",
    "scala/collection/immutable/IndexedSeq#",
    "scala/Array#"
  )

  private def sequenceType(term: Term)(using SemanticDocument): Boolean =
    definitions.typeSymbol(term).exists(sequences)

  private def optionType(term: Term)(using SemanticDocument): Boolean =
    definitions.typeSymbol(term).exists(symbol => symbol.endsWith("Option#") || symbol.endsWith("Some#"))
}
