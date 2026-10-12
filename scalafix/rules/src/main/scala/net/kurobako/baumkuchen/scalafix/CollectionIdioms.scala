package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Collection rewrites with bounded arithmetic and unchanged evaluation of user expressions. */
class CollectionIdioms extends SemanticRule("CollectionIdioms") {
  // Restrict rewrites to known strict implementations. In particular, arbitrary Iterable,
  // Set, Iterator, lazy collections and application methods need not share these semantics.
  private val strictSequences = Set(
    "scala/collection/immutable/List#",
    "scala/collection/immutable/Vector#",
    "scala/collection/immutable/ArraySeq#",
    "scala/collection/immutable/Range#",
    "scala/collection/immutable/Range.Inclusive#"
  )

  private val strictMaps = Set(
    "scala/collection/immutable/Map#",
    "scala/collection/immutable/HashMap#",
    "scala/collection/immutable/TreeMap#"
  )

  private val identityConversions = Map(
    "toSeq"    -> "scala/collection/immutable/Seq#",
    "toList"   -> "scala/collection/immutable/List#",
    "toVector" -> "scala/collection/immutable/Vector#"
  )

  override def fix(using document: SemanticDocument): Patch = document.tree.collect {
    case call: Term.Apply            => rewrite(call) + filterBeforeSorted(call) + emptyCollection(call)
    case comparison: Term.ApplyInfix => nonEmpty(comparison)
    case selection: Term.Select      => redundantConversion(selection)
  }.asPatch

  private def standardMethod(selection: Term.Select)(using SemanticDocument): Boolean =
    selection.symbol.value.startsWith("scala/collection/")

  private def strictSequence(term: Term)(using SemanticDocument): Boolean =
    typeSymbol(term).exists(strictSequences)

  private def typeSymbol(term: Term)(using SemanticDocument): Option[String] = (term match {
    case reference: Term.Name   => Some(reference.symbol)
    case selection: Term.Select => Some(selection.symbol)
    case _                      => None
  }).flatMap(definitions.valueType(_)).map(_.value)

  private def rewrite(call: Term.Apply)(using SemanticDocument): Patch =
    (call.fun, call.argClause.values) match {
      case (selection @ Term.Select(receiver, Term.Name(method @ ("exists" | "forall"))), List(predicate))
          if standardMethod(selection) && strictSequence(receiver) =>
        constantTarget(predicate, if (method == "exists") "==" else "!=").map { target =>
          val replacement = s"${receiver.syntax}.contains(${target.syntax})"
          replace(
            call,
            if (method == "exists") replacement else s"!($replacement)",
            if (method == "exists") "exists-to-contains" else "forall-to-not-contains",
            "use contains for equality with a constant"
          )
        }.getOrElse(Patch.empty)

      case (outer @ Term.Select(inner: Term.Apply, Term.Name("getOrElse")), List(default))
          if outer.symbol.value.startsWith("scala/Option#getOrElse") =>
        (inner.fun, inner.argClause.values) match {
          case (get @ Term.Select(receiver, Term.Name("get")), List(key))
              if get.symbol.value.startsWith("scala/collection/") &&
                typeSymbol(receiver).exists(strictMaps) =>
            replace(
              call,
              s"${receiver.syntax}.getOrElse(${key.syntax}, ${default.syntax})",
              "get-get-or-else",
              "use getOrElse(key, default)"
            )
          case _ => Patch.empty
        }

      case (take @ Term.Select(inner: Term.Apply, Term.Name("take")), List(Lit.Int(length)))
          if length >= 0 && standardMethod(take) =>
        (inner.fun, inner.argClause.values) match {
          case (drop @ Term.Select(receiver, Term.Name("drop")), List(Lit.Int(from)))
              if from >= 0 && from.toLong + length <= Int.MaxValue &&
                standardMethod(drop) && strictSequence(receiver) =>
            replace(
              call,
              s"${receiver.syntax}.slice($from, ${from + length})",
              "drop-take-to-slice",
              "use slice for constant nonnegative bounds"
            )
          case _ => Patch.empty
        }
      case _ => Patch.empty
    }

  private def constantTarget(predicate: Term, operator: String): Option[Lit] = {
    def comparison(body: Term, parameter: Term => Boolean): Option[Lit] = body match {
      case infix: Term.ApplyInfix if infix.op.value == operator && parameter(infix.lhs) =>
        infix.argClause.values match {
          // Null and custom equality are deliberately excluded. Literals cannot capture
          // the predicate parameter, throw, or introduce extra evaluations on an empty input.
          case List(value @ (_: Lit.Int | _: Lit.Long | _: Lit.String | _: Lit.Char | _: Lit.Boolean)) => Some(value)
          case _                                                                                       => None
        }
      case _ => None
    }
    predicate match {
      case Term.AnonymousFunction(body) => comparison(body, { case _: Term.Placeholder => true; case _ => false })
      case function: Term.Function      => function.paramClause.values match {
          case List(parameter) => comparison(
              function.body,
              {
                case name: Term.Name => name.value == parameter.name.value
                case _               => false
              }
            )
          case _ => None
        }
      case _ => None
    }
  }

  private def nonEmpty(comparison: Term.ApplyInfix)(using SemanticDocument): Patch =
    (comparison.op.value, comparison.lhs, comparison.argClause.values) match {
      case (">", size @ Term.Select(_, Term.Name("size")), List(Lit.Int(0)))
          if standardMethod(size) && !size.symbol.value.startsWith("scala/collection/StringOps") =>
        lint("size-to-non-empty", "use nonEmpty instead of size > 0 on a collection", comparison.pos) +
          Patch.replaceTree(size.name, "nonEmpty") +
          Patch.removeTokens(comparison.tokens.drop(size.tokens.length))
      case _ => Patch.empty
    }

  // reports only: reordering changes predicate and ordering evaluation
  private def filterBeforeSorted(call: Term.Apply)(using SemanticDocument): Patch = call.fun match {
    case selection @ Term.Select(sorted, Term.Name("filter"))
        if call.argClause.values.size == 1 && standardMethod(selection) && sortedCall(sorted) =>
      lint(
        "filter-before-sorted",
        "prefer filter(...).sorted when changing predicate and ordering evaluation is safe",
        call.pos
      )
    case _ => Patch.empty
  }

  private def sortedCall(term: Term)(using SemanticDocument): Boolean =
    methodSelection(term).exists(selection => selection.name.value == "sorted" && standardMethod(selection))

  @scala.annotation.tailrec
  private def methodSelection(term: Term): Option[Term.Select] = term match {
    case selection: Term.Select      => Some(selection)
    case application: Term.Apply     => methodSelection(application.fun)
    case application: Term.ApplyType => methodSelection(application.fun)
    case _                           => None
  }

  // identity conversions are removed, except toSeq on a strict sequence inside assertEquals
  private def redundantConversion(selection: Term.Select)(using SemanticDocument): Patch = selection match {
    case Term.Select(receiver, Term.Name(conversion))
        if standardMethod(selection) &&
          (identityConversions.get(conversion).exists(typeSymbol(receiver).contains) ||
            (conversion == "toSeq" && strictSequence(receiver) && assertionArgument(selection))) =>
      Patch.replaceTree(selection, receiver.syntax) +
        lint("redundant-collection-conversion", "remove the identity collection conversion", selection.pos)
    case _ => Patch.empty
  }

  private def assertionArgument(selection: Term.Select)(using SemanticDocument): Boolean =
    selection.parent.exists {
      case clause: Term.ArgClause => clause.parent.exists {
          case call: Term.Apply => call.fun.symbol.value.startsWith("munit/Assertions#assertEquals")
          case _                => false
        }
      case _ => false
    }

  private val companions = Set(
    "Array",
    "ArrayBuffer",
    "ArrayDeque",
    "ArraySeq",
    "BitSet",
    "Buffer",
    "HashMap",
    "HashSet",
    "IndexedSeq",
    "Iterable",
    "Iterator",
    "LazyList",
    "LinearSeq",
    "LinkedHashMap",
    "LinkedHashSet",
    "List",
    "ListBuffer",
    "ListMap",
    "ListSet",
    "Map",
    "Queue",
    "Seq",
    "SeqMap",
    "Set",
    "SortedMap",
    "SortedSet",
    "Stack",
    "TreeMap",
    "TreeSet",
    "Vector",
    "VectorMap"
  )

  // An extractor pattern such as `case Seq() =>` is not an application, so only constructions are rewritten.
  private def emptyCollection(call: Term.Apply)(using SemanticDocument): Patch =
    if (call.argClause.values.nonEmpty) Patch.empty
    else {
      val (companion, types) = call.fun match {
        case application: Term.ApplyType => (application.fun, application.argClause.syntax)
        case other                       => (other, "")
      }
      companion match {
        case reference: (Term.Name | Term.Select)
            if reference.symbol.value.startsWith("scala/") && reference.symbol.isGlobal &&
              reference.symbol.value.endsWith(".") && companions(reference.symbol.displayName) =>
          replace(call, s"${companion.syntax}.empty$types", "empty-collection", "use empty for an empty collection")
        case _ => Patch.empty
      }
    }

  private def replace(tree: Tree, replacement: String, id: String, message: String): Patch =
    Patch.replaceTree(tree, replacement) + lint(id, message, tree.pos)
}
