package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Chained collection calls rewritten to traverse less, and chains whose rewrite would change how often a caller's
  * expression runs reported instead: filter then headOption against find, collect then headOption against collectFirst,
  * reverse then find against findLast, and sorted then head or last against min, max and their By forms all evaluate
  * user code differently.
  */
class CollectionChains extends SemanticRule("CollectionChains") {

  override def fix(using document: SemanticDocument): Patch = document.tree.collect {
    case outer @ Term.Select(inner, Term.Name(name)) => chained(outer, inner, name) + sortedTail(outer, inner, name)
    case application: Term.Apply                     => applied(application)
    case comparison: Term.ApplyInfix                 => infix(comparison)
  }.asPatch

  /** A method called on the result of another call, such as `filter(p).size`, `map(f).flatten` or `xs.unzip._1`. */
  private def chained(outer: Term, inner: Term, name: String)(using SemanticDocument): Patch = inner match {
    case call: Term.Apply => appliedSelection(outer, call, name)

    case Term.Select(receiver, Term.Name("unzip")) if name == "_1" || name == "_2" =>
      unzipSingle(outer, receiver, name)

    case Term.Select(receiver, Term.Name("unzip3")) if name == "_1" || name == "_2" || name == "_3" =>
      unzipSingle(outer, receiver, name)

    case toSet @ Term.Select(receiver, Term.Name("toSet"))
        if (name == "toSeq" || name == "toList") && libraryMethod(toSet) && strictSequence(receiver) =>
      lint.replace(
        outer,
        s"${receiver.syntax}.distinct",
        "to-set-and-back-to-distinct",
        s"use distinct for toSet then $name"
      )

    case _ => Patch.empty
  }

  private def appliedSelection(outer: Term, call: Term.Apply, name: String)(using SemanticDocument): Patch =
    (call.fun, call.argClause.values) match {
      case (map @ Term.Select(receiver, Term.Name("map")), List(function)) if name == "flatten" && libraryMethod(map) =>
        lint.replace(
          outer,
          s"${receiver.syntax}.flatMap(${function.syntax})",
          "map-flatten-to-flat-map",
          "use flatMap for map and flatten"
        )

      // A size compared with zero states emptiness, which CollectionIdioms rewrites and filter-emptiness reports.
      case (filter @ Term.Select(receiver, Term.Name("filter")), List(predicate))
          if (name == "size" || name == "length") && !definitions.zeroComparison(outer) && libraryMethod(filter) =>
        lint.replace(
          outer,
          s"${receiver.syntax}.count(${predicate.syntax})",
          "filter-to-count",
          "use count for filter and size"
        )

      case (find @ Term.Select(receiver, Term.Name("find")), List(predicate)) if libraryMethod(find) =>
        name match {
          case "isDefined" | "nonEmpty" => findToExists(outer, exists(receiver, predicate))
          case "isEmpty"                => findToExists(outer, s"!${exists(receiver, predicate)}")
          case _                        => Patch.empty
        }

      case (take @ Term.Select(reverse @ Term.Select(receiver, Term.Name("reverse")), Term.Name("take")), List(taken))
          if name == "reverse" && libraryMethod(take) && libraryMethod(reverse) =>
        lint.replace(
          outer,
          s"${receiver.syntax}.takeRight(${taken.syntax})",
          "reverse-take-to-take-right",
          "use takeRight for reverse and take and reverse"
        )

      case (traversal @ Term.Select(_, Term.Name(method)), _) =>
        traversals.collectFirst {
          case (methods, names, id, prefix, alternative)
              if methods(method) && names(name) && libraryMethod(traversal) =>
            lint(id, s"$prefix then $name traverses every element; $alternative", outer.pos)
        }.getOrElse(Patch.empty)

      case _ => Patch.empty
    }

  private val traversals = List(
    (
      Set("filter", "filterNot"),
      Set("isEmpty", "nonEmpty"),
      "filter-emptiness",
      "filtering",
      "exists and forall stop at the first deciding one"
    ),
    (
      Set("filter"),
      Set("headOption", "lastOption"),
      "filter-head-option",
      "filter",
      "find stops at the first match"
    ),
    (
      Set("collect"),
      Set("headOption", "lastOption"),
      "collect-head-option",
      "collect",
      "collectFirst stops at the first match"
    )
  )

  // A stable sort ends with the last of equal elements, where max and maxBy keep the first.
  private def oneMinMax(tail: String): String =
    if (tail.startsWith("last"))
      "max and maxBy compare in one pass, but return the first of equal elements where the sort ends with the last"
    else "min and minBy compare in one pass"

  /** `sorted`, `sortBy` and `sortWith` followed by an end of the collection compare every element instead. */
  private def sortedTail(outer: Term, inner: Term, name: String)(using SemanticDocument): Patch =
    if (!SortedTails(name)) Patch.empty
    else
      inner match {
        case sorted @ Term.Select(_, Term.Name(sort @ ("sorted" | "sortBy" | "sortWith"))) if libraryMethod(sorted) =>
          lint("sorted-min-max", s"$sort then $name sorts every element; ${oneMinMax(name)}", outer.pos)
        case appliedSort: Term.Apply if appliedSort.argClause.values.nonEmpty =>
          appliedSort.fun match {
            case sort @ Term.Select(_, Term.Name(sortName @ ("sortBy" | "sortWith"))) if libraryMethod(sort) =>
              lint("sorted-min-max", s"$sortName then $name sorts every element; ${oneMinMax(name)}", outer.pos)
            case _ => Patch.empty
          }
        case _ => Patch.empty
      }

  private val SortedTails = Set("head", "last", "headOption", "lastOption")

  private def exists(receiver: Term, predicate: Term): String = s"${receiver.syntax}.exists(${predicate.syntax})"

  private def findToExists(outer: Term, replacement: String): Patch =
    lint.replace(outer, replacement, "find-to-exists", "use exists for find and an emptiness check")

  private def unzipSingle(outer: Term, receiver: Term, field: String): Patch =
    lint.replace(
      outer,
      s"${receiver.syntax}.map(_.$field)",
      "unzip-to-map",
      s"use map(_.$field) for unzip and a component"
    )

  private def applied(application: Term.Apply)(using SemanticDocument): Patch = {
    val arguments = application.argClause.values
    application.fun match {
      case find @ Term.Select(reverse @ Term.Select(_, Term.Name("reverse")), Term.Name("find"))
          if libraryMethod(find) && libraryMethod(reverse) =>
        lint(
          "reverse-find",
          "reverse then find reverses the collection and searches from the front; findLast searches from the end",
          application.pos
        )

      case flatMap @ Term.Select(receiver, Term.Name("flatMap"))
          if arguments.size == 1 && identityFunction(arguments.head) && libraryMethod(flatMap) =>
        lint.replace(
          application,
          s"${receiver.syntax}.flatten",
          "flat-map-identity-to-flatten",
          "use flatten for flatMap of identity"
        )

      case Term.Select(mapCall: Term.Apply, Term.Name("contains"))
          if arguments.size == 1 && arguments.head.is[Lit.Boolean] =>
        mapCall.fun match {
          case map @ Term.Select(_, Term.Name("map")) if libraryMethod(map) =>
            lint(
              "map-contains",
              "map then contains traverses every element; exists and forall stop at the first deciding one",
              application.pos
            )
          case _ => Patch.empty
        }

      case foldCall: Term.Apply =>
        (foldCall.fun, foldCall.argClause.values, arguments) match {
          case (
                fold @ Term.Select(receiver, Term.Name("foldLeft")),
                List(Lit.Boolean(true)),
                List(function: Term.Function)
              )
              if libraryMethod(fold) =>
            foldForall(application, receiver, function)
          case _ => Patch.empty
        }

      case zip @ Term.Select(receiver, Term.Name("zip")) if arguments.size == 1 && libraryMethod(zip) =>
        arguments.head match {
          case Term.Select(indices, Term.Name("indices"))
              if indices.syntax == receiver.syntax && sequenceType(receiver) =>
            lint.replace(
              application,
              s"${receiver.syntax}.zipWithIndex",
              "zip-indices-to-zip-with-index",
              "use zipWithIndex for zip of the indices"
            )
          case _ => Patch.empty
        }

      case map @ Term.Select(receiver, Term.Name("map")) if arguments.size == 1 && libraryMethod(map) =>
        mapProjection(application, receiver, arguments.head)

      case _ => Patch.empty
    }
  }

  /** `map(_._1)` and `map(_._2)` on a map read its keys or values through a new collection. */
  private def mapProjection(application: Term.Apply, receiver: Term, function: Term)(using SemanticDocument): Patch =
    projection(function).flatMap(projections.get) match {
      case Some((method, component)) if definitions.mapType(receiver) =>
        lint.replace(
          application,
          s"${receiver.syntax}.$method",
          s"map-to-$method",
          s"use $method for map of the $component component"
        )
      case _ => Patch.empty
    }

  private val projections = Map("_1" -> ("keys", "first"), "_2" -> ("values", "second"))

  /** The field a function selects from its parameter, for `_._1` and `pair => pair._1`. */
  private def projection(function: Term): Option[String] = function match {
    case Term.Select(Term.Placeholder(), Term.Name(field))                         => Some(field)
    case Term.AnonymousFunction(Term.Select(Term.Placeholder(), Term.Name(field))) => Some(field)
    case function: Term.Function                                                   =>
      (function.paramClause.values, function.body) match {
        case (List(parameter), Term.Select(name: Term.Name, Term.Name(field))) if name.value == parameter.name.value =>
          Some(field)
        case _ => None
      }
    case _ => None
  }

  private def identityFunction(function: Term)(using SemanticDocument): Boolean = function match {
    case Term.Name("identity") | Term.Select(_, Term.Name("identity")) =>
      function.symbol.value.startsWith("scala/Predef.identity")
    case _ => false
  }

  /** `foldLeft(true)((acc, x) => acc && p(x))` evaluates `p` until it is false in either form, so it is forall. */
  private def foldForall(application: Term.Apply, receiver: Term, function: Term.Function): Patch =
    function.body match {
      case body: Term.ApplyInfix if body.op.value == "&&" =>
        (function.paramClause.values, body.lhs, body.argClause.values) match {
          case (List(first, second), accumulator: Term.Name, List(predicate))
              if second.decltpe.isEmpty && accumulator.value == first.name.value &&
                predicate.collect { case name: Term.Name if name.value == first.name.value => name }.isEmpty =>
            lint.replace(
              application,
              s"${receiver.syntax}.forall(${second.name.value} => ${predicate.syntax})",
              "fold-true-and-to-forall",
              "use forall for a fold that ands a predicate"
            )
          case _ => Patch.empty
        }
      case _ => Patch.empty
    }

  private def infix(comparison: Term.ApplyInfix)(using SemanticDocument): Patch =
    (comparison.op.value, comparison.lhs, comparison.argClause.values) match {
      case ("!=", findCall: Term.Apply, List(none)) if definitions.isNone(none) =>
        (findCall.fun, findCall.argClause.values) match {
          case (find @ Term.Select(receiver, Term.Name("find")), List(predicate)) if libraryMethod(find) =>
            findToExists(comparison, exists(receiver, predicate))
          case _ => Patch.empty
        }

      case ("until", Lit.Int(0), List(end)) => rangeIndices(comparison, end)

      case ("to", Lit.Int(0), List(difference: Term.ApplyInfix)) if difference.op.value == "-" =>
        difference.argClause.values match {
          case List(Lit.Int(1)) => rangeIndices(comparison, difference.lhs)
          case _                => Patch.empty
        }

      case _ => Patch.empty
    }

  private def rangeIndices(comparison: Term.ApplyInfix, end: Term)(using SemanticDocument): Patch = end match {
    case size @ Term.Select(receiver, Term.Name("size" | "length")) if libraryMethod(size) && sequenceType(receiver) =>
      lint.replace(
        comparison,
        s"${receiver.syntax}.indices",
        "range-to-indices",
        "use indices for a range over the size"
      )
    case _ => Patch.empty
  }

  private def libraryMethod(selection: Term.Select)(using SemanticDocument): Boolean = {
    val symbol = selection.symbol.value
    symbol.startsWith("scala/collection/") || symbol.startsWith("scala/Option#")
  }

  private val strictSequences = Set(
    "scala/collection/immutable/List#",
    "scala/collection/immutable/Vector#",
    "scala/collection/immutable/ArraySeq#",
    "scala/collection/immutable/Range#",
    "scala/collection/immutable/Range.Inclusive#",
    "scala/collection/immutable/NumericRange#",
    "scala/collection/immutable/NumericRange.Inclusive#",
    "scala/collection/immutable/Seq#",
    "scala/collection/Seq#",
    "scala/Array#"
  )

  private val sequences = strictSequences ++ Set(
    "scala/collection/immutable/IndexedSeq#",
    "scala/collection/IndexedSeq#"
  )

  private def sequenceType(term: Term)(using SemanticDocument): Boolean =
    definitions.typeSymbol(term).exists(sequences)

  private def strictSequence(term: Term)(using SemanticDocument): Boolean =
    definitions.typeSymbol(term).exists(strictSequences)
}
