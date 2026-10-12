package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Chained collection calls rewritten to traverse less, and chains whose rewrite would change how often a caller's
  * expression runs reported instead: filter then headOption against find, collect then headOption against collectFirst,
  * reverse then find against findLast, and sorted then head or last against min, max and their By forms all evaluate
  * user code differently.
  */
class CollectionChains extends SemanticRule("CollectionChains") {

  override def fix(using document: SemanticDocument): Patch = document.tree.collect { case term: Term =>
    rewrite(term)
  }.asPatch

  private def rewrite(tree: Term)(using SemanticDocument): Patch = tree match {
    case outer @ Term.Select(inner: Term, name: Term.Name) => selection(outer, inner, name.value)
    case application: Term.Apply                           => applied(application)
    case comparison: Term.ApplyInfix                       => infix(comparison)
    case _                                                 => Patch.empty
  }

  private def selection(outer: Term, inner: Term, name: String)(using SemanticDocument): Patch =
    chained(outer, inner, name) + sortedTail(outer, inner, name)

  /** A method called on the result of another call, such as `filter(p).size`, `map(f).flatten` or `xs.unzip._1`. */
  private def chained(outer: Term, inner: Term, name: String)(using SemanticDocument): Patch = inner match {
    case call: Term.Apply => appliedSelection(outer, call, name)

    case Term.Select(receiver, Term.Name("unzip")) if name == "_1" || name == "_2" =>
      unzipSingle(outer, receiver, name)

    case Term.Select(receiver, Term.Name("unzip3")) if name == "_1" || name == "_2" || name == "_3" =>
      unzipSingle(outer, receiver, name)

    case toSet @ Term.Select(receiver, Term.Name("toSet"))
        if (name == "toSeq" || name == "toList") && libraryMethod(toSet) && strictSequence(receiver) =>
      replace(
        outer,
        s"${receiver.syntax}.distinct",
        "to-set-and-back-to-distinct",
        s"use distinct for toSet then $name"
      )

    case _ => Patch.empty
  }

  private def appliedSelection(outer: Term, call: Term.Apply, name: String)(using SemanticDocument): Patch =
    (call.fun, call.argClause.values) match {
      case (map @ Term.Select(receiver, Term.Name("map")), List(function)) if libraryMethod(map) && name == "flatten" =>
        replace(
          outer,
          s"${receiver.syntax}.flatMap(${function.syntax})",
          "map-flatten-to-flat-map",
          "use flatMap for map and flatten"
        )

      case (filter @ Term.Select(receiver, Term.Name("filter")), List(predicate))
          if libraryMethod(filter) && (name == "size" || name == "length") =>
        replace(
          outer,
          s"${receiver.syntax}.count(${predicate.syntax})",
          "filter-to-count",
          "use count for filter and size"
        )

      case (find @ Term.Select(receiver, Term.Name("find")), List(predicate)) if libraryMethod(find) =>
        name match {
          case "isDefined" | "nonEmpty" => findToExists(outer, receiver, predicate, negated = false)
          case "isEmpty"                => findToExists(outer, receiver, predicate, negated = true)
          case _                        => Patch.empty
        }

      case (filter @ Term.Select(_, Term.Name("filter" | "filterNot")), _)
          if libraryMethod(filter) && (name == "isEmpty" || name == "nonEmpty") =>
        lint(
          "filter-emptiness",
          s"filtering then $name traverses every element; exists and forall stop at the first deciding one",
          outer.pos
        )

      case (filter @ Term.Select(_, Term.Name("filter")), _)
          if libraryMethod(filter) && (name == "headOption" || name == "lastOption") =>
        lint(
          "filter-head-option",
          s"filter then $name traverses every element; find stops at the first match",
          outer.pos
        )

      case (collect @ Term.Select(_, Term.Name("collect")), _)
          if libraryMethod(collect) && (name == "headOption" || name == "lastOption") =>
        lint(
          "collect-head-option",
          s"collect then $name traverses every element; collectFirst stops at the first match",
          outer.pos
        )

      case (take @ Term.Select(reverse @ Term.Select(receiver, Term.Name("reverse")), Term.Name("take")), List(taken))
          if name == "reverse" && libraryMethod(take) && libraryMethod(reverse) =>
        replace(
          outer,
          s"${receiver.syntax}.takeRight(${taken.syntax})",
          "reverse-take-to-take-right",
          "use takeRight for reverse and take and reverse"
        )

      case _ => Patch.empty
    }

  /** `sorted`, `sortBy` and `sortWith` followed by an end of the collection compare every element instead. */
  private def sortedTail(outer: Term, inner: Term, name: String)(using SemanticDocument): Patch =
    if (!SortedTails(name)) Patch.empty
    else
      inner match {
        case sorted @ Term.Select(_, Term.Name(sort @ ("sorted" | "sortBy" | "sortWith"))) if libraryMethod(sorted) =>
          lint(
            "sorted-min-max",
            s"$sort then $name sorts every element; min, max, minBy and maxBy compare in one pass",
            outer.pos
          )
        case appliedSort: Term.Apply if appliedSort.argClause.values.nonEmpty =>
          appliedSort.fun match {
            case sort @ Term.Select(_, Term.Name(sortName @ ("sortBy" | "sortWith"))) if libraryMethod(sort) =>
              lint(
                "sorted-min-max",
                s"$sortName then $name sorts every element; min, max, minBy and maxBy compare in one pass",
                outer.pos
              )
            case _ => Patch.empty
          }
        case _ => Patch.empty
      }

  private val SortedTails = Set("head", "last", "headOption", "lastOption")

  private def findToExists(outer: Term, receiver: Term, predicate: Term, negated: Boolean): Patch = {
    val replaced = s"${receiver.syntax}.exists(${predicate.syntax})"
    replace(
      outer,
      if (negated) s"!$replaced" else replaced,
      "find-to-exists",
      "use exists for find and an emptiness check"
    )
  }

  private def unzipSingle(outer: Term, receiver: Term, field: String): Patch =
    replace(outer, s"${receiver.syntax}.map(_.$field)", "unzip-to-map", s"use map(_.$field) for unzip and a component")

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
          if libraryMethod(flatMap) && arguments.size == 1 && identityFunction(arguments.head) =>
        replace(
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
          case (fold @ Term.Select(receiver, Term.Name("foldLeft")), List(Lit.Boolean(true)), List(function))
              if libraryMethod(fold) =>
            foldForall(application, receiver, function)
          case _ => Patch.empty
        }

      case zip @ Term.Select(receiver, Term.Name("zip")) if libraryMethod(zip) && arguments.size == 1 =>
        arguments.head match {
          case Term.Select(indices, Term.Name("indices"))
              if indices.syntax == receiver.syntax && sequenceType(receiver) =>
            replace(
              application,
              s"${receiver.syntax}.zipWithIndex",
              "zip-indices-to-zip-with-index",
              "use zipWithIndex for zip of the indices"
            )
          case _ => Patch.empty
        }

      case map @ Term.Select(receiver, Term.Name("map")) if libraryMethod(map) && arguments.size == 1 =>
        mapProjection(application, receiver, arguments.head)

      case _ => Patch.empty
    }
  }

  /** `map(_._1)` and `map(_._2)` on a map read its keys or values through a new collection. */
  private def mapProjection(application: Term.Apply, receiver: Term, function: Term)(using SemanticDocument): Patch =
    if (!mapType(receiver)) Patch.empty
    else
      projection(function) match {
        case Some("_1") =>
          replace(application, s"${receiver.syntax}.keys", "map-to-keys", "use keys for map of the first component")
        case Some("_2") =>
          replace(
            application,
            s"${receiver.syntax}.values",
            "map-to-values",
            "use values for map of the second component"
          )
        case _ => Patch.empty
      }

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
    case Term.Name("identity")                 => function.symbol.value.startsWith("scala/Predef.identity")
    case Term.Select(_, Term.Name("identity")) => function.symbol.value.startsWith("scala/Predef.identity")
    case _                                     => false
  }

  /** `foldLeft(true)((acc, x) => acc && p(x))` evaluates `p` until it is false in either form, so it is forall. */
  private def foldForall(application: Term.Apply, receiver: Term, function: Term): Patch =
    function match {
      case function: Term.Function =>
        function.body match {
          case body: Term.ApplyInfix if body.op.value == "&&" && body.argClause.values.size == 1 =>
            (function.paramClause.values, body.lhs, body.argClause.values.head) match {
              case (List(first, second), accumulator: Term.Name, predicate)
                  if second.decltpe.isEmpty && accumulator.value == first.name.value &&
                    !predicate.collect { case name: Term.Name if name.value == first.name.value => name }.nonEmpty =>
                replace(
                  application,
                  s"${receiver.syntax}.forall(${second.name.value} => ${predicate.syntax})",
                  "fold-true-and-to-forall",
                  "use forall for a fold that ands a predicate"
                )
              case _ => Patch.empty
            }
          case _ => Patch.empty
        }
      case _ => Patch.empty
    }

  private def infix(comparison: Term.ApplyInfix)(using SemanticDocument): Patch = {
    val arguments = comparison.argClause.values
    comparison.op.value match {
      case "!=" if arguments.size == 1 && noneIsNone(arguments.head) =>
        comparison.lhs match {
          case findCall: Term.Apply =>
            (findCall.fun, findCall.argClause.values) match {
              case (find @ Term.Select(receiver, Term.Name("find")), List(predicate)) if libraryMethod(find) =>
                findToExists(comparison, receiver, predicate, negated = false)
              case _ => Patch.empty
            }
          case _ => Patch.empty
        }

      case "until" if arguments.size == 1 && comparison.lhs.is[Lit.Int] =>
        rangeIndices(comparison, comparison.lhs.asInstanceOf[Lit.Int].value, arguments.head)

      case "to" if arguments.size == 1 && comparison.lhs.is[Lit.Int] =>
        arguments.head match {
          case difference: Term.ApplyInfix if difference.op.value == "-" =>
            difference.argClause.values match {
              case List(one: Lit.Int) if one.value == 1 =>
                rangeIndices(comparison, comparison.lhs.asInstanceOf[Lit.Int].value, difference.lhs)
              case _ => Patch.empty
            }
          case _ => Patch.empty
        }

      case _ => Patch.empty
    }
  }

  private def rangeIndices(comparison: Term.ApplyInfix, start: Int, end: Term)(using SemanticDocument): Patch =
    end match {
      case size @ Term.Select(receiver, Term.Name("size" | "length"))
          if start == 0 && libraryMethod(size) && sequenceType(receiver) =>
        replace(comparison, s"${receiver.syntax}.indices", "range-to-indices", "use indices for a range over the size")
      case _ => Patch.empty
    }

  private def noneIsNone(tree: Term)(using SemanticDocument): Boolean = tree match {
    case Term.Name("None") => tree.symbol.value == "scala/None."
    case _                 => false
  }

  private def libraryMethod(selection: Term.Select)(using SemanticDocument): Boolean =
    selection.symbol.value.startsWith("scala/collection/") || selection.symbol.value.startsWith("scala/Option#")

  /** The type constructor of a value, through aliases. */
  private def typeSymbol(term: Term)(using SemanticDocument): Option[String] = (term match {
    case reference: Term.Name   => Some(reference.symbol)
    case selection: Term.Select => Some(selection.symbol)
    case _                      => None
  }).flatMap(definitions.valueType(_)).map(_.value)

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

  private def sequenceType(term: Term)(using SemanticDocument): Boolean = typeSymbol(term).exists(sequences)

  private def strictSequence(term: Term)(using SemanticDocument): Boolean = typeSymbol(term).exists(strictSequences)

  private def mapType(term: Term)(using SemanticDocument): Boolean = typeSymbol(term).exists(_.endsWith("Map#"))

  private def replace(tree: Tree, replacement: String, id: String, message: String): Patch =
    Patch.replaceTree(tree, replacement) + lint(id, message, tree.pos)
}
