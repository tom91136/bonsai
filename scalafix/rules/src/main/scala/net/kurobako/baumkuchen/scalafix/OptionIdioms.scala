package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Rewrites of Option and Map plumbing that read the same values with fewer intermediate calls: get then get, get then
  * an emptiness check, map then getOrElse, comparisons with Some or None, emulated headOption and lastOption, index
  * lookups and bounds checks, and redundant iteration of an Option.
  */
class OptionIdioms extends SemanticRule("OptionIdioms") {

  override def fix(using document: SemanticDocument): Patch = document.tree.collect { case term: Term =>
    rewrite(term)
  }.asPatch

  private def rewrite(tree: Term)(using SemanticDocument): Patch = tree match {
    case selection: Term.Select      => selected(selection)
    case application: Term.Apply     => applied(application)
    case conditional: Term.If        => conditional_(conditional)
    case comparison: Term.ApplyInfix => infix(comparison)
    case _                           => Patch.empty
  }

  /** `map.get(k).get`, `map.get(k).isEmpty` and an Option iterated by headOption or lastOption. */
  private def selected(selection: Term.Select)(using SemanticDocument): Patch = {
    val name = selection.name.value
    selection.qual match {
      case getCall: Term.Apply if name == "get" || name == "isEmpty" || name == "nonEmpty" || name == "isDefined" =>
        (getCall.fun, getCall.argClause.values) match {
          case (get @ Term.Select(map, Term.Name("get")), List(key)) if libraryMethod(get) && mapType(map) =>
            name match {
              case "get" =>
                replace(
                  selection,
                  s"${map.syntax}(${key.syntax})",
                  "map-get-to-apply",
                  "use the map as a function instead of get and get"
                )
              case "isEmpty" =>
                replace(
                  selection,
                  s"!${map.syntax}.contains(${key.syntax})",
                  "map-get-emptiness",
                  "use contains instead of get and an emptiness check"
                )
              case _ =>
                replace(
                  selection,
                  s"${map.syntax}.contains(${key.syntax})",
                  "map-get-emptiness",
                  "use contains instead of get and an emptiness check"
                )
            }
          case _ => Patch.empty
        }

      case receiver if (name == "headOption" || name == "lastOption") && optionType(receiver) =>
        replace(selection, receiver.syntax, "redundant-option-iteration", s"Option.$name is the option itself")

      case _ => Patch.empty
    }
  }

  /** `opt.map(p).getOrElse(default)` is `opt.fold(default)(p)`, `map.lift(k)` is `map.get(k)`, and index lookups. */
  private def applied(application: Term.Apply)(using SemanticDocument): Patch = {
    val arguments = application.argClause.values
    application.fun match {
      case getOrElse @ Term.Select(mapCall: Term.Apply, Term.Name("getOrElse")) if libraryMethod(getOrElse) =>
        (mapCall.fun, mapCall.argClause.values, arguments) match {
          case (map @ Term.Select(option, Term.Name("map")), List(function), List(default))
              if libraryMethod(map) && optionType(option) =>
            replace(
              application,
              s"${option.syntax}.fold(${default.syntax})(${function.syntax})",
              "map-get-or-else-to-fold",
              "use fold for map and getOrElse"
            )
          case _ => Patch.empty
        }

      case lift @ Term.Select(receiver, Term.Name("lift")) if libraryMethod(lift) && arguments.size == 1 =>
        liftOf(application, receiver, arguments.head)

      case apply @ Term.Select(receiver, Term.Name("apply")) if libraryMethod(apply) && arguments.size == 1 =>
        index(application, receiver, arguments.head)

      case name: Term.Name if arguments.size == 1 =>
        index(application, name, arguments.head)

      case _ => Patch.empty
    }
  }

  private def liftOf(application: Term.Apply, receiver: Term, argument: Term)(using SemanticDocument): Patch =
    typeSymbol(receiver) match {
      case Some(map) if map.endsWith("Map#") =>
        replace(
          application,
          s"${receiver.syntax}.get(${argument.syntax})",
          "map-lift-to-get",
          "use get for lift on a map"
        )
      case _ if sequenceType(receiver) =>
        argument match {
          case Lit.Int(0) =>
            replace(
              application,
              s"${receiver.syntax}.headOption",
              "head-or-last-option",
              "use headOption for lift of the first index"
            )
          case difference: Term.ApplyInfix if difference.op.value == "-" && lastOf(difference, receiver) =>
            replace(
              application,
              s"${receiver.syntax}.lastOption",
              "head-or-last-option",
              "use lastOption for lift of the last index"
            )
          case _ => Patch.empty
        }
      case _ => Patch.empty
    }

  /** `xs(0)` is head and `xs(xs.size - 1)` is last on a sequence indexed by position. */
  private def index(application: Term.Apply, receiver: Term, argument: Term)(using SemanticDocument): Patch =
    if (!sequenceType(receiver)) Patch.empty
    else
      argument match {
        case Lit.Int(0) =>
          replace(application, s"${receiver.syntax}.head", "zero-index-to-head", "use head for the first element")
        case difference: Term.ApplyInfix
            if difference.op.value == "-" && lastOf(difference, receiver) && linearSequence(receiver) =>
          replace(application, s"${receiver.syntax}.last", "last-index-to-last", "use last for the last element")
        case _ => Patch.empty
      }

  /** Whether the subtraction is `size - 1` or `length - 1` of `receiver`. */
  private def lastOf(difference: Term.ApplyInfix, receiver: Term)(using SemanticDocument): Boolean =
    (difference.lhs, difference.argClause.values) match {
      case (size @ Term.Select(inner, Term.Name("size" | "length")), List(one: Lit.Int)) =>
        one.value == 1 && inner.syntax == receiver.syntax && libraryMethod(size)
      case _ => false
    }

  /** Emulations of headOption, lastOption and lift, and if statements that are a filter of an Option. */
  private def conditional_(tree: Term.If)(using SemanticDocument): Patch =
    emptyOrLast(tree) + simulatedFilter(tree) + filteredOption(tree)

  private def emptyOrLast(tree: Term.If)(using SemanticDocument): Patch = {
    val (condition, positive, negative) = (tree.cond, tree.thenp, tree.elsep)
    condition match {
      case nonEmpty @ Term.Select(receiver, Term.Name("nonEmpty"))
          if libraryMethod(nonEmpty) && sequenceType(receiver) =>
        (someOf(positive), isNone(negative)) match {
          case (Some(Term.Select(inner, Term.Name(tail))), true) if inner.syntax == receiver.syntax =>
            tailOf(tree, receiver, tail)
          case _ => Patch.empty
        }

      case isEmpty @ Term.Select(receiver, Term.Name("isEmpty")) if libraryMethod(isEmpty) && sequenceType(receiver) =>
        (isNone(positive), someOf(negative)) match {
          case (true, Some(Term.Select(inner, Term.Name(tail)))) if inner.syntax == receiver.syntax =>
            tailOf(tree, receiver, tail)
          case _ => Patch.empty
        }

      case comparison: Term.ApplyInfix if comparison.op.value == "<" && comparison.argClause.values.size == 1 =>
        // `if (i < xs.length) Some(xs(i)) else None` is `xs.lift(i)`.
        comparison.argClause.values.head match {
          case length @ Term.Select(receiver, Term.Name("length"))
              if libraryMethod(length) && sequenceType(receiver) && isNone(tree.elsep) =>
            someOf(tree.thenp) match {
              case Some(indexed: Term.Apply) if indexed.fun.syntax == receiver.syntax =>
                indexed.argClause.values match {
                  case List(position) if position.syntax == comparison.lhs.syntax =>
                    replace(
                      tree,
                      s"${receiver.syntax}.lift(${position.syntax})",
                      "bounds-to-lift",
                      "use lift for an index bounds check"
                    )
                  case _ => Patch.empty
                }
              case _ => Patch.empty
            }
          case _ => Patch.empty
        }

      case _ => Patch.empty
    }
  }

  private def tailOf(tree: Term, receiver: Term, tail: String): Patch = tail match {
    case "head" =>
      replace(
        tree,
        s"${receiver.syntax}.headOption",
        "head-or-last-option",
        "use headOption for an emptiness check and head"
      )
    case "last" =>
      replace(
        tree,
        s"${receiver.syntax}.lastOption",
        "head-or-last-option",
        "use lastOption for an emptiness check and last"
      )
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
              replace(
                tree,
                s"${option.syntax}.filter(${predicate.syntax})",
                "simulated-filter",
                "use filter for an isDefined check and get"
              )
            case _ => Patch.empty
          }
        case _ => Patch.empty
      }
    case _ => Patch.empty
  }

  /** `if (p(v)) Some(v) else None` is `Some(v).filter(p)` for a stable `v`. */
  private def filteredOption(tree: Term.If)(using SemanticDocument): Patch = tree.cond match {
    case condition: Term.Apply if condition.argClause.values.size == 1 && isNone(tree.elsep) =>
      condition.argClause.values.head match {
        case name: Term.Name =>
          someOf(tree.thenp) match {
            case Some(value: Term.Name) if value.value == name.value =>
              replace(
                tree,
                s"Some(${value.syntax}).filter(${condition.fun.syntax})",
                "if-else-to-filtered-option",
                "use filter for an if that wraps one value"
              )
            case _ => Patch.empty
          }
        case _ => Patch.empty
      }
    case _ => Patch.empty
  }

  /** `opt == Some(x)` is contains, and `opt == None` is an emptiness check. */
  private def infix(comparison: Term.ApplyInfix)(using SemanticDocument): Patch = {
    val arguments = comparison.argClause.values
    comparison.op.value match {
      case "==" | "!=" if arguments.size == 1 =>
        val (left, right) = (comparison.lhs, arguments.head)
        val negated       = comparison.op.value == "!="
        if (isNone(right) && optionType(left)) noneCheck(comparison, left, negated)
        else if (isNone(left) && optionType(right)) noneCheck(comparison, right, negated)
        else
          someOf(right) match {
            case Some(value) if optionType(left) => contains(comparison, left, value, negated)
            case _                               =>
              someOf(left) match {
                case Some(value) if optionType(right) => contains(comparison, right, value, negated)
                case _                                => Patch.empty
              }
          }
      case _ => Patch.empty
    }
  }

  private def contains(outer: Term, option: Term, value: Term, negated: Boolean): Patch = {
    val replaced = s"${option.syntax}.contains(${value.syntax})"
    replace(
      outer,
      if (negated) s"!$replaced" else replaced,
      "some-to-contains",
      "use contains for an equality check with Some"
    )
  }

  private def noneCheck(outer: Term, option: Term, negated: Boolean): Patch =
    if (negated) replace(outer, s"${option.syntax}.isDefined", "none-check", "use isDefined for a check against None")
    else replace(outer, s"${option.syntax}.isEmpty", "none-check", "use isEmpty for a check against None")

  private def someOf(tree: Term)(using SemanticDocument): Option[Term] = tree match {
    case application: Term.Apply
        if application.fun.symbol.value == "scala/Some." && application.argClause.values.size == 1 =>
      Some(application.argClause.values.head)
    case _ => None
  }

  private def isNone(tree: Term)(using SemanticDocument): Boolean = tree match {
    case Term.Name("None")                 => tree.symbol.value == "scala/None."
    case Term.Select(_, Term.Name("None")) => tree.symbol.value == "scala/None."
    case _                                 => false
  }

  private def libraryMethod(selection: Term.Select)(using SemanticDocument): Boolean =
    selection.symbol.value.startsWith("scala/collection/") || selection.symbol.value.startsWith("scala/Option#") ||
      selection.symbol.value.startsWith("scala/PartialFunction#")

  private def typeSymbol(term: Term)(using SemanticDocument): Option[String] = (term match {
    case reference: Term.Name   => Some(reference.symbol)
    case selection: Term.Select => Some(selection.symbol)
    case _                      => None
  }).flatMap(definitions.valueType(_)).map(_.value)

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

  private val linear =
    Set("scala/collection/immutable/List#", "scala/collection/immutable/Seq#", "scala/collection/Seq#")

  private def sequenceType(term: Term)(using SemanticDocument): Boolean = typeSymbol(term).exists(sequences)

  private def linearSequence(term: Term)(using SemanticDocument): Boolean = typeSymbol(term).exists(linear)

  private def optionType(term: Term)(using SemanticDocument): Boolean =
    typeSymbol(term).exists(symbol => symbol.endsWith("Option#") || symbol.endsWith("Some#"))

  private def mapType(term: Term)(using SemanticDocument): Boolean = typeSymbol(term).exists(_.endsWith("Map#"))

  private def replace(tree: Tree, replacement: String, id: String, message: String): Patch =
    Patch.replaceTree(tree, replacement) + lint(id, message, tree.pos)
}
