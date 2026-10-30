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
  private def selected(selection: Term.Select)(using SemanticDocument): Patch =
    (selection.qual, selection.name.value) match {
      case (getCall: Term.Apply, name @ ("get" | "isEmpty" | "nonEmpty" | "isDefined")) =>
        (getCall.fun, getCall.argClause.values) match {
          case (get @ Term.Select(map, Term.Name("get")), List(key))
              if libraryMethod(get) && definitions.mapType(map) =>
            if (name == "get")
              lint.replace(
                selection,
                s"${map.syntax}(${key.syntax})",
                "map-get-to-apply",
                "use the map as a function instead of get and get"
              )
            else if (primitiveKey(map)) Patch.empty
            else
              lint.replace(
                selection,
                s"${negation(name == "isEmpty")}${map.syntax}.contains(${key.syntax})",
                "map-get-emptiness",
                "use contains instead of get and an emptiness check"
              )
          case _ => Patch.empty
        }

      case (receiver, name @ ("headOption" | "lastOption")) if optionType(receiver) =>
        lint.replace(selection, receiver.syntax, "redundant-option-iteration", s"Option.$name is the option itself")

      case _ => Patch.empty
    }

  // The immutable maps specialised for primitive keys look them up unboxed in get, but box them in contains.
  private def primitiveKey(map: Term)(using SemanticDocument): Boolean =
    definitions.typeSymbol(map).exists(Set("scala/collection/immutable/LongMap#", "scala/collection/immutable/IntMap#"))

  /** `opt.map(p).getOrElse(default)` is `opt.fold(default)(p)`, `map.lift(k)` is `map.get(k)`, and index lookups. */
  private def applied(application: Term.Apply)(using SemanticDocument): Patch = {
    val arguments = application.argClause.values
    application.fun match {
      case getOrElse @ Term.Select(mapCall: Term.Apply, Term.Name("getOrElse")) if libraryMethod(getOrElse) =>
        (mapCall.fun, mapCall.argClause.values, arguments) match {
          // Reported without a fix: fold takes its result type from the default, which can be narrower than what the
          // function returns, as None or Nil are, and the rule cannot see either type.
          case (map @ Term.Select(option, Term.Name("map")), List(_), List(_))
              if libraryMethod(map) && optionType(option) =>
            lint("map-get-or-else-to-fold", "use fold for map and getOrElse", application.pos)
          case _ => Patch.empty
        }

      case lift @ Term.Select(receiver, Term.Name("lift")) if arguments.size == 1 && libraryMethod(lift) =>
        liftOf(application, receiver, arguments.head)

      case apply @ Term.Select(receiver, Term.Name("apply")) if arguments.size == 1 && libraryMethod(apply) =>
        index(application, receiver, arguments.head)

      case name: Term.Name if arguments.size == 1 =>
        index(application, name, arguments.head)

      case _ => Patch.empty
    }
  }

  private def liftOf(application: Term.Apply, receiver: Term, argument: Term)(using SemanticDocument): Patch =
    if (definitions.mapType(receiver))
      lint.replace(
        application,
        s"${receiver.syntax}.get(${argument.syntax})",
        "map-lift-to-get",
        "use get for lift on a map"
      )
    else {
      val target = argument match {
        case Lit.Int(0)                      => Some(("head", "first"))
        case _ if lastOf(argument, receiver) => Some(("last", "last"))
        case _                               => None
      }
      target match {
        case Some((method, position)) if sequenceType(receiver) =>
          lint.replace(
            application,
            s"${receiver.syntax}.${method}Option",
            "head-or-last-option",
            s"use ${method}Option for lift of the $position index"
          )
        case _ => Patch.empty
      }
    }

  /** `xs(0)` is head and `xs(xs.size - 1)` is last on a linear sequence, unless the definition around it also indexes
    * the same collection elsewhere, where positions read in parallel. Indexed sequences and arrays are indexed by
    * position idiomatically, and their apply throws IndexOutOfBoundsException where head throws NoSuchElementException.
    */
  private def index(application: Term.Apply, receiver: Term, argument: Term)(using SemanticDocument): Patch =
    if (assigned(application)) Patch.empty
    else argument match {
      case Lit.Int(0) if linearSequence(receiver) && !parallel(application, receiver, argument) =>
        lint.replace(application, s"${receiver.syntax}.head", "zero-index-to-head", "use head for the first element")
      case _ if lastOf(argument, receiver) && linearSequence(receiver) && !parallel(application, receiver, argument) =>
        lint.replace(application, s"${receiver.syntax}.last", "last-index-to-last", "use last for the last element")
      case _ => Patch.empty
    }

  // `xs(0) = v` is an update, which head cannot stand in for.
  private def assigned(application: Term.Apply): Boolean = application.parent.exists {
    case assignment: Term.Assign => assignment.lhs eq application
    case _                       => false
  }

  /** Whether `receiver`, a named collection, is indexed at another position: anywhere for a local, which only its own
    * block can see, or within the method, function or definition enclosing `application` for a member.
    */
  private def parallel(application: Term.Apply, receiver: Term, argument: Term)(using
      document: SemanticDocument
  ): Boolean =
    receiver match {
      case reference @ (_: Term.Name | _: Term.Select) if !reference.symbol.isNone =>
        val symbol    = reference.symbol
        val ancestors = definitions.ancestors(application)
        val scope     =
          if (symbol.isLocal) Some(document.tree)
          else
            ancestors.collectFirst { case scope @ (_: Defn.Def | _: Term.Function) => scope }
              .orElse(ancestors.collectFirst { case scope @ (_: Defn.Val | _: Defn.Var) => scope })
        scope.exists(_.collect {
          case other: Term.Apply
              if indexed(other).exists((target, index) =>
                target.symbol == symbol && index.syntax != argument.syntax
              ) => other
        }.nonEmpty)
      case _ => false
    }

  /** The collection and index of `xs(i)` or `xs.apply(i)`. */
  private def indexed(application: Term.Apply): Option[(Term, Term)] =
    (application.fun, application.argClause.values) match {
      case (Term.Select(target, Term.Name("apply")), List(index)) => Some(target -> index)
      case (target: Term.Name, List(index))                       => Some(target -> index)
      case _                                                      => None
    }

  /** Whether `index` is `size - 1` or `length - 1` of `receiver`. */
  private def lastOf(index: Term, receiver: Term)(using SemanticDocument): Boolean = index match {
    case difference: Term.ApplyInfix if difference.op.value == "-" =>
      (difference.lhs, difference.argClause.values) match {
        case (size @ Term.Select(inner, Term.Name("size" | "length")), List(Lit.Int(1))) =>
          inner.syntax == receiver.syntax && libraryMethod(size)
        case _ => false
      }
    case _ => false
  }

  /** Emulations of headOption, lastOption and lift, and if statements that are a filter of an Option. */
  private def conditional_(tree: Term.If)(using SemanticDocument): Patch =
    emptyOrLast(tree) + boundsToLift(tree) + simulatedFilter(tree) + filteredOption(tree)

  private def emptyOrLast(tree: Term.If)(using SemanticDocument): Patch = tree.cond match {
    case check @ Term.Select(receiver, Term.Name(emptiness @ ("nonEmpty" | "isEmpty")))
        if libraryMethod(check) && sequenceType(receiver) =>
      val (present, absent) = if (emptiness == "nonEmpty") (tree.thenp, tree.elsep) else (tree.elsep, tree.thenp)
      present match {
        case SomeOf(Term.Select(inner, Term.Name(tail @ ("head" | "last"))))
            if inner.syntax == receiver.syntax && definitions.isNone(absent) =>
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
              sequenceType(receiver) && definitions.isNone(tree.elsep) =>
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
              definitions.isNone(tree.elsep) =>
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
    case condition: Term.Apply if condition.argClause.values.size == 1 && definitions.isNone(tree.elsep) =>
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

  /** `opt == Some(x)` is contains, and `opt == None` is an emptiness check. */
  private def infix(comparison: Term.ApplyInfix)(using SemanticDocument): Patch =
    (comparison.op.value, comparison.argClause.values) match {
      case (operator @ ("==" | "!="), List(right)) =>
        val negated = operator == "!="
        List(comparison.lhs -> right, right -> comparison.lhs).collectFirst {
          case (option, none)
              if definitions.isNone(none) && optionType(option) &&
                !(negated && (option eq comparison.lhs) && findCall(option)) =>
            noneCheck(comparison, option, negated)
          case (option, SomeOf(value)) if optionType(option) =>
            contains(comparison, option, value, negated)
        }.getOrElse(Patch.empty)
      case _ => Patch.empty
    }

  /** `find(p) != None`, which CollectionChains rewrites to exists. */
  private def findCall(tree: Term)(using SemanticDocument): Boolean = tree match {
    case application: Term.Apply =>
      (application.fun, application.argClause.values) match {
        case (find @ Term.Select(_, Term.Name("find")), List(_)) => libraryMethod(find)
        case _                                                   => false
      }
    case _ => false
  }

  private def contains(outer: Term, option: Term, value: Term, negated: Boolean): Patch =
    lint.replace(
      outer,
      s"${negation(negated)}${option.syntax}.contains(${value.syntax})",
      "some-to-contains",
      "use contains for an equality check with Some"
    )

  private def noneCheck(outer: Term, option: Term, negated: Boolean): Patch = {
    val method = if (negated) "isDefined" else "isEmpty"
    lint.replace(outer, s"${option.syntax}.$method", "none-check", s"use $method for a check against None")
  }

  private def negation(negated: Boolean): String = if (negated) "!" else ""

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
    symbol.startsWith("scala/collection/") || symbol.startsWith("scala/Option#") ||
    symbol.startsWith("scala/PartialFunction#")
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

  private val linear =
    Set("scala/collection/immutable/List#", "scala/collection/immutable/Seq#", "scala/collection/Seq#")

  private def sequenceType(term: Term)(using SemanticDocument): Boolean =
    definitions.typeSymbol(term).exists(sequences)

  private def linearSequence(term: Term)(using SemanticDocument): Boolean =
    definitions.typeSymbol(term).exists(linear)

  private def optionType(term: Term)(using SemanticDocument): Boolean =
    definitions.typeSymbol(term).exists(symbol => symbol.endsWith("Option#") || symbol.endsWith("Some#"))
}
