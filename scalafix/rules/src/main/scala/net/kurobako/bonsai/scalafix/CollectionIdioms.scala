package net.kurobako.bonsai.scalafix

import scala.meta.*
import scala.meta.internal.semanticdb.Scala.*
import scala.meta.tokens.Token
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
    "toVector" -> "scala/collection/immutable/Vector#",
    "toMap"    -> "scala/collection/immutable/Map#",
    "toSet"    -> "scala/collection/immutable/Set#"
  )

  override def fix(using document: SemanticDocument): Patch = document.tree.collect {
    case call: Term.Apply => rewrite(call) + filterBeforeSorted(call) + emptyCollection(call) + arrayPrint(call)
    case comparison: Term.ApplyInfix => nonEmpty(comparison) + tautology(comparison)
    case selection: Term.Select      => redundantConversion(selection) + sizeToLength(selection)
  }.asPatch

  private def standardMethod(selection: Term.Select)(using SemanticDocument): Boolean =
    selection.symbol.value.startsWith("scala/collection/")

  private def strictSequence(term: Term)(using SemanticDocument): Boolean =
    definitions.typeSymbol(term).exists(strictSequences)

  private def rewrite(call: Term.Apply)(using SemanticDocument): Patch =
    (call.fun, call.argClause.values) match {
      case (selection @ Term.Select(receiver, Term.Name(method @ ("exists" | "forall"))), List(predicate))
          if standardMethod(selection) && strictSequence(receiver) =>
        constantTarget(predicate, if (method == "exists") "==" else "!=").map { target =>
          val replacement = s"${receiver.syntax}.contains(${target.syntax})"
          lint.replace(
            call,
            if (method == "exists") replacement else s"!($replacement)",
            if (method == "exists") "exists-to-contains" else "forall-to-not-contains",
            "use contains for equality with a constant"
          )
        }.getOrElse(Patch.empty)

      case (filter @ Term.Select(receiver, Term.Name("filter")), List(argument))
          if standardMethod(filter) && setLike(receiver) =>
        containsTarget(argument).map { target =>
          Patch.replaceTree(call, s"${receiver.syntax}.intersect(${target.syntax})") +
            lint("filter-contains-to-intersect", "use intersect instead of filter and contains", call.pos)
        }.getOrElse(Patch.empty)

      case (outer @ Term.Select(inner: Term.Apply, Term.Name("getOrElse")), List(default))
          if outer.symbol.value.startsWith("scala/Option#getOrElse") =>
        (inner.fun, inner.argClause.values) match {
          case (get @ Term.Select(receiver, Term.Name("get")), List(key))
              if standardMethod(get) && definitions.typeSymbol(receiver).exists(strictMaps) =>
            lint.replace(
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
            lint.replace(
              call,
              s"${receiver.syntax}.slice($from, ${from + length})",
              "drop-take-to-slice",
              "use slice for constant nonnegative bounds"
            )
          case _ => Patch.empty
        }

      case (drop @ Term.Select(inner: Term.Apply, Term.Name("drop")), List(Lit.Int(from)))
          if from >= 0 && standardMethod(drop) =>
        (inner.fun, inner.argClause.values) match {
          case (take @ Term.Select(receiver, Term.Name("take")), List(Lit.Int(length)))
              if length >= 0 && standardMethod(take) && strictSequence(receiver) =>
            lint.replace(
              call,
              s"${receiver.syntax}.slice($from, $length)",
              "take-drop-to-slice",
              "use slice for constant nonnegative bounds"
            )
          case _ => Patch.empty
        }

      case (drop @ Term.Select(receiver, Term.Name("drop")), List(Lit.Int(0)))
          if standardMethod(drop) && strictSequence(receiver) =>
        lint.replace(call, receiver.syntax, "drop-zero", "drop(0) returns the collection itself")

      case (take @ Term.Select(receiver, Term.Name("take")), List(Lit.Int(0)))
          if standardMethod(take) && strictSequence(receiver) =>
        lint("take-zero", "take(0) is always empty; return an empty collection or check the bound", call.pos)

      case _ => Patch.empty
    }

  private def constantTarget(predicate: Term, operator: String)(using SemanticDocument): Option[Term] = {
    def comparison(body: Term, parameter: Term => Boolean): Option[Term] = body match {
      case infix: Term.ApplyInfix if infix.op.value == operator && parameter(infix.lhs) =>
        infix.argClause.values match {
          // Null and custom equality are deliberately excluded. Literals and stable references cannot capture the
          // predicate parameter, throw, or introduce extra evaluations on an empty input.
          case List(value @ (_: Lit.Int | _: Lit.Long | _: Lit.String | _: Lit.Char | _: Lit.Boolean)) => Some(value)
          case List(value) if stableReference(value)                                                   => Some(value)
          case _                                                                                       => None
        }
      case _ => None
    }
    predicate match {
      case Term.AnonymousFunction(body) => comparison(body, { case _: Term.Placeholder => true; case _ => false })
      case function: Term.Function      => function.paramClause.values match {
          case List(parameter) =>
            comparison(
              function.body,
              {
                case name: Term.Name => name.value == parameter.name.value
                case _               => false
              }
            ).filterNot(value => mentions(value, parameter.name.value))
          case _ => None
        }
      case _ => None
    }
  }

  /** Whether the term names the given name, which a value moved out of the predicate cannot. */
  private def mentions(term: Term, name: String): Boolean =
    term.collect { case Term.Name(`name`) => () }.nonEmpty

  /** The set whose membership the predicate tests, when the predicate is a bare `contains` call or a lambda around one.
    */
  private def containsTarget(predicate: Term)(using SemanticDocument): Option[Term] =
    predicate match {
      case Term.Select(collection, Term.Name("contains")) if setLike(collection) =>
        Some(collection)
      case function: Term.Function =>
        function.paramClause.values match {
          case List(parameter: Term.Param) =>
            (parameter.name, function.body) match {
              case (
                    name: Term.Name,
                    Term.Apply(
                      Term.Select(collection, Term.Name("contains")),
                      List(Term.Name(argument))
                    )
                  ) if argument == name.value && setLike(collection) =>
                Some(collection)
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }

  /** Whether the term is a set, including a set built by `Set(...)`, whose static type is the method's result. */
  private def setLike(term: Term)(using SemanticDocument): Boolean =
    definitions.typeSymbol(term).contains("scala/collection/immutable/Set#") ||
      (term match {
        case Term.Apply(fun, _) => fun.syntax == "Set" || fun.syntax == "scala.collection.immutable.Set"
        case _                  => false
      })

  /** Whether the term is a reference the predicate evaluates the same way on every element, without side effects. */
  private def stableReference(term: Term)(using SemanticDocument): Boolean = term match {
    case name: Term.Name              => !resolvedMethod(name.symbol)
    case _: Term.This                 => true
    case Term.Select(qualifier, name) =>
      !resolvedMethod(name.symbol) &&
      (qualifier match {
        case _: Term.Name | _: Term.This => true
        case select: Term.Select         => stableReference(select)
        case _                           => false
      })
    case _ => false
  }

  private def resolvedMethod(symbol: Symbol): Boolean = symbol.isGlobal && symbol.value.desc.isMethod

  private def nonEmpty(comparison: Term.ApplyInfix)(using SemanticDocument): Patch =
    (comparison.op.value, comparison.lhs, comparison.argClause.values) match {
      case (operator, size @ Term.Select(_, Term.Name("size" | "length")), List(Lit.Int(0)))
          if definitions.zeroComparisons(operator) && sizeMethod(size) =>
        val empty        = operator == "=="
        val (method, id) = if (empty) ("isEmpty", "size-to-is-empty") else ("nonEmpty", "size-to-non-empty")
        filtered(size.qual).fold(
          emptiness(comparison, size, method, id, s"use $method instead of a size comparison with zero")
        ) { (receiver, predicate, negated) =>
          val quantifier = if (negated) "forall" else "exists"
          lint.replace(
            comparison,
            s"${if (empty != negated) "!" else ""}${receiver.syntax}.$quantifier(${predicate.syntax})",
            id,
            s"use $quantifier instead of ${if (negated) "filterNot" else "filter"} and a size comparison with zero"
          )
        }
      case _ => Patch.empty
    }

  private def filtered(term: Term)(using SemanticDocument): Option[(Term, Term, Boolean)] = term match {
    case call: Term.Apply =>
      (call.fun, call.argClause.values) match {
        case (filter @ Term.Select(receiver, Term.Name(method @ ("filter" | "filterNot"))), List(predicate))
            if standardMethod(filter) =>
          Some((receiver, predicate, method == "filterNot"))
        case _ => None
      }
    case _ => None
  }

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
    } && operands(chain, operator).flatMap(emptinessTest(_)).filter((receiver, _) => stable(receiver))
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

  private def emptiness(
      comparison: Term.ApplyInfix,
      size: Term.Select,
      replacement: String,
      id: String,
      message: String
  ): Patch =
    lint(id, message, comparison.pos) +
      Patch.replaceTree(size.name, replacement) +
      Patch.removeTokens(comparison.tokens.drop(size.tokens.length))

  // Array and String sizes come through implicit conversions; length is their own method. A comparison with zero is
  // rewritten to isEmpty or nonEmpty instead, so the two rewrites never overlap.
  private def sizeToLength(selection: Term.Select)(using SemanticDocument): Patch = selection match {
    case Term.Select(receiver, Term.Name("size"))
        if standardMethod(selection) && !definitions.zeroComparison(selection) &&
          definitions.typeSymbol(receiver).exists(symbol => symbol == "scala/Array#" || symbol.endsWith("String#")) =>
      lint.replace(selection, s"${receiver.syntax}.length", "size-to-length", "use length on an array or string")
    case _ => Patch.empty
  }

  private def arrayType(term: Term)(using SemanticDocument): Boolean =
    definitions.typeSymbol(term).contains("scala/Array#")

  // An array printed directly renders its identity, not its elements.
  private def arrayPrint(call: Term.Apply)(using SemanticDocument): Patch = call.fun match {
    case print @ Term.Name("println" | "print") if printing(print) && call.argClause.values.exists(arrayType(_)) =>
      lint("array-to-string", "printing an array renders its identity; use mkString or print its elements", call.pos)
    case _ => Patch.empty
  }

  private def printing(name: Term.Name)(using SemanticDocument): Boolean = {
    val symbol = name.symbol.value
    symbol.startsWith("scala/Predef.print") || symbol.startsWith("scala/Console.print")
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
          (identityConversion(conversion, receiver) ||
            (conversion == "toSeq" && strictSequence(receiver) && assertionArgument(selection))) =>
      lint.replace(
        selection,
        receiver.syntax,
        "redundant-collection-conversion",
        "remove the identity collection conversion"
      )
    case _ => Patch.empty
  }

  private def identityConversion(conversion: String, receiver: Term)(using SemanticDocument): Boolean =
    identityConversions.get(conversion).exists(definitions.typeSymbol(receiver).contains) ||
      (conversion == "toMap" && mapAppend(receiver)) ||
      (conversion == "toSet" && setAppend(receiver))

  /** Whether appending another collection to a map or a set leaves the same kind of collection. */
  private def mapAppend(term: Term)(using SemanticDocument): Boolean = term match {
    case infix: Term.ApplyInfix if infix.op.value == "++" => definitions.typeSymbol(infix.lhs).exists(strictMaps)
    case _                                                => false
  }

  private def setAppend(term: Term)(using SemanticDocument): Boolean = term match {
    case infix: Term.ApplyInfix if infix.op.value == "++" =>
      definitions.typeSymbol(infix.lhs).exists(_ == "scala/collection/immutable/Set#")
    case _ => false
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
      val companion = call.fun match {
        case application: Term.ApplyType => application.fun
        case other                       => other
      }
      companion match {
        case reference: (Term.Name | Term.Select)
            if reference.symbol.value.startsWith("scala/") && reference.symbol.isGlobal &&
              reference.symbol.value.endsWith(".") && companions(reference.symbol.displayName) =>
          lint("empty-collection", "use empty for an empty collection", call.pos) +
            Patch.addLeft(call.tokens(companion.tokens.length), ".empty") + Patch.removeTokens(call.argClause.tokens)
        case _ => Patch.empty
      }
    }
}
