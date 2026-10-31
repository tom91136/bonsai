package net.kurobako.bonsai.scalafix

import scala.meta.*
import scalafix.v1.*

/** Elements of a sequence read from its end by index while iterating its indices, as `xs(xs.size - 1 - i)` over
  * `xs.indices`. Iterating `reverseIterator` reads the same elements without the arithmetic or a reversed copy. A body
  * that also reads `xs(i)` pairs both ends and is left alone, as are arrays, which hot code indexes to avoid iterators.
  */
class ReversedIndexing extends SemanticRule("ReversedIndexing") {
  override def fix(using document: SemanticDocument): Patch = document.tree.collect {
    case call: Term.Apply if call.argClause.values.sizeIs == 1 && !definitions.arrayType(call.fun) =>
      fromEnd(call.argClause.values.head, call.fun).filter(iterated(call, call.fun, _)).map(_ =>
        lint(
          "reversed-indexing",
          s"${call.fun.syntax} is read from its end by index while its indices are iterated; iterate " +
            s"${call.fun.syntax}.reverseIterator, which copies nothing, and keep the index only when the position " +
            "matters",
          call.pos
        )
      )
  }.flatten.asPatch

  private def same(left: Tree, right: Tree): Boolean = left.structure == right.structure

  private def difference(term: Term): Option[(Term, Term)] = term match {
    case infix: Term.ApplyInfix if infix.op.value == "-" && infix.argClause.values.sizeIs == 1 =>
      Some(infix.lhs -> infix.argClause.values.head)
    case _ => None
  }

  private def sizeOf(term: Term, collection: Term): Boolean = term match {
    case Term.Select(owner, Term.Name("size" | "length")) => same(owner, collection)
    case _                                                => false
  }

  private def one(term: Term): Boolean = term match {
    case Lit.Int(1) => true
    case _          => false
  }

  private def name(term: Term): Option[String] = term match {
    case Term.Name(value) => Some(value)
    case _                => None
  }

  // The variable of `xs.size - 1 - i`, `xs.size - i - 1` and `xs.size - (i + 1)`.
  private def fromEnd(index: Term, collection: Term): Option[String] = difference(index).flatMap { (inner, last) =>
    difference(inner) match {
      case Some((size, offset)) if sizeOf(size, collection) && one(offset) => name(last)
      case Some((size, offset)) if sizeOf(size, collection) && one(last)   => name(offset)
      case _                                                               =>
        last match {
          case plus: Term.ApplyInfix if sizeOf(inner, collection) && plus.op.value == "+" =>
            (plus.lhs, plus.argClause.values) match {
              case (variable, List(offset)) if one(offset) => name(variable)
              case _                                       => None
            }
          case _ => None
        }
    }
  }

  private def indices(range: Term, collection: Term): Boolean = range match {
    case Term.Select(owner, Term.Name("indices")) => same(owner, collection)
    case infix: Term.ApplyInfix                   =>
      (infix.lhs, infix.op.value, infix.argClause.values) match {
        case (Lit.Int(0), "until", List(end)) => sizeOf(end, collection)
        case (Lit.Int(0), "to", List(end))    =>
          difference(end).exists((size, offset) => sizeOf(size, collection) && one(offset))
        case _ => false
      }
    case _ => false
  }

  private def parameters(argument: Term): List[String] = argument match {
    case function: Term.Function                   => function.paramClause.values.map(_.name.value)
    case Term.Block(List(function: Term.Function)) => function.paramClause.values.map(_.name.value)
    case _                                         => Nil
  }

  // Whether `variable` takes the indices of `collection`, in a lambda or a generator around `call`, whose body reads no
  // element at the variable itself.
  private def iterated(call: Term.Apply, collection: Term, variable: String): Boolean =
    ancestry.ancestors(call).exists {
      case outer: Term.Apply =>
        outer.fun match {
          case Term.Select(range, _) =>
            indices(range, collection) && outer.argClause.values.exists(parameters(_).contains(variable)) &&
            unpaired(outer, collection, variable)
          case _ => false
        }
      case loop: Term.For =>
        generated(loop.enumsBlock.enums, collection, variable) && unpaired(loop.body, collection, variable)
      case loop: Term.ForYield =>
        generated(loop.enumsBlock.enums, collection, variable) && unpaired(loop.body, collection, variable)
      case _ => false
    }

  private def generated(enumerators: List[Enumerator], collection: Term, variable: String): Boolean =
    enumerators.exists {
      case generator: Enumerator.Generator =>
        generator.pat match {
          case Pat.Var(Term.Name(bound)) => bound == variable && indices(generator.rhs, collection)
          case _                         => false
        }
      case _ => false
    }

  private def unpaired(scope: Tree, collection: Term, variable: String): Boolean = scope.collect {
    case read: Term.Apply if same(read.fun, collection) && read.argClause.values.exists(name(_).contains(variable)) =>
      read
  }.isEmpty
}
