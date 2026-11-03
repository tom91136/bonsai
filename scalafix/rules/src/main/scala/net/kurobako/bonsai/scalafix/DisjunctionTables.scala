package net.kurobako.bonsai.scalafix

import scala.annotation.tailrec
import scala.collection.mutable
import scala.meta.*
import scalafix.v1.*

/** Boolean expressions that spell out a table. Six or more equality tests against constants over at most two operands
  * are a lookup in a `Set`, or in a `Map` of `Set`s for two operands. Four or more `||` operands that are the same call
  * differing in one constant argument are an `exists` over the constants. A disjunction of `==` tests of one operand
  * against literals of its own type is replaced by a `Set` lookup.
  */
class DisjunctionTables extends SemanticRule("DisjunctionTables") {
  private val EqualityTests = 6
  private val OperandLimit  = 2
  private val CallVariants  = 4

  private val combinators = Set("||", "&&")

  private case class Test(operand: Term, constant: Term, infix: Boolean)

  override def fix(using document: SemanticDocument): Patch = {
    val expressions = document.tree.collect { case tree: Term if combinator(tree) && !combined(tree) => tree }
    val tables      = expressions.flatMap(table(_))
    val reported    = tables.map(_._1)
    val calls       = document.tree.collect {
      case chain: Term.ApplyInfix if disjunction(chain) && !disjoined(chain) && !within(chain, reported) => chain
    }
    tables.map(_._2).asPatch + calls.flatMap(repeated(_)).asPatch
  }

  private def table(expression: Term)(using SemanticDocument): Option[(Tree, Patch)] = {
    val leaves = flatten(expression)
    val tests  = leaves.flatMap(test(_))
    Option.when(tests.size >= EqualityTests && tests.map(_.operand.syntax).distinct.size <= OperandLimit) {
      val message = s"${tests.size} equality tests against constants; look the value up in a Set or a Map of Sets"
      val patch   = lookup(expression, leaves, tests) match {
        case Some(replacement) => lint.replace(expression, replacement, "equality-disjunction", message)
        case None              => lint("equality-disjunction", message, expression.pos)
      }
      expression -> patch
    }
  }

  private def lookup(expression: Term, leaves: List[Term], tests: List[Test])(using SemanticDocument): Option[String] =
    if (!onlyDisjunctions(expression) || tests.size != leaves.size || !tests.forall(_.infix)) None
    else {
      val operand = tests.head.operand
      val types   = tests.map(test => literalType(test.constant))
      Option.when(
        tests.map(_.operand.syntax).distinct.size == 1 && types.distinct.size == 1 &&
          types.head.exists(expected => definitions.typeSymbol(operand).exists(expected))
      )(s"Set(${tests.map(_.constant.syntax).mkString(", ")}).contains(${operand.syntax})")
    }

  private def literalType(constant: Term): Option[Set[String]] = constant match {
    case _: Lit.String => Some(Set("java/lang/String#", "scala/Predef.String#"))
    case _: Lit.Char   => Some(Set("scala/Char#"))
    case _: Lit.Int    => Some(Set("scala/Int#"))
    case _             => None
  }

  private def repeated(chain: Term.ApplyInfix): Option[Patch] = {
    val operands = disjuncts(chain)
    Option.when(operands.size >= CallVariants && operands.forall(call(_)) && variesInOneConstant(operands)) {
      lint(
        "repeated-call-disjunction",
        s"${operands.size} calls differ in one constant; test the constants with exists over a collection",
        chain.pos
      )
    }
  }

  private def call(term: Term): Boolean = term match {
    case _: Term.Apply => test(term).isEmpty
    case _             => false
  }

  private def variesInOneConstant(operands: List[Term]): Boolean = {
    val alignments = operands.tail.map(align(operands.head, _))
    alignments.forall(_.isDefined) && {
      val varying = alignments.flatten.transpose.filter(_.exists((first, other) => first != other))
      varying match {
        case List(column) => (column.head._1 :: column.map(_._2)).distinct.size == operands.size
        case _            => false
      }
    }
  }

  private def align(left: Term, right: Term): Option[List[(String, String)]] = {
    val (leftShape, leftHoles)   = shape(left)
    val (rightShape, rightHoles) = shape(right)
    Option.when(leftShape == rightShape)(leftHoles.zip(rightHoles))
  }

  // Literals are holes, so a placeholder in a shape always marks a hole.
  private def shape(term: Term): (String, List[String]) = {
    val holes    = mutable.Buffer.empty[String]
    val skeleton = term.transform {
      case hole: Term if isHole(hole) =>
        holes += hole.syntax
        Lit.Unit()
    }
    skeleton.structure -> holes.toList
  }

  private def isHole(term: Term): Boolean = term match {
    case Lit.Null() => false
    case _: Lit     => true
    case _          => constant(term) && term.parent.exists(_.isInstanceOf[Term.ArgClause])
  }

  private def test(leaf: Term): Option[Test] = leaf match {
    case infix: Term.ApplyInfix if infix.op.value == "==" && infix.argClause.values.size == 1 =>
      val other = infix.argClause.values.head
      if (constant(other) && !constant(infix.lhs)) Some(Test(infix.lhs, other, true))
      else if (constant(infix.lhs) && !constant(other)) Some(Test(other, infix.lhs, true))
      else None
    case application: Term.Apply =>
      (application.fun, application.argClause.values) match {
        case (Term.Select(receiver, Term.Name("equals" | "startsWith")), List(argument))
            if constant(argument) && !constant(receiver) =>
          Some(Test(receiver, argument, false))
        case _ => None
      }
    case _ => None
  }

  private def constant(term: Term): Boolean = term match {
    case Lit.Null()                              => false
    case _: Lit                                  => true
    case Term.Name(name)                         => capitalised(name)
    case Term.Select(qualifier, Term.Name(name)) => capitalised(name) && stable(qualifier)
    case _                                       => false
  }

  private def capitalised(name: String): Boolean = name.headOption.exists(_.isUpper)

  @tailrec
  private def stable(term: Term): Boolean = term match {
    case _: Term.Name | _: Term.This => true
    case Term.Select(qualifier, _)   => stable(qualifier)
    case _                           => false
  }

  private def combinator(tree: Tree): Boolean = tree match {
    case infix: Term.ApplyInfix             => combinators(infix.op.value) && infix.argClause.values.size == 1
    case Term.ApplyUnary(Term.Name("!"), _) => true
    case _                                  => false
  }

  private def disjunction(tree: Tree): Boolean = tree match {
    case infix: Term.ApplyInfix => infix.op.value == "||" && infix.argClause.values.size == 1
    case _                      => false
  }

  private def operandParent(tree: Tree): Option[Tree] = tree.parent.flatMap {
    case clause: Term.ArgClause => clause.parent
    case parent                 => Some(parent)
  }

  private def combined(tree: Tree): Boolean = operandParent(tree).exists(combinator(_))

  private def disjoined(tree: Tree): Boolean = operandParent(tree).exists(disjunction(_))

  private def within(tree: Tree, trees: List[Tree]): Boolean =
    trees.exists(other => (other eq tree) || ancestry.ancestors(tree).exists(_ eq other))

  private def flatten(term: Term): List[Term] = term match {
    case infix: Term.ApplyInfix if combinator(infix) => flatten(infix.lhs) ++ flatten(infix.argClause.values.head)
    case Term.ApplyUnary(Term.Name("!"), operand)    => flatten(operand)
    case other                                       => List(other)
  }

  private def disjuncts(term: Term): List[Term] = term match {
    case infix: Term.ApplyInfix if disjunction(infix) => disjuncts(infix.lhs) ++ disjuncts(infix.argClause.values.head)
    case other                                        => List(other)
  }

  private def onlyDisjunctions(term: Term): Boolean = term match {
    case infix: Term.ApplyInfix if combinator(infix) =>
      disjunction(infix) && onlyDisjunctions(infix.lhs) && onlyDisjunctions(infix.argClause.values.head)
    case Term.ApplyUnary(Term.Name("!"), _) => false
    case _                                  => true
  }
}
