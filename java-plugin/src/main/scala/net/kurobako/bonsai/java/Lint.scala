package net.kurobako.bonsai.java

import com.sun.source.tree.*
import net.kurobako.bonsai.java.Javac.*

object Lint {

  /** The expression under the parentheses and the casts around an expression. */
  @annotation.tailrec
  def unwrapped(tree: ExpressionTree): ExpressionTree = tree match {
    case parenthesized: ParenthesizedTree => unwrapped(parenthesized.getExpression)
    case cast: TypeCastTree               => unwrapped(cast.getExpression)
    case _                                => tree
  }

  /** The tree under the parentheses and the casts around a tree. */
  def unwrapped(tree: Tree): Tree = tree match {
    case expression: ExpressionTree => unwrapped(expression)
    case _                          => tree
  }

  /** The statement a block of only one statement holds, through any blocks around it. */
  @annotation.tailrec
  def single(statement: StatementTree): StatementTree = statement match {
    case block: BlockTree if block.getStatements.size == 1 => single(block.getStatements.get(0))
    case _                                                 => statement
  }

  /** The boolean a statement returns when it is a return of a literal, or a block of only that. */
  def booleanReturn(statement: StatementTree): Option[Boolean] = single(statement) match {
    case ret: ReturnTree =>
      ret.returned.collect {
        case literal: LiteralTree if literal.getKind == Tree.Kind.BOOLEAN_LITERAL =>
          literal.getValue == true
      }
    case _ => None
  }
}

/** A lint of the Java code, which the plugin finds in the code it compiles.
  *
  * The lints are of the code of one file, as the trees the compiler gives are of one compilation; the rules that need
  * the evidence of the whole build are of the analysis, which holds the model of the build.
  */
trait Lint {

  /** The id the lint is reported under, which the analysis and the user name it by. */
  val id: String

  /** The kinds of tree the lint reads; the code is walked once, and the lint is asked about the trees of its kinds. */
  val kinds: Set[Tree.Kind]

  /** Whether the lint runs without the build naming it; a lint that asks a project to choose a style is off by default.
    */
  def enabledByDefault: Boolean = true

  /** What the lint finds in the code of a tree of one of its kinds. */
  def check(context: Context, tree: Tree): Vector[Found]
}
