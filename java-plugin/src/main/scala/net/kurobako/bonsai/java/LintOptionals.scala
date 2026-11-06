package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.tree.Tree.Kind
import javax.lang.model.`type`.DeclaredType
import javax.lang.model.`type`.TypeKind
import javax.lang.model.element.TypeElement
import net.kurobako.bonsai.java.Javac.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** The lints of the `Optional` values of the Java code.
  *
  * An optional that the code opens with `get` without checking it is present fails at run time, which the first lint
  * reports. The second reports the checks that read as a branch and can be one functional expression instead.
  */
object LintOptionals {

  val lints: Vector[Lint] = Vector(Get, IsPresent)

  private[java] val Optionals = Set(
    "java.util.Optional",
    "java.util.OptionalInt",
    "java.util.OptionalLong",
    "java.util.OptionalDouble"
  )

  private val Getters = Set("get", "getAsInt", "getAsLong", "getAsDouble")

  /** A call of `Optional.get()` that the code does not guard with a check that the value is present. */
  private object Get extends Lint {
    val id               = LintId.OptionalGet.id
    val kinds: Set[Kind] = Set(Tree.Kind.METHOD_INVOCATION)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val invocation = tree.asInstanceOf[MethodInvocationTree]
      receiver(context, invocation, Getters) match {
        case None           => Vector.empty
        case Some(receiver) =>
          if (guarded(context, receiver.toString)) Vector.empty
          else Vector(context.found(s"$invocation is called without checking that the optional is present"))
      }
    }
  }

  /** An `isPresent` check whose branch calls `get`, which a functional expression states in one step. */
  private object IsPresent extends Lint {
    val id               = LintId.OptionalIsPresent.id
    val kinds: Set[Kind] = Set(Tree.Kind.METHOD_INVOCATION)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val invocation = tree.asInstanceOf[MethodInvocationTree]
      receiver(context, invocation, Set("isPresent", "isEmpty")).flatMap { receiver =>
        val present = invocation.getMethodSelect.asInstanceOf[MemberSelectTree].getIdentifier.contentEquals("isPresent")
        context.ancestors.collectFirst {
          case ifTree: IfTree
              if within(context, ifTree.getCondition) &&
                !leavesLoop(ifTree.getThenStatement) &&
                !ifTree.elseStatement.exists(leavesLoop(_)) =>
            (ifTree.getThenStatement, ifTree.elseStatement)
          case conditional: ConditionalExpressionTree if within(context, conditional.getCondition) =>
            (conditional.getTrueExpression, Some(conditional.getFalseExpression))
        }.flatMap { (whenTrue, whenFalse) =>
          val calledWhenTrue  = called(whenTrue, receiver.toString)
          val calledWhenFalse = whenFalse.exists(called(_, receiver.toString))
          val target          = if (present) calledWhenTrue else calledWhenFalse
          Option.when(target) {
            context.found(
              s"$invocation with ${getter(context, receiver.toString)} can be one functional expression"
            )
          }
        }
      }.toVector
    }
  }

  /** The receiver of a call of one of the methods, when it is an optional. */
  private def receiver(context: Context, invocation: MethodInvocationTree, methods: Set[String]): Option[Tree] =
    invocation.getMethodSelect match {
      case select: MemberSelectTree
          if methods(select.getIdentifier.toString) && invocation.getArguments.isEmpty &&
            optional(context, select.getExpression) =>
        Some(select.getExpression)
      case _ => None
    }

  /** Whether the type of the tree is one of the optionals of the standard library. */
  private def optional(context: Context, tree: Tree): Boolean =
    context.mirrorOf(tree)
      .flatMap {
        case declared: DeclaredType if declared.getKind == TypeKind.DECLARED =>
          declared.asElement match {
            case element: TypeElement => Option(element.getQualifiedName.toString)
            case _                    => None
          }
        case _ => None
      }
      .exists(Optionals)

  /** Whether the expression checks that the optional whose text is `value` is present, in the branch the code is in. */
  private def guarded(context: Context, value: String): Boolean =
    context.ancestors.exists {
      case ifTree: IfTree =>
        (present(ifTree.getCondition, true, value) && within(context, ifTree.getThenStatement)) ||
        (present(ifTree.getCondition, false, value) && ifTree.elseStatement.exists(within(context, _)))
      case conditional: ConditionalExpressionTree =>
        (present(conditional.getCondition, true, value) && within(context, conditional.getTrueExpression)) ||
        (present(conditional.getCondition, false, value) && within(context, conditional.getFalseExpression))
      case binary: BinaryTree if binary.getKind == Tree.Kind.CONDITIONAL_AND =>
        present(binary.getLeftOperand, true, value) && within(context, binary.getRightOperand)
      case binary: BinaryTree if binary.getKind == Tree.Kind.CONDITIONAL_OR =>
        present(binary.getLeftOperand, false, value) && within(context, binary.getRightOperand)
      case _ => false
    } || earlyExit(context, value)

  /** Whether the expression, once it evaluated to `outcome`, shows that the optional whose text is `value` is present.
    */
  private def present(expression: ExpressionTree, outcome: Boolean, value: String): Boolean = expression match {
    case parenthesized: ParenthesizedTree => present(parenthesized.getExpression, outcome, value)
    case unary: UnaryTree if unary.getKind == Tree.Kind.LOGICAL_COMPLEMENT =>
      present(unary.getExpression, !outcome, value)
    case binary: BinaryTree if binary.getKind == Tree.Kind.CONDITIONAL_AND && outcome =>
      present(binary.getLeftOperand, true, value) || present(binary.getRightOperand, true, value)
    case binary: BinaryTree if binary.getKind == Tree.Kind.CONDITIONAL_OR && !outcome =>
      present(binary.getLeftOperand, false, value) || present(binary.getRightOperand, false, value)
    case invocation: MethodInvocationTree =>
      invocation.getMethodSelect match {
        case select: MemberSelectTree if invocation.getArguments.isEmpty && select.getExpression.toString == value =>
          select.getIdentifier.toString match {
            case "isPresent" => outcome
            case "isEmpty"   => !outcome
            case _           => false
          }
        case _ => false
      }
    case _ => false
  }

  /** Whether a statement before the code checks that the optional is present and leaves the method when it is not. */
  private def earlyExit(context: Context, value: String): Boolean =
    context.ancestors.collectFirst { case block: BlockTree => block }.exists { block =>
      val statements = block.getStatements.asScala.toVector
      val start      = position(context, context.tree).map(_._1).getOrElse(-1L)
      val index      = statements.indexWhere { statement =>
        val range = position(context, statement)
        range.exists((from, to) => from <= start && start < to)
      }
      if (index < 0) false
      else
        statements.take(index).exists {
          case ifTree: IfTree if ifTree.elseStatement.isEmpty =>
            present(ifTree.getCondition, false, value) && exits(ifTree.getThenStatement)
          case _ => false
        }
    }

  /** Whether the statement always leaves the code it is in. */
  private def exits(statement: StatementTree): Boolean = statement match {
    case _: ReturnTree | _: ThrowTree | _: BreakTree | _: ContinueTree => true
    case block: BlockTree => block.getStatements.asScala.lastOption.exists(exits(_))
    case ifTree: IfTree   =>
      ifTree.elseStatement.exists(other => exits(ifTree.getThenStatement) && exits(other))
    case _ => false
  }

  /** Whether the branch leaves the loop, which a functional expression of the optional cannot do. */
  private def leavesLoop(branch: StatementTree): Boolean =
    Scan.exists(branch) {
      case exit: BreakTree    => exit.label.nonEmpty
      case exit: ContinueTree => exit.label.nonEmpty
    } ||
      Scan.exists(branch, tree => !isLoop(tree) && !isSwitch(tree)) { case _: BreakTree => true } ||
      Scan.exists(branch, tree => !isLoop(tree)) { case _: ContinueTree => true }

  private def isLoop(tree: Tree): Boolean = tree match {
    case _: ForLoopTree | _: EnhancedForLoopTree | _: WhileLoopTree | _: DoWhileLoopTree => true
    case _                                                                               => false
  }

  private def isSwitch(tree: Tree): Boolean = tree match {
    case _: SwitchTree | _: SwitchExpressionTree => true
    case _                                       => false
  }

  /** Whether the code of the branch calls a getter of the optional whose text is `value`. */
  private def called(branch: Tree, value: String): Boolean =
    Scan.exists(branch) {
      case node: MethodInvocationTree =>
        node.getMethodSelect match {
          case select: MemberSelectTree =>
            Getters(select.getIdentifier.toString) && node.getArguments.isEmpty &&
            select.getExpression.toString == value
          case _ => false
        }
    }

  /** The getter the branch calls, which the detail names. */
  private def getter(context: Context, value: String): String =
    context.ancestors.view.collectFirst {
      case method: MethodTree =>
        val names = mutable.ArrayBuffer.empty[String]
        Scan.foreach(method) {
          case node: MethodInvocationTree =>
            node.getMethodSelect match {
              case select: MemberSelectTree
                  if Getters(select.getIdentifier.toString) && select.getExpression.toString == value =>
                names += s"${select.getIdentifier}()"
              case _ => ()
            }
        }
        names.lastOption.getOrElse("get()")
    }.getOrElse("get()")

  /** Whether the source of the code lies within the source of the tree. */
  private def within(context: Context, tree: Tree): Boolean =
    position(context, context.tree).exists { (from, to) =>
      position(context, tree).exists((start, end) => from >= start && to <= end)
    }

  private def position(context: Context, tree: Tree): Option[(Long, Long)] = {
    val positions = context.trees.getSourcePositions
    val start     = positions.getStartPosition(context.unit, tree)
    val end       = positions.getEndPosition(context.unit, tree)
    Option.when(start >= 0 && end >= 0)((start, end))
  }
}
