package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.tree.Tree.Kind
import javax.lang.model.element.Element
import javax.lang.model.element.ElementKind
import javax.lang.model.element.ExecutableElement
import javax.lang.model.element.Modifier
import javax.lang.model.element.TypeElement
import net.kurobako.bonsai.java.Javac.*

/** The lints of the threading of the Java code, which the code of a type holds.
  *
  * The lints are of the code of one file, as the trees the compiler gives are of one compilation; the rules that need
  * the evidence of the whole build are of the analysis, which holds the model of the build.
  */
object LintThreading {

  /** The lints of the threading. */
  val lints: Vector[Lint] = Vector(SynchronizedOnNonFinal, WaitNotInLoop, BusyWait, VolatileReread)

  /** A lock of a field that may be replaced while the code waits for it. */
  private object SynchronizedOnNonFinal extends Lint {
    val id               = LintId.SynchronizedOnNonFinal.id
    val kinds: Set[Kind] = Set(Tree.Kind.SYNCHRONIZED)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val synchronized = tree.asInstanceOf[SynchronizedTree]
      val lock         = synchronized.getExpression
      // A lock of a field that may be replaced is a lock of two things at once.
      val element = LintThreading.expression(lock) match {
        case identifier: IdentifierTree => context.namedElement(identifier)
        case _                          => None
      }
      val replaceable = element.exists(found =>
        found.getKind == ElementKind.FIELD && !found.getModifiers.contains(Modifier.FINAL)
      )
      if (replaceable)
        Vector(context.found("the lock is a field that may be replaced while the code waits for it"))
      else Vector.empty
    }
  }

  /** The expression of the code, which the parentheses of the code hold as a tree of their own. */
  private def expression(tree: Tree): Tree = tree match {
    case parenthesized: ParenthesizedTree => LintThreading.expression(parenthesized.getExpression)
    case _                                => tree
  }

  /** A wait that is not in a loop, which can miss the notification that wakes the thread. */
  private object WaitNotInLoop extends Lint {
    val id               = LintId.WaitNotInLoop.id
    val kinds: Set[Kind] = Set(Tree.Kind.METHOD_INVOCATION)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val invocation = tree.asInstanceOf[MethodInvocationTree]
      val method     = invocation.getMethodSelect
      val waits      = Call.named(method, "wait")
      if (waits && !context.inLoop)
        Vector(context.found("the wait is not in a loop, so it can miss the notification that wakes the thread"))
      else Vector.empty
    }
  }

  /** A sleep in a loop, which waits for a condition by polling instead of being notified of it. */
  private object BusyWait extends Lint {
    val id               = LintId.BusyWait.id
    val kinds: Set[Kind] = Set(Tree.Kind.METHOD_INVOCATION)

    private val Polling = Set("sleep", "onSpinWait")

    def check(context: Context, tree: Tree): Vector[Found] =
      context.element match {
        case method: ExecutableElement
            if Polling(method.getSimpleName.toString) && context.inLoop && method.enclosing.exists {
              case owner: TypeElement => owner.getQualifiedName.contentEquals("java.lang.Thread")
              case _                  => false
            } =>
          Vector(context.found(
            s"Thread.${method.getSimpleName} in a loop waits by polling; use a condition, latch or blocking queue"
          ))
        case _ => Vector.empty
      }
  }

  /** A volatile field read again after its null check, which can see a different value from the check. */
  private object VolatileReread extends Lint {
    val id               = LintId.VolatileReread.id
    val kinds: Set[Kind] = Set(Tree.Kind.EQUAL_TO, Tree.Kind.NOT_EQUAL_TO)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val comparison = tree.asInstanceOf[BinaryTree]
      val field      =
        if (isNull(comparison.getRightOperand)) volatileField(context, comparison.getLeftOperand)
        else if (isNull(comparison.getLeftOperand)) volatileField(context, comparison.getRightOperand)
        else None
      val scope = context.enclosing(Set(Tree.Kind.LAMBDA_EXPRESSION, Tree.Kind.METHOD)).flatMap {
        case lambda: LambdaExpressionTree => Some(lambda.getBody)
        case method: MethodTree           => Option(method.getBody)
        case _                            => None
      }
      val checkEnd = context.trees.getSourcePositions.getEndPosition(context.unit, tree)
      (field, scope) match {
        case (Some(variable), Some(body)) if rereads(context, body, variable, checkEnd) =>
          Vector(context.found(
            s"volatile field '${variable.getSimpleName}' is read again after this null check; copy it to a local first"
          ))
        case _ => Vector.empty
      }
    }

    private def isNull(tree: ExpressionTree): Boolean = Lint.unwrapped(tree).getKind == Tree.Kind.NULL_LITERAL

    private def volatileField(context: Context, candidate: ExpressionTree): Option[Element] =
      Option(context.elementOf(Lint.unwrapped(candidate)))
        .filter(element => element.getKind == ElementKind.FIELD && element.getModifiers.contains(Modifier.VOLATILE))

    private def rereads(context: Context, scope: Tree, field: Element, checkEnd: Long): Boolean = {
      val assigned  = Scan.collect(scope, _ => true) { case assignment: AssignmentTree => assignment.getVariable }
      val positions = context.trees.getSourcePositions
      Scan.exists(scope) {
        case node @ (_: IdentifierTree | _: MemberSelectTree) =>
          !assigned.exists(_ eq node) && context.elementOf(node) == field &&
          positions.getStartPosition(context.unit, node) > checkEnd
      }
    }
  }
}
