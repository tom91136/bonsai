package net.kurobako.bonsai.java

import com.sun.source.tree.*
import javax.lang.model.element.ElementKind
import javax.lang.model.element.ExecutableElement
import javax.lang.model.element.Modifier
import javax.lang.model.element.TypeElement

/** The lints of the threading of the Java code, which the code of a type holds.
  *
  * The lints are of the code of one file, as the trees the compiler gives are of one compilation; the rules that need
  * the evidence of the whole build are of the analysis, which holds the model of the build.
  */
object LintThreading {

  /** The lints of the threading. */
  val lints: Vector[Lint] = Vector(SynchronizedOnNonFinal, WaitNotInLoop, BusyWait)

  /** A lock of a field that may be replaced while the code waits for it. */
  private object SynchronizedOnNonFinal extends Lint {
    val id    = JavaLint.SynchronizedOnNonFinal.id
    val kinds = Set(Tree.Kind.SYNCHRONIZED)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val synchronized = tree.asInstanceOf[SynchronizedTree]
      val lock         = synchronized.getExpression
      // A lock of a field that may be replaced is a lock of two things at once.
      val element = LintThreading.expression(lock) match {
        case identifier: IdentifierTree => context.elementOf(identifier)
        case _                          => null
      }
      val replaceable = element != null && element.getKind == ElementKind.FIELD &&
        !element.getModifiers.contains(Modifier.FINAL)
      if replaceable then
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
    val id    = JavaLint.WaitNotInLoop.id
    val kinds = Set(Tree.Kind.METHOD_INVOCATION)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val invocation = tree.asInstanceOf[MethodInvocationTree]
      val method     = invocation.getMethodSelect
      val waits      = Call.named(method, "wait")
      if waits && !context.inLoop then
        Vector(context.found("the wait is not in a loop, so it can miss the notification that wakes the thread"))
      else Vector.empty
    }
  }

  /** A sleep in a loop, which waits for a condition by polling instead of being notified of it. */
  private object BusyWait extends Lint {
    val id    = JavaLint.BusyWait.id
    val kinds = Set(Tree.Kind.METHOD_INVOCATION)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val sleeping = context.element match {
        case method: ExecutableElement =>
          method.getSimpleName.contentEquals("sleep") &&
          Option(method.getEnclosingElement).exists {
            case owner: TypeElement => owner.getQualifiedName.contentEquals("java.lang.Thread")
            case _                  => false
          }
        case _ => false
      }
      if sleeping && context.inLoop then
        Vector(context.found("Thread.sleep in a loop waits by polling; use a condition, latch or blocking queue"))
      else Vector.empty
    }
  }
}
