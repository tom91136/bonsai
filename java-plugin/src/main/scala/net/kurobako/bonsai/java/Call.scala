package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.Trees
import javax.lang.model.element.ExecutableElement

object Call {

  /** The receiver, if any, and the simple name of the method a call names, bare or through a receiver. */
  def selected(method: ExpressionTree): Option[(Option[ExpressionTree], String)] = method match {
    case select: MemberSelectTree   => Some((Some(select.getExpression), select.getIdentifier.toString))
    case identifier: IdentifierTree => Some((None, identifier.getName.toString))
    case _                          => None
  }

  /** Whether the expression names a method of the simple name, called bare or through a receiver. */
  def named(method: ExpressionTree, name: String): Boolean = selected(method).exists(_._2 == name)

  /** Whether the tree is the `this` identifier. */
  def isThis(tree: Tree): Boolean = tree match {
    case identifier: IdentifierTree => identifier.getName.contentEquals("this")
    case _                          => false
  }

  /** Whether the statement is a call of `this(..)`, which hands the work of a constructor to another one. */
  def isThisCall(statement: StatementTree): Boolean = statement match {
    case expression: ExpressionStatementTree =>
      expression.getExpression match {
        case call: MethodInvocationTree => isThis(call.getMethodSelect)
        case _                          => false
      }
    case _ => false
  }

  /** The call a path holds, if it is a call of a method. */
  def of(trees: Trees, path: TreePath): Option[Call] = path.getLeaf match {
    case invocation: MethodInvocationTree =>
      (trees.getElement(path), selected(invocation.getMethodSelect)) match {
        case (method: ExecutableElement, Some((receiver, name))) =>
          Some(Call(path, invocation, method, receiver, name, Lints.arguments(invocation)))
        case _ => None
      }
    case _ => None
  }
}

/** A call of a method, with the receiver and the simple name the code gives the method. */
case class Call(
    path: TreePath,
    invocation: MethodInvocationTree,
    method: ExecutableElement,
    receiver: Option[ExpressionTree],
    name: String,
    arguments: Vector[ExpressionTree]
)
