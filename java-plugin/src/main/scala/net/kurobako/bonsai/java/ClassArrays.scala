package net.kurobako.bonsai.java

import com.sun.source.tree.*
import javax.lang.model.`type`.*
import javax.lang.model.element.*
import scala.jdk.CollectionConverters.*

/** Evaluates the arrays of class literals that code builds to name a reflected signature, through the local variables
  * that hold them and the helper methods of the build that extend them.
  *
  * The evaluation runs the statements of a helper over values it knows: arrays of classes, a class, a length. It gives
  * up, and the signature stays unknown, at any statement it does not read.
  */
private[java] final class ClassArrays(context: Context) {
  import ClassArrays.*
  import Known.*

  private val Depth = 4

  /** The classes the expression lists, when every one of them is known. */
  def classes(tree: ExpressionTree): Option[Vector[TypeMirror]] =
    evaluate(tree, Map.empty, 0).collect { case Items(items) => items }.filter(_.forall(_.isDefined)).map(_.flatten)

  private def evaluate(tree: ExpressionTree, env: Env, depth: Int): Option[Known] =
    if depth > Depth then None
    else
      tree match {
        case parenthesized: ParenthesizedTree => evaluate(parenthesized.getExpression, env, depth)
        case literal: LiteralTree             =>
          literal.getValue match {
            case number: java.lang.Integer => Some(Count(number))
            case _                         => None
          }
        case select: MemberSelectTree if select.getIdentifier.contentEquals("class") =>
          classOf(tree).map(Item(_))
        case select: MemberSelectTree if select.getIdentifier.contentEquals("length") =>
          evaluate(select.getExpression, env, depth).collect { case Items(items) => Count(items.size) }
        case binary: BinaryTree if binary.getKind == Tree.Kind.PLUS || binary.getKind == Tree.Kind.MINUS =>
          for {
            left  <- evaluate(binary.getLeftOperand, env, depth)
            right <- evaluate(binary.getRightOperand, env, depth)
            sum   <- (left, right) match {
              case (Count(a), Count(b)) => Some(Count(if binary.getKind == Tree.Kind.PLUS then a + b else a - b))
              case _                    => None
            }
          } yield sum
        case array: NewArrayTree =>
          if array.getInitializers != null then {
            val items = array.getInitializers.asScala.toVector.map(evaluate(_, env, depth))
            Option.when(items.forall(_.exists(_.isInstanceOf[Item])))(
              Items(items.flatMap(_.collect { case Item(tpe) => Some(tpe) }))
            )
          } else
            array.getDimensions.asScala.toVector match {
              case Vector(size) =>
                evaluate(size, env, depth).collect { case Count(count) => Items(Vector.fill(count)(None)) }
              case _ => None
            }
        case access: ArrayAccessTree =>
          for {
            array <- evaluate(access.getExpression, env, depth)
            index <- evaluate(access.getIndex, env, depth)
            item  <- (array, index) match {
              case (Items(items), Count(at)) => items.lift(at).flatten.map(Item(_))
              case _                         => None
            }
          } yield item
        case identifier: IdentifierTree       => lookup(identifier, env, depth)
        case invocation: MethodInvocationTree => call(invocation, env, depth)
        case _                                => None
      }

  private def classOf(tree: ExpressionTree): Option[TypeMirror] =
    context.typeOf(tree) match {
      case declared: DeclaredType if declared.asElement.toString == "java.lang.Class" =>
        declared.getTypeArguments.asScala.headOption.filter(argument =>
          argument.getKind == TypeKind.DECLARED || argument.getKind == TypeKind.ARRAY || argument.getKind.isPrimitive
        )
      case _ => None
    }

  /** A variable's value: what the walk of a helper bound it to, or else the initializer of a local the code never
    * assigns again.
    */
  private def lookup(identifier: IdentifierTree, env: Env, depth: Int): Option[Known] =
    Option(context.elementOf(identifier)).flatMap { element =>
      env.get(element).orElse {
        Option(context.trees.getTree(element)).collect { case variable: VariableTree => variable }
          .filter(_.getInitializer != null).filter(_ => element.getKind == ElementKind.LOCAL_VARIABLE)
          .filter(_ => !reassigned(element))
          .flatMap(variable => evaluate(variable.getInitializer, Map.empty, depth + 1))
      }
    }

  private def reassigned(element: Element): Boolean = {
    var found = false
    context.ancestors.collectFirst { case method: MethodTree => method }.foreach { method =>
      new com.sun.source.util.TreeScanner[Unit, Unit] {
        override def visitAssignment(node: AssignmentTree, unused: Unit): Unit = {
          if Option(context.elementOf(node.getVariable)).contains(element) then found = true
          super.visitAssignment(node, unused)
        }
      }.scan(method, ())
    }
    found
  }

  private def invokedMethod(invocation: MethodInvocationTree): Option[ExecutableElement] =
    Option(context.elementOf(invocation.getMethodSelect)).collect { case method: ExecutableElement => method }

  /** The value a call of a helper of the build gives: its statements run over the values of the arguments. */
  private def call(invocation: MethodInvocationTree, env: Env, depth: Int): Option[Known] =
    invokedMethod(invocation).flatMap {
      method =>
        if Lints.owner(method).contains("java.util.Arrays") && method.getSimpleName.contentEquals("copyOf")
        then
          Lints.arguments(invocation) match {
            case Vector(source, size) =>
              for {
                array <- evaluate(source, env, depth)
                count <- evaluate(size, env, depth)
                copy  <- (array, count) match {
                  case (Items(items), Count(n)) => Some(Items(items.take(n) ++ Vector.fill(n - items.size)(None)))
                  case _                        => None
                }
              } yield copy
            case _ => None
          }
        else
          Option(context.trees.getTree(method)).filter(_.getBody != null).flatMap { source =>
            val arguments = Lints.arguments(invocation).map(evaluate(_, env, depth))
            if arguments.exists(_.isEmpty) || arguments.size != method.getParameters.size then None
            else {
              val bound = method.getParameters.asScala.zip(arguments.flatten).map((p, v) => (p: Element) -> v).toMap
              run(source.getBody.getStatements.asScala.toList, bound, depth + 1)
            }
          }
    }

  /** Runs the statements of a helper, which may declare locals, copy arrays and set elements, until it returns. */
  private def run(statements: List[StatementTree], env: Env, depth: Int): Option[Known] =
    statements match {
      case Nil          => None
      case head :: rest =>
        head match {
          case ret: ReturnTree        => Option(ret.getExpression).flatMap(evaluate(_, env, depth))
          case variable: VariableTree =>
            for {
              element <- Option(context.elementOf(variable))
              initial <- Option(variable.getInitializer).flatMap(evaluate(_, env, depth))
              result  <- run(rest, env + (element -> initial), depth)
            } yield result
          case statement: ExpressionStatementTree =>
            statement.getExpression match {
              case assignment: AssignmentTree =>
                assignment.getVariable match {
                  case access: ArrayAccessTree =>
                    for {
                      array   <- evaluate(access.getExpression, env, depth)
                      index   <- evaluate(access.getIndex, env, depth)
                      value   <- evaluate(assignment.getExpression, env, depth)
                      updated <- (array, index, value) match {
                        case (Items(items), Count(at), Item(tpe)) if at >= 0 && at < items.size =>
                          Some(Items(items.updated(at, Some(tpe))))
                        case _ => None
                      }
                      target <- Option(context.elementOf(access.getExpression))
                      result <- run(rest, env + (target -> updated), depth)
                    } yield result
                  case _ => None
                }
              case copy: MethodInvocationTree => copying(copy, env, depth).flatMap(run(rest, _, depth))
              case _                          => None
            }
          case _ => None
        }
    }

  /** The environment after `System.arraycopy`, which copies a range of classes between arrays. */
  private def copying(invocation: MethodInvocationTree, env: Env, depth: Int): Option[Env] =
    invokedMethod(invocation)
      .filter(method =>
        Lints.owner(method).contains("java.lang.System") && method.getSimpleName.contentEquals("arraycopy")
      ).flatMap { _ =>
        Lints.arguments(invocation) match {
          case Vector(from, fromAt, to, toAt, length) =>
            for {
              source  <- evaluate(from, env, depth)
              a       <- evaluate(fromAt, env, depth)
              target  <- evaluate(to, env, depth)
              b       <- evaluate(toAt, env, depth)
              n       <- evaluate(length, env, depth)
              element <- Option(context.elementOf(to))
              result  <- (source, a, target, b, n) match {
                case (Items(copied), Count(i), Items(into), Count(j), Count(count))
                    if i + count <= copied.size && j + count <= into.size =>
                  Some(Items(into.patch(j, copied.slice(i, i + count), count)))
                case _ => None
              }
            } yield env + (element -> result)
          case _ => None
        }
      }
}

private[java] object ClassArrays {
  def apply(context: Context): ClassArrays = new ClassArrays(context)

  private type Env = Map[Element, Known]

  /** What the evaluation knows of a value. */
  private enum Known {
    case Item(tpe: TypeMirror)
    case Items(types: Vector[Option[TypeMirror]])
    case Count(value: Int)
  }
}
