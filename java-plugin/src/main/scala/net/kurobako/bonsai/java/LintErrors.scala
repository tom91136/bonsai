package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.tree.Tree.Kind
import javax.lang.model.`type`.ArrayType
import javax.lang.model.`type`.DeclaredType
import javax.lang.model.`type`.ExecutableType
import javax.lang.model.`type`.TypeKind
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.Element
import javax.lang.model.element.ExecutableElement
import net.kurobako.bonsai.java.Javac.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** The lints of the error handling of the Java code, which the code of a type holds.
  *
  * The lints are of the code of one file, as the trees the compiler gives are of one compilation; the rules that need
  * the evidence of the whole build are of the analysis, which holds the model of the build.
  */
object LintErrors {

  /** The lints of the error handling. */
  val lints: Vector[Lint] =
    Vector(SwallowedException, CatchThrowable, PrintStackTrace, CatchMayIgnoreException, RedundantThrows)

  /** An exception that is caught and never looked at. */
  private object SwallowedException extends Lint {
    val id               = LintId.SwallowedException.id
    val kinds: Set[Kind] = Set(Tree.Kind.CATCH)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val catchTree = tree.asInstanceOf[CatchTree]
      val parameter = catchTree.getParameter
      val name      = parameter.getName.toString
      val element   = context.elementOf(parameter)
      val ignored   = name == "ignored" || name.contains("ignored")
      if (ignored) Vector.empty
      else {
        val throws = mutable.ArrayBuffer.empty[ThrowTree]
        Scan.foreach(catchTree.getBlock, !_.isInstanceOf[CatchTree | ClassTree | LambdaExpressionTree]) {
          case node: ThrowTree => throws += node
        }

        val derived = derivedElements(context, catchTree.getBlock, element, carries(context))
        if (
          throws.nonEmpty && !callsInitCause(context, catchTree.getBlock, derived) &&
          throws.exists(t => !mentionsAny(context, t.getExpression, derived))
        )

          Vector(context.found("the exception thrown from the catch block drops the caught exception as its cause"))
        else {
          val restored = context.attributedType(parameter).exists(Lints.isType(_, "java.lang.InterruptedException")) &&
            LintErrors.callsInterrupt(catchTree.getBlock)
          if (mentionsAny(context, catchTree.getBlock, Set(element)) || restored) Vector.empty
          else Vector(context.found("the exception is caught and never looked at"))
        }
      }
    }
  }

  private def mentionsAny(context: Context, tree: Tree, elements: scala.collection.Set[Element]): Boolean =
    Scan.exists(tree) { case node: IdentifierTree => elements.contains(context.elementOf(node)) }

  private def derivedElements(
      context: Context,
      block: BlockTree,
      caught: Element,
      carries: Tree => Boolean
  ): Set[Element] = {
    val derived = mutable.Set[Element](caught)

    @annotation.tailrec
    def settle(): Unit = {
      val before = derived.size
      Scan.foreach(block, !_.isInstanceOf[CatchTree | ClassTree]) {
        case node: VariableTree =>
          if (
            node.initializer.isDefined && mentionsAny(context, node.getInitializer, derived) &&
            carries(node.getInitializer)
          ) derived += context.elementOf(node)
        case node: AssignmentTree =>
          if (mentionsAny(context, node.getExpression, derived) && carries(node.getExpression))
            node.getVariable match {
              case id: IdentifierTree => derived += context.elementOf(id)
              case _                  => ()
            }
        case node: InstanceOfTree =>
          if (mentionsAny(context, node.getExpression, derived) && node.pattern.isDefined)
            Scan.foreach(node.getPattern) {
              case pat: BindingPatternTree => derived += context.elementOf(pat.getVariable)
              case variable: VariableTree  => derived += context.elementOf(variable)
            }
        case node: MethodInvocationTree =>
          node.getMethodSelect match {
            case select: MemberSelectTree
                if (select.getIdentifier.contentEquals("initCause") ||
                  select.getIdentifier.contentEquals("addSuppressed")) &&
                  Lints.arguments(node).exists(arg => mentionsAny(context, arg, derived)) =>
              select.getExpression match {
                case id: IdentifierTree => derived += context.elementOf(id)
                case _                  => ()
              }
            case _ => ()
          }
      }
      if (derived.size > before) settle()
    }

    settle()
    derived.toSet
  }

  private def callsMethod(body: Tree, name: String, takes: MethodInvocationTree => Boolean): Boolean =
    Scan.exists(body) {
      case node: MethodInvocationTree =>
        node.getMethodSelect match {
          case select: MemberSelectTree => select.getIdentifier.contentEquals(name) && takes(node)
          case _                        => false
        }
    }

  private def callsInitCause(context: Context, block: BlockTree, elements: scala.collection.Set[Element]): Boolean =
    callsMethod(block, "initCause", call => Lints.arguments(call).exists(arg => mentionsAny(context, arg, elements)))

  /** Whether the code of a block calls `interrupt`, which sets the flag of the thread again. */
  private def callsInterrupt(body: Tree): Boolean = callsMethod(body, "interrupt", _ => true)

  /** The names the code of a block uses, which the lints that care of what the code catches read. */
  private[java] def named(body: Tree): Set[String] = {
    val names = mutable.Set.empty[String]
    Scan.foreach(body) {
      case node: IdentifierTree   => names += node.getName.toString
      case node: MemberSelectTree => names += node.getIdentifier.toString
    }
    names.toSet
  }

  private def carries(context: Context): Tree => Boolean = {
    val throwable = context.elements.typeNamed("java.lang.Throwable").map(_.asType())
    def contains(base: TypeMirror, tpe: TypeMirror): Boolean = Lints.subtype(context, tpe, base) ||
      (tpe match {
        case declared: DeclaredType => declared.getTypeArguments.asScala.exists(contains(base, _))
        case array: ArrayType       => contains(base, array.getComponentType)
        case _                      => false
      })
    tree =>
      throwable.exists { base =>
        context.attributedType(tree).exists(typed => Lints.subtype(context, base, typed) || contains(base, typed))
      }
  }

  private def handsOn(context: Context, block: BlockTree, elements: scala.collection.Set[Element]): Boolean = {
    def passes(expression: ExpressionTree): Boolean = Lint.unwrapped(expression) match {
      case id: IdentifierTree => elements.contains(context.elementOf(id))
      case _                  => false
    }
    Scan.exists(block) {
      case node: MethodInvocationTree => Lints.arguments(node).exists(passes(_))
      case node: NewClassTree         => Lints.arguments(node).exists(passes(_))
      case node: ThrowTree            => passes(node.getExpression)
      case node: ReturnTree           => node.returned.exists(passes(_))
      case node: AssignmentTree       => passes(node.getExpression)
      case node: VariableTree         => node.initializer.exists(passes(_))
    }
  }

  /** An exception that is caught so generally that the failures of the program are hidden. */
  private object CatchThrowable extends Lint {
    val id               = LintId.CatchThrowable.id
    val kinds: Set[Kind] = Set(Tree.Kind.CATCH)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val catchTree = tree.asInstanceOf[CatchTree]
      val caught    = context.attributedType(catchTree.getParameter)
      val generic   =
        caught.exists(tpe => Lints.isType(tpe, "java.lang.Throwable") || Lints.isType(tpe, "java.lang.Error"))
      val element = context.elementOf(catchTree.getParameter)
      if (
        generic &&
        !handsOn(context, catchTree.getBlock, derivedElements(context, catchTree.getBlock, element, carries(context)))
      )

        Vector(context.found("the exception is caught so generally that the failures of the program are hidden"))
      else Vector.empty
    }
  }

  /** A stack trace that is written to the output of the program instead of being reported. */
  private object PrintStackTrace extends Lint {
    val id               = LintId.PrintStackTrace.id
    val kinds: Set[Kind] = Set(Tree.Kind.METHOD_INVOCATION)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val invocation = tree.asInstanceOf[MethodInvocationTree]
      val method     = invocation.getMethodSelect
      val printed    = method match {
        case identifier: IdentifierTree => identifier.getName.toString == "printStackTrace"
        case _                          => false
      }
      if (printed)
        Vector(context.found("the stack trace is written to the output of the program instead of being reported"))
      else Vector.empty
    }
  }

  /** A checked exception a method declares but never throws. */
  private object RedundantThrows extends Lint {
    val id               = LintId.RedundantThrows.id
    val kinds: Set[Kind] = Set(Tree.Kind.METHOD)

    def check(context: Context, tree: Tree): Vector[Found] = tree match {
      case method: MethodTree if method.body.isDefined =>
        context.element match {
          case executable: ExecutableElement
              if Hierarchy.hidden(executable) && Hierarchy.fixed(executable) &&
                Hierarchy.overridden(context, executable).isEmpty =>
            executable.getThrownTypes.asScala.view
              .filter(tpe =>
                tpe.getKind != TypeKind.TYPEVAR && !unchecked(context, tpe) && !thrown(context, method.getBody, tpe)
              )
              .map(tpe => context.found(s"the declared exception $tpe is never thrown"))
              .toVector
          case _ => Vector.empty
        }
      case _ => Vector.empty
    }

    /** Whether the declared type is one the compiler does not require the method to declare. */
    private def unchecked(context: Context, tpe: TypeMirror): Boolean =
      Seq("java.lang.RuntimeException", "java.lang.Error").exists { name =>
        context.elements.typeNamed(name).exists(unchecked => Lints.subtype(context, tpe, unchecked.asType()))
      }

    /** Whether the body throws the declared type, or calls something that declares it. */
    private def thrown(context: Context, body: BlockTree, declared: TypeMirror): Boolean =
      Scan.exists(body) {
        case node: ThrowTree => context.mirrorOf(node.getExpression).exists(Lints.subtype(context, _, declared))
        case node: MethodInvocationTree =>
          val select   = node.getMethodSelect
          val declares = context.elementOf(select) match {
            case executable: ExecutableElement => executable.getThrownTypes.asScala.toVector
            case _                             => Vector.empty
          }
          // the type the call infers for a thrown type variable, as orElseThrow does
          val inferred = context.attributedType(select) match {
            case Some(executable: ExecutableType) => executable.getThrownTypes.asScala.toVector
            case _                                => Vector.empty
          }
          (declares ++ inferred).exists(tpe => Lints.subtype(context, tpe, declared))
        case node: NewClassTree =>
          context.elementOf(node) match {
            case executable: ExecutableElement =>
              executable.getThrownTypes.asScala.exists(tpe => Lints.subtype(context, tpe, declared))
            case _ => false
          }
      }

  }

  /** A catch parameter named as ignored that the catch block reads, which says the code meant to handle it. */
  private object CatchMayIgnoreException extends Lint {
    val id               = LintId.CatchMayIgnoreException.id
    val kinds: Set[Kind] = Set(Tree.Kind.CATCH)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val catchTree = tree.asInstanceOf[CatchTree]
      val name      = catchTree.getParameter.getName.toString
      if (!ignored(name)) Vector.empty
      else {
        if (mentionsAny(context, catchTree.getBlock, Set(context.elementOf(catchTree.getParameter))))
          Vector(context.found(s"the catch parameter '$name' is named as ignored but the block reads it"))
        else Vector.empty
      }
    }

    private def ignored(name: String): Boolean = {
      val text = name.toLowerCase(java.util.Locale.ROOT)
      name == "_" || text.startsWith("ignored") || text.startsWith("unused")
    }
  }
}
