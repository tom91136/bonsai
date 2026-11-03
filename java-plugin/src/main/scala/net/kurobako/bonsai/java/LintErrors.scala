package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreeScanner
import javax.lang.model.`type`.ExecutableType
import javax.lang.model.`type`.TypeKind
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.Element
import javax.lang.model.element.ExecutableElement
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
    val id    = JavaLint.SwallowedException.id
    val kinds = Set(Tree.Kind.CATCH)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val catchTree = tree.asInstanceOf[CatchTree]
      val parameter = catchTree.getParameter
      val name      = parameter.getName.toString
      val element   = context.elementOf(parameter)
      val ignored   = name == "ignored" || name.contains("ignored")
      if (ignored) Vector.empty
      else {
        val throws = scala.collection.mutable.ArrayBuffer.empty[ThrowTree]
        new TreeScanner[Unit, Unit] {
          override def visitThrow(node: ThrowTree, unused: Unit): Unit = {
            throws += node
            super.visitThrow(node, unused)
          }
          override def visitCatch(node: CatchTree, unused: Unit): Unit                       = ()
          override def visitClass(node: ClassTree, unused: Unit): Unit                       = ()
          override def visitLambdaExpression(node: LambdaExpressionTree, unused: Unit): Unit = ()
        }.scan(catchTree.getBlock, ())

        val derived = derivedElements(context, catchTree.getBlock, element)
        if throws.nonEmpty && !callsInitCause(context, catchTree.getBlock, derived) &&
          throws.exists(t => !mentionsAny(context, t.getExpression, derived))
        then
          Vector(context.found("the exception thrown from the catch block drops the caught exception as its cause"))
        else {
          val restored = Lints.isType(context.typeOf(parameter), "java.lang.InterruptedException") &&
            LintErrors.callsInterrupt(catchTree.getBlock)
          if mentionsAny(context, catchTree.getBlock, Set(element)) || restored then Vector.empty
          else Vector(context.found("the exception is caught and never looked at"))
        }
      }
    }
  }

  private def mentionsAny(context: Context, tree: Tree, elements: scala.collection.Set[Element]): Boolean = {
    var found = false
    new TreeScanner[Unit, Unit] {
      override def visitIdentifier(node: IdentifierTree, unused: Unit): Unit = {
        if elements.contains(context.elementOf(node)) then found = true
        super.visitIdentifier(node, unused)
      }
    }.scan(tree, ())
    found
  }

  private def derivedElements(
      context: Context,
      block: BlockTree,
      caught: Element,
      carries: Tree => Boolean = _ => true
  ): Set[Element] = {
    val derived = mutable.Set[Element](caught)
    var changed = true
    while changed do {
      val before = derived.size
      new TreeScanner[Unit, Unit] {
        override def visitVariable(node: VariableTree, unused: Unit): Unit = {
          if node.getInitializer != null && mentionsAny(context, node.getInitializer, derived) &&
            carries(node.getInitializer)
          then derived += context.elementOf(node)
          super.visitVariable(node, unused)
        }
        override def visitAssignment(node: AssignmentTree, unused: Unit): Unit = {
          if mentionsAny(context, node.getExpression, derived) && carries(node.getExpression) then
            node.getVariable match {
              case id: IdentifierTree => derived += context.elementOf(id)
              case _                  => ()
            }
          super.visitAssignment(node, unused)
        }
        override def visitInstanceOf(node: InstanceOfTree, unused: Unit): Unit = {
          if mentionsAny(context, node.getExpression, derived) && node.getPattern != null then
            new TreeScanner[Unit, Unit] {
              override def visitBindingPattern(pat: BindingPatternTree, u: Unit): Unit = {
                if pat.getVariable != null then derived += context.elementOf(pat.getVariable)
                super.visitBindingPattern(pat, u)
              }
              override def visitVariable(v: VariableTree, u: Unit): Unit = {
                derived += context.elementOf(v)
                super.visitVariable(v, u)
              }
            }.scan(node.getPattern, ())
          super.visitInstanceOf(node, unused)
        }
        override def visitMethodInvocation(node: MethodInvocationTree, unused: Unit): Unit = {
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
          super.visitMethodInvocation(node, unused)
        }
        override def visitCatch(node: CatchTree, unused: Unit): Unit = ()
        override def visitClass(node: ClassTree, unused: Unit): Unit = ()
      }.scan(block, ())
      changed = derived.size > before
    }
    derived.toSet
  }

  private def callsMethod(body: Tree, name: String, takes: MethodInvocationTree => Boolean): Boolean = {
    var called = false
    new TreeScanner[Unit, Unit] {
      override def visitMethodInvocation(node: MethodInvocationTree, unused: Unit): Unit = {
        node.getMethodSelect match {
          case select: MemberSelectTree if select.getIdentifier.contentEquals(name) && takes(node) => called = true
          case _                                                                                   => ()
        }
        super.visitMethodInvocation(node, unused)
      }
    }.scan(body, ())
    called
  }

  private def callsInitCause(context: Context, block: BlockTree, elements: scala.collection.Set[Element]): Boolean =
    callsMethod(block, "initCause", call => Lints.arguments(call).exists(arg => mentionsAny(context, arg, elements)))

  /** Whether the code of a block calls `interrupt`, which sets the flag of the thread again. */
  private def callsInterrupt(body: Tree): Boolean = callsMethod(body, "interrupt", _ => true)

  /** The names the code of a block uses, which the lints that care of what the code catches read. */
  private[java] def named(body: Tree): Set[String] = {
    val names   = mutable.Set.empty[String]
    val scanner = new TreeScanner[Unit, Unit] {
      override def visitIdentifier(node: IdentifierTree, unused: Unit): Unit = {
        names += node.getName.toString
        super.visitIdentifier(node, unused)
      }
      override def visitMemberSelect(node: MemberSelectTree, unused: Unit): Unit = {
        names += node.getIdentifier.toString
        super.visitMemberSelect(node, unused)
      }
    }
    scanner.scan(body, ())
    names.toSet
  }

  private def carries(context: Context): Tree => Boolean = {
    val throwable = Option(context.elements.getTypeElement("java.lang.Throwable")).map(_.asType())
    tree =>
      throwable.exists { base =>
        val typed = context.typeOf(tree)
        Lints.subtype(context, typed, base) || Lints.subtype(context, base, typed)
      }
  }

  private def handsOn(context: Context, block: BlockTree, elements: scala.collection.Set[Element]): Boolean = {
    def passes(expression: ExpressionTree): Boolean = Lint.unwrapped(expression) match {
      case id: IdentifierTree => elements.contains(context.elementOf(id))
      case _                  => false
    }
    var found = false
    new TreeScanner[Unit, Unit] {
      override def visitMethodInvocation(node: MethodInvocationTree, unused: Unit): Unit = {
        if Lints.arguments(node).exists(passes) then found = true
        super.visitMethodInvocation(node, unused)
      }
      override def visitNewClass(node: NewClassTree, unused: Unit): Unit = {
        if Lints.arguments(node).exists(passes) then found = true
        super.visitNewClass(node, unused)
      }
      override def visitThrow(node: ThrowTree, unused: Unit): Unit = {
        if passes(node.getExpression) then found = true
        super.visitThrow(node, unused)
      }
      override def visitReturn(node: ReturnTree, unused: Unit): Unit = {
        if node.getExpression != null && passes(node.getExpression) then found = true
        super.visitReturn(node, unused)
      }
      override def visitAssignment(node: AssignmentTree, unused: Unit): Unit = {
        if passes(node.getExpression) then found = true
        super.visitAssignment(node, unused)
      }
      override def visitVariable(node: VariableTree, unused: Unit): Unit = {
        if node.getInitializer != null && passes(node.getInitializer) then found = true
        super.visitVariable(node, unused)
      }
    }.scan(block, ())
    found
  }

  /** An exception that is caught so generally that the failures of the program are hidden. */
  private object CatchThrowable extends Lint {
    val id    = JavaLint.CatchThrowable.id
    val kinds = Set(Tree.Kind.CATCH)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val catchTree = tree.asInstanceOf[CatchTree]
      val caught    = context.typeOf(catchTree.getParameter)
      val generic   = Lints.isType(caught, "java.lang.Throwable") || Lints.isType(caught, "java.lang.Error")
      val element   = context.elementOf(catchTree.getParameter)
      if generic &&
        !handsOn(context, catchTree.getBlock, derivedElements(context, catchTree.getBlock, element, carries(context)))
      then
        Vector(context.found("the exception is caught so generally that the failures of the program are hidden"))
      else Vector.empty
    }
  }

  /** A stack trace that is written to the output of the program instead of being reported. */
  private object PrintStackTrace extends Lint {
    val id    = JavaLint.PrintStackTrace.id
    val kinds = Set(Tree.Kind.METHOD_INVOCATION)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val invocation = tree.asInstanceOf[MethodInvocationTree]
      val method     = invocation.getMethodSelect
      val printed    = method match {
        case identifier: IdentifierTree => identifier.getName.toString == "printStackTrace"
        case _                          => false
      }
      if printed then
        Vector(context.found("the stack trace is written to the output of the program instead of being reported"))
      else Vector.empty
    }
  }

  /** A checked exception a method declares but never throws. */
  private object RedundantThrows extends Lint {
    val id    = JavaLint.RedundantThrows.id
    val kinds = Set(Tree.Kind.METHOD)

    def check(context: Context, tree: Tree): Vector[Found] = tree match {
      case method: MethodTree if method.getBody != null =>
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
    private def unchecked(context: Context, tpe: TypeMirror): Boolean = {
      val runtime = context.elements.getTypeElement("java.lang.RuntimeException")
      val error   = context.elements.getTypeElement("java.lang.Error")
      Lints.subtype(context, tpe, Option(runtime).map(_.asType()).orNull) ||
      Lints.subtype(context, tpe, Option(error).map(_.asType()).orNull)
    }

    /** Whether the body throws the declared type, or calls something that declares it. */
    private def thrown(context: Context, body: BlockTree, declared: TypeMirror): Boolean = {
      var found = false
      new TreeScanner[Unit, Unit] {
        override def visitThrow(node: ThrowTree, unused: Unit): Unit = {
          val tpe = context.mirrorOf(node.getExpression).orNull
          if Lints.subtype(context, tpe, declared) then found = true
          super.visitThrow(node, unused)
        }

        override def visitMethodInvocation(node: MethodInvocationTree, unused: Unit): Unit = {
          val select   = node.getMethodSelect
          val declares = context.elementOf(select) match {
            case executable: ExecutableElement => executable.getThrownTypes.asScala.toVector
            case _                             => Vector.empty
          }
          // the type the call infers for a thrown type variable, as orElseThrow does
          val inferred = Option(context.typeOf(select)) match {
            case Some(executable: ExecutableType) => executable.getThrownTypes.asScala.toVector
            case _                                => Vector.empty
          }
          if (declares ++ inferred).exists(tpe => Lints.subtype(context, tpe, declared)) then found = true
          super.visitMethodInvocation(node, unused)
        }

        override def visitNewClass(node: NewClassTree, unused: Unit): Unit = {
          context.elementOf(node) match {
            case executable: ExecutableElement
                if executable.getThrownTypes.asScala.exists(tpe => Lints.subtype(context, tpe, declared)) =>
              found = true
            case _ => ()
          }
          super.visitNewClass(node, unused)
        }
      }.scan(body, ())
      found
    }

  }

  /** A catch parameter named as ignored that the catch block reads, which says the code meant to handle it. */
  private object CatchMayIgnoreException extends Lint {
    val id    = JavaLint.CatchMayIgnoreException.id
    val kinds = Set(Tree.Kind.CATCH)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val catchTree = tree.asInstanceOf[CatchTree]
      val name      = catchTree.getParameter.getName.toString
      if !ignored(name) then Vector.empty
      else {
        if mentionsAny(context, catchTree.getBlock, Set(context.elementOf(catchTree.getParameter))) then
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
