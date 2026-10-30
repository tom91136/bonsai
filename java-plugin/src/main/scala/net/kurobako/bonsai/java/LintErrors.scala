package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreeScanner
import javax.lang.model.`type`.ExecutableType
import javax.lang.model.`type`.TypeKind
import javax.lang.model.`type`.TypeMirror
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
  object SwallowedException extends Lint {
    val id    = JavaLint.SwallowedException.id
    val kinds = Set(Tree.Kind.CATCH)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val catchTree = tree.asInstanceOf[CatchTree]
      val parameter = catchTree.getParameter
      val name      = parameter.getName.toString
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

        val derived = derivedNames(catchTree.getBlock, name)
        if throws.nonEmpty && !callsInitCause(catchTree.getBlock, derived) &&
          throws.exists(t => !namesAny(t.getExpression, derived))
        then
          Vector(context.found("the exception thrown from the catch block drops the caught exception as its cause"))
        else {
          val named    = LintErrors.named(catchTree.getBlock).contains(name)
          val restored = Lints.isType(context.typeOf(parameter), "java.lang.InterruptedException") &&
            LintErrors.callsInterrupt(catchTree.getBlock)
          if named || restored then Vector.empty
          else Vector(context.found("the exception is caught and never looked at"))
        }
      }
    }
  }

  private def namesAny(tree: Tree, names: scala.collection.Set[String]): Boolean = {
    var found = false
    new TreeScanner[Unit, Unit] {
      override def visitIdentifier(node: IdentifierTree, unused: Unit): Unit = {
        if names.contains(node.getName.toString) then found = true
        super.visitIdentifier(node, unused)
      }
    }.scan(tree, ())
    found
  }

  private def derivedNames(block: BlockTree, caught: String): Set[String] = {
    val derived = mutable.Set(caught)
    var changed = true
    while changed do {
      val before = derived.size
      new TreeScanner[Unit, Unit] {
        override def visitVariable(node: VariableTree, unused: Unit): Unit = {
          if node.getInitializer != null && namesAny(node.getInitializer, derived) then
            derived += node.getName.toString
          super.visitVariable(node, unused)
        }
        override def visitAssignment(node: AssignmentTree, unused: Unit): Unit = {
          if namesAny(node.getExpression, derived) then
            node.getVariable match {
              case id: IdentifierTree => derived += id.getName.toString
              case _                  => ()
            }
          super.visitAssignment(node, unused)
        }
        override def visitInstanceOf(node: InstanceOfTree, unused: Unit): Unit = {
          if namesAny(node.getExpression, derived) && node.getPattern != null then
            new TreeScanner[Unit, Unit] {
              override def visitBindingPattern(pat: BindingPatternTree, u: Unit): Unit = {
                if pat.getVariable != null then derived += pat.getVariable.getName.toString
                super.visitBindingPattern(pat, u)
              }
              override def visitVariable(v: VariableTree, u: Unit): Unit = {
                derived += v.getName.toString
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
                  node.getArguments.asScala.exists(arg => namesAny(arg, derived)) =>
              select.getExpression match {
                case id: IdentifierTree => derived += id.getName.toString
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

  private def callsInitCause(block: BlockTree, names: Set[String]): Boolean = {
    var called = false
    new TreeScanner[Unit, Unit] {
      override def visitMethodInvocation(node: MethodInvocationTree, unused: Unit): Unit = {
        node.getMethodSelect match {
          case select: MemberSelectTree if select.getIdentifier.contentEquals("initCause") =>
            if node.getArguments.asScala.exists(arg => namesAny(arg, names)) then called = true
          case _ => ()
        }
        super.visitMethodInvocation(node, unused)
      }
    }.scan(block, ())
    called
  }

  /** Whether the code of a block calls `interrupt`, which sets the flag of the thread again. */
  private def callsInterrupt(body: Tree): Boolean = {
    var called = false
    new TreeScanner[Unit, Unit] {
      override def visitMethodInvocation(node: MethodInvocationTree, unused: Unit): Unit = {
        node.getMethodSelect match {
          case select: MemberSelectTree if select.getIdentifier.contentEquals("interrupt") => called = true
          case _                                                                           => ()
        }
        super.visitMethodInvocation(node, unused)
      }
    }.scan(body, ())
    called
  }

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

  /** An exception that is caught so generally that the failures of the program are hidden. */
  object CatchThrowable extends Lint {
    val id    = JavaLint.CatchThrowable.id
    val kinds = Set(Tree.Kind.CATCH)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val catchTree = tree.asInstanceOf[CatchTree]
      val caught    = context.typeOf(catchTree.getParameter)
      if Lints.isType(caught, "java.lang.Throwable") || Lints.isType(caught, "java.lang.Error") then
        Vector(context.found("the exception is caught so generally that the failures of the program are hidden"))
      else Vector.empty
    }
  }

  /** A stack trace that is written to the output of the program instead of being reported. */
  object PrintStackTrace extends Lint {
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
  object RedundantThrows extends Lint {
    val id    = JavaLint.RedundantThrows.id
    val kinds = Set(Tree.Kind.METHOD)

    def check(context: Context, tree: Tree): Vector[Found] = tree match {
      case method: MethodTree if method.getBody != null =>
        context.element match {
          case executable: ExecutableElement =>
            executable.getThrownTypes.asScala.toVector
              .filter(tpe =>
                tpe.getKind != TypeKind.TYPEVAR && !unchecked(context, tpe) && !thrown(context, method.getBody, tpe)
              )
              .map(tpe => context.found(s"the declared exception $tpe is never thrown"))
          case _ => Vector.empty
        }
      case _ => Vector.empty
    }

    /** Whether the declared type is one the compiler does not require the method to declare. */
    private def unchecked(context: Context, tpe: TypeMirror): Boolean = {
      val runtime = context.elements.getTypeElement("java.lang.RuntimeException")
      val error   = context.elements.getTypeElement("java.lang.Error")
      subtype(context, tpe, Option(runtime).map(_.asType()).orNull) ||
      subtype(context, tpe, Option(error).map(_.asType()).orNull)
    }

    /** Whether the body throws the declared type, or calls something that declares it. */
    private def thrown(context: Context, body: BlockTree, declared: TypeMirror): Boolean = {
      var found = false
      new TreeScanner[Unit, Unit] {
        override def visitThrow(node: ThrowTree, unused: Unit): Unit = {
          val tpe = typeOf(context, node.getExpression)
          if subtype(context, tpe, declared) then found = true
          super.visitThrow(node, unused)
        }

        override def visitMethodInvocation(node: MethodInvocationTree, unused: Unit): Unit = {
          val path     = context.trees.getPath(context.unit, node.getMethodSelect)
          val declares = context.trees.getElement(path) match {
            case executable: ExecutableElement => executable.getThrownTypes.asScala.toVector
            case _                             => Vector.empty
          }
          // the type the call infers for a thrown type variable, as orElseThrow does
          val inferred = Option(context.trees.getTypeMirror(path)) match {
            case Some(executable: ExecutableType) => executable.getThrownTypes.asScala.toVector
            case _                                => Vector.empty
          }
          if (declares ++ inferred).exists(tpe => subtype(context, tpe, declared)) then found = true
          super.visitMethodInvocation(node, unused)
        }

        override def visitNewClass(node: NewClassTree, unused: Unit): Unit = {
          context.trees.getElement(context.trees.getPath(context.unit, node)) match {
            case executable: ExecutableElement
                if executable.getThrownTypes.asScala.exists(tpe => subtype(context, tpe, declared)) =>
              found = true
            case _ => ()
          }
          super.visitNewClass(node, unused)
        }
      }.scan(body, ())
      found
    }

    private def typeOf(context: Context, tree: Tree): TypeMirror =
      Option(context.trees.getPath(context.unit, tree)).flatMap(path =>
        Option(context.trees.getTypeMirror(path))
      ).orNull

    private def subtype(context: Context, left: TypeMirror, right: TypeMirror): Boolean =
      left != null && right != null &&
        (try context.types.isSubtype(left, right)
        catch { case _: IllegalArgumentException => false })
  }

  /** A catch parameter named as ignored that the catch block reads, which says the code meant to handle it. */
  object CatchMayIgnoreException extends Lint {
    val id    = JavaLint.CatchMayIgnoreException.id
    val kinds = Set(Tree.Kind.CATCH)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val catchTree = tree.asInstanceOf[CatchTree]
      val name      = catchTree.getParameter.getName.toString
      if !ignored(name) then Vector.empty
      else {
        var read = false
        new TreeScanner[Unit, Unit] {
          override def visitIdentifier(node: IdentifierTree, unused: Unit): Unit = {
            if node.getName.contentEquals(name) then read = true
            super.visitIdentifier(node, unused)
          }
        }.scan(catchTree.getBlock, ())
        if read then
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
