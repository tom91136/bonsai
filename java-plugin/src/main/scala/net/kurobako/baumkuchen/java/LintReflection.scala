package net.kurobako.baumkuchen.java

import com.sun.source.tree.*
import javax.lang.model.`type`.DeclaredType
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.*
import scala.jdk.CollectionConverters.*

/** Lints of reflective member access and of code shapes that a named method or constant states more directly. */
object LintReflection {

  val lints: Vector[Lint] = Vector(ExtractMethod, ReflectionMember, ReflectionInvocation, MagicConstant)

  /** An anonymous implementation large enough to extract into a named type. */
  object ExtractMethod extends Lint {
    val id    = JavaLint.ExtractMethod.id
    val kinds = Set(Tree.Kind.VARIABLE)

    def check(context: Context, tree: Tree): Vector[Found] = tree match {
      case variable: VariableTree =>
        variable.getInitializer match {
          case newClass: NewClassTree
              if newClass.getClassBody != null && newClass.getClassBody.getMembers.size >= 2 =>
            Vector(context.found("the anonymous implementation is large enough to extract into a named type"))
          case _ => Vector.empty
        }
      case _ => Vector.empty
    }
  }

  /** A reflected member that the target type does not declare. */
  object ReflectionMember extends Lint {
    val id    = JavaLint.ReflectionMember.id
    val kinds = Set(Tree.Kind.METHOD_INVOCATION)

    private val Methods      = Set("getMethod", "getDeclaredMethod")
    private val Constructors = Set("getConstructor", "getDeclaredConstructor")
    private val Fields       = Set("getField", "getDeclaredField")

    def check(context: Context, tree: Tree): Vector[Found] = tree match {
      case invocation: MethodInvocationTree =>
        val method = context.element match {
          case executable: ExecutableElement => Some(executable)
          case _                             => None
        }
        method.filter(executable => owner(executable).contains("java.lang.Class")).flatMap { executable =>
          invocation.getMethodSelect match {
            case select: MemberSelectTree =>
              classType(context, select.getExpression).flatMap { target =>
                val name   = executable.getSimpleName.toString
                val exists =
                  if Methods(name) then
                    invocation.getArguments.asScala.toVector match {
                      case (literal: LiteralTree) +: parameters =>
                        Some(literal.getValue).collect { case text: String => text }.flatMap { methodName =>
                          signature(context, parameters).map(types => hasMethod(context, target, methodName, types))
                        }
                      case _ => None
                    }
                  else if Constructors(name) then
                    signature(context, invocation.getArguments.asScala.toVector)
                      .map(types => hasConstructor(context, target, types))
                  else if Fields(name) then
                    invocation.getArguments.asScala.toVector match {
                      case Vector(literal: LiteralTree) =>
                        Option(literal.getValue).collect { case text: String => text }
                          .map(field => hasField(target, field))
                      case _ => None
                    }
                  else None
                exists.filter(!_).map { _ =>
                  // the finding is placed on the name of the member, which a lookup that spans lines starts below the call
                  context.found(
                    s"the reflected member ${executable.getSimpleName} does not name a member of $target",
                    invocation.getArguments.asScala.headOption.getOrElse(invocation)
                  )
                }
              }
            case _ => None
          }
        }.toVector
      case _ => Vector.empty
    }

    private def classType(context: Context, tree: ExpressionTree): Option[TypeElement] =
      context.typeOf(tree) match {
        case declared: DeclaredType if declared.asElement.toString == "java.lang.Class" =>
          declared.getTypeArguments.asScala.headOption.flatMap { argument =>
            Option(context.types.asElement(argument)).collect { case element: TypeElement => element }
          }
        case _ => None
      }

    /** The parameter types a reflective lookup names: class literals, or one array of them that the code builds. */
    private def signature(context: Context, trees: Vector[ExpressionTree]): Option[Vector[TypeMirror]] =
      trees match {
        case Vector(array) if context.typeOf(array).getKind == javax.lang.model.`type`.TypeKind.ARRAY =>
          ClassArrays(context).classes(array)
        case _ => parameterTypes(context, trees)
      }

    private def parameterTypes(context: Context, trees: Vector[ExpressionTree]): Option[Vector[TypeMirror]] = {
      val types = trees.map { tree =>
        context.typeOf(tree) match {
          case declared: DeclaredType if declared.asElement.toString == "java.lang.Class" =>
            declared.getTypeArguments.asScala.headOption.filter { argument =>
              argument.getKind == javax.lang.model.`type`.TypeKind.DECLARED ||
              argument.getKind == javax.lang.model.`type`.TypeKind.ARRAY ||
              argument.getKind.isPrimitive
            }
          case _ => None
        }
      }
      if types.forall(_.isDefined) then Some(types.flatten) else None
    }

    private def hasMethod(
        context: Context,
        target: TypeElement,
        name: String,
        parameters: Vector[TypeMirror]
    ): Boolean =
      context.elements.getAllMembers(target).asScala.exists {
        case method: ExecutableElement =>
          method.getSimpleName.contentEquals(name) &&
          matchParameters(context, method.getParameters.asScala.toVector, parameters)
        case _ => false
      }

    private def hasConstructor(context: Context, target: TypeElement, parameters: Vector[TypeMirror]): Boolean =
      target.getEnclosedElements.asScala.exists {
        case constructor: ExecutableElement
            if constructor.getKind == ElementKind.CONSTRUCTOR &&
              matchParameters(context, constructor.getParameters.asScala.toVector, parameters) =>
          true
        case _ => false
      }

    private def hasField(target: TypeElement, name: String): Boolean =
      target.getEnclosedElements.asScala.exists {
        case field: VariableElement if field.getKind == ElementKind.FIELD && field.getSimpleName.contentEquals(name) =>
          true
        case _ => false
      }

    private def matchParameters(
        context: Context,
        declared: Vector[VariableElement],
        actual: Vector[TypeMirror]
    ): Boolean =
      declared.size == actual.size && declared.zip(actual).forall { (parameter, tpe) =>
        sameType(context, parameter.asType(), tpe)
      }

    private def sameType(context: Context, declared: TypeMirror, actual: TypeMirror): Boolean = {
      val direct = context.types.isSameType(context.types.erasure(declared), context.types.erasure(actual))
      if direct then true
      else {
        val boxed = (tpe: TypeMirror) =>
          try Some(context.types.unboxedType(tpe))
          catch case _: IllegalArgumentException => None
        boxed(declared).exists(context.types.isSameType(_, actual)) ||
        boxed(actual).exists(context.types.isSameType(declared, _))
      }
    }
  }

  /** A reflective call whose resolved member the arguments do not fit. */
  object ReflectionInvocation extends Lint {
    val id    = JavaLint.ReflectionInvocation.id
    val kinds = Set(Tree.Kind.METHOD_INVOCATION)

    def check(context: Context, tree: Tree): Vector[Found] = tree match {
      case invocation: MethodInvocationTree =>
        val reflection = context.element match {
          case method: ExecutableElement
              if owner(method).exists(owner =>
                owner == "java.lang.reflect.Method" || owner == "java.lang.reflect.Constructor"
              ) && Set("invoke", "newInstance")(method.getSimpleName.toString) =>
            Some(method)
          case _ => None
        }
        reflection.flatMap { method =>
          invocation.getMethodSelect match {
            case select: MemberSelectTree =>
              val resolved = member(context, select.getExpression)
              resolved.flatMap { target =>
                val arguments =
                  if method.getSimpleName.contentEquals("invoke") then
                    invocation.getArguments.asScala.toVector.drop(1)
                  else invocation.getArguments.asScala.toVector
                mismatch(context, target, arguments)
              }
            case _ => None
          }
        }.map(detail => context.found(detail)).toVector
      case _ => Vector.empty
    }

    /** The member a local, field or expression names, when the code says which member it is. */
    private def member(context: Context, tree: ExpressionTree): Option[ExecutableElement] = tree match {
      case identifier: IdentifierTree =>
        val e = Option(context.elementOf(identifier))
        e.flatMap(element => declaration(context, element))
      case select: MemberSelectTree =>
        Option(context.elementOf(select)).flatMap(element => declaration(context, element))
      case invocation: MethodInvocationTree       => factory(context, invocation)
      case parenthesized: ParenthesizedTree       => member(context, parenthesized.getExpression)
      case cast: TypeCastTree                     => member(context, cast.getExpression)
      case conditional: ConditionalExpressionTree =>
        member(context, conditional.getTrueExpression).orElse(member(context, conditional.getFalseExpression))
      case _ => None
    }

    private def targetClass(context: Context, tree: ExpressionTree): Option[TypeElement] =
      context.typeOf(tree) match {
        case declared: DeclaredType if declared.asElement.toString == "java.lang.Class" =>
          declared.getTypeArguments.asScala.headOption.flatMap { argument =>
            Option(context.types.asElement(argument)).collect { case element: TypeElement => element }
          }
        case _ => None
      }

    private def classParameters(context: Context, trees: Vector[ExpressionTree]): Option[Vector[TypeMirror]] = {
      val types = trees.map { tree =>
        context.typeOf(tree) match {
          case declared: DeclaredType if declared.asElement.toString == "java.lang.Class" =>
            declared.getTypeArguments.asScala.headOption.filter { argument =>
              argument.getKind == javax.lang.model.`type`.TypeKind.DECLARED ||
              argument.getKind == javax.lang.model.`type`.TypeKind.ARRAY ||
              argument.getKind.isPrimitive
            }
          case _ => None
        }
      }
      if types.forall(_.isDefined) then Some(types.flatten) else None
    }

    private def parametersMatch(
        context: Context,
        declared: Vector[VariableElement],
        actual: Vector[TypeMirror]
    ): Boolean =
      declared.size == actual.size && declared.zip(actual).forall { (parameter, tpe) =>
        sameType(context, parameter.asType(), tpe)
      }

    private def sameType(context: Context, declared: TypeMirror, actual: TypeMirror): Boolean = {
      val direct = context.types.isSameType(context.types.erasure(declared), context.types.erasure(actual))
      if direct then true
      else {
        val boxed = (tpe: TypeMirror) =>
          try Some(context.types.unboxedType(tpe))
          catch case _: IllegalArgumentException => None
        boxed(declared).exists(context.types.isSameType(_, actual)) ||
        boxed(actual).exists(context.types.isSameType(declared, _))
      }
    }

    private def declaration(context: Context, element: Element): Option[ExecutableElement] = {
      val leaf = Option(context.trees.getPath(element)).map(_.getLeaf)
      leaf.collect {
        case variable: VariableTree if variable.getInitializer != null => variable.getInitializer
      }.flatMap {
        case invocation: MethodInvocationTree => factory(context, invocation)
        case parenthesized: ParenthesizedTree => declaration(context, context.elementOf(parenthesized))
        case cast: TypeCastTree               => declaration(context, context.elementOf(cast))
        case _                                => None
      }
    }

    /** The member that a getMethod, getDeclaredMethod, getConstructor or getDeclaredConstructor call names. */
    private def factory(context: Context, invocation: MethodInvocationTree): Option[ExecutableElement] = {
      val method = context.elementOf(invocation.getMethodSelect) match {
        case executable: ExecutableElement => Some(executable)
        case _                             => None
      }
      method.filter(executable => owner(executable).contains("java.lang.Class")).flatMap { executable =>
        invocation.getMethodSelect match {
          case select: MemberSelectTree =>
            val tc = targetClass(context, select.getExpression)
            tc.flatMap { target =>
              val name      = executable.getSimpleName.toString
              val arguments = invocation.getArguments.asScala.toVector
              if Set("getMethod", "getDeclaredMethod")(name) then
                arguments match {
                  case (literal: LiteralTree) +: parameters =>
                    Option(literal.getValue).collect { case text: String => text }.flatMap { methodName =>
                      val pt = classParameters(context, parameters)
                      pt.flatMap { types =>
                        val found = findMethod(context, target, methodName, types, name == "getDeclaredMethod")
                        found
                      }
                    }
                  case _ => None
                }
              else if Set("getConstructor", "getDeclaredConstructor")(name) then
                classParameters(context, arguments).flatMap { types =>
                  findConstructor(context, target, types, name == "getDeclaredConstructor")
                }
              else None
            }
          case _ => None
        }
      }
    }

    private def findMethod(
        context: Context,
        target: TypeElement,
        name: String,
        parameters: Vector[TypeMirror],
        declaredOnly: Boolean
    ): Option[ExecutableElement] = {
      val members =
        if declaredOnly then target.getEnclosedElements.asScala.toVector
        else context.elements.getAllMembers(target).asScala.toVector
      members.collectFirst {
        case method: ExecutableElement
            if method.getSimpleName.contentEquals(name) &&
              parametersMatch(context, method.getParameters.asScala.toVector, parameters) =>
          method
      }
    }

    private def findConstructor(
        context: Context,
        target: TypeElement,
        parameters: Vector[TypeMirror],
        declaredOnly: Boolean
    ): Option[ExecutableElement] = {
      val members =
        if declaredOnly then target.getEnclosedElements.asScala.toVector
        else target.getEnclosedElements.asScala.toVector
      members.collectFirst {
        case constructor: ExecutableElement
            if constructor.getKind == ElementKind.CONSTRUCTOR &&
              parametersMatch(context, constructor.getParameters.asScala.toVector, parameters) =>
          constructor
      }
    }

    private def mismatch(
        context: Context,
        target: ExecutableElement,
        arguments: Vector[ExpressionTree]
    ): Option[String] = {
      val parameters = target.getParameters.asScala.toVector
      val fixed      = if target.isVarArgs then parameters.size - 1 else parameters.size
      if target.isVarArgs && arguments.size >= fixed then None
      else if arguments.size != parameters.size then
        Some(s"the reflective call passes ${arguments.size} argument(s) but the member requires ${parameters.size}")
      else
        arguments.zip(parameters).collectFirst {
          case (argument, parameter) if !matches(context, argument, parameter.asType()) =>
            val name = if target.getKind == ElementKind.CONSTRUCTOR then target.getEnclosingElement.getSimpleName
            else target.getSimpleName
            Some(s"the reflective call passes ${context.typeOf(argument)} where $name requires ${parameter.asType()}")
        }.flatten
    }

    private def matches(context: Context, argument: ExpressionTree, declared: TypeMirror): Boolean = {
      val actual = context.typeOf(argument)
      actual match {
        case _: javax.lang.model.`type`.NullType                           => !declared.getKind.isPrimitive
        case _ if actual.getKind == javax.lang.model.`type`.TypeKind.ERROR => true
        case _                                                             =>
          val declaredErased = context.types.erasure(declared)
          val actualErased   = context.types.erasure(actual)
          context.types.isAssignable(actualErased, declaredErased) ||
          context.types.isSameType(actualErased, declaredErased) ||
          boxed(context, actual).exists(boxedActual =>
            context.types.isAssignable(boxedActual, declaredErased) ||
              context.types.isSameType(boxedActual, declaredErased)
          ) ||
          boxed(context, declared).exists(boxedDeclared =>
            context.types.isAssignable(actualErased, boxedDeclared) ||
              context.types.isSameType(actualErased, boxedDeclared)
          ) ||
          (declared.getKind.isPrimitive && (
            boxed(context, actual).exists { boxedActual =>
              context.types.isAssignable(boxedActual, declaredErased) ||
              context.types.isSameType(boxedActual, declaredErased)
            } ||
              unboxed(context, actual).exists { unboxedActual =>
                context.types.isAssignable(unboxedActual, declaredErased) ||
                context.types.isSameType(unboxedActual, declaredErased)
              }
          )) ||
          declaredErased.toString == "java.lang.Object" ||
          // a value typed as a supertype of the parameter may hold an instance of it, which the reflective call needs
          (!actual.getKind.isPrimitive && !declared.getKind.isPrimitive &&
            context.types.isAssignable(declaredErased, actualErased))
      }
    }

    private def boxed(context: Context, tpe: TypeMirror): Option[TypeMirror] =
      tpe match {
        case primitive: javax.lang.model.`type`.PrimitiveType => Some(context.types.boxedClass(primitive).asType())
        case _                                                => None
      }

    private def unboxed(context: Context, tpe: TypeMirror): Option[TypeMirror] =
      try Some(context.types.unboxedType(tpe))
      catch case _: IllegalArgumentException => None
  }

  /** A modifier argument written as a magic number or unchecked variable. */
  object MagicConstant extends Lint {
    val id    = JavaLint.MagicConstant.id
    val kinds = Set(Tree.Kind.NEW_CLASS)

    def check(context: Context, tree: Tree): Vector[Found] = tree match {
      case newClass: NewClassTree if newClass.getArguments.size > 3 =>
        val owner = context.element match {
          case constructor: ExecutableElement => LintReflection.owner(constructor)
          case _                              => None
        }
        val modifiers = newClass.getArguments.get(3)
        val magic     = modifiers match {
          case literal: LiteralTree       => !java.lang.Integer.valueOf(0).equals(literal.getValue)
          case identifier: IdentifierTree => identifier.getName.toString.toLowerCase.contains("modifier")
          case _                          => false
        }
        if owner.exists(name => name == "java.awt.event.MouseEvent" || name == "java.awt.event.KeyEvent") && magic then
          Vector(context.found("use the InputEvent or KeyEvent mask constants for the modifier argument"))
        else Vector.empty
      case _ => Vector.empty
    }
  }

  private def owner(element: Element): Option[String] =
    Option(element.getEnclosingElement).collect { case owner: TypeElement => owner.getQualifiedName.toString }
}
