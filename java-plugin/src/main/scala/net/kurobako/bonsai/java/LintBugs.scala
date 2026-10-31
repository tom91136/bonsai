package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import javax.lang.model.`type`.TypeKind
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.Element
import javax.lang.model.element.ElementKind
import javax.lang.model.element.ExecutableElement
import javax.lang.model.element.Modifier
import javax.lang.model.element.TypeElement
import javax.lang.model.element.VariableElement
import scala.jdk.CollectionConverters.*

/** The lints of the probable bugs of the Java code, which the code of a type holds.
  *
  * The lints are of the code of one file, as the trees the compiler gives are of one compilation; the rules that need
  * the evidence of the whole build are of the analysis, which holds the model of the build.
  */
object LintBugs {

  /** The lints of the probable bugs. */
  val lints: Vector[Lint] =
    Vector(
      RedundantCast,
      SelfAssignment,
      StringReferenceEquality,
      EqualsWithoutHashCode,
      MissingFieldInitializer,
      CastNarrowingVariable,
      StandardCharset,
      SystemGetProperty,
      ThreadDefaultRun,
      ExplicitArrayFilling,
      PrimitiveArrayVarargs,
      IgnoredCallResult,
      ReadString,
      SameReturnValue,
      SuspiciousInvocationHandler,
      OptionalSignature
    )

  /** A cast of a value to the type it already has. */
  private object RedundantCast extends Lint {
    val id    = JavaLint.RedundantCast.id
    val kinds = Set(Tree.Kind.TYPE_CAST)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val cast = tree.asInstanceOf[TypeCastTree]
      val from = context.typeOf(cast.getExpression)
      val to   = context.typeOf(cast)
      // The type of a call of a signature polymorphic method is the type of the cast of it, so the cast is what gives
      // the call its type rather than being of no use.
      val polymorphic = context.elementOf(cast.getExpression) match {
        case method: ExecutableElement =>
          val owner = Option(method.getEnclosingElement).map(_.toString).getOrElse("")
          (owner == "java.lang.invoke.MethodHandle" || owner == "java.lang.invoke.VarHandle") && method.isVarArgs
        case _ => false
      }
      // A cast of a lambda or a member reference names the functional interface the compiler is to read the value as,
      // which the overloads of a call such as AccessController.doPrivileged need, so it is not a cast to a type the
      // value already has.
      val functional = cast.getExpression match {
        case _: LambdaExpressionTree => true
        case _: MemberReferenceTree  => true
        case _                       => false
      }
      // A cast of a value to the type it has is of no use.
      if !polymorphic && !functional && from != null && to != null && from.toString == to.toString then
        Vector(context.found(s"the cast to $to is redundant"))
      else Vector.empty
    }
  }

  /** A value that is assigned to itself. */
  private object SelfAssignment extends Lint {
    val id    = JavaLint.SelfAssignment.id
    val kinds = Set(Tree.Kind.ASSIGNMENT)

    private def qualifier(tree: ExpressionTree): String = tree match {
      case select: MemberSelectTree => select.getExpression.toString
      case _                        => "this"
    }

    def check(context: Context, tree: Tree): Vector[Found] = {
      val assignment = tree.asInstanceOf[AssignmentTree]
      val variable   = assignment.getVariable
      val value      = assignment.getExpression
      val left       = context.elementOf(variable)
      val right      = context.elementOf(value)
      // A value that is assigned to itself keeps the value it has; the same field of another object is a copy.
      if left != null && right != null && left == right && qualifier(variable) == qualifier(value) then
        Vector(context.found("the value is assigned to itself"))
      else Vector.empty
    }
  }

  /** Two strings that are compared by reference and not by their text. */
  private object StringReferenceEquality extends Lint {
    val id    = JavaLint.StringReferenceEquality.id
    val kinds = Set(Tree.Kind.EQUAL_TO, Tree.Kind.NOT_EQUAL_TO)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val binary = tree.asInstanceOf[BinaryTree]
      val left   = context.typeOf(binary.getLeftOperand)
      val right  = context.typeOf(binary.getRightOperand)
      if Lints.isType(left, "java.lang.String") && Lints.isType(right, "java.lang.String") then
        Vector(context.found("the strings are compared by reference and not by their text"))
      else Vector.empty
    }
  }

  /** A type that compares its values and does not give them a hash code. */
  private object EqualsWithoutHashCode extends Lint {
    val id    = JavaLint.EqualsWithoutHashCode.id
    val kinds = Set(Tree.Kind.CLASS, Tree.Kind.ENUM, Tree.Kind.RECORD)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val members = tree match {
        case typeTree: ClassTree => typeTree.getMembers.asScala.toVector
        case _                   => Vector.empty
      }
      val methods = members.collect { case method: MethodTree => method }
      val equals  = methods.exists(method => method.getName.contentEquals("equals") && method.getParameters.size() == 1)
      val hash    = methods.exists(method => method.getName.contentEquals("hashCode") && method.getParameters.isEmpty)
      // A type that compares its values but does not give them a hash code is not found in a hash-based collection.
      if equals && !hash then
        Vector(context.found("the type compares its values but does not give them a hash code"))
      else Vector.empty
    }
  }

  /** A field of the state of the type that the code does not give the value it starts with. */
  object MissingFieldInitializer extends Lint {
    val id    = JavaLint.MissingFieldInitializer.id
    val kinds = Set(Tree.Kind.VARIABLE)

    def check(context: Context, tree: Tree): Vector[Found] =
      if !required(context.element) || tree.asInstanceOf[VariableTree].getInitializer != null then Vector.empty
      else Vector(context.found("the field is not given the value it starts with"))

    /** Whether the element is a primitive field of the state of the type, which the code says the start value of. */
    def required(element: Element): Boolean = {
      // A local variable and a parameter are not fields; a final field is given its value where it is built.
      val field = element != null && element.getKind == ElementKind.FIELD
      val state = field && !element.getModifiers.contains(Modifier.FINAL) &&
        (element.getModifiers.contains(Modifier.STATIC) || element.getModifiers.contains(Modifier.VOLATILE))
      // The value a field of the state of the type starts with is part of the state, and the code says it. A field
      // the code gives a value is one whose start the code says, and an instance field of the code is not the state
      // of the type, so it starts at the value of its type without the code saying so.
      state && element.asType().getKind.isPrimitive
    }
  }

  /** A cast that a narrower declaration of the variable would make unnecessary. */
  private object CastNarrowingVariable extends Lint {
    val id    = JavaLint.CastNarrowingVariable.id
    val kinds = Set(Tree.Kind.TYPE_CAST)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val cast       = tree.asInstanceOf[TypeCastTree]
      val expression = Lints.unparenthesised(cast.getExpression)
      val element    = context.elementOf(expression)
      Option(element).collect {
        case variable: VariableElement if variable.getKind == ElementKind.LOCAL_VARIABLE =>
          variable
      }.flatMap { variable =>
        Option(context.trees.getTree(variable)).collect { case declaration: VariableTree => declaration }.flatMap {
          declaration =>
            Option(declaration.getInitializer).flatMap { initializer =>
              val declared = context.typeOf(expression)
              val castType = context.typeOf(cast)
              val initType = context.typeOf(initializer)
              if Lints.subtype(context, initType, castType) && Lints.subtype(context, castType, declared) &&
                !context.types.isSameType(castType, declared) && !LintBugs.assigned(context, variable)
              then
                Some(context.found(
                  s"the cast to $castType can be removed by declaring ${variable.getSimpleName} as $castType (verified by the earlier tree check)"
                ))
              else None
            }
        }
      }.toVector
    }
  }

  /** A charset name or `Charset.forName` call that names one of the standard charsets. */
  private object StandardCharset extends Lint {
    val id    = JavaLint.StandardCharset.id
    val kinds = Set(Tree.Kind.METHOD_INVOCATION)

    private val Constants = Map(
      "UTF-8"      -> "UTF_8",
      "US-ASCII"   -> "US_ASCII",
      "ISO-8859-1" -> "ISO_8859_1",
      "UTF-16"     -> "UTF_16",
      "UTF-16BE"   -> "UTF_16BE",
      "UTF-16LE"   -> "UTF_16LE"
    )

    def check(context: Context, tree: Tree): Vector[Found] = {
      val invocation = tree.asInstanceOf[MethodInvocationTree]
      context.element match {
        case method: ExecutableElement
            if method.getSimpleName.contentEquals("forName") &&
              Lints.owner(method).contains("java.nio.charset.Charset") =>
          if invocation.getArguments.size == 1 then
            invocation.getArguments.get(0) match {
              case literal: LiteralTree =>
                Option(literal.getValue).collect { case name: String => name }.flatMap(Constants.get).map { constant =>
                  context.found(s"use StandardCharsets.$constant instead of Charset.forName")
                }.toVector
              case _ => Vector.empty
            }
          else Vector.empty
        case method: ExecutableElement if method.getSimpleName.contentEquals("name") =>
          invocation.getMethodSelect match {
            case select: MemberSelectTree =>
              context.elementOf(select.getExpression) match {
                case field: VariableElement
                    if field.getKind == ElementKind.FIELD &&
                      Lints.owner(field).contains("java.nio.charset.StandardCharsets") =>
                  Vector(context.found(s"use ${select.getExpression} instead of ${select.getExpression}.name()"))
                case _ => Vector.empty
              }
            case _ => Vector.empty
          }
        case _ => Vector.empty
      }
    }
  }

  /** A system property that a standard constant or method already names. */
  private object SystemGetProperty extends Lint {
    val id    = JavaLint.SystemGetProperty.id
    val kinds = Set(Tree.Kind.METHOD_INVOCATION)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val invocation = tree.asInstanceOf[MethodInvocationTree]
      context.element match {
        case method: ExecutableElement
            if method.getSimpleName.contentEquals("getProperty") &&
              Lints.owner(method).contains("java.lang.System") &&
              invocation.getArguments.size == 1 =>
          invocation.getArguments.get(0) match {
            case literal: LiteralTree =>
              Option(literal.getValue).collect {
                case "file.separator" => "File.separator"
                case "path.separator" => "File.pathSeparator"
                case "line.separator" => "System.lineSeparator()"
              }.map(replacement => context.found(s"use $replacement instead of System.getProperty")).toVector
            case _ => Vector.empty
          }
        case _ => Vector.empty
      }
    }
  }

  /** A thread built without a task, whose default run method does nothing. */
  private object ThreadDefaultRun extends Lint {
    val id    = JavaLint.ThreadDefaultRun.id
    val kinds = Set(Tree.Kind.NEW_CLASS)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val newClass = tree.asInstanceOf[NewClassTree]
      val thread   = context.element match {
        case constructor: ExecutableElement =>
          Lints.owner(constructor).contains("java.lang.Thread")
        case _ => false
      }
      val taskless = newClass.getArguments.isEmpty ||
        (newClass.getArguments.size == 1 &&
          Lints.isType(context.typeOf(newClass.getArguments.get(0)), "java.lang.String"))
      if thread && newClass.getClassBody == null && taskless then
        Vector(context.found("the thread has no task, so its default run method does nothing"))
      else Vector.empty
    }
  }

  /** A loop that fills an array with one value, which Arrays.fill says in one call. */
  private object ExplicitArrayFilling extends Lint {
    val id    = JavaLint.ExplicitArrayFilling.id
    val kinds = Set(Tree.Kind.FOR_LOOP)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val loop = tree.asInstanceOf[ForLoopTree]
      val fill = loop.getInitializer.asScala.toVector match {
        case Vector(variable: VariableTree) if variable.getInitializer != null =>
          Option(loop.getCondition).flatMap(_ =>
            Option(
              loop.getStatement
            ).collect { case block: BlockTree => block }.flatMap(_.getStatements.asScala.toVector match {
              case Vector(statement: ExpressionStatementTree) => Some(statement.getExpression)
              case _                                          => None
            })
          )
        case _ => None
      }
      val index = loop.getInitializer.asScala.collectFirst { case variable: VariableTree => variable.getName.toString }
      fill.collect {
        case assignment: AssignmentTree
            if assignment.getVariable.isInstanceOf[ArrayAccessTree] &&
              !index.exists(name => LintErrors.named(assignment.getExpression).contains(name)) &&
              loop.getUpdate.asScala.forall(update => update.getExpression.isInstanceOf[UnaryTree]) =>
          context.found("the loop fills an array with one value; Arrays.fill says it in one call")
      }.toVector
    }
  }

  /** A primitive array passed to a varargs method without saying it is one argument. */
  private object PrimitiveArrayVarargs extends Lint {
    val id    = JavaLint.PrimitiveArrayVarargs.id
    val kinds = Set(Tree.Kind.METHOD_INVOCATION)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val invocation = tree.asInstanceOf[MethodInvocationTree]
      context.element match {
        case method: ExecutableElement
            if method.isVarArgs && Lints.owner(method).contains("java.lang.reflect.Method") =>
          Lints.arguments(invocation).zipWithIndex.collectFirst {
            case (argument, index) if index > 0 && LintBugs.primitiveArray(context, argument) =>
              context.found("a primitive array passed to a varargs method is one argument; cast it to Object to say so")
          }.toVector
        case _ => Vector.empty
      }
    }
  }

  /** A call whose result the code drops, where the result says whether the call worked. */
  private object IgnoredCallResult extends Lint {
    val id    = JavaLint.IgnoredCallResult.id
    val kinds = Set(Tree.Kind.METHOD_INVOCATION)

    private val Checked = Set(
      "java.io.InputStream#read",
      "java.io.Reader#read",
      "java.util.concurrent.BlockingQueue#offer",
      "java.util.concurrent.BlockingQueue#offerFirst",
      "java.util.concurrent.BlockingQueue#offerLast",
      "java.util.concurrent.CountDownLatch#await"
    )

    def check(context: Context, tree: Tree): Vector[Found] =
      context.element match {
        case method: ExecutableElement =>
          val key = Lints.owner(method).map(_ + "#" + method.getSimpleName).getOrElse("")
          if Checked(key) && method.getReturnType.getKind != TypeKind.VOID && Collector.discarded(context.path) then
            Vector(context.found(s"the result of ${method.getSimpleName} says whether the call worked"))
          else Vector.empty
        case _ => Vector.empty
      }
  }

  /** A string built from the bytes of a file, which Files.readString reads directly. */
  private object ReadString extends Lint {
    val id    = JavaLint.ReadString.id
    val kinds = Set(Tree.Kind.NEW_CLASS)

    def check(context: Context, tree: Tree): Vector[Found] = tree match {
      case newClass: NewClassTree if Lints.arguments(newClass).nonEmpty =>
        val string = context.element match {
          case constructor: ExecutableElement => Lints.owner(constructor).contains("java.lang.String")
          case _                              => false
        }
        val reads = newClass.getArguments.get(0) match {
          case invocation: MethodInvocationTree =>
            context.elementOf(invocation) match {
              case method: ExecutableElement =>
                method.getSimpleName.contentEquals("readAllBytes") &&
                Lints.owner(method).contains("java.nio.file.Files")
              case _ => false
            }
          case _ => false
        }
        if string && reads && LintBugs.readStringAvailable(context) && Release.atLeast(context, 11) then
          Vector(context.found("use Files.readString instead of a String built from the bytes of a file"))
        else Vector.empty
      case _ => Vector.empty
    }
  }

  private def readStringAvailable(context: Context): Boolean =
    Option(context.elements.getTypeElement("java.nio.file.Files")).exists { files =>
      context.elements.getAllMembers(files).asScala.exists(_.getSimpleName.contentEquals("readString"))
    }

  /** An InvocationHandler whose invoke returns null for every method, including equals. */
  private object SuspiciousInvocationHandler extends Lint {
    val id    = JavaLint.SuspiciousInvocationHandler.id
    val kinds = Set(Tree.Kind.METHOD, Tree.Kind.LAMBDA_EXPRESSION)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val returned = tree match {
        case method: MethodTree if method.getName.contentEquals("invoke") && method.getParameters.size == 3 =>
          val owner = Option(context.element).map(_.getEnclosingElement)
          Option(context.elements.getTypeElement("java.lang.reflect.InvocationHandler")).filter(handlerType =>
            owner.exists(element => element != null && context.types.isSubtype(element.asType(), handlerType.asType()))
          ).flatMap(_ => returnsOnlyNull(method.getBody))
        case lambda: LambdaExpressionTree =>
          context.typeOf match {
            case declared: javax.lang.model.`type`.DeclaredType
                if declared.asElement.toString == "java.lang.reflect.InvocationHandler" =>
              lambda.getBody match {
                case body: BlockTree => returnsOnlyNull(body)
                case _               => None
              }
            case _ => None
          }
        case _ => None
      }
      returned.map(at =>
        context.found("returning null from invoke may break equals, hashCode and toString", at)
      ).toVector
    }

    /** The first value the body returns, when it returns, and returns null on every path that does. */
    private def returnsOnlyNull(body: BlockTree): Option[ExpressionTree] = {
      val returned = Lints.returns(body).map(node => Option(node.getExpression))
      val allNull  = returned.nonEmpty && returned.forall {
        case Some(literal: LiteralTree) => literal.getKind == Tree.Kind.NULL_LITERAL
        case _                          => false
      }
      if allNull then returned.head else None
    }
  }

  /** An Optional used as a field or parameter, which a project chooses to forbid or allow. */
  private object OptionalSignature extends Lint {
    val id    = JavaLint.OptionalSignature.id
    val kinds = Set(Tree.Kind.VARIABLE)

    private val Optionals = Set(
      "java.util.Optional",
      "java.util.OptionalInt",
      "java.util.OptionalLong",
      "java.util.OptionalDouble"
    )

    private def isOptional(tpe: TypeMirror): Boolean =
      tpe match {
        case declared: javax.lang.model.`type`.DeclaredType =>
          declared.asElement match {
            case owner: TypeElement => Optionals(owner.getQualifiedName.toString)
            case _                  => false
          }
        case _ => false
      }

    def check(context: Context, tree: Tree): Vector[Found] = {
      val element = context.element
      val place   = element != null &&
        (element.getKind == ElementKind.FIELD ||
          element.getKind == ElementKind.PARAMETER || element.getKind == ElementKind.RECORD_COMPONENT)
      val optional = element != null && isOptional(element.asType())
      // the parameters of a lambda are set by the functional interface it implements
      val lambda = context.ancestors.headOption.exists {
        case lambda: LambdaExpressionTree => lambda.getParameters.contains(tree)
        case _                            => false
      }
      if place && optional && !lambda then {
        val method = Option(element.getEnclosingElement).collect { case executable: ExecutableElement => executable }
        val api    =
          method.exists(m => m.getModifiers.contains(Modifier.PUBLIC) || m.getModifiers.contains(Modifier.PROTECTED))
        val inherited = element.getKind == ElementKind.PARAMETER &&
          method.exists(Hierarchy.overridden(context, _).nonEmpty)
        val several = method.exists(_.getParameters.asScala.count(parameter => isOptional(parameter.asType())) > 1)
        val advice  =
          if inherited then None
          else if element.getKind != ElementKind.PARAMETER then
            Some("an Optional field is a nullable field in JSpecify: annotate the field @Nullable instead")
          else if !api then Some("an Optional parameter of a helper is a @Nullable parameter: annotate it instead")
          else if several then
            Option.when(method.exists(_.getParameters.asScala.find(p => isOptional(p.asType())).contains(element)))(
              "several Optional parameters: annotate them @Nullable, or group them in a parameter object or builder"
            )
          else
            Some(
              "replace the Optional parameter with an overload that omits the argument; only as a last resort " +
                "annotate the parameter @Nullable"
            )
        advice.map(context.found(_)).toVector
      } else Vector.empty
    }
  }

  /** A method that always returns the same literal value, and so does every method that overrides it. */
  private object SameReturnValue extends Lint {
    val id    = JavaLint.SameReturnValue.id
    val kinds = Set(Tree.Kind.METHOD)

    def check(context: Context, tree: Tree): Vector[Found] = {
      tree match {
        case method: MethodTree if !method.getName.contentEquals("<init>") =>
          Option(context.element).collect { case executable: ExecutableElement => executable }.foreach { element =>
            val value  = Option.when(method.getBody != null)(returnedValue(method)).flatten
            val parent = Hierarchy.overridden(context, element)
            if parent.nonEmpty then context.registry.overrides(Registry.Returns, parent, value)
            else if Hierarchy.closed(element) &&
              (value.isDefined || element.getModifiers.contains(Modifier.ABSTRACT) || method.getBody == null)
            then
              context.registry.declares(
                Registry.Returns,
                element,
                value,
                detail => Linter.found(this, context, Found(detail, method)),
                context.suppressed
              )
          }
        case _ => ()
      }
      Vector.empty
    }

    /** The literal every return of the method gives, if they all give the same. */
    private def returnedValue(method: MethodTree): Option[String] = {
      val returned = Lints.returns(method.getBody).flatMap(node => Option(node.getExpression))
      val values   = returned.map(_.toString).distinct
      Option.when(returned.nonEmpty && values.size == 1 && returned.forall(_.isInstanceOf[LiteralTree]))(values.head)
    }
  }

  private def primitiveArray(context: Context, tree: Tree): Boolean =
    Option(context.typeOf(tree)) match {
      case Some(array: javax.lang.model.`type`.ArrayType) => array.getComponentType.getKind.isPrimitive
      case _                                              => false
    }

  /** Whether the code assigns the variable after its declaration. */
  private def assigned(context: Context, variable: Element): Boolean = {
    val scope = context.ancestors.collectFirst {
      case declared: (MethodTree | ClassTree) => declared
    }
    scope.exists(root => Effects.assigns(context, new TreePath(new TreePath(context.unit), root), variable))
  }
}
