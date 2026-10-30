package net.kurobako.bonsai.java

import com.sun.source.tree.*
import javax.lang.model.element.Element
import javax.lang.model.element.ExecutableElement
import javax.lang.model.element.TypeElement
import scala.jdk.CollectionConverters.*

/** Uses of JDK types and members that the JDK does not support, named in the code or by a reflective lookup of a name.
  */
object LintInternals {

  val lints: Vector[Lint] = Vector(JdkInternalApi)

  val fileLints: Vector[Lint] = Vector(JdkImports)

  private val Advice = "a suppression should name the supported alternative or the JDK versions it is keyed on"

  private val Lookups = Set(
    "getField",
    "getDeclaredField",
    "getMethod",
    "getDeclaredMethod",
    "getConstructor",
    "getDeclaredConstructor"
  )

  private def detail(subject: String, reason: String): String = s"$subject is $reason; $Advice"

  private def owner(element: Element): Option[TypeElement] = element match {
    case declared: TypeElement => Some(declared)
    case _                     =>
      element.getEnclosingElement match {
        case declared: TypeElement => Some(declared)
        case _                     => None
      }
  }

  private def reason(context: Context, declared: TypeElement): Option[String] =
    JdkPackages.unsupported(context.elements.getPackageOf(declared).getQualifiedName.toString)

  private def constant(tree: ExpressionTree): Option[String] = tree match {
    case literal: LiteralTree => Option(literal.getValue).collect { case text: String => text }
    case _                    => None
  }

  /** The method a call invokes, by the name of the type that declares it and its own name. */
  private def invoked(context: Context, invocation: MethodInvocationTree): Option[(String, String)] =
    context.elementOf(invocation) match {
      case executable: ExecutableElement =>
        owner(executable).map(declared => declared.getQualifiedName.toString -> executable.getSimpleName.toString)
      case _ => None
    }

  /** The class an expression names by a class literal, or by a lookup of a constant name, when the JDK does not support
    * it.
    */
  private def classReason(context: Context, tree: ExpressionTree): Option[(String, String)] = tree match {
    case select: MemberSelectTree if select.getIdentifier.contentEquals("class") =>
      context.elementOf(select.getExpression) match {
        case declared: TypeElement => reason(context, declared).map(declared.getQualifiedName.toString -> _)
        case _                     => None
      }
    case invocation: MethodInvocationTree
        if invoked(context, invocation).contains("java.lang.Class" -> "forName") =>
      invocation.getArguments.asScala.headOption.flatMap(constant).flatMap { name =>
        JdkPackages.unsupportedName(name).map(name -> _)
      }
    case _ => None
  }

  /** A type or member the code names. */
  object JdkInternalApi extends Lint {
    val id    = JavaLint.JdkInternalApi.id
    val kinds = Set(Tree.Kind.IDENTIFIER, Tree.Kind.MEMBER_SELECT, Tree.Kind.METHOD_INVOCATION)

    def check(context: Context, tree: Tree): Vector[Found] = tree match {
      case invocation: MethodInvocationTree => call(context, invocation)
      case _                                => reference(context, tree)
    }

    private def reference(context: Context, tree: Tree): Vector[Found] = {
      val qualifier = tree match {
        case select: MemberSelectTree => Option(context.elementOf(select.getExpression))
        case _                        => None
      }
      val qualifierUnsupported = qualifier.exists {
        case declared: TypeElement => reason(context, declared).isDefined
        case _                     => false
      }
      Option(context.element).toVector.flatMap {
        case declared: TypeElement =>
          reason(context, declared).map { why =>
            context.found(detail(s"the type ${declared.getQualifiedName}", why))
          }.toVector
        case member if !qualifierUnsupported =>
          owner(member).toVector.flatMap { declared =>
            reason(context, declared).map(why =>
              context.found(detail(s"the member ${declared.getQualifiedName}.${member.getSimpleName}", why))
            ).toVector
          }
        case _ => Vector.empty
      }
    }

    private def call(context: Context, invocation: MethodInvocationTree): Vector[Found] = {
      val arguments = invocation.getArguments.asScala.toVector
      invoked(context, invocation).toVector.flatMap { case (holder, name) =>
        val named = holder match {
          case "java.lang.Class" | "java.lang.ClassLoader" | "java.lang.invoke.MethodHandles.Lookup"
              if Set("forName", "loadClass", "findClass")(name) =>
            arguments.headOption.flatMap(constant).flatMap { text =>
              JdkPackages.unsupportedName(text).map(why =>
                context.found(detail(s"the class $text named by $holder.$name", why), arguments.head)
              )
            }.toVector
          case _ => Vector.empty
        }
        val lookup = invocation.getMethodSelect match {
          case select: MemberSelectTree if holder == "java.lang.Class" && Lookups(name) =>
            classReason(context, select.getExpression).toVector.map { (declared, why) =>
              val member = arguments.headOption.flatMap(constant).getOrElse(name)
              context.found(detail(s"the reflective lookup of $member in $declared", why))
            }
          case _ if holder == "java.lang.invoke.MethodHandles.Lookup" && name.startsWith("find") =>
            arguments.headOption.flatMap(classReason(context, _)).toVector.map { (declared, why) =>
              val member = arguments.lift(1).flatMap(constant).getOrElse(name)
              context.found(detail(s"the reflective lookup of $member in $declared", why))
            }
          case _ => Vector.empty
        }
        named ++ lookup
      }
    }
  }

  /** The imports of a file, which name a package or a type the JDK does not support. */
  object JdkImports extends Lint {
    val id    = JavaLint.JdkInternalApi.id
    val kinds = Set.empty[Tree.Kind]

    def check(context: Context, tree: Tree): Vector[Found] = tree match {
      case unit: CompilationUnitTree =>
        unit.getImports.asScala.toVector.flatMap { imported =>
          val name = imported.getQualifiedIdentifier.toString.stripSuffix(".*")
          JdkPackages.unsupportedName(name).map(why => context.found(detail(s"the import $name", why), imported))
        }
      case _ => Vector.empty
    }
  }
}
