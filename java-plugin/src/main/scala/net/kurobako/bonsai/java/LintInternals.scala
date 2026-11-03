package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.tree.Tree.Kind
import javax.lang.model.element.Element
import javax.lang.model.element.ExecutableElement
import javax.lang.model.element.TypeElement
import net.kurobako.bonsai.java.Javac.*
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

  /** A type or member the code names. */
  private object JdkInternalApi extends Lint {
    val id               = LintId.JdkInternalApi.id
    val kinds: Set[Kind] = Set(Tree.Kind.IDENTIFIER, Tree.Kind.MEMBER_SELECT, Tree.Kind.METHOD_INVOCATION)

    private def reason(context: Context, declared: Element): Option[String] =
      JdkPackages.unsupported(context.elements.getPackageOf(declared).getQualifiedName.toString)

    /** The method a call invokes, by the name of the type that declares it and its own name. */
    private def invoked(context: Context, invocation: MethodInvocationTree): Option[(String, String)] =
      context.elementOf(invocation) match {
        case executable: ExecutableElement =>
          Lints.owner(executable).map(_ -> executable.getSimpleName.toString)
        case _ => None
      }

    /** The class an expression names by a class literal, or by a lookup of a constant name, when the JDK does not
      * support it.
      */
    private def classReason(context: Context, tree: ExpressionTree): Option[(String, String)] = tree match {
      case select: MemberSelectTree if select.getIdentifier.contentEquals("class") =>
        context.elementOf(select.getExpression) match {
          case declared: TypeElement => reason(context, declared).map(declared.getQualifiedName.toString -> _)
          case _                     => None
        }
      case invocation: MethodInvocationTree
          if invoked(context, invocation).contains("java.lang.Class" -> "forName") =>
        Lints.arguments(invocation).headOption.flatMap(Lints.literalText).flatMap { name =>
          JdkPackages.unsupportedName(name).map(name -> _)
        }
      case _ => None
    }

    def check(context: Context, tree: Tree): Vector[Found] = tree match {
      case invocation: MethodInvocationTree => call(context, invocation)
      case _                                => reference(context, tree)
    }

    private def reference(context: Context, tree: Tree): Vector[Found] = {
      val qualifier = tree match {
        case select: MemberSelectTree => context.namedElement(select.getExpression)
        case _                        => None
      }
      val qualifierUnsupported = qualifier.exists {
        case declared: TypeElement => reason(context, declared).isDefined
        case _                     => false
      }
      context.namedElement.toVector.flatMap {
        case declared: TypeElement =>
          reason(context, declared).map { why =>
            context.found(detail(s"the type ${declared.getQualifiedName}", why))
          }.toVector
        case member if !qualifierUnsupported =>
          Lints.owner(member).toVector.flatMap { holder =>
            reason(context, member).map(why =>
              context.found(detail(s"the member $holder.${member.getSimpleName}", why))
            ).toVector
          }
        case _ => Vector.empty
      }
    }

    private def lookupFound(context: Context, member: String, declared: String, why: String): Found =
      context.found(detail(s"the reflective lookup of $member in $declared", why))

    private def call(context: Context, invocation: MethodInvocationTree): Vector[Found] = {
      val arguments = Lints.arguments(invocation)
      invoked(context, invocation).toVector.flatMap { case (holder, name) =>
        val named = holder match {
          case "java.lang.Class" | "java.lang.ClassLoader" | "java.lang.invoke.MethodHandles.Lookup"
              if Set("forName", "loadClass", "findClass")(name) =>
            arguments.headOption.flatMap(Lints.literalText).flatMap { text =>
              JdkPackages.unsupportedName(text).map(why =>
                context.found(detail(s"the class $text named by $holder.$name", why), arguments.head)
              )
            }.toVector
          case _ => Vector.empty
        }
        val lookup = invocation.getMethodSelect match {
          case select: MemberSelectTree if holder == "java.lang.Class" && Lookups(name) =>
            classReason(context, select.getExpression).toVector.map { (declared, why) =>
              lookupFound(context, arguments.headOption.flatMap(Lints.literalText).getOrElse(name), declared, why)
            }
          case _ if holder == "java.lang.invoke.MethodHandles.Lookup" && name.startsWith("find") =>
            arguments.headOption.flatMap(classReason(context, _)).toVector.map { (declared, why) =>
              lookupFound(context, arguments.lift(1).flatMap(Lints.literalText).getOrElse(name), declared, why)
            }
          case _ => Vector.empty
        }
        named ++ lookup
      }
    }
  }

  /** The imports of a file, which name a package or a type the JDK does not support. */
  private object JdkImports extends Lint {
    val id               = LintId.JdkInternalApi.id
    val kinds: Set[Kind] = Set.empty[Tree.Kind]

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
