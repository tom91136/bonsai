package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.tree.Tree.Kind
import javax.lang.model.element.ElementKind
import javax.lang.model.element.TypeElement
import net.kurobako.bonsai.java.Javac.*
import scala.jdk.CollectionConverters.*

/** The lints of the serialization of the Java code, which the code of a type holds.
  *
  * The lints are of the code of one file, as the trees the compiler gives are of one compilation; the rules that need
  * the evidence of the whole build are of the analysis, which holds the model of the build.
  */
object LintSerialization {

  /** The lints of the serialization. */
  val lints: Vector[Lint] = Vector(MissingSerialVersionUid, MissingSerialAnnotation)

  /** A serial version field that can carry the `@Serial` annotation. */
  private object MissingSerialAnnotation extends Lint {
    val id               = LintId.MissingSerialAnnotation.id
    val kinds: Set[Kind] = Set(Tree.Kind.VARIABLE)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val variable = tree.asInstanceOf[VariableTree]
      val element  = context.namedElement
      // `java.io.Serial` arrived in Java 14; a build that targets an older API cannot write the annotation.
      lazy val supported = context.elements.typeNamed("java.io.Serial").isDefined
      val field          = element.exists(found =>
        found.getKind == ElementKind.FIELD && Lints.constant(found) &&
          variable.getName.contentEquals("serialVersionUID")
      )
      lazy val annotated = element.exists(Nullness.hasAnnotation(_, "java.io.Serial"))
      if (field && supported && !annotated)
        Vector(context.found("the serialVersionUID field can be annotated with @Serial"))
      else Vector.empty
    }
  }

  /** A type that is serialized and does not name the version of its form. */
  private object MissingSerialVersionUid extends Lint {
    val id               = LintId.MissingSerialVersionUid.id
    val kinds: Set[Kind] = Set(Tree.Kind.CLASS, Tree.Kind.ENUM, Tree.Kind.RECORD)

    def check(context: Context, tree: Tree): Vector[Found] =
      context.element match {
        case typeElement: TypeElement =>
          val serializable = typeElement.getInterfaces.asScala.exists(Lints.isType(_, "java.io.Serializable"))
          // The version of the form is the name of the field the serializer reads it from.
          val named = typeElement.getEnclosedElements.asScala.exists { member =>
            member.getKind == ElementKind.FIELD && member.getSimpleName.toString == "serialVersionUID"
          }
          if (serializable && !named)
            Vector(context.found("the type is serialized and does not name the version of its form"))
          else Vector.empty
        case _ => Vector.empty
      }
  }
}
