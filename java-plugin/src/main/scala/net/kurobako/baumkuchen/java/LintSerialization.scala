package net.kurobako.baumkuchen.java

import com.sun.source.tree.*
import javax.lang.model.element.ElementKind
import javax.lang.model.element.Modifier
import javax.lang.model.element.TypeElement
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
  object MissingSerialAnnotation extends Lint {
    val id    = JavaLint.MissingSerialAnnotation.id
    val kinds = Set(Tree.Kind.VARIABLE)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val variable = tree.asInstanceOf[VariableTree]
      val element  = context.element
      // `java.io.Serial` arrived in Java 14; a build that targets an older API cannot write the annotation.
      val supported = context.elements.getTypeElement("java.io.Serial") != null
      val field     = element != null && element.getKind == ElementKind.FIELD &&
        element.getModifiers.contains(Modifier.STATIC) && element.getModifiers.contains(Modifier.FINAL) &&
        variable.getName.contentEquals("serialVersionUID")
      val annotated = element != null && element.getAnnotationMirrors.asScala.exists { annotation =>
        annotation.getAnnotationType.asElement match {
          case owner: TypeElement => owner.getQualifiedName.contentEquals("java.io.Serial")
          case _                  => false
        }
      }
      if supported && field && !annotated then
        Vector(context.found("the serialVersionUID field can be annotated with @Serial"))
      else Vector.empty
    }
  }

  /** A type that is serialized and does not name the version of its form. */
  object MissingSerialVersionUid extends Lint {
    val id    = JavaLint.MissingSerialVersionUid.id
    val kinds = Set(Tree.Kind.CLASS, Tree.Kind.ENUM, Tree.Kind.RECORD)

    def check(context: Context, tree: Tree): Vector[Found] =
      context.element match {
        case typeElement: TypeElement =>
          val serializable = typeElement.getInterfaces.asScala.exists(_.toString == "java.io.Serializable")
          // The version of the form is the name of the field the serializer reads it from.
          val named = typeElement.getEnclosedElements.asScala.exists { member =>
            member.getKind == ElementKind.FIELD && member.getSimpleName.toString == "serialVersionUID"
          }
          if serializable && !named then
            Vector(context.found("the type is serialized and does not name the version of its form"))
          else Vector.empty
        case _ => Vector.empty
      }
  }
}
