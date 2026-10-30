package net.kurobako.baumkuchen.java

import javax.lang.model.AnnotatedConstruct
import javax.lang.model.`type`.*
import javax.lang.model.element.*
import scala.jdk.CollectionConverters.*

/** What the JSpecify annotations of the code say about the nullness of its declarations and types. */
private[java] object Nullness {

  val Nullable     = "org.jspecify.annotations.Nullable"
  val NullMarked   = "org.jspecify.annotations.NullMarked"
  val NullUnmarked = "org.jspecify.annotations.NullUnmarked"
  val Boundary     = "NullableBoundary"

  def hasAnnotation(target: AnnotatedConstruct, name: String): Boolean =
    target.getAnnotationMirrors.asScala.exists(annotation => annotationName(annotation) == name)

  def hasSimpleName(target: AnnotatedConstruct, name: String): Boolean =
    target.getAnnotationMirrors.asScala.exists { annotation =>
      annotation.getAnnotationType.asElement match {
        case owner: TypeElement => owner.getSimpleName.contentEquals(name)
        case _                  => false
      }
    }

  private def annotationName(annotation: AnnotationMirror): String =
    annotation.getAnnotationType.asElement match {
      case owner: TypeElement => owner.getQualifiedName.toString
      case _                  => ""
    }

  /** Whether a value of the type may be null: the type is annotated, or is a variable or a wildcard bounded by one. */
  def nullableType(tpe: TypeMirror): Boolean =
    tpe != null &&
      (hasAnnotation(tpe, Nullable) ||
        (tpe match {
          case variable: TypeVariable         => nullableType(variable.getUpperBound)
          case wildcard: WildcardType         => nullableType(wildcard.getExtendsBound)
          case intersection: IntersectionType => intersection.getBounds.asScala.exists(nullableType)
          case _                              => false
        }))

  /** Whether the declaration says its value may be null. The annotation of an array declares its elements, so that of
    * the array is the one on its type.
    */
  def nullableElement(element: Element): Boolean =
    if hasSimpleName(element, Boundary) then true
    else if element.asType.getKind == TypeKind.ARRAY then nullableType(element.asType)
    else hasAnnotation(element, Nullable) || nullableType(element.asType)

  /** Whether the method says its result may be null. */
  def nullableMethod(method: ExecutableElement): Boolean =
    hasAnnotation(method, Nullable) || hasSimpleName(method, Boundary) || nullableType(method.getReturnType)

  /** Whether the method declares a nullable result itself, which a boundary annotation does not: it only says that the
    * method meets values the scope cannot vouch for.
    */
  def nullableResult(method: ExecutableElement): Boolean =
    hasAnnotation(method, Nullable) || nullableType(method.getReturnType)

  /** Whether the nearest annotation of the scope of the element marks it null-marked. A package marks its own types
    * only, not those of its subpackages, and a module marks all of its packages, which the enclosing elements reach.
    */
  def marked(element: Element): Boolean = {
    var current = element
    while current != null do {
      if hasSimpleName(current, Boundary) then return false
      if hasAnnotation(current, NullMarked) then return true
      if hasAnnotation(current, NullUnmarked) then return false
      current = current.getEnclosingElement
    }
    false
  }

  def hasBoundary(element: Element): Boolean = {
    var current = element
    while current != null do {
      if hasSimpleName(current, Boundary) then return true
      current = current.getEnclosingElement
    }
    false
  }

  /** Whether a parameter of a null-marked scope must be given a value that is not null. */
  def nonNullParameter(parameter: VariableElement): Boolean =
    !nullableElement(parameter) && marked(parameter)

  /** Methods whose result the JDK promises to be non-null, which the compiler does not annotate for JSpecify. */
  def knownNonNull(method: ExecutableElement): Boolean = {
    val owner = Option(method.getEnclosingElement).map(_.toString).getOrElse("")
    val name  = method.getSimpleName.toString
    (owner == "java.util.Objects" && Set("requireNonNull", "requireNonNullElse", "toString").contains(name)) ||
    (owner == "java.util.Optional" && name == "of") ||
    (owner == "java.util.List" && name == "of") ||
    (owner == "java.util.Set" && name == "of") ||
    (owner == "java.util.Map" && Set("of", "ofEntries").contains(name)) ||
    (owner == "java.lang.String" && name == "valueOf")
  }

  /** Whether the type is a box of a primitive. */
  def boxed(tpe: TypeMirror): Boolean =
    tpe != null && tpe.getKind == TypeKind.DECLARED &&
      (tpe match {
        case declared: DeclaredType =>
          declared.asElement match {
            case owner: TypeElement =>
              Set(
                "java.lang.Integer",
                "java.lang.Long",
                "java.lang.Short",
                "java.lang.Byte",
                "java.lang.Character",
                "java.lang.Boolean",
                "java.lang.Float",
                "java.lang.Double"
              ).contains(owner.getQualifiedName.toString)
            case _ => false
          }
        case _ => false
      })
}
