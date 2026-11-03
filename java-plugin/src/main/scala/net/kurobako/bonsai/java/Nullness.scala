package net.kurobako.bonsai.java

import javax.lang.model.AnnotatedConstruct
import javax.lang.model.`type`.*
import javax.lang.model.element.*
import net.kurobako.bonsai.java.Javac.*
import scala.jdk.CollectionConverters.*

/** What the JSpecify annotations of the code say about the nullness of its declarations and types. */
private object Nullness {

  val Nullable             = "org.jspecify.annotations.Nullable"
  private val NullMarked   = "org.jspecify.annotations.NullMarked"
  private val NullUnmarked = "org.jspecify.annotations.NullUnmarked"
  private val Boundary     = "NullableBoundary"

  def hasAnnotation(target: AnnotatedConstruct, name: String): Boolean =
    target.getAnnotationMirrors.asScala.exists(annotation =>
      annotation.getAnnotationType.asElement match {
        case owner: TypeElement => owner.getQualifiedName.contentEquals(name)
        case _                  => false
      }
    )

  def hasSimpleName(target: AnnotatedConstruct, name: String): Boolean =
    target.getAnnotationMirrors.asScala.exists { annotation =>
      annotation.getAnnotationType.asElement match {
        case owner: TypeElement => owner.getSimpleName.contentEquals(name)
        case _                  => false
      }
    }

  /** Whether a value of the type may be null: the type is annotated, or is a variable or a wildcard bounded by one. */
  def nullableType(tpe: TypeMirror): Boolean =
    hasAnnotation(tpe, Nullable) ||
      (tpe match {
        case variable: TypeVariable         => nullableType(variable.getUpperBound)
        case wildcard: WildcardType         => Option(wildcard.getExtendsBound).exists(nullableType(_))
        case intersection: IntersectionType => intersection.getBounds.asScala.exists(nullableType(_))
        case _                              => false
      })

  /** Whether the declaration says its value may be null. The annotation of an array declares its elements, so that of
    * the array is the one on its type.
    */
  def nullableElement(element: Element): Boolean =
    if (hasSimpleName(element, Boundary)) true
    else if (element.asType.getKind == TypeKind.ARRAY) nullableType(element.asType)
    else hasAnnotation(element, Nullable) || nullableType(element.asType)

  /** Whether the method says its result is never null. */
  def nonNullMethod(method: ExecutableElement): Boolean =
    !hasSimpleName(method, Boundary) && !nullableResult(method)

  /** Whether the method declares a nullable result itself, which a boundary annotation does not: it only says that the
    * method meets values the scope cannot vouch for.
    */
  def nullableResult(method: ExecutableElement): Boolean =
    hasAnnotation(method, Nullable) || nullableType(method.getReturnType)

  /** Whether the nearest annotation of the scope of the element marks it null-marked. A package marks its own types
    * only, not those of its subpackages, and a module marks all of its packages, which the enclosing elements reach.
    */
  def marked(element: Element): Boolean =
    Memo(markings, element)(scope(element).getOrElse(element.enclosing.exists(marked(_))))

  private val markings = java.util.Collections.synchronizedMap(java.util.WeakHashMap[Element, Boolean]())

  /** Whether the element settles the marking of its code itself: a boundary and an unmarked scope do not mark it. */
  private def scope(element: Element): Option[Boolean] =
    if (hasSimpleName(element, Boundary)) Some(false)
    else if (hasAnnotation(element, NullMarked)) Some(true)
    else if (hasAnnotation(element, NullUnmarked)) Some(false)
    else None

  @annotation.tailrec
  def hasBoundary(element: Element): Boolean =
    hasSimpleName(element, Boundary) ||
      (element.enclosing match {
        case Some(outer) => hasBoundary(outer)
        case None        => false
      })

  /** Whether a parameter of a null-marked scope must be given a value that is not null. */
  def nonNullParameter(parameter: VariableElement): Boolean =
    !nullableElement(parameter) && marked(parameter)
}
