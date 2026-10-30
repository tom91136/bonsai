package net.kurobako.bonsai.java

import javax.lang.model.`type`.DeclaredType
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** What the types of the code say about which methods override which. */
private[java] object Hierarchy {

  /** The methods of the supertypes that the method overrides, which the compiler can see. */
  def overridden(context: Context, method: ExecutableElement): Vector[ExecutableElement] =
    method.getEnclosingElement match {
      case owner: TypeElement =>
        val seen                                         = mutable.Set.empty[String]
        def supers(tpe: TypeMirror): Vector[TypeElement] =
          context.types.directSupertypes(tpe).asScala.toVector.flatMap {
            case declared: DeclaredType =>
              declared.asElement match {
                case element: TypeElement if seen.add(element.getQualifiedName.toString) =>
                  element +: supers(declared)
                case _ => Vector.empty
              }
            case _ => Vector.empty
          }
        supers(owner.asType).flatMap { parent =>
          parent.getEnclosedElements.asScala.toVector.collect {
            case candidate: ExecutableElement
                if candidate.getSimpleName == method.getSimpleName &&
                  !candidate.getModifiers.contains(Modifier.STATIC) &&
                  context.elements.overrides(method, candidate, owner) =>
              candidate
          }
        }
      case _ => Vector.empty
    }
}
