package net.kurobako.bonsai.java

import javax.lang.model.`type`.DeclaredType
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.*
import net.kurobako.bonsai.java.Javac.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** What the types of the code say about which methods override which. */
private object Hierarchy {

  /** The methods of the supertypes that the method overrides, which the compiler can see. */
  def overridden(context: Context, method: ExecutableElement): Vector[ExecutableElement] =
    method.getEnclosingElement match {
      case owner: TypeElement =>
        val seen                         = mutable.Set.empty[String]
        val found                        = mutable.ArrayBuffer.empty[TypeElement]
        def visit(tpe: TypeMirror): Unit =
          context.types.directSupertypes(tpe).asScala.foreach {
            case declared: DeclaredType =>
              declared.asElement match {
                case element: TypeElement if seen.add(element.getQualifiedName.toString) =>
                  found += element
                  visit(declared)
                case _ =>
              }
            case _ =>
          }
        visit(owner.asType)
        found.toVector.flatMap { parent =>
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

  /** Whether no method can override the method: it is private, static or final, or its class is final. */
  def fixed(method: ExecutableElement): Boolean = {
    val modifiers = method.getModifiers
    modifiers.contains(Modifier.PRIVATE) || modifiers.contains(Modifier.STATIC) || modifiers.contains(Modifier.FINAL) ||
    enclosingTypes(method).nextOption().exists(_.getModifiers.contains(Modifier.FINAL))
  }

  /** Whether every method that overrides the method is in the code the compilation sees. */
  def closed(method: ExecutableElement): Boolean = fixed(method) || hidden(method)

  /** Whether only the code of the file can call the method: it is private, or its class is private or local. */
  def hidden(method: ExecutableElement): Boolean =
    method.getModifiers.contains(Modifier.PRIVATE) || enclosingTypes(method).exists { tpe =>
      tpe.getModifiers.contains(Modifier.PRIVATE) ||
      tpe.getNestingKind == NestingKind.LOCAL || tpe.getNestingKind == NestingKind.ANONYMOUS
    }

  private def enclosingTypes(element: Element): Iterator[TypeElement] =
    element.enclosings.collect {
      case tpe: TypeElement => tpe
    }
}
