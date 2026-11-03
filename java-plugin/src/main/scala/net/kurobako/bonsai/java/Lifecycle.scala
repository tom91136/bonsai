package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.TreePathScanner
import com.sun.source.util.Trees
import javax.lang.model.element.*
import net.kurobako.bonsai.java.Javac.*
import scala.jdk.CollectionConverters.*

/** The setup and teardown methods of the JUnit tests, which run around the code of a test and so see its fields before
  * the setup has filled them and after a failed setup has left them empty.
  */
private object Lifecycle {

  private val setup = Set(
    "org.junit.jupiter.api.BeforeAll",
    "org.junit.jupiter.api.BeforeEach",
    "org.junit.BeforeClass",
    "org.junit.Before"
  )

  private val teardown = Set(
    "org.junit.jupiter.api.AfterAll",
    "org.junit.jupiter.api.AfterEach",
    "org.junit.AfterClass",
    "org.junit.After"
  )

  private val cache = java.util.Collections.synchronizedMap(java.util.WeakHashMap[Element, Boolean]())

  def isTeardown(method: Element): Boolean = teardown.exists(Nullness.hasAnnotation(method, _))

  /** Whether the code at the point, a lambda or a local class, is created where the setup has not yet filled the field:
    * in an initialiser or a constructor, or in a setup method before it assigns the field.
    */
  def createdBeforeSetup(trees: Trees, unit: CompilationUnitTree, point: TreePath, field: Element): Boolean = {
    val member = point.withParents
      .find(candidate => candidate.parent.exists(_.getLeaf.isInstanceOf[ClassTree]))
    member.exists { path =>
      path.getLeaf match {
        case method: MethodTree if method.getName.contentEquals("<init>") => true
        case _: MethodTree                                                =>
          trees.namedElement(path).exists { element =>
            isTeardown(element) || isSetup(element) && {
              val start = trees.getSourcePositions.getStartPosition(unit, point.getLeaf)
              assignments(trees, path, field).exists(a => start < trees.getSourcePositions.getEndPosition(unit, a))
            }
          }
        case _ => true
      }
    }
  }

  private def isSetup(method: Element): Boolean = setup.exists(Nullness.hasAnnotation(method, _))

  /** Whether a setup method of the class that declares the field assigns it. */
  def assignedInSetup(trees: Trees, field: Element): Boolean =
    Memo(cache, field)(compute(trees, field))

  private def compute(trees: Trees, field: Element): Boolean =
    field.enclosing.flatMap(owner => Option(trees.getPath(owner))).exists { path =>
      path.getLeaf match {
        case holder: ClassTree =>
          holder.getMembers.asScala.exists {
            case method: MethodTree =>
              val methodPath = TreePath(path, method)
              trees.namedElement(methodPath).exists(isSetup(_)) && assignments(trees, methodPath, field).nonEmpty
            case _ => false
          }
        case _ => false
      }
    }

  // A javac visitor is an abstract class, so it is written inline.
  @SuppressWarnings(Array("scalafix:BannedSyntax.anonymous-allocation"))
  private def assignments(trees: Trees, method: TreePath, field: Element): Vector[AssignmentTree] = {
    val found = Vector.newBuilder[AssignmentTree]
    new TreePathScanner[Unit, Unit] {
      override def visitAssignment(assignment: AssignmentTree, unused: Unit): Unit = {
        val named = assignment.getVariable match {
          case identifier: IdentifierTree => identifier.getName.contentEquals(field.getSimpleName)
          case select: MemberSelectTree   => select.getIdentifier.contentEquals(field.getSimpleName)
          case _                          => false
        }
        if (named && Option(trees.getElement(TreePath(getCurrentPath, assignment.getVariable))).forall(_ == field))

          found += assignment
        super.visitAssignment(assignment, unused)
      }
    }.scan(method, ())
    found.result()
  }
}
