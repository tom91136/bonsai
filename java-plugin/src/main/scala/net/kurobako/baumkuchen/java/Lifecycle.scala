package net.kurobako.baumkuchen.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.TreePathScanner
import com.sun.source.util.Trees
import javax.lang.model.element.*
import scala.jdk.CollectionConverters.*

/** The setup and teardown methods of the JUnit tests, which run around the code of a test and so see its fields before
  * the setup has filled them and after a failed setup has left them empty.
  */
private[java] object Lifecycle {

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

  private val cache = new java.util.WeakHashMap[Element, java.lang.Boolean]

  def isTeardown(method: Element): Boolean = teardown.exists(Nullness.hasAnnotation(method, _))

  /** The class declaration of the outermost local or anonymous class that holds the path, if any. */
  def enclosingLocalClass(path: TreePath): Option[TreePath] =
    Iterator
      .iterate(path)(_.getParentPath)
      .takeWhile(_ != null)
      .filter(candidate => candidate.getLeaf.isInstanceOf[ClassTree])
      .filter(candidate => !Option(candidate.getParentPath).exists(_.getLeaf.isInstanceOf[ClassTree]))
      .filter(candidate => Option(candidate.getParentPath).exists(!_.getLeaf.isInstanceOf[CompilationUnitTree]))
      .toVector
      .lastOption

  /** Whether the code at the point, a lambda or a local class, is created where the setup has not yet filled the field:
    * in an initialiser or a constructor, or in a setup method before it assigns the field.
    */
  def createdBeforeSetup(trees: Trees, unit: CompilationUnitTree, point: TreePath, field: Element): Boolean = {
    val member = Iterator
      .iterate(point)(_.getParentPath)
      .takeWhile(_ != null)
      .find(candidate => Option(candidate.getParentPath).exists(_.getLeaf.isInstanceOf[ClassTree]))
    member.exists { path =>
      path.getLeaf match {
        case method: MethodTree if method.getName.contentEquals("<init>") => true
        case _: MethodTree                                                =>
          Option(trees.getElement(path)).exists { element =>
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
    Option(cache.get(field)).fold {
      val found = compute(trees, field)
      cache.put(field, found)
      found
    }(_.booleanValue)

  private def compute(trees: Trees, field: Element): Boolean =
    Option(field.getEnclosingElement).flatMap(owner => Option(trees.getPath(owner))).exists { path =>
      path.getLeaf match {
        case holder: ClassTree =>
          holder.getMembers.asScala.exists {
            case method: MethodTree =>
              val methodPath = new TreePath(path, method)
              Option(trees.getElement(methodPath)).exists(isSetup) && assignments(trees, methodPath, field).nonEmpty
            case _ => false
          }
        case _ => false
      }
    }

  private def assignments(trees: Trees, method: TreePath, field: Element): Vector[AssignmentTree] = {
    val found = Vector.newBuilder[AssignmentTree]
    new TreePathScanner[Unit, Unit] {
      override def visitAssignment(assignment: AssignmentTree, unused: Unit): Unit = {
        val named = assignment.getVariable match {
          case identifier: IdentifierTree => identifier.getName.contentEquals(field.getSimpleName)
          case select: MemberSelectTree   => select.getIdentifier.contentEquals(field.getSimpleName)
          case _                          => false
        }
        if named && Option(trees.getElement(new TreePath(getCurrentPath, assignment.getVariable))).forall(_ == field)
        then
          found += assignment
        super.visitAssignment(assignment, unused)
      }
    }.scan(method, ())
    found.result()
  }
}
