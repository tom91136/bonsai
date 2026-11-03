package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.Trees
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.Element
import javax.lang.model.util.Elements
import javax.lang.model.util.Types
import scala.jdk.CollectionConverters.*

/** The code a lint reads, which is the tree it is asked about and the code that holds it.
  *
  * The paths of the code are the ones the walk keeps, so that the lints read the elements and the types of the code the
  * compiler attributed, whichever compiler the build runs.
  *
  * @param trees
  *   the trees of the compiler
  * @param unit
  *   the code of the file, which the lint reads
  * @param path
  *   the path of the tree the lint is asked about
  * @param owner
  *   the name of the type that holds the code, which the finding is placed on
  * @param method
  *   the name of the method that holds the code, if any
  * @param ancestors
  *   the trees that hold the code, the closest first
  * @param registry
  *   what the lints of one compilation leave for each other, and for the end of it
  */
class Context(
    val trees: Trees,
    val elements: Elements,
    val types: Types,
    val unit: CompilationUnitTree,
    val path: TreePath,
    val owner: String,
    val method: Option[String],
    val ancestors: List[Tree],
    val registry: Registry = Registry(),
    val suppressed: Set[String] = Set.empty
) {

  /** The tree the lint is asked about. */
  def tree: Tree = path.getLeaf

  /** The element the tree names, which is the type or the member it holds. */
  def element: Element = trees.getElement(path)

  /** The element a tree of the code names, which is the variable or the member it holds. */
  def elementOf(tree: Tree): Element = Lints.elementOf(trees, path, tree)

  /** Whether a tree of the code, under the path, names the element. */
  def refers(path: TreePath, tree: Tree, element: Element): Boolean = Lints.elementOf(trees, path, tree) == element

  /** The type of the code the lint is asked about. */
  def typeOf: TypeMirror = trees.getTypeMirror(path)

  /** The type of a tree of the code, which the lints compare the types of the code by. */
  def typeOf(tree: Tree): TypeMirror = Lints.typeOf(trees, path, tree)

  /** The type of a tree of the file, if the compiler attributed one. */
  def mirrorOf(tree: Tree): Option[TypeMirror] =
    Option(trees.getPath(unit, tree)).flatMap(found => Option(trees.getTypeMirror(found)))

  /** The tree of the code that holds this one and is of one of the kinds, if any. */
  def enclosing(kinds: Set[Tree.Kind]): Option[Tree] = ancestors.find(tree => kinds.contains(tree.getKind))

  /** The statements of the block that holds the code, with the place of the code among them. */
  def siblings: Option[(Vector[StatementTree], Int)] =
    ancestors.headOption.collect { case block: BlockTree => block.getStatements.asScala.toVector }.flatMap {
      statements => Some(statements.indexWhere(_ eq tree)).filter(_ >= 0).map(statements -> _)
    }

  /** The statement of the block that holds the code, that many places after it, or before it for a negative number. */
  def sibling(offset: Int): Option[StatementTree] =
    siblings.flatMap((statements, index) => statements.lift(index + offset))

  /** Whether the code is in a loop, which the lints that care of the turns of a loop read. */
  def inLoop: Boolean = enclosing(Lints.loops).isDefined

  /** A finding of the lint, of the code it was asked about. */
  def found(detail: String): Found = Found(detail, tree)

  /** A finding of thelint, of the code it names. */
  def found(detail: String, at: Tree): Found = Found(detail, at)
}
