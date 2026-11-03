package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.Trees
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.Element
import javax.lang.model.element.Modifier
import javax.lang.model.element.TypeElement
import net.kurobako.bonsai.java.Javac.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** The lints of the Java code, which the plugin runs over the code it compiles.
  */
object Lints {

  /** The kinds of tree that hold a loop, which the lints that care of the turns of a loop read; the loop of a
    * collection is a loop as well.
    */
  val loops: Set[Tree.Kind] =
    Set(Tree.Kind.WHILE_LOOP, Tree.Kind.FOR_LOOP, Tree.Kind.DO_WHILE_LOOP, Tree.Kind.ENHANCED_FOR_LOOP)

  /** The path of a tree under the path of the tree that holds it. */
  def child(path: TreePath, tree: Tree): TreePath = TreePath(path, tree)

  /** The element a tree under the path names. */
  def elementOf(trees: Trees, path: TreePath, tree: Tree): Element = trees.getElement(child(path, tree))

  /** The type a tree under the path has. */
  def typeOf(trees: Trees, path: TreePath, tree: Tree): TypeMirror = trees.getTypeMirror(child(path, tree))

  /** The declaration of the type the compiler has just attributed, which the file holds. */
  def declaration(trees: Trees, compilation: CompilationUnitTree, typeElement: TypeElement): Option[ClassTree] = {
    val name = typeElement.getQualifiedName.toString
    val root = TreePath(compilation)
    compilation.getTypeDecls.asScala.collectFirst {
      case tree: ClassTree if Collector.qualified(trees, TreePath(root, tree)) == name =>
        tree
    }
  }

  /** Whether the element is a static final field, the constant the lints read as a value that does not change. */
  def constant(element: Element): Boolean =
    element.getModifiers.contains(Modifier.STATIC) && element.getModifiers.contains(Modifier.FINAL)

  /** The expression under the parentheses around a tree. */
  @annotation.tailrec
  def unparenthesised(tree: ExpressionTree): ExpressionTree = tree match {
    case parenthesized: ParenthesizedTree => unparenthesised(parenthesized.getExpression)
    case _                                => tree
  }

  /** The path at or above a path that is not parenthesised, if any. */
  @annotation.tailrec
  def enclosing(path: TreePath): Option[TreePath] = Option(path) match {
    case Some(found) if found.getLeaf.isInstanceOf[ParenthesizedTree] => enclosing(found.getParentPath)
    case found                                                        => found
  }

  /** The return statements of a body, leaving out those of nested classes and lambdas. */
  def returns(body: Tree): Vector[ReturnTree] = {
    val returned = mutable.ArrayBuffer.empty[ReturnTree]
    Scan.foreach(body, !_.isInstanceOf[ClassTree | LambdaExpressionTree]) { case node: ReturnTree =>
      returned += node
    }
    returned.toVector
  }

  /** The text of a string literal. */
  def literalText(tree: Tree): Option[String] = tree match {
    case literal: LiteralTree => literal.value.collect { case text: String => text }
    case _                    => None
  }

  /** The arguments of a call or an instance creation, in the order the code gives them. */
  def arguments(call: MethodInvocationTree | NewClassTree): Vector[ExpressionTree] =
    (call match {
      case invocation: MethodInvocationTree => invocation.getArguments
      case construction: NewClassTree       => construction.getArguments
    }).asScala.toVector

  /** Whether the type is the one of the name, which the lints compare the types of the code by. */
  def isType(tpe: TypeMirror, name: String): Boolean = tpe.toString == name

  /** The value of a number a literal holds. */
  def numeric(literal: LiteralTree): Option[Double] = (literal.getValue: Any) match {
    case number: Int    => Some(number.toDouble)
    case number: Long   => Some(number.toDouble)
    case number: Float  => Some(number.toDouble)
    case number: Double => Some(number)
    case _              => None
  }

  /** The name of the type that declares an element, if it has one. */
  def owner(element: Element): Option[String] =
    element.enclosing.collect { case owner: TypeElement => owner.getQualifiedName.toString }

  /** Whether a type is a subtype of another, false where the compiler cannot relate the two. */
  def subtype(context: Context, left: TypeMirror, right: TypeMirror): Boolean =
    try context.types.isSubtype(left, right)
    catch { case _: IllegalArgumentException => false }
}
