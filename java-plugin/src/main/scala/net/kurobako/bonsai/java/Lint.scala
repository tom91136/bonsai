package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.TreeScanner
import com.sun.source.util.Trees
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.Element
import javax.lang.model.element.ExecutableElement
import javax.lang.model.element.Modifier
import javax.lang.model.element.TypeElement
import javax.lang.model.util.Elements
import javax.lang.model.util.Types
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** What a lint finds in the code of a tree, which the analysis reports as a finding of its own.
  *
  * @param detail
  *   what is wrong with the code
  * @param at
  *   the code the finding is about, which is the code the lint was asked about unless it says otherwise
  * @param offset
  *   where in the file the finding is placed, for a finding that is of no tree of its own such as a comment, or none
  * @param id
  *   the id the finding is reported under, for a lint that reports under several, or empty for the id of the lint
  */
final case class Found(detail: String, at: Tree, offset: Int = -1, id: String = "") {

  /** The id the finding is reported under, which the lint that found it is the default of. */
  def reportedAs(lint: Lint): String = if id.isEmpty then lint.id else id
}

/** A call of a method, with the receiver and the simple name the code gives the method. */
final case class Call(
    path: TreePath,
    invocation: MethodInvocationTree,
    method: ExecutableElement,
    receiver: Option[ExpressionTree],
    name: String,
    arguments: Vector[ExpressionTree]
)

object Call {

  /** The receiver, if any, and the simple name of the method a call names, bare or through a receiver. */
  def selected(method: ExpressionTree): Option[(Option[ExpressionTree], String)] = method match {
    case select: MemberSelectTree   => Some((Some(select.getExpression), select.getIdentifier.toString))
    case identifier: IdentifierTree => Some((None, identifier.getName.toString))
    case _                          => None
  }

  /** Whether the expression names a method of the simple name, called bare or through a receiver. */
  def named(method: ExpressionTree, name: String): Boolean = selected(method).exists(_._2 == name)

  /** Whether the tree is the `this` identifier. */
  def isThis(tree: Tree): Boolean = tree match {
    case identifier: IdentifierTree => identifier.getName.contentEquals("this")
    case _                          => false
  }

  /** Whether the statement is a call of `this(..)`, which hands the work of a constructor to another one. */
  def isThisCall(statement: StatementTree): Boolean = statement match {
    case expression: ExpressionStatementTree =>
      expression.getExpression match {
        case call: MethodInvocationTree => isThis(call.getMethodSelect)
        case _                          => false
      }
    case _ => false
  }

  /** The call a path holds, if it is a call of a method. */
  def of(trees: Trees, path: TreePath): Option[Call] = path.getLeaf match {
    case invocation: MethodInvocationTree =>
      (trees.getElement(path), selected(invocation.getMethodSelect)) match {
        case (method: ExecutableElement, Some((receiver, name))) =>
          Some(Call(path, invocation, method, receiver, name, Lints.arguments(invocation)))
        case _ => None
      }
    case _ => None
  }
}

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
final class Context(
    val trees: Trees,
    val elements: Elements,
    val types: Types,
    val unit: CompilationUnitTree,
    val path: TreePath,
    val owner: String,
    val method: Option[String],
    val ancestors: List[Tree],
    val registry: Registry = new Registry(),
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

/** A lint of the Java code, which the plugin finds in the code it compiles.
  *
  * The lints are of the code of one file, as the trees the compiler gives are of one compilation; the rules that need
  * the evidence of the whole build are of the analysis, which holds the model of the build.
  */
trait Lint {

  /** The id the lint is reported under, which the analysis and the user name it by. */
  val id: String

  /** The kinds of tree the lint reads; the code is walked once, and the lint is asked about the trees of its kinds. */
  val kinds: Set[Tree.Kind]

  /** Whether the lint runs without the build naming it; a lint that asks a project to choose a style is off by default.
    */
  def enabledByDefault: Boolean = true

  /** What the lint finds in the code of a tree of one of its kinds. */
  def check(context: Context, tree: Tree): Vector[Found]
}

object Lint {

  /** The expression under the parentheses and the casts around an expression. */
  @annotation.tailrec
  def unwrapped(tree: ExpressionTree): ExpressionTree = tree match {
    case parenthesized: ParenthesizedTree => unwrapped(parenthesized.getExpression)
    case cast: TypeCastTree               => unwrapped(cast.getExpression)
    case _                                => tree
  }

  /** The tree under the parentheses and the casts around a tree. */
  def unwrapped(tree: Tree): Tree = tree match {
    case expression: ExpressionTree => unwrapped(expression)
    case _                          => tree
  }

  /** The statement a block of only one statement holds, through any blocks around it. */
  @annotation.tailrec
  def single(statement: StatementTree): StatementTree = statement match {
    case block: BlockTree if block.getStatements.size == 1 => single(block.getStatements.get(0))
    case _                                                 => statement
  }

  /** The boolean a statement returns when it is a return of a literal, or a block of only that. */
  def booleanReturn(statement: StatementTree): Option[Boolean] = single(statement) match {
    case ret: ReturnTree =>
      Option(ret.getExpression).collect {
        case literal: LiteralTree if literal.getKind == Tree.Kind.BOOLEAN_LITERAL =>
          literal.getValue == java.lang.Boolean.TRUE
      }
    case _ => None
  }
}

/** The lints of the Java code, which the plugin runs over the code it compiles.
  */
object Lints {

  /** The kinds of tree that hold a loop, which the lints that care of the turns of a loop read; the loop of a
    * collection is a loop as well.
    */
  val loops: Set[Tree.Kind] =
    Set(Tree.Kind.WHILE_LOOP, Tree.Kind.FOR_LOOP, Tree.Kind.DO_WHILE_LOOP, Tree.Kind.ENHANCED_FOR_LOOP)

  /** The path of a tree under the path of the tree that holds it. */
  def child(path: TreePath, tree: Tree): TreePath = new TreePath(path, tree)

  /** The element a tree under the path names. */
  def elementOf(trees: Trees, path: TreePath, tree: Tree): Element = trees.getElement(child(path, tree))

  /** The type a tree under the path has. */
  def typeOf(trees: Trees, path: TreePath, tree: Tree): TypeMirror = trees.getTypeMirror(child(path, tree))

  /** The declaration of the type the compiler has just attributed, which the file holds. */
  def declaration(trees: Trees, compilation: CompilationUnitTree, typeElement: TypeElement): Option[ClassTree] = {
    val name = typeElement.getQualifiedName.toString
    val root = new TreePath(compilation)
    compilation.getTypeDecls.asScala.collectFirst {
      case tree: ClassTree if Collector.qualified(trees.getElement(new TreePath(root, tree)))(using trees) == name =>
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
    new TreeScanner[Unit, Unit] {
      override def visitReturn(node: ReturnTree, unused: Unit): Unit = {
        returned += node
        super.visitReturn(node, unused)
      }
      override def visitClass(node: ClassTree, unused: Unit): Unit                       = ()
      override def visitLambdaExpression(node: LambdaExpressionTree, unused: Unit): Unit = ()
    }.scan(body, ())
    returned.toVector
  }

  /** The text of a string literal. */
  def literalText(tree: Tree): Option[String] = tree match {
    case literal: LiteralTree => Option(literal.getValue).collect { case text: String => text }
    case _                    => None
  }

  /** The arguments of a call or an instance creation, in the order the code gives them. */
  def arguments(call: MethodInvocationTree | NewClassTree): Vector[ExpressionTree] =
    (call match {
      case invocation: MethodInvocationTree => invocation.getArguments
      case construction: NewClassTree       => construction.getArguments
    }).asScala.toVector

  /** Whether the type is the one of the name, which the lints compare the types of the code by. */
  def isType(tpe: TypeMirror, name: String): Boolean = tpe != null && tpe.toString == name

  /** The name of the type that declares an element, if it has one. */
  def owner(element: Element): Option[String] =
    Option(element.getEnclosingElement).collect { case owner: TypeElement => owner.getQualifiedName.toString }

  /** Whether a type is a subtype of another, false where the compiler cannot relate the two. */
  def subtype(context: Context, left: TypeMirror, right: TypeMirror): Boolean =
    left != null && right != null &&
      (try context.types.isSubtype(left, right)
      catch { case _: IllegalArgumentException => false })
}
