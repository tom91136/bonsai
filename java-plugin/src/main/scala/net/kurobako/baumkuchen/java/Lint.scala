package net.kurobako.baumkuchen.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.Trees
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.Element
import javax.lang.model.element.TypeElement
import javax.lang.model.util.Elements
import javax.lang.model.util.Types
import scala.jdk.CollectionConverters.*

/** What a lint finds in the code of a tree, which the analysis reports as a finding of its own.
  *
  * @param detail
  *   what is wrong with the code
  * @param at
  *   the code the finding is about, which is the code the lint was asked about unless it says otherwise
  */
final case class Found(detail: String, at: Tree)

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
  def elementOf(tree: Tree): Element = trees.getElement(new TreePath(path, tree))

  /** The type of the code the lint is asked about. */
  def typeOf: TypeMirror = trees.getTypeMirror(path)

  /** The type of a tree of the code, which the lints compare the types of the code by. */
  def typeOf(tree: Tree): TypeMirror = trees.getTypeMirror(new TreePath(path, tree))

  /** The line a tree of the code is on. */
  def line(tree: Tree): Int = Lints.line(trees, unit, tree)

  /** The tree of the code that holds this one and is of one of the kinds, if any. */
  def enclosing(kinds: Set[Tree.Kind]): Option[Tree] = ancestors.find(tree => kinds.contains(tree.getKind))

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

/** The lints of the Java code, which the plugin runs over the code it compiles.
  */
object Lints {

  /** The lints of the code of a type, which the compiler attributes one type at a time. */
  val classLints: Vector[Lint] =
    LintBugs.lints ++
      LintRedundant.lints ++
      LintControlFlow.lints ++
      LintErrors.lints ++
      LintThreading.lints ++
      LintPerformance.lints ++
      LintSerialization.lints ++
      LintOptionals.lints ++
      LintFlow.lints ++
      LintScopes.lints ++
      LintReflection.lints ++
      LintInternals.lints

  /** The lints of the file as a whole, which the compiler reads once. */
  val fileLints: Vector[Lint] = LintRedundant.fileLints ++ LintLogging.lints ++ LintInternals.fileLints

  /** The lints of each kind of tree, so that the walk asks only the lints that read the tree. */
  private[java] val byKind: Map[Tree.Kind, Vector[Lint]] =
    classLints.flatMap(lint => lint.kinds.map(kind => kind -> lint)).groupBy(_._1).view.mapValues(_.map(_._2)).toMap

  /** The kinds of tree that hold a loop, which the lints that care of the turns of a loop read; the loop of a
    * collection is a loop as well.
    */
  val loops: Set[Tree.Kind] =
    Set(Tree.Kind.WHILE_LOOP, Tree.Kind.FOR_LOOP, Tree.Kind.DO_WHILE_LOOP, Tree.Kind.ENHANCED_FOR_LOOP)

  /** The kinds of tree that hold a type, which the finding of a lint is placed on. */
  val owners: Set[Tree.Kind] =
    Set(Tree.Kind.CLASS, Tree.Kind.INTERFACE, Tree.Kind.ENUM, Tree.Kind.RECORD, Tree.Kind.ANNOTATION_TYPE)

  /** The line the code of a tree is on. */
  def line(trees: Trees, unit: CompilationUnitTree, tree: Tree): Int = {
    // a method is placed on the line of its result type, which is where its name is, as its annotations and modifiers
    // may be on lines above it
    val positions = trees.getSourcePositions
    val offset    = tree match {
      case method: MethodTree if method.getReturnType != null =>
        positions.getStartPosition(unit, method.getReturnType)
      case _ => positions.getStartPosition(unit, tree)
    }
    if offset < 0 then 0 else unit.getLineMap.getLineNumber(offset).toInt
  }

  /** The declaration of the type the compiler has just attributed, which the file holds. */
  def declaration(trees: Trees, compilation: CompilationUnitTree, typeElement: TypeElement): Option[ClassTree] = {
    val name = typeElement.getQualifiedName.toString
    val root = new TreePath(compilation)
    compilation.getTypeDecls.asScala.collectFirst {
      case tree: ClassTree if Collector.qualified(trees.getElement(new TreePath(root, tree))) == name => tree
    }
  }

  /** The file the code is in. */
  def path(unit: CompilationUnitTree): String =
    Option(unit.getSourceFile).map(_.toUri.getPath).getOrElse("")

  /** Whether the type is the one of the name, which the lints compare the types of the code by. */
  def isType(tpe: TypeMirror, name: String): Boolean = tpe != null && tpe.toString == name
}
