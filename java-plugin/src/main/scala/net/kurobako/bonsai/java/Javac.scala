package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.Trees
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.Element
import javax.lang.model.element.TypeElement
import javax.lang.model.element.VariableElement
import javax.lang.model.util.Elements

/** What javac leaves out, which it gives as null, read as `Option` so that the lints never see a null. */
private object Javac {

  extension (tree: VariableTree) {
    def initializer: Option[ExpressionTree] = Option(tree.getInitializer)
  }

  extension (tree: MethodTree) {
    def body: Option[BlockTree]  = Option(tree.getBody)
    def returnType: Option[Tree] = Option(tree.getReturnType)
  }

  extension (tree: NewClassTree) {
    def classBody: Option[ClassTree] = Option(tree.getClassBody)
  }

  extension (tree: NewArrayTree) {
    def initializers: Option[java.util.List[? <: ExpressionTree]] = Option(tree.getInitializers)
    def elementType: Option[Tree]                                 = Option(tree.getType)
  }

  extension (tree: CaseTree) {
    def body: Option[Tree]                                     = Option(tree.getBody)
    def guard: Option[ExpressionTree]                          = Option(tree.getGuard)
    def statements: Option[java.util.List[? <: StatementTree]] = Option(tree.getStatements)
  }

  extension (tree: IfTree) {
    def elseStatement: Option[StatementTree] = Option(tree.getElseStatement)
  }

  extension (tree: ForLoopTree) {
    def condition: Option[ExpressionTree] = Option(tree.getCondition)
  }

  extension (tree: TryTree) {
    def finallyBlock: Option[BlockTree] = Option(tree.getFinallyBlock)
  }

  extension (tree: ReturnTree) {
    def returned: Option[ExpressionTree] = Option(tree.getExpression)
  }

  extension (tree: BreakTree) {
    def label: Option[javax.lang.model.element.Name] = Option(tree.getLabel)
  }

  extension (tree: ContinueTree) {
    def label: Option[javax.lang.model.element.Name] = Option(tree.getLabel)
  }

  extension (tree: InstanceOfTree) {
    def pattern: Option[PatternTree] = Option(tree.getPattern)
  }

  extension (tree: LiteralTree) {
    def value: Option[Object] = Option(tree.getValue)
  }

  extension (element: VariableElement) {
    def constant: Option[Object] = Option(element.getConstantValue)
  }

  extension (path: TreePath) {
    def parent: Option[TreePath] = Option(path.getParentPath)

    /** The path and the paths that hold it, the closest first. */
    def withParents: Iterator[TreePath] = Iterator.unfold(Option(path))(_.map(found => found -> found.parent))

    def parents: Iterator[TreePath] = withParents.drop(1)
  }

  extension (element: Element) {
    def enclosing: Option[Element] = Option(element.getEnclosingElement)

    /** The element and the elements that hold it, the closest first. */
    def withEnclosing: Iterator[Element] = Iterator.unfold(Option(element))(_.map(found => found -> found.enclosing))

    def enclosings: Iterator[Element] = withEnclosing.drop(1)
  }

  extension (elements: Elements) {
    def typeNamed(name: String): Option[TypeElement] = Option(elements.getTypeElement(name))
  }

  extension (context: Context) {
    def namedElement: Option[Element]                  = Option(context.element)
    def namedElement(tree: Tree): Option[Element]      = Option(context.elementOf(tree))
    def attributedType(tree: Tree): Option[TypeMirror] = Option(context.typeOf(tree))
  }

  extension (trees: Trees) {
    def namedElement(path: TreePath): Option[Element] = Option(trees.getElement(path))
  }
}
