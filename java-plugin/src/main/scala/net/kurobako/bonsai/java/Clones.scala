package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.TreePathScanner
import com.sun.source.util.Trees
import javax.lang.model.element.Element
import javax.lang.model.element.ElementKind
import javax.lang.model.element.ExecutableElement
import javax.lang.model.element.PackageElement
import javax.lang.model.element.TypeElement
import javax.lang.model.element.VariableElement
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.hashing.MurmurHash3

/** Finds the code of the Java build that recurs elsewhere, the way a duplicate locator does: the statements of the
  * method bodies are hashed with the local definitions anonymised, and the fragments that share a hash are confirmed
  * with a hash that numbers the locals by their first occurrence, so `a + a` and `a + b` differ while the consistent
  * renamings match. The fragments that also match once the values of the literals are ignored are near duplicates,
  * reported with the literals that differ as holes.
  *
  * The fragments are gathered here, as the trees the compiler gives are of the code of one compilation only; the copies
  * they make are grouped by the analysis, which holds the code of the whole build.
  */
class Clones(trees: Trees) {

  private val found = mutable.ArrayBuffer.empty[Model.Fragment]

  /** The fragments of the body of a method, which the type that holds the method names. */
  def add(body: Tree, path: TreePath, owner: String): Unit = {
    val nodes = mutable.ArrayBuffer.empty[Clones.Node]
    Clones.collect(nodes, trees, new TreePath(path, body))
    nodes.indices.foreach { index =>
      nodes(index).tree match {
        case _: BlockTree =>
          // A block of statements is a fragment of its own, as a copy of a block of code is what recurs.
          val statements = nodes(index).children
          if statements.size >= 2 then {
            found += Clones.fragment(nodes, statements, owner)
            // A run of statements from each place in the block, of the size that makes it worth reporting.
            var start = 0
            while start < statements.size do {
              var end   = start
              var total = 0
              while end < statements.size && total < Clones.WindowCost do {
                total += Clones.cost(nodes, statements(end))
                end += 1
              }
              if end - start >= 2 && total >= Clones.WindowCost then
                found += Clones.fragment(nodes, statements.slice(start, end), owner)
              start += 1
            }
          }
        case _ => ()
      }
    }
  }

  /** The fragments of the code of the build, which the analysis groups into the copies that recur. */
  def fragments: Vector[Model.Fragment] = found.toVector
}

object Clones {

  // A run of statements must hold this many nodes to be worth reporting.
  val WindowCost = 24

  /** The fragment of the code of a run of statements, with the hashes that tell the copies apart. */
  private def fragment(nodes: mutable.ArrayBuffer[Node], items: Vector[Int], owner: String): Model.Fragment = {
    val locals   = mutable.LinkedHashMap.empty[Element, Long]
    val literals = mutable.ArrayBuffer.empty[String]
    var exact    = 17L
    var loose    = 17L
    var cost     = 0
    items.foreach { item =>
      val (valued, ignored) = numbered(nodes, item, locals, literals)
      exact = mix(exact * 31 + valued)
      loose = mix(loose * 31 + ignored)
      cost += Clones.cost(nodes, item)
    }
    val first = nodes(items.head)
    val last  = nodes(items.last)
    Model.Fragment(
      owner,
      first.path.getCompilationUnit.getSourceFile.toUri.getPath,
      first.startLine,
      last.endLine,
      first.from,
      last.to,
      cost,
      exact,
      loose,
      literals.toVector
    )
  }

  /** The hash of the code of a node, with the values of the literals as they are and as they are ignored, and the
    * literals it holds, which the near duplicates differ in.
    */
  private def numbered(
      nodes: mutable.ArrayBuffer[Node],
      index: Int,
      locals: mutable.LinkedHashMap[Element, Long],
      literals: mutable.ArrayBuffer[String]
  ): (Long, Long) = {
    val node              = nodes(index)
    val (valued, ignored) = own(node, locals, literals)
    var exact             = valued
    var loose             = ignored
    node.children.foreach { child =>
      val (childValued, childIgnored) = numbered(nodes, child, locals, literals)
      exact = mix(exact * 31 + childValued)
      loose = mix(loose * 31 + childIgnored)
    }
    (exact, loose)
  }

  /** The part of a node the hash takes, which the local definitions are anonymised in. */
  private def own(
      node: Node,
      locals: mutable.LinkedHashMap[Element, Long],
      literals: mutable.ArrayBuffer[String]
  ): (Long, Long) = {
    val kind      = node.tree.getKind.name.hashCode.toLong
    val reference = node.symbol match {
      case None          => 0L
      case Some(element) =>
        if Clones.local(element) then locals.getOrElseUpdate(element, locals.size.toLong + 1L)
        else Clones.identities.getOrElseUpdate(element, Clones.identity(element))
    }
    val literal = node.literal match {
      case None          => (0L, 0L)
      case Some(literal) =>
        literals += literal.value
        (
          literal.kind.hashCode.toLong * 31 + literal.value.hashCode.toLong,
          literal.kind.hashCode.toLong
        )
    }
    val base = mix(kind * 31 + reference)
    (mix(base * 31 + literal._1), mix(base * 31 + literal._2))
  }

  /** The number of the nodes of the code of a statement, which is what makes it worth reporting. */
  private def cost(nodes: mutable.ArrayBuffer[Node], index: Int): Int = {
    val node = nodes(index)
    if node.cost == 0 then {
      val total = 1 + node.children.map(child => cost(nodes, child)).sum
      node.cost = total
      total
    } else node.cost
  }

  /** Gathers the nodes of the code of a body, with the paths they are at, which the elements are read from. */
  private def collect(nodes: mutable.ArrayBuffer[Node], trees: Trees, root: TreePath): Unit = {
    val stack   = mutable.Stack.empty[Int]
    val scanner = new TreePathScanner[Unit, Unit] {
      override def scan(tree: Tree, unused: Unit): Unit =
        if tree != null then {
          val index = nodes.size
          nodes += new Node(trees, tree, new TreePath(getCurrentPath(), tree))
          stack.headOption.foreach(parent => nodes(parent).children = nodes(parent).children :+ index)
          stack.push(index)
          try super.scan(tree, unused)
          finally stack.pop()
        }
    }
    val rootIndex = nodes.size
    nodes += new Node(trees, root.getLeaf, root)
    stack.push(rootIndex)
    try scanner.scan(root, ())
    finally stack.pop()
  }

  /** Whether an element is a local definition, which the hashes anonymise. */
  def local(element: Element): Boolean =
    element.getKind match {
      case ElementKind.LOCAL_VARIABLE | ElementKind.PARAMETER | ElementKind.EXCEPTION_PARAMETER |
          ElementKind.RESOURCE_VARIABLE | ElementKind.BINDING_VARIABLE => true
      case _ => false
    }

  /** The identity of an element, which is its name in the build, so the same field is the same everywhere. */
  def identity(element: Element): Long = {
    val name = Clones.name(element)
    (MurmurHash3.stringHash(name).toLong << 32) | (name.hashCode & 0xffffffffL)
  }

  /** The name of an element, which is the name of the type that holds it and its own name. */
  def name(element: Element): String = element match {
    case e: ExecutableElement =>
      val parameters = e.getParameters.asScala.map(parameter => parameter.asType().toString).mkString(",")
      s"${Clones.owner(e)}.${e.getSimpleName}($parameters)"
    case e: VariableElement => s"${Clones.owner(e)}.${e.getSimpleName}"
    case e: TypeElement     => e.getQualifiedName.toString
    case e: PackageElement  => e.getQualifiedName.toString
    case other              => other.toString
  }

  /** The name of the type that holds an element. */
  private def owner(element: Element): String = Option(element.getEnclosingElement) match {
    case Some(e: TypeElement)    => e.getQualifiedName.toString
    case Some(e: PackageElement) => e.getQualifiedName.toString
    case Some(other)             => other.getSimpleName.toString
    case None                    => ""
  }

  /** The identities of the elements of the build, which are the same everywhere. */
  private val identities = mutable.HashMap.empty[Element, Long]

  private def mix(value: Long): Long = {
    val first  = (value ^ (value >>> 33)) * 0xff51afd7ed558ccdL
    val second = (first ^ (first >>> 33)) * 0xc4ceb9fe1a85ec53L
    second ^ (second >>> 33)
  }

  /** A node of the code, which the hashes take. */
  private final class Node(val trees: Trees, val tree: Tree, val path: TreePath) {
    var children: Vector[Int]            = Vector.empty
    var cost: Int                        = 0
    private var element: Option[Element] = None
    private var known: Boolean           = false

    /** The element the node names, which is the variable or the method it holds. */
    def symbol: Option[Element] = {
      if !known then {
        known = true
        element = Option(trees.getElement(path))
      }
      element
    }

    /** The literal the node holds, if it holds one. */
    def literal: Option[Clones.Literal] =
      tree match {
        case literalTree: LiteralTree =>
          Option(literalTree.getValue).map(value => Clones.Literal(value.getClass.getSimpleName, String.valueOf(value)))
        case _ => None
      }

    /** The line the code of the node begins on. */
    def startLine: Int = {
      val unit      = path.getCompilationUnit
      val positions = trees.getSourcePositions
      val offset    = positions.getStartPosition(unit, tree)
      if offset < 0 then 0 else unit.getLineMap.getLineNumber(offset).toInt
    }

    /** The line the code of the node ends on. */
    def endLine: Int = {
      val unit      = path.getCompilationUnit
      val positions = trees.getSourcePositions
      val offset    = positions.getEndPosition(unit, tree)
      if offset < 0 then 0 else unit.getLineMap.getLineNumber(offset).toInt
    }

    /** The place the code of the node begins at, which the copies are told apart by. */
    def from: Int = {
      val unit      = path.getCompilationUnit
      val positions = trees.getSourcePositions
      val offset    = positions.getStartPosition(unit, tree)
      if offset < 0 then 0 else offset.toInt
    }

    /** The place the code of the node ends at. */
    def to: Int = {
      val unit      = path.getCompilationUnit
      val positions = trees.getSourcePositions
      val offset    = positions.getEndPosition(unit, tree)
      if offset < 0 then 0 else offset.toInt
    }
  }

  /** A literal of the code, which the near duplicates differ in. */
  case class Literal(kind: String, value: String)
}
