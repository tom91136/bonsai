package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.TreePathScanner
import com.sun.source.util.Trees
import javax.lang.model.element.Element
import javax.lang.model.element.ElementKind
import javax.lang.model.element.ExecutableElement
import javax.lang.model.element.Modifier
import javax.lang.model.element.NestingKind
import javax.lang.model.element.PackageElement
import javax.lang.model.element.TypeElement
import javax.lang.model.element.VariableElement
import net.kurobako.bonsai.collections.CloneCost
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
  def add(body: Tree, path: TreePath, owner: String, method: ExecutableElement, overriding: Boolean): Unit = {
    val nodes = mutable.ArrayBuffer.empty[Clones.Node]
    Clones.collect(nodes, trees, new TreePath(path, body))
    val context = Clones.Context(owner, method, overriding)
    val entire  = Clones.entire(nodes, 0)
    nodes.indices.foreach { index =>
      nodes(index).tree match {
        case _: BlockTree =>
          // A block of statements is a fragment of its own, as a copy of a block of code is what recurs.
          val statements = nodes(index).children
          if statements.size >= 2 then {
            val total = Clones.cost(nodes, index)
            if total >= CloneCost.Expression then
              found += Clones.fragment(
                nodes,
                statements,
                context,
                Model.Roles.Whole,
                total - statements.map(Clones.cost(nodes, _)).sum
              )
            CloneCost.windows(statements.map(Clones.cost(nodes, _))).foreach((start, end) =>
              found += Clones.fragment(nodes, statements.slice(start, end), context, Model.Roles.Whole)
            )
          }
        case _ => ()
      }
      if Clones.call(nodes(index).tree) || entire.contains(index) then
        Clones.expression(nodes, index, entire, context).foreach(found += _)
    }
  }

  /** The fragments of the code of the build, which the analysis groups into the copies that recur. */
  def fragments: Vector[Model.Fragment] = found.toVector
}

object Clones {

  /** Where the fragments of a method come from. */
  private case class Context(owner: String, method: ExecutableElement, overriding: Boolean)

  /** The variables a fragment holds, as the groups of the analysis read them: a literal or a reference, with the
    * parameter of the method a reference starts at, if it does.
    */
  case class Variable(kind: Char, text: String, shown: String, parameter: Int, suffix: String)

  /** What a fragment is told apart by, besides its tree: the locals its code defines, and the parameters of its method.
    */
  private final class Scope(val declared: Set[Element], val parameters: Vector[Element]) {
    val variables = mutable.ArrayBuffer.empty[Variable]
    val inside    = mutable.LinkedHashMap.empty[Element, Int]
    val outside   = mutable.LinkedHashMap.empty[Element, Int]
  }

  /** The node of the single expression that the body of a method consists of, if it consists of one. */
  private def entire(nodes: mutable.ArrayBuffer[Node], root: Int): Option[Int] =
    nodes(root).children match {
      case Vector(statement) =>
        nodes(statement).tree match {
          case _: ReturnTree | _: ExpressionStatementTree => nodes(statement).children.headOption
          case _                                          => None
        }
      case _ => None
    }

  private def call(tree: Tree): Boolean = tree match {
    case _: MethodInvocationTree | _: NewClassTree => true
    case _                                         => false
  }

  /** The fragment of an expression, if it is worth reporting as a chain of calls, the body of a method, or the copy of
    * the body of one.
    */
  private def expression(
      nodes: mutable.ArrayBuffer[Node],
      index: Int,
      entire: Option[Int],
      context: Context
  ): Option[Model.Fragment] = {
    val tree    = nodes(index).tree
    val total   = cost(nodes, index)
    val calls   = steps(tree)
    val applied = invocations(nodes, index) > 0
    val small   = call(tree) && total >= CloneCost.Small && total < CloneCost.Expression &&
      (calls >= 2 || arguments(tree) >= 2)
    val whole  = applied && entire.contains(index)
    val tiny   = whole && !context.overriding && total >= CloneCost.Tiny && total < CloneCost.Expression
    val helper = whole && !context.overriding && context.method.getKind == ElementKind.METHOD &&
      total >= CloneCost.Helper &&
      total <= CloneCost.MaximumHelper
    val roles =
      (if small then Model.Roles.Small else 0) | (if tiny then Model.Roles.Tiny else 0) |
        (if whole then Model.Roles.Entire else 0) | (if call(tree) && calls == 1 then Model.Roles.Direct else 0) |
        (if helper then Model.Roles.Helper else 0)
    Option.when(applied && (small || whole || total >= CloneCost.Helper))(fragment(
      nodes,
      Vector(index),
      context,
      roles
    ))
  }

  /** The number of calls and constructions in the code of a node. */
  private def invocations(nodes: mutable.ArrayBuffer[Node], index: Int): Int =
    (if call(nodes(index).tree) then 1 else 0) + nodes(index).children.map(child => invocations(nodes, child)).sum

  /** The steps of the chain an expression ends in, a call or a selection from a result, as `a.b(c).d` holds three. */
  private def steps(tree: Tree): Int = Lint.unwrapped(tree) match {
    case invocation: MethodInvocationTree =>
      invocation.getMethodSelect match {
        case select: MemberSelectTree => 1 + steps(select.getExpression)
        case _                        => 1
      }
    case _: NewClassTree          => 1
    case select: MemberSelectTree => if path(select.getExpression) then 0 else 1 + steps(select.getExpression)
    case _                        => 0
  }

  private def arguments(tree: Tree): Int = tree match {
    case invocation: MethodInvocationTree => invocation.getArguments.size
    case construction: NewClassTree       => construction.getArguments.size
    case _                                => 0
  }

  /** Whether a tree names a variable through a chain of selections, which a copy may refer to another of. */
  private def path(tree: Tree): Boolean = tree match {
    case _: IdentifierTree        => true
    case select: MemberSelectTree => path(select.getExpression)
    case _                        => false
  }

  /** Whether a node names a variable, a field or a type that qualifies a selection, as a copy may refer to another. */
  private def reference(node: Node): Boolean = {
    def names(element: Element): Boolean =
      element.isInstanceOf[VariableElement] || element.isInstanceOf[TypeElement] && qualifies(node)
    node.tree match {
      case identifier: IdentifierTree => identifier.getName.toString != "this" && node.symbol.exists(names)
      case select: MemberSelectTree   => path(select.getExpression) && node.symbol.exists(names)
      case _                          => false
    }
  }

  private def qualifies(node: Node): Boolean =
    Option(node.path.getParentPath).map(_.getLeaf).exists {
      case select: MemberSelectTree => select.getExpression eq node.tree
      case _                        => false
    }

  /** The fragment of the code of a run of statements, with the hashes that tell the copies apart. */
  private def fragment(
      nodes: mutable.ArrayBuffer[Node],
      items: Vector[Int],
      context: Context,
      roles: Int,
      own: Int = 0
  ): Model.Fragment = {
    val locals                = mutable.LinkedHashMap.empty[Element, Long]
    val literals              = mutable.ArrayBuffer.empty[String]
    val scope                 = Scope(declared(nodes, items), context.method.getParameters.asScala.toVector)
    val (exact, loose, broad) = items.foldLeft((17L, 17L, 17L))((seed, item) =>
      chained(seed, numbered(nodes, item, locals, literals, scope, covered = false))
    )
    val cost  = own + items.map(Clones.cost(nodes, _)).sum
    val first = nodes(items.head)
    val last  = nodes(items.last)
    Model.Fragment(
      context.owner,
      first.path.getCompilationUnit.getSourceFile.toUri.getPath,
      first.startLine,
      last.endLine,
      first.from,
      last.to,
      cost,
      exact,
      loose,
      literals.toVector,
      member = context.method.getSimpleName.toString,
      broad = if items.size == 1 then broad else 0L,
      variables = scope.variables.toVector,
      roles = roles,
      reach = if everywhere(context.method) then "" else context.owner
    )
  }

  /** Whether a method is visible to all the code of the build, as a public static method of a public type is. */
  private def everywhere(method: ExecutableElement): Boolean = {
    def open(element: Element): Boolean = element match {
      case _: PackageElement        => true
      case typeElement: TypeElement =>
        val modifiers = typeElement.getModifiers
        modifiers.contains(Modifier.PUBLIC) &&
        (typeElement.getNestingKind == NestingKind.TOP_LEVEL || modifiers.contains(Modifier.STATIC)) &&
        open(typeElement.getEnclosingElement)
      case _ => false
    }
    val modifiers = method.getModifiers
    modifiers.contains(Modifier.PUBLIC) && modifiers.contains(Modifier.STATIC) && open(method.getEnclosingElement)
  }

  /** The variables and parameters that the code of the nodes defines. */
  private def declared(nodes: mutable.ArrayBuffer[Node], items: Vector[Int]): Set[Element] = {
    def go(index: Int): Vector[Element] =
      (nodes(index).tree match {
        case _: VariableTree => nodes(index).symbol.toVector
        case _               => Vector.empty
      }) ++ nodes(index).children.flatMap(go(_))
    items.flatMap(go(_)).toSet
  }

  /** The hash of the code of a node, with the values of the literals as they are and as they are ignored, then with the
    * references ignored too, and the literals it holds, which the near duplicates differ in. A node that a reference
    * covers holds nothing for the last.
    */
  private def numbered(
      nodes: mutable.ArrayBuffer[Node],
      index: Int,
      locals: mutable.LinkedHashMap[Element, Long],
      literals: mutable.ArrayBuffer[String],
      scope: Scope,
      covered: Boolean
  ): (Long, Long, Long) = {
    val node                    = nodes(index)
    val (valued, ignored, wide) = own(node, locals, literals)
    val named                   = !covered && reference(node)
    if named then scope.variables += variable(nodes, index, scope)
    else if !covered then
      node.literal.foreach(literal =>
        scope.variables += Variable(
          if literal.kind == "String" then 'S' else 'L',
          literal.value,
          literal.value,
          -1,
          ""
        )
      )
    val (exact, loose, broad) = node.children.foldLeft((valued, ignored, wide))((seed, child) =>
      chained(seed, numbered(nodes, child, locals, literals, scope, covered || named))
    )
    (exact, loose, if named || node.literal.isDefined then Reference else broad)
  }

  private def chained(seed: (Long, Long, Long), next: (Long, Long, Long)): (Long, Long, Long) =
    (mix(seed._1 * 31 + next._1), mix(seed._2 * 31 + next._2), mix(seed._3 * 31 + next._3))

  /** The reference a node makes, shown without the names the compiler gives, and the parameter it starts at. */
  private def variable(nodes: mutable.ArrayBuffer[Node], index: Int, scope: Scope): Variable = {
    def text(at: Int): (String, String) = {
      val node = nodes(at)
      node.tree match {
        case select: MemberSelectTree =>
          val (key, shown) = text(node.children.head)
          (s"$key.${select.getIdentifier}", s"$shown.${select.getIdentifier}")
        case identifier: IdentifierTree =>
          node.symbol match {
            case Some(element) if Clones.local(element) =>
              val (prefix, numbers) =
                if scope.declared.contains(element) then ("bound#", scope.inside) else ("free#", scope.outside)
              (prefix + numbers.getOrElseUpdate(element, numbers.size + 1), identifier.getName.toString)
            case Some(element) => (Clones.name(element), identifier.getName.toString)
            case None          => (identifier.getName.toString, identifier.getName.toString)
          }
        case other => (other.toString, other.toString)
      }
    }
    def head(at: Int): Int = nodes(at).tree match {
      case _: MemberSelectTree => head(nodes(at).children.head)
      case _                   => at
    }
    def selections(at: Int): String = nodes(at).tree match {
      case select: MemberSelectTree => s"${selections(nodes(at).children.head)}.${select.getIdentifier}"
      case _                        => ""
    }
    val (key, shown) = text(index)
    val parameter    = nodes(head(index)).symbol.map(scope.parameters.indexOf(_)).getOrElse(-1)
    Variable('R', key, shown, parameter, if parameter >= 0 then selections(index) else "")
  }

  /** The part of a node the hash takes, which the local definitions are anonymised in. The last hash leaves out what a
    * call names, as the node that names it holds that.
    */
  private def own(
      node: Node,
      locals: mutable.LinkedHashMap[Element, Long],
      literals: mutable.ArrayBuffer[String]
  ): (Long, Long, Long) = {
    val kind   = node.tree.getKind.name.hashCode.toLong
    val shared = node.symbol match {
      case None          => 0L
      case Some(element) =>
        if Clones.local(element) then Local else identities.getOrElseUpdate(element, nameHash(name(element)))
    }
    val reference = node.symbol match {
      case Some(element) if Clones.local(element) => locals.getOrElseUpdate(element, locals.size.toLong + 1L)
      case _                                      => shared
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
    val wide = if Clones.call(node.tree) then mix(kind * 31) else mix(kind * 31 + shared)
    (mix(base * 31 + literal._1), mix(base * 31 + literal._2), wide)
  }

  /** The cost of the code of a node on the scale of `CloneCost`, which is what makes it worth reporting. */
  private def cost(nodes: mutable.ArrayBuffer[Node], index: Int): Int = {
    val node = nodes(index)
    if node.cost < 0 then node.cost = weight(node) + node.children.map(child => cost(nodes, child)).sum
    node.cost
  }

  /** What a node adds to the cost: nothing for what Scala leaves out or writes without a node, as the modifiers, the
    * types, the wrapper of an expression statement, the parentheses, the block of a single statement, and the return
    * that ends a method or a lambda; a loop over a collection counts as the call of `foreach` with a lambda that Scala
    * writes it as.
    */
  private def weight(node: Node): Int = node.tree match {
    case _: ModifiersTree | _: AnnotationTree | _: PrimitiveTypeTree | _: ArrayTypeTree | _: ParameterizedTypeTree |
        _: AnnotatedTypeTree | _: WildcardTree | _: TypeParameterTree | _: UnionTypeTree | _: IntersectionTypeTree |
        _: ExpressionStatementTree | _: ParenthesizedTree => 0
    case _ if named(node.path)                                                  => 0
    case _: ReturnTree if last(node.path)                                       => 0
    case block: BlockTree if block.getStatements.size == 1                      => 0
    case _: EnhancedForLoopTree                                                 => 3
    case _: CompoundAssignmentTree                                              => 3
    case unary: UnaryTree if Effects.stepping(unary)                            => 4
    case identifier: IdentifierTree if identifier.getName.contentEquals("this") => 0
    case _                                                                      => 1
  }

  /** Whether a node names a type, which the declaration of a variable, a construction, a cast, a test and the type
    * arguments of a call write as a tree, and the names within that name do.
    */
  private def named(path: TreePath): Boolean =
    Option(path.getParentPath).exists { parent =>
      val tree = path.getLeaf
      parent.getLeaf match {
        case variable: VariableTree           => variable.getType eq tree
        case construction: NewClassTree       => construction.getIdentifier eq tree
        case cast: TypeCastTree               => cast.getType eq tree
        case test: InstanceOfTree             => test.getType eq tree
        case method: MethodTree               => method.getReturnType eq tree
        case invocation: MethodInvocationTree => invocation.getTypeArguments.asScala.exists(_ eq tree)
        case select: MemberSelectTree         => (select.getExpression eq tree) && named(parent)
        case _: ParameterizedTypeTree | _: ArrayTypeTree | _: AnnotatedTypeTree | _: WildcardTree |
            _: TypeParameterTree | _: UnionTypeTree | _: IntersectionTypeTree => true
        case _ => false
      }
    }

  /** Whether a return statement is the last of the body of a method or a lambda, which Scala writes as the value. */
  private def last(path: TreePath): Boolean =
    Option(path.getParentPath).exists { block =>
      block.getLeaf match {
        case body: BlockTree =>
          body.getStatements.asScala.lastOption.exists(_ eq path.getLeaf) &&
          Option(block.getParentPath).exists(owner =>
            owner.getLeaf.isInstanceOf[MethodTree] || owner.getLeaf.isInstanceOf[LambdaExpressionTree]
          )
        case _ => false
      }
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
          finally { val _ = stack.pop() }
        }
    }
    val rootIndex = nodes.size
    nodes += new Node(trees, root.getLeaf, root)
    stack.push(rootIndex)
    try scanner.scan(root, ())
    finally { val _ = stack.pop() }
  }

  /** Whether an element is a local definition, which the hashes anonymise. */
  private def local(element: Element): Boolean =
    element.getKind match {
      case ElementKind.LOCAL_VARIABLE | ElementKind.PARAMETER | ElementKind.EXCEPTION_PARAMETER |
          ElementKind.RESOURCE_VARIABLE | ElementKind.BINDING_VARIABLE => true
      case _ => false
    }

  /** The name of an element, which is the name of the type that holds it and its own name. */
  private def name(element: Element): String = element match {
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

  /** A 64-bit finaliser that spreads every bit of `value`, which the fragment hashes of both languages chain with. */
  def mix(value: Long): Long = {
    val first  = (value ^ (value >>> 33)) * 0xff51afd7ed558ccdL
    val second = (first ^ (first >>> 33)) * 0xc4ceb9fe1a85ec53L
    second ^ (second >>> 33)
  }

  val Local     = 0x5bd1e995L
  val Reference = 0x7f4a7c15L

  /** The hash of a name in the build, so the same field or method is the same everywhere. */
  def nameHash(name: String): Long = (MurmurHash3.stringHash(name).toLong << 32) | (name.hashCode & 0xffffffffL)

  def combine(seed: Long, hashes: Iterable[Long]): Long =
    hashes.foldLeft(seed)((hash, child) => mix(hash * 31 + child))

  /** A node of the code, which the hashes take. */
  private final class Node(val trees: Trees, val tree: Tree, val path: TreePath) {
    var children: Vector[Int] = Vector.empty
    var cost: Int             = -1

    /** The element the node names, which is the variable or the method it holds. */
    lazy val symbol: Option[Element] = Option(trees.getElement(path))

    /** The literal the node holds, if it holds one. */
    def literal: Option[Clones.Literal] =
      tree match {
        case literalTree: LiteralTree =>
          Option(literalTree.getValue).map(value => Clones.Literal(value.getClass.getSimpleName, String.valueOf(value)))
        case _ => None
      }

    private def startOffset: Long = trees.getSourcePositions.getStartPosition(path.getCompilationUnit, tree)

    private def endOffset: Long = trees.getSourcePositions.getEndPosition(path.getCompilationUnit, tree)

    private def lineOf(offset: Long): Int =
      if offset < 0 then 0 else path.getCompilationUnit.getLineMap.getLineNumber(offset).toInt

    /** The line the code of the node begins on. */
    def startLine: Int = lineOf(startOffset)

    /** The line the code of the node ends on. */
    def endLine: Int = lineOf(endOffset)

    /** The place the code of the node begins at, which the copies are told apart by. */
    def from: Int = startOffset.max(0L).toInt

    /** The place the code of the node ends at. */
    def to: Int = endOffset.max(0L).toInt
  }

  /** A literal of the code, which the near duplicates differ in. */
  case class Literal(kind: String, value: String)
}
