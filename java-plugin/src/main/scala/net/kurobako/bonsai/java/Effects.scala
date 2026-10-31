package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.TreePathScanner
import javax.lang.model.`type`.DeclaredType
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.Element
import javax.lang.model.element.ElementKind
import javax.lang.model.element.ExecutableElement
import javax.lang.model.element.Modifier
import javax.lang.model.element.TypeElement
import javax.lang.model.element.VariableElement
import javax.lang.model.util.Elements
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** What the code of a tree reads and changes, which the lints that move code across a loop or into a lambda ask for.
  *
  * Only the calls the JDK is known to leave the state alone with, and the getters of final fields, count as reads; any
  * other call may change what the code around it reads.
  */
object Effects {

  /** What an expression that changes nothing reads: the variables it names, and the final fields its getters return. */
  final case class Reads(variables: Set[Element], getters: Set[Element]) {
    def all: Set[Element] = variables ++ getters
  }

  private val Immutable = Set(
    "java.lang.String",
    "java.lang.Boolean",
    "java.lang.Byte",
    "java.lang.Short",
    "java.lang.Integer",
    "java.lang.Long",
    "java.lang.Character",
    "java.lang.Float",
    "java.lang.Double"
  )

  private val Lookups = Set(
    "contains",
    "containsAll",
    "containsKey",
    "containsValue",
    "isEmpty",
    "size",
    "get",
    "getOrDefault",
    "isPresent",
    "equals",
    "hashCode"
  )

  private val Comparisons = Set("equals", "isNull", "nonNull", "hash")

  private val Containers = Vector("java.util.Collection", "java.util.Map", "java.util.Optional")

  private val Variables = Set(
    ElementKind.LOCAL_VARIABLE,
    ElementKind.PARAMETER,
    ElementKind.EXCEPTION_PARAMETER,
    ElementKind.RESOURCE_VARIABLE,
    ElementKind.BINDING_VARIABLE
  )

  /** Whether the element is a variable of a method rather than a field. */
  def local(element: Element): Boolean = element != null && Variables.contains(element.getKind)

  /** The variables a tree reads if it changes nothing, or nothing when it may change something. */
  def reads(context: Context, tree: Tree): Option[Reads] = {
    val variables = mutable.Set.empty[Element]
    val getters   = mutable.Set.empty[Element]

    def variable(element: VariableElement): Boolean = {
      if !Lints.constant(element) && element.getSimpleName.toString != "length" then variables += element
      true
    }

    def all(trees: java.util.List[? <: Tree]): Boolean = trees.asScala.forall(go)

    def go(tree: Tree): Boolean = Lint.unwrapped(tree) match {
      case null                       => true
      case _: LiteralTree             => true
      case identifier: IdentifierTree =>
        Call.isThis(identifier) || identifier.getName.contentEquals("super") ||
        (context.elementOf(identifier) match {
          case element: VariableElement => variable(element)
          case null                     => false
          case _                        => true
        })
      case select: MemberSelectTree =>
        select.getIdentifier.contentEquals("class") ||
        (context.elementOf(select) match {
          case element: VariableElement => variable(element) && go(select.getExpression)
          case null                     => false
          case _                        => go(select.getExpression)
        })
      case test: InstanceOfTree                   => go(test.getExpression)
      case unary: UnaryTree                       => !stepping(unary) && go(unary.getExpression)
      case binary: BinaryTree                     => go(binary.getLeftOperand) && go(binary.getRightOperand)
      case access: ArrayAccessTree                => go(access.getExpression) && go(access.getIndex)
      case conditional: ConditionalExpressionTree =>
        go(conditional.getCondition) && go(conditional.getTrueExpression) && go(conditional.getFalseExpression)
      case call: MethodInvocationTree =>
        context.elementOf(call) match {
          case method: ExecutableElement =>
            readOnly(context, call, method).exists { field =>
              field.foreach(getters += _)
              val receiver = call.getMethodSelect match {
                case select: MemberSelectTree => go(select.getExpression)
                case _                        => true
              }
              receiver && all(call.getArguments)
            }
          case _ => false
        }
      case _ => false
    }

    Option.when(go(tree))(Reads(variables.toSet, getters.toSet))
  }

  private[java] def stepping(unary: UnaryTree): Boolean = unary.getKind match {
    case Tree.Kind.PREFIX_INCREMENT | Tree.Kind.PREFIX_DECREMENT | Tree.Kind.POSTFIX_INCREMENT |
        Tree.Kind.POSTFIX_DECREMENT =>
      true
    case _ => false
  }

  /** Whether a call changes nothing: a lookup of a JDK container or text, or the getter of a final field, which it
    * reads; the call reads nothing of its own to name when it is not a getter.
    */
  private def readOnly(
      context: Context,
      call: MethodInvocationTree,
      method: ExecutableElement
  ): Option[Option[Element]] = {
    val owner = method.getEnclosingElement match {
      case tpe: TypeElement => tpe.getQualifiedName.toString
      case _                => ""
    }
    val name = method.getSimpleName.toString
    if Immutable(owner) || (owner == "java.util.Objects" && Comparisons(name)) then Some(None)
    else if Lookups(name) && container(context, call) then Some(None)
    else getter(context, method).map(Some(_))
  }

  private val resolved = java.util.Collections.synchronizedMap(
    new java.util.WeakHashMap[Elements, Vector[TypeMirror]]()
  )

  private def containers(context: Context): Vector[TypeMirror] =
    resolved.computeIfAbsent(
      context.elements,
      elements =>
        Containers.flatMap(name =>
          Option(elements.getTypeElement(name)).map(element => context.types.erasure(element.asType))
        )
    )

  private def container(context: Context, call: MethodInvocationTree): Boolean =
    call.getMethodSelect match {
      case select: MemberSelectTree =>
        context.typeOf(select.getExpression) match {
          case declared: DeclaredType =>
            val types = context.types
            val name  = declared.asElement.asInstanceOf[TypeElement].getQualifiedName.toString
            !name.startsWith("java.util.concurrent.") &&
            containers(context).exists(container => types.isSubtype(types.erasure(declared), container))
          case _ => false
        }
      case _ => false
    }

  /** The final field the method returns when it only does that, if no subclass can change it.
    *
    * The compiler drops the trees of the types it has finished with, so a method without a tree is a getter if it is
    * named for a final field of its type, which it returns.
    */
  private def getter(context: Context, method: ExecutableElement): Option[Element] = {
    val owner = method.getEnclosingElement
    val fixed = method.getModifiers.contains(Modifier.FINAL) || method.getModifiers.contains(Modifier.PRIVATE) ||
      method.getModifiers.contains(Modifier.STATIC) || owner.getModifiers.contains(Modifier.FINAL) ||
      owner.getKind.name == "RECORD"
    if !fixed || !method.getParameters.isEmpty then None
    else
      Option(context.trees.getTree(method)) match {
        case Some(tree) => returned(context, tree)
        case None       =>
          owner.getEnclosedElements.asScala.collectFirst {
            case field: VariableElement
                if field.getKind == ElementKind.FIELD && field.getModifiers.contains(Modifier.FINAL) &&
                  field.getSimpleName == method.getSimpleName &&
                  context.types.isSameType(field.asType, method.getReturnType) =>
              field
          }
      }
  }

  private def returned(context: Context, method: MethodTree): Option[Element] =
    Option(method.getBody).map(_.getStatements.asScala.toVector).collect {
      case Vector(statement: ReturnTree) => Lints.unparenthesised(statement.getExpression)
    }.flatMap { expression =>
      val field = expression match {
        case identifier: IdentifierTree => Option(context.elementOf(identifier))
        case select: MemberSelectTree if select.getExpression.toString == "this" =>
          Option(context.elementOf(select))
        case _ => None
      }
      field.collect {
        case element: VariableElement
            if element.getKind == ElementKind.FIELD && element.getModifiers.contains(Modifier.FINAL) =>
          element
      }
    }

  private def immutable(tpe: TypeMirror): Boolean = tpe != null &&
    (tpe.getKind.isPrimitive ||
      (tpe match {
        case declared: DeclaredType =>
          declared.asElement match {
            case element: TypeElement => Immutable(element.getQualifiedName.toString) ||
              element.getKind == ElementKind.ENUM
            case _ => false
          }
        case _ => false
      }))

  /** Whether any of the elements may change, or be declared anew, within the tree. */
  def changes(context: Context, scope: Tree, elements: Set[Element]): Boolean = {
    var changed = false
    val shared  = elements.filterNot(element => immutable(element.asType))
    val root    = new TreePath(context.path, scope)
    val targets = written(root)
    val mention = (path: TreePath) => {
      val element = context.trees.getElement(path)
      if !changed && element != null && elements(element) then
        changed = (path.getLeaf ne scope) && targets.contains(path.getLeaf) ||
          (shared(element) && escapes(context, path, scope))
    }
    new TreePathScanner[Unit, Unit] {
      override def visitIdentifier(node: IdentifierTree, unused: Unit): Unit = mention(getCurrentPath)

      override def visitMemberSelect(node: MemberSelectTree, unused: Unit): Unit = {
        mention(getCurrentPath)
        super.visitMemberSelect(node, unused)
      }

      override def visitVariable(node: VariableTree, unused: Unit): Unit = {
        val element = context.trees.getElement(getCurrentPath)
        if element != null && elements(element) then changed = true
        super.visitVariable(node, unused)
      }
    }.scan(new TreePath(context.path, scope), ())
    changed
  }

  /** Whether the code of the tree assigns the element, after its declaration. */
  def assigns(context: Context, scope: TreePath, element: Element): Boolean =
    writes(scope).exists(write => context.refers(write.at, write.target, element))

  /** A variable, or a chain of fields read from one, with the index of the array element it ends in, if any. */
  final case class AccessPath(root: Element, fields: List[Element] = Nil, index: Option[String] = None)

  /** The access path an expression names, if every element on it is accepted. */
  def accessPath(tree: ExpressionTree, element: Tree => Element, accepts: Element => Boolean): Option[AccessPath] = {
    def of(tree: ExpressionTree): Option[AccessPath] = Lints.unparenthesised(tree) match {
      case identifier: IdentifierTree =>
        if Call.isThis(identifier) then None else Option(element(identifier)).filter(accepts).map(AccessPath(_))
      case assignment: AssignmentTree => of(assignment.getVariable)
      case select: MemberSelectTree   =>
        Option(element(select)).filter(el => el.getKind == ElementKind.FIELD && accepts(el)).flatMap { field =>
          Lints.unparenthesised(select.getExpression) match {
            case owner if Call.isThis(owner) => Some(AccessPath(field))
            case owner                       => of(owner).map(path => path.copy(fields = path.fields :+ field))
          }
        }
      case access: ArrayAccessTree =>
        of(access.getExpression).map { base =>
          val index = Lints.unparenthesised(access.getIndex) match {
            case identifier: IdentifierTree => identifier.getName.toString
            case literal: LiteralTree       => literal.getValue.toString
            case other                      => other.toString
          }
          base.copy(index = Some(index))
        }
      case _ => None
    }
    of(tree)
  }

  /** A write to a variable: the assignment, compound assignment or step at the path, and the expression it writes. */
  final case class Write(at: TreePath, target: ExpressionTree, plain: Boolean)

  /** The writes the code under the path makes, in source order. */
  def writes(root: TreePath): List[Write] = {
    val found = mutable.ListBuffer.empty[Write]
    new TreePathScanner[Unit, Unit] {
      override def visitAssignment(node: AssignmentTree, unused: Unit): Unit = {
        found += Write(getCurrentPath, node.getVariable, plain = true)
        super.visitAssignment(node, unused)
      }

      override def visitCompoundAssignment(node: CompoundAssignmentTree, unused: Unit): Unit = {
        found += Write(getCurrentPath, node.getVariable, plain = false)
        super.visitCompoundAssignment(node, unused)
      }

      override def visitUnary(node: UnaryTree, unused: Unit): Unit = {
        if stepping(node) then found += Write(getCurrentPath, node.getExpression, plain = false)
        super.visitUnary(node, unused)
      }
    }.scan(root, ())
    found.toList
  }

  /** Whether the code of the tree calls a method or a constructor that declares a checked exception. */
  def throwsChecked(context: Context, tree: Tree): Boolean = {
    var thrown    = false
    val unchecked = Vector("java.lang.RuntimeException", "java.lang.Error").flatMap(name =>
      Option(context.elements.getTypeElement(name)).map(_.asType)
    )
    def check(path: TreePath): Unit =
      context.trees.getElement(path) match {
        case method: ExecutableElement =>
          if method.getThrownTypes.asScala.exists(tpe => !unchecked.exists(context.types.isSubtype(tpe, _))) then
            thrown = true
        case _ => ()
      }
    new TreePathScanner[Unit, Unit] {
      override def visitMethodInvocation(node: MethodInvocationTree, unused: Unit): Unit = {
        check(getCurrentPath)
        super.visitMethodInvocation(node, unused)
      }

      override def visitNewClass(node: NewClassTree, unused: Unit): Unit = {
        check(getCurrentPath)
        super.visitNewClass(node, unused)
      }
    }.scan(new TreePath(context.path, tree), ())
    thrown
  }

  /** The variables the code of the tree names. */
  def variables(context: Context, tree: Tree): Set[Element] = {
    val named = mutable.Set.empty[Element]
    new TreePathScanner[Unit, Unit] {
      private def name(): Unit = context.trees.getElement(getCurrentPath) match {
        case variable: VariableElement => named += variable
        case _                         => ()
      }

      override def visitIdentifier(node: IdentifierTree, unused: Unit): Unit = name()

      override def visitMemberSelect(node: MemberSelectTree, unused: Unit): Unit = {
        name()
        super.visitMemberSelect(node, unused)
      }
    }.scan(new TreePath(context.path, tree), ())
    named.toSet
  }

  /** The variables of a method the code of the tree names. */
  def locals(context: Context, tree: Tree): Set[Element] = variables(context, tree).filter(local)

  /** The variables the code of the tree declares. */
  def declared(context: Context, tree: Tree): Set[Element] = {
    val declared = mutable.Set.empty[Element]
    new TreePathScanner[Unit, Unit] {
      override def visitVariable(node: VariableTree, unused: Unit): Unit = {
        Option(context.trees.getElement(getCurrentPath)).foreach(declared += _)
        super.visitVariable(node, unused)
      }
    }.scan(new TreePath(context.path, tree), ())
    declared.toSet
  }

  private def written(scope: TreePath): java.util.Set[Tree] = {
    val trees = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[Tree, java.lang.Boolean])
    @annotation.tailrec
    def mark(target: Tree): Unit = {
      trees.add(target)
      target match {
        case select: MemberSelectTree => mark(select.getExpression)
        case access: ArrayAccessTree  => mark(access.getExpression)
        case _                        =>
      }
    }
    writes(scope).foreach(write => mark(write.target))
    trees
  }

  /** Whether the value the path names reaches code that may change it: a call that is not a lookup, or a place that
    * keeps it.
    */
  private def escapes(context: Context, path: TreePath, scope: Tree): Boolean = {
    @annotation.tailrec
    def climb(below: TreePath): Boolean = {
      val child  = below.getLeaf
      val parent = below.getParentPath
      if parent == null || (child eq scope) then false
      else
        parent.getLeaf match {
          case _: ParenthesizedTree | _: TypeCastTree | _: BinaryTree | _: ArrayAccessTree => climb(parent)
          case unary: UnaryTree                       => stepping(unary) || climb(parent)
          case conditional: ConditionalExpressionTree =>
            (conditional.getCondition ne child) && climb(parent)
          case select: MemberSelectTree if select.getExpression eq child => climb(parent)
          case call: MethodInvocationTree                                =>
            context.trees.getElement(parent) match {
              case method: ExecutableElement if readOnly(context, call, method).isDefined => climb(parent)
              case _ => (call.getMethodSelect eq child) || !immutable(context.typeOf(child))
            }
          case _: InstanceOfTree | _: IfTree | _: WhileLoopTree | _: DoWhileLoopTree | _: ForLoopTree |
              _: EnhancedForLoopTree | _: SwitchTree | _: ReturnTree | _: ThrowTree | _: AssertTree |
              _: ExpressionStatementTree =>
            false
          case _ => !immutable(context.typeOf(child))
        }
    }
    climb(path)
  }
}
