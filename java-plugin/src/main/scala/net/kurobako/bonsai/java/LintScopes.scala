package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.TreePathScanner
import com.sun.source.util.TreeScanner
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.*
import scala.jdk.CollectionConverters.*

/** The lints of values whose nullness and lifetime the JSpecify scope and the code that owns them settle. */
object LintScopes {

  val lints: Vector[Lint] = Vector(
    AutoCloseableResource,
    NotNullField
  )

  /** An AutoCloseable local used outside try-with-resources. */
  private object AutoCloseableResource extends Lint {
    val id    = JavaLint.AutoCloseableResource.id
    val kinds = Set(Tree.Kind.VARIABLE)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val variable = tree.asInstanceOf[VariableTree]
      val element  = context.element
      val local    = element != null && element.getKind == ElementKind.LOCAL_VARIABLE
      val resource = context.ancestors.exists {
        case tryTree: TryTree => tryTree.getResources.asScala.exists(_ eq variable)
        case _                => false
      }
      val closeable     = element != null && subtype(context, element.asType(), "java.lang.AutoCloseable")
      val owned         = variable.getInitializer != null && ownedResource(context, variable.getInitializer)
      val resourceLater = usedAsResource(context, variable.getName)
      val use           = if element != null then resourceUse(context, element) else ResourceUse(false, false)
      if local && closeable && owned && !resource && !resourceLater && !use.escaped && !use.closed then
        Vector(context.found("the resource is not used in try-with-resources, so it may not be closed"))
      else Vector.empty
    }
  }

  /** A non-null field that is not initialized where it is declared. */
  private object NotNullField extends Lint {
    val id    = JavaLint.NotNullField.id
    val kinds = Set(Tree.Kind.VARIABLE)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val variable = tree.asInstanceOf[VariableTree]
      val element  = context.element
      if element != null && element.getKind == ElementKind.FIELD && variable.getInitializer == null &&
        !element.asType().getKind.isPrimitive && Nullness.marked(element) && !Nullness.nullableType(element.asType()) &&
        !assignedInInitializer(context, variable.getName)
      then Vector(context.found("the field is not initialized where it is declared"))
      else Vector.empty
    }
  }

  /** How a local is used: whether its owner is handed on, and whether the local is closed by hand. */
  private case class ResourceUse(escaped: Boolean, closed: Boolean)

  /** Whether the local is passed, returned, captured or stored, or is closed by hand. */
  private def resourceUse(context: Context, element: Element): ResourceUse = {
    val method     = context.ancestors.collectFirst { case method: MethodTree => method }
    val root       = method.flatMap(method => Option(context.trees.getPath(context.unit, method)))
    val declaredIn = context.ancestors.collect {
      case declared: (LambdaExpressionTree | ClassTree) => declared
    }.toSet
    var escaped = false
    var closed  = false
    root.foreach { methodPath =>
      new TreePathScanner[Unit, Unit] {
        private def use(path: TreePath): Unit = {
          var holder   = path.getParentPath
          var captured = false
          while holder != null && (holder.getLeaf ne methodPath.getLeaf) && !captured do {
            holder.getLeaf match {
              case lambda: LambdaExpressionTree if !declaredIn(lambda) => captured = true
              case clazz: ClassTree if !declaredIn(clazz)              => captured = true
              case _                                                   => ()
            }
            holder = holder.getParentPath
          }
          if captured then escaped = true

          val parent = path.getParentPath
          if parent != null then
            parent.getLeaf match {
              case ret: ReturnTree if ret.getExpression eq path.getLeaf => escaped = true
              case invocation: MethodInvocationTree                     =>
                if Lints.arguments(invocation).exists(_ eq path.getLeaf) then escaped = true
              case select: MemberSelectTree if select.getExpression eq path.getLeaf =>
                val grandparent = parent.getParentPath
                if grandparent != null then
                  grandparent.getLeaf match {
                    case invocation: MethodInvocationTree
                        if (invocation.getMethodSelect eq select) && Closing(select.getIdentifier.toString) =>
                      closed = true
                    case _ => ()
                  }
              case newClass: NewClassTree if Lints.arguments(newClass).exists(_ eq path.getLeaf) => escaped = true
              case assignment: AssignmentTree if assignment.getExpression eq path.getLeaf        => escaped = true
              case variable: VariableTree if variable.getInitializer eq path.getLeaf             => escaped = true
              case _: MemberReferenceTree                                                        => escaped = true
              case array: NewArrayTree
                  if array.getInitializers != null && array.getInitializers.asScala.exists(_ eq path.getLeaf) =>
                escaped = true
              case _ => ()
            }
        }

        override def visitIdentifier(node: IdentifierTree, unused: Unit): Unit = {
          if Option(context.trees.getElement(getCurrentPath)).contains(element) then use(getCurrentPath)
          super.visitIdentifier(node, unused)
        }

        override def visitMemberSelect(node: MemberSelectTree, unused: Unit): Unit = {
          if Option(context.trees.getElement(getCurrentPath)).contains(element) then use(getCurrentPath)
          super.visitMemberSelect(node, unused)
        }
      }.scan(methodPath, ())
    }
    ResourceUse(escaped, closed)
  }

  /** The methods that close a resource: its own close, and the release or disposal it closes through. */
  private val Closing = Set("close", "release", "dispose")

  /** Types whose close does nothing, which the code need not call. */
  private val NoOpClose = Set("java.io.ByteArrayOutputStream", "java.io.ByteArrayInputStream")

  /** Names of methods that open a resource, which the code that calls one closes. */
  private val Opening = "(create|open|new|build|start|connect|from|subscribe|acquire)[A-Z0-9_]?.*".r

  /** The static factories of resources, by the class that declares them. */
  private val Factories = Map(
    "java.nio.file.Files" -> Set(
      "newInputStream",
      "newOutputStream",
      "newBufferedReader",
      "newBufferedWriter",
      "lines",
      "list",
      "walk",
      "find"
    ),
    "java.nio.channels.FileChannel"             -> Set("open"),
    "java.nio.channels.AsynchronousFileChannel" -> Set("open"),
    "java.nio.channels.Channels"                ->
      Set("newChannel", "newInputStream", "newOutputStream", "newReader", "newWriter"),
    "java.nio.channels.SocketChannel"       -> Set("open"),
    "java.nio.channels.ServerSocketChannel" -> Set("open"),
    "java.nio.channels.DatagramChannel"     -> Set("open"),
    "java.nio.file.FileSystems"             -> Set("newFileSystem"),
    "java.net.http.HttpClient"              -> Set("newHttpClient"),
    "java.net.http.HttpClient.Builder"      -> Set("build"),
    "java.lang.ProcessBuilder"              -> Set("start"),
    "java.util.concurrent.ForkJoinPool"     -> Set("commonPool")
  )

  private def ownedResource(context: Context, tree: Tree, depth: Int = 0): Boolean = Lint.unwrapped(tree) match {
    case newClass: NewClassTree =>
      !elementAt(context, newClass).exists(constructor =>
        NoOpClose(constructor.getEnclosingElement.toString)
      )
    case conditional: ConditionalExpressionTree =>
      ownedResource(context, conditional.getTrueExpression, depth) &&
      ownedResource(context, conditional.getFalseExpression, depth)
    case invocation: MethodInvocationTree =>
      elementAt(context, invocation.getMethodSelect).exists {
        case method: ExecutableElement =>
          val owner = Lints.owner(method).getOrElse("")
          val name  = method.getSimpleName.toString
          Factories.get(owner).exists(_.contains(name)) ||
          (owner == "java.util.concurrent.Executors" && name.startsWith("new")) ||
          unwrapped(context, invocation, owner, name, depth) ||
          opening(context, method, depth)
        case _ => false
      }
    case _ => false
  }

  /** Whether the call takes the resource out of an optional that a call opened. */
  private def unwrapped(
      context: Context,
      invocation: MethodInvocationTree,
      owner: String,
      name: String,
      depth: Int
  ): Boolean =
    owner == "java.util.Optional" && Set("orElse", "orElseThrow", "orElseGet", "get").contains(name) &&
      (invocation.getMethodSelect match {
        case select: MemberSelectTree => ownedResource(context, select.getExpression, depth)
        case _                        => false
      })

  /** Whether a method that returns a resource, or an optional of one, allocates it: the code of the method does when
    * the build has it, and its name says so when it does not.
    */
  private def opening(context: Context, method: ExecutableElement, depth: Int): Boolean = {
    val result = method.getReturnType
    val holds  = subtype(context, result, "java.lang.AutoCloseable") ||
      (result match {
        case declared: javax.lang.model.`type`.DeclaredType =>
          declared.asElement.toString == "java.util.Optional" &&
          declared.getTypeArguments.asScala.exists(subtype(context, _, "java.lang.AutoCloseable"))
        case _ => false
      })
    holds && depth < 3 &&
    (Option(context.trees.getTree(method)) match {
      case Some(source) if source.getBody != null =>
        val returned = Lints.returns(source.getBody).flatMap(node => Option(node.getExpression))
        returned.nonEmpty && returned.forall(expression => allocated(context, expression, depth + 1))
      case Some(_) => false
      case None    => Opening.matches(method.getSimpleName.toString)
    })
  }

  /** Whether the expression allocates a resource, which an optional factory wraps. */
  private def allocated(context: Context, tree: ExpressionTree, depth: Int): Boolean = tree match {
    case invocation: MethodInvocationTree
        if elementAt(context, invocation.getMethodSelect).exists(method =>
          method.getEnclosingElement.toString == "java.util.Optional" &&
            Set("of", "ofNullable").contains(method.getSimpleName.toString)
        ) && invocation.getArguments.size == 1 =>
      ownedResource(context, invocation.getArguments.get(0), depth)
    case other => ownedResource(context, other, depth)
  }

  /** Whether a later try in the enclosing method takes the local as one of its resources. */
  private def usedAsResource(context: Context, name: CharSequence): Boolean = {
    var found = false
    context.ancestors.collectFirst { case method: MethodTree => method }.foreach { method =>
      new TreeScanner[Unit, Unit] {
        override def visitTry(node: TryTree, unused: Unit): Unit = {
          if node.getResources.asScala.exists {
              case identifier: IdentifierTree => identifier.getName.contentEquals(name)
              case _                          => false
            }
          then found = true
          super.visitTry(node, unused)
        }
      }.scan(method.getBody, ())
    }
    found
  }

  /** Whether a constructor or an initializer block assigns the field. */
  private def assignedInInitializer(context: Context, name: CharSequence): Boolean = {
    val owner = context.ancestors.collectFirst { case tree: ClassTree => tree }
    owner.exists { classTree =>
      val bodies = classTree.getMembers.asScala.flatMap {
        case method: MethodTree =>
          elementAt(context, method).collect {
            case constructor: ExecutableElement if constructor.getKind == ElementKind.CONSTRUCTOR => method.getBody
          }
        case block: BlockTree => Some(block)
        case _                => None
      }
      var found = false
      bodies.foreach { body =>
        if body != null then
          new TreeScanner[Unit, Unit] {
            override def visitAssignment(node: AssignmentTree, unused: Unit): Unit = {
              val target = node.getVariable match {
                case identifier: IdentifierTree if identifier.getName.contentEquals(name) => true
                case select: MemberSelectTree if select.getIdentifier.contentEquals(name) => true
                case _                                                                    => false
              }
              if target then found = true
              super.visitAssignment(node, unused)
            }
          }.scan(body, ())
      }
      found
    }
  }

  private def elementAt(context: Context, tree: Tree): Option[Element] = Option(context.elementOf(tree))

  private def subtype(context: Context, tpe: TypeMirror, name: String): Boolean =
    Option(context.elements.getTypeElement(name)).exists(target => context.types.isSubtype(tpe, target.asType()))
}
