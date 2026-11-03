package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.tree.Tree.Kind
import com.sun.source.util.TreePath
import com.sun.source.util.TreePathScanner
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.*
import net.kurobako.bonsai.java.Javac.*
import scala.jdk.CollectionConverters.*

/** The lints of values whose nullness and lifetime the JSpecify scope and the code that owns them settle. */
object LintScopes {

  val lints: Vector[Lint] = Vector(
    AutoCloseableResource,
    NotNullField
  )

  /** An AutoCloseable local used outside try-with-resources. */
  private object AutoCloseableResource extends Lint {
    val id               = LintId.AutoCloseableResource.id
    val kinds: Set[Kind] = Set(Tree.Kind.VARIABLE)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val variable = tree.asInstanceOf[VariableTree]
      val element  = context.namedElement
      val local    = element.exists(_.getKind == ElementKind.LOCAL_VARIABLE)
      val resource = context.ancestors.exists {
        case tryTree: TryTree => tryTree.getResources.asScala.exists(_ eq variable)
        case _                => false
      }
      val closeable     = element.exists(found => subtype(context, found.asType(), "java.lang.AutoCloseable"))
      val owned         = variable.initializer.exists(ownedResource(context, _))
      val resourceLater = usedAsResource(context, variable.getName)
      val use           = element.fold(ResourceUse(false, false))(resourceUse(context, _))
      if (local && closeable && owned && !resource && !resourceLater && !use.escaped && !use.closed)
        Vector(context.found("the resource is not used in try-with-resources, so it may not be closed"))
      else Vector.empty
    }
  }

  /** A non-null field that is not initialized where it is declared. */
  private object NotNullField extends Lint {
    val id               = LintId.NotNullField.id
    val kinds: Set[Kind] = Set(Tree.Kind.VARIABLE)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val variable = tree.asInstanceOf[VariableTree]
      val field    = context.namedElement.filter(_.getKind == ElementKind.FIELD)
      if (
        field.exists(element =>
          variable.initializer.isEmpty && !element.asType().getKind.isPrimitive && Nullness.marked(element) &&
            !Nullness.nullableType(element.asType())
        ) && !assignedInInitializer(context, variable.getName)
      ) Vector(context.found("the field is not initialized where it is declared"))
      else Vector.empty
    }
  }

  /** How a local is used: whether its owner is handed on, and whether the local is closed by hand. */
  private case class ResourceUse(escaped: Boolean, closed: Boolean)

  /** Whether the local is passed, returned, captured or stored, or is closed by hand. */
  // A javac visitor is an abstract class, so it is written inline.
  // The state is updated as the walk proceeds.
  @SuppressWarnings(Array("scalafix:BannedSyntax.anonymous-allocation", "scalafix:DisableSyntax.var"))
  private def resourceUse(context: Context, element: Element): ResourceUse = {
    val root =
      context.path.parents.find(_.getLeaf.isInstanceOf[MethodTree])
    val declaredIn = context.ancestors.collect {
      case declared: (LambdaExpressionTree | ClassTree) => declared
    }.toSet
    var escaped = false
    var closed  = false
    root.foreach { methodPath =>
      new TreePathScanner[Unit, Unit] {
        private def use(path: TreePath): Unit = {
          val captured = path.parents.takeWhile(_.getLeaf ne methodPath.getLeaf).exists { holder =>
            holder.getLeaf match {
              case lambda: LambdaExpressionTree => !declaredIn(lambda)
              case clazz: ClassTree             => !declaredIn(clazz)
              case _                            => false
            }
          }
          if (captured) escaped = true

          path.parent.foreach { parent =>
            parent.getLeaf match {
              case ret: ReturnTree if ret.getExpression eq path.getLeaf => escaped = true
              case invocation: MethodInvocationTree                     =>
                if (Lints.arguments(invocation).exists(_ eq path.getLeaf)) escaped = true
              case select: MemberSelectTree if select.getExpression eq path.getLeaf =>
                parent.parent.foreach { grandparent =>
                  grandparent.getLeaf match {
                    case invocation: MethodInvocationTree
                        if (invocation.getMethodSelect eq select) && Closing(select.getIdentifier.toString) =>
                      closed = true
                    case _ => ()
                  }
                }
              case newClass: NewClassTree if Lints.arguments(newClass).exists(_ eq path.getLeaf) => escaped = true
              case assignment: AssignmentTree if assignment.getExpression eq path.getLeaf        => escaped = true
              case variable: VariableTree if variable.getInitializer eq path.getLeaf             => escaped = true
              case _: MemberReferenceTree                                                        => escaped = true
              case array: NewArrayTree if array.initializers.exists(_.asScala.exists(_ eq path.getLeaf)) =>
                escaped = true
              case _ => ()
            }
          }
        }

        override def visitIdentifier(node: IdentifierTree, unused: Unit): Unit = {
          if (context.trees.namedElement(getCurrentPath).contains(element)) use(getCurrentPath)
          super.visitIdentifier(node, unused)
        }

        override def visitMemberSelect(node: MemberSelectTree, unused: Unit): Unit = {
          if (context.trees.namedElement(getCurrentPath).contains(element)) use(getCurrentPath)
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
      case Some(source) if source.body.isDefined =>
        val returned = source.body.toVector.flatMap(Lints.returns(_)).flatMap(_.returned)
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
  private def usedAsResource(context: Context, name: CharSequence): Boolean =
    context.ancestors.collectFirst { case method: MethodTree => method }.exists { method =>
      Scan.exists(method.getBody) {
        case node: TryTree =>
          node.getResources.asScala.exists {
            case identifier: IdentifierTree => identifier.getName.contentEquals(name)
            case _                          => false
          }
      }
    }

  /** Whether a constructor or an initializer block assigns the field. */
  private def assignedInInitializer(context: Context, name: CharSequence): Boolean = {
    val owner = context.ancestors.collectFirst { case tree: ClassTree => tree }
    owner.exists { classTree =>
      val bodies = classTree.getMembers.asScala.flatMap {
        case method: MethodTree =>
          elementAt(context, method).collect {
            case constructor: ExecutableElement if constructor.getKind == ElementKind.CONSTRUCTOR => constructor
          }.flatMap(_ => method.body)
        case block: BlockTree => Some(block)
        case _                => None
      }
      bodies.exists { body =>
        Scan.exists(body) {
          case node: AssignmentTree =>
            node.getVariable match {
              case identifier: IdentifierTree => identifier.getName.contentEquals(name)
              case select: MemberSelectTree   => select.getIdentifier.contentEquals(name)
              case _                          => false
            }
        }
      }
    }
  }

  private def elementAt(context: Context, tree: Tree): Option[Element] = context.namedElement(tree)

  private def subtype(context: Context, tpe: TypeMirror, name: String): Boolean =
    context.elements.typeNamed(name).exists(target => context.types.isSubtype(tpe, target.asType()))
}
