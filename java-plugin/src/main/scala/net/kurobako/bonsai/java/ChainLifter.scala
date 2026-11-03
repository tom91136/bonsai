package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import javax.lang.model.`type`.TypeKind
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.Element
import javax.lang.model.element.ElementKind
import javax.lang.model.element.ExecutableElement
import javax.lang.model.element.Modifier
import javax.lang.model.element.TypeElement
import javax.lang.model.element.VariableElement
import javax.lang.model.util.Elements
import javax.lang.model.util.Types
import net.kurobako.bonsai.collections.Arg
import net.kurobako.bonsai.collections.Chain
import net.kurobako.bonsai.collections.Comparison
import net.kurobako.bonsai.collections.Kind
import net.kurobako.bonsai.collections.Step
import scala.jdk.CollectionConverters.*

/** A chain of calls as the laws see it, with the expression that runs each step: `ends(i)` is the call, or the
  * comparison or negation of one, that completes step `i`. A chain whose first call names no receiver runs on `this`,
  * which `chain.source` then stands for.
  */
final case class Lifted(chain: Chain, ends: Vector[ExpressionTree], implicitThis: Boolean = false) {

  /** The text of the expression the step `from` runs on. */
  def receiver(from: Int): String = if from == 0 then chain.source.text else ends(from - 1).toString
}

/** An argument of a step, which is a function written in place, a reference to a method or a plain expression. */
private final class JavaArg(
    val text: String,
    val pure: Boolean,
    override val identity: Boolean,
    override val literal: Option[Any],
    override val comparison: Option[Comparison] = None
) extends Arg {
  def applied(value: String): String = if identity then value else s"($text).apply($value)"
}

/** The names of the Java methods the lifter reads as steps of the algebra, by the receiver they are called on and the
  * number of their arguments. The steps they lower to are the methods the algebra knows, `Ops` giving their costs.
  */
object ChainMethods {

  val Streams: Map[(String, Int), String] = Map(
    ("filter", 1)      -> "filter",
    ("map", 1)         -> "map",
    ("mapToInt", 1)    -> "map",
    ("mapToLong", 1)   -> "map",
    ("mapToDouble", 1) -> "map",
    ("mapToObj", 1)    -> "map",
    ("flatMap", 1)     -> "flatMap",
    ("sorted", 0)      -> "sorted",
    ("sorted", 1)      -> "sortWith",
    ("distinct", 0)    -> "distinct",
    ("limit", 1)       -> "take",
    ("skip", 1)        -> "drop",
    ("takeWhile", 1)   -> "takeWhile",
    ("dropWhile", 1)   -> "dropWhile",
    ("findFirst", 0)   -> "headOption",
    ("findAny", 0)     -> "headOption",
    ("anyMatch", 1)    -> "exists",
    ("allMatch", 1)    -> "forall",
    ("noneMatch", 1)   -> "noneMatch",
    ("count", 0)       -> "size",
    ("min", 0)         -> "min",
    ("min", 1)         -> "min",
    ("max", 0)         -> "max",
    ("max", 1)         -> "max",
    ("toList", 0)      -> "toList"
  )

  val Collections: Map[(String, Int), String] = Map(
    ("size", 0)     -> "size",
    ("isEmpty", 0)  -> "isEmpty",
    ("contains", 1) -> "contains",
    ("stream", 0)   -> "stream"
  )

  val Maps: Map[(String, Int), String] = Map(("size", 0) -> "size", ("isEmpty", 0) -> "isEmpty")

  val Optionals: Map[(String, Int), String] = Map(
    ("isPresent", 0)   -> "isDefined",
    ("isEmpty", 0)     -> "isEmpty",
    ("get", 0)         -> "get",
    ("getAsInt", 0)    -> "get",
    ("getAsLong", 0)   -> "get",
    ("getAsDouble", 0) -> "get",
    ("map", 1)         -> "map",
    ("filter", 1)      -> "filter",
    ("flatMap", 1)     -> "flatMap",
    ("orElse", 1)      -> "getOrElse",
    ("orElseGet", 1)   -> "getOrElse"
  )

  /** The static factories whose result a chain starts from, by the class that has them. */
  val Factories: Map[String, Set[String]] = Map("java.util.List" -> Set("of"), "java.util.Arrays" -> Set("asList"))

  /** The unmodifiable views of a copy, by the method of `Collections`, with the type of the collection each copies. */
  val Unmodifiable: Map[String, String] = Map(
    "unmodifiableList" -> "java.util.Collection",
    "unmodifiableSet"  -> "java.util.Collection",
    "unmodifiableMap"  -> "java.util.Map"
  )

  private[java] val Names
      : Set[String] = (Streams.keySet ++ Collections.keySet ++ Maps.keySet ++ Optionals.keySet).map(_._1) ++
    Factories.values.flatten ++
    Unmodifiable.keySet ++ Set("collect", "reduce", "orElseThrow")
}

/** Lifts the calls of the Java collections, streams and optionals in attributed trees to the chains the laws work on.
  *
  * The kind of every receiver comes from its type, and a call is the step of a standard method only when the method it
  * names is declared by the standard library, so a type of the code that merely shares the names of one is not lifted.
  * Calls that a comparison or a negation completes are lowered to the pseudo methods of the algebra.
  */
object ChainLifter {

  private enum Family {
    case Stream, Collection, Map, Optional, Array
  }

  private val scalar = Kind(library = false)

  private final case class Link(
      end: ExpressionTree,
      receiver: TreePath,
      method: String,
      args: Vector[Arg],
      from: Kind,
      to: Kind,
      typed: Boolean,
      walks: Boolean = true,
      implicitThis: Boolean = false
  )

  private final class Roots(elements: Elements, val types: Types) {
    private def erased(name: String): Option[TypeMirror] =
      Option(elements.getTypeElement(name)).map(element => types.erasure(element.asType))

    private val stream     = erased("java.util.stream.BaseStream")
    private val map        = erased("java.util.Map")
    private val sortedMap  = erased("java.util.SortedMap")
    private val linkedMap  = erased("java.util.LinkedHashMap")
    private val collection = erased("java.util.Collection")
    private val set        = erased("java.util.Set")
    private val sortedSet  = erased("java.util.SortedSet")
    private val linkedSet  = erased("java.util.LinkedHashSet")
    private val list       = erased("java.util.List")
    private val queue      = erased("java.util.Queue")

    private def below(mirror: TypeMirror, root: Option[TypeMirror]): Boolean =
      root.exists(found => types.isSubtype(mirror, found))

    def family(mirror: TypeMirror): Option[(Family, Kind)] =
      Option(mirror).map(types.erasure).flatMap { erasure =>
        erasure.getKind match {
          case TypeKind.ARRAY    => Some((Family.Array, Kind(library = true, name = "java.array")))
          case TypeKind.DECLARED => declared(erasure)
          case _                 => None
        }
      }

    private def declared(erasure: TypeMirror): Option[(Family, Kind)] = {
      val name = types.asElement(erasure) match {
        case element: TypeElement => element.getQualifiedName.toString
        case _                    => ""
      }
      if (LintOptionals.Optionals(name)) Some((Family.Optional, Kind(library = true, single = true, name = name)))
      else if (below(erasure, stream)) Some((Family.Stream, Kind(library = true, strict = false, name = name)))
      else if (below(erasure, map)) {
        val ordered = below(erasure, sortedMap) || below(erasure, linkedMap)
        Some((
          Family.Map,
          Kind(library = true, ordered = ordered, keyed = true, name = name, sorted = below(erasure, sortedMap))
        ))
      } else if (below(erasure, collection)) {
        val kind =
          if (below(erasure, set))
            Kind(
              library = true,
              ordered = below(erasure, sortedSet) || below(erasure, linkedSet),
              distinct = true,
              name = name,
              sorted = below(erasure, sortedSet)
            )
          else if (below(erasure, list) || below(erasure, queue)) Kind(library = true, name = name)
          else Kind(library = true, ordered = false, name = name)
        Some((Family.Collection, kind))
      } else None
    }
  }

  /** The chain that ends at the call of the context, unless a call after it carries the chain on. */
  def lift(context: Context): Option[Lifted] =
    context.tree match {
      case invocation: MethodInvocationTree if named(invocation) && !continues(context, context.path) =>
        build(context, context.path)
      case _ => None
    }

  private def named(invocation: MethodInvocationTree): Boolean =
    Call.selected(invocation.getMethodSelect).exists((_, name) => ChainMethods.Names(name))

  /** The call that is made on the result of the call at the path, if any. */
  private def called(path: TreePath): Option[TreePath] =
    Lints.enclosing(path.getParentPath).flatMap { parent =>
      parent.getLeaf match {
        case select: MemberSelectTree
            if bareOf(new TreePath(parent, select.getExpression)).getLeaf eq path.getLeaf =>
          Option(parent.getParentPath).filter(_.getLeaf match {
            case invocation: MethodInvocationTree => invocation.getMethodSelect eq select
            case _                                => false
          })
        case _ => None
      }
    }

  /** Whether a call after the one at the path, or two calls after it where the first is taken in by the second, has a
    * step that runs on its result, so the chain of that call includes this one.
    */
  private def continues(context: Context, path: TreePath): Boolean = {
    def runsOn(outer: TreePath) =
      link(context, outer).exists(found =>
        found.walks && (bareOf(found.receiver).getLeaf eq path.getLeaf)
      )
    called(path).exists(next => runsOn(next) || called(next).exists(runsOn))
  }

  private def build(context: Context, top: TreePath): Option[Lifted] = {
    @annotation.tailrec
    def walk(current: TreePath, links: List[Link]): (Option[ExpressionTree], List[Link]) =
      link(context, current) match {
        case Some(found) =>
          val receiver = bareOf(found.receiver)
          receiver.getLeaf match {
            case _: MethodInvocationTree if found.walks => walk(receiver, found :: links)
            case _                                      =>
              (Some(found.receiver.getLeaf).collect { case expression: ExpressionTree => expression }, found :: links)
          }
        case None => (
            current.getLeaf match {
              case expression: ExpressionTree if links.nonEmpty => Some(expression)
              case _                                            => None
            },
            links
          )
      }
    val (source, links) = walk(top, Nil)
    source.filter(_ => links.nonEmpty).flatMap { origin =>
      val merged = found(links)
      val steps  = merged.map(link => Step(link.method, link.args, link.from, link.to, link.typed))
      val onThis = merged.head.implicitThis
      Option.when(steps.exists(_.receiver.library)) {
        val source = new JavaArg(if onThis then "this" else origin.toString, false, false, None)
        Lifted(Chain(source, steps), merged.map(_.end), onThis)
      }
    }
  }

  private def found(links: List[Link]): Vector[Link] =
    links.foldLeft(Vector.empty[Link]) { (built, next) =>
      built.lastOption match {
        case Some(previous) if previous.method == "filter" && next.method == "headOption" && !previous.from.strict =>
          built.init :+
            previous.copy(end = next.end, method = "find", to = next.to, typed = previous.typed || next.typed)
        case _ => built :+ next
      }
    }

  private def library(method: ExecutableElement): Boolean = Lints.owner(method).exists(_.startsWith("java.util."))

  @annotation.tailrec
  private def bareOf(path: TreePath): TreePath = path.getLeaf match {
    case parenthesized: ParenthesizedTree => bareOf(new TreePath(path, parenthesized.getExpression))
    case _                                => path
  }

  /** A call of a method of the standard library, with what the lifting of it reads. */
  private final case class Site(context: Context, call: Call, roots: Roots) {
    export call.invocation
    export call.method
    export call.name
    export call.path

    def typed: Boolean = invocation.getTypeArguments.size > 0

    def arguments: Vector[Arg] =
      call.arguments.map(tree => argument(context, new TreePath(path, tree), tree))

    def family(at: TreePath): Option[(Family, Kind)] = roots.family(context.trees.getTypeMirror(at))

    def result: Kind = family(path).fold(scalar)(_._2)
  }

  private def link(context: Context, path: TreePath): Option[Link] =
    Call.of(context.trees, path).flatMap { found =>
      val site = Site(context, found, Roots(context.elements, context.types))
      found.invocation.getMethodSelect match {
        case select: MemberSelectTree =>
          val receiver = new TreePath(new TreePath(path, select), select.getExpression)
          factory(site, receiver).orElse(unmodifiable(site)).orElse(arrayStream(site)).orElse(
            call(site, receiver, site.family(receiver))
          )
        case name: IdentifierTree =>
          own(site).flatMap(family => call(site, new TreePath(path, name), Some(family)))
            .map(_.copy(implicitThis = true))
        case _ => None
      }
    }

  /** The kind of the collection a call without a receiver runs on, which is the class around the call when it has the
    * method as a member.
    */
  private def own(site: Site): Option[(Family, Kind)] =
    Iterator.iterate(site.path)(_.getParentPath).takeWhile(_ != null).find(_.getLeaf.isInstanceOf[ClassTree])
      .flatMap(path => Option(site.context.trees.getElement(path)))
      .collect {
        case holder: TypeElement if site.context.elements.getAllMembers(holder).contains(site.method) => holder
      }
      .flatMap(holder => site.roots.family(holder.asType))
      .filter((family, _) => family == Family.Collection || family == Family.Map)

  /** `Arrays.stream(a)` and `Stream.of(a)` of an array, which stream the array as `stream()` streams a collection. */
  private def arrayStream(site: Site): Option[Link] = {
    val streams = site.method.getModifiers.contains(Modifier.STATIC) && site.invocation.getArguments.size == 1 &&
      ((Lints.owner(site.method).contains("java.util.Arrays") && site.name == "stream") ||
        (Lints.owner(site.method).contains("java.util.stream.Stream") && site.name == "of" && site.method.isVarArgs))
    lazy val source = new TreePath(site.path, site.invocation.getArguments.get(0))
    Option.when(streams)(site.family(source)).flatten.collect { case (Family.Array, kind) =>
      Link(site.invocation, source, "stream", Vector.empty, kind, site.result, site.typed, walks = false)
    }
  }

  private def factory(site: Site, receiver: TreePath): Option[Link] =
    Option.when(site.method.getModifiers.contains(Modifier.STATIC) &&
      Lints.owner(site.method).flatMap(ChainMethods.Factories.get).exists(_(site.name))) {
      Link(
        site.invocation,
        receiver,
        site.name,
        site.arguments,
        Kind(library = true, name = Lints.owner(site.method).getOrElse("")),
        site.result,
        site.typed
      )
    }

  private def unmodifiable(site: Site): Option[Link] =
    ChainMethods.Unmodifiable.get(site.name).filter(_ =>
      Lints.owner(site.method).contains("java.util.Collections")
    ).flatMap {
      copied =>
        site.invocation.getArguments.asScala.toList match {
          case List(construction: NewClassTree)
              if construction.getClassBody == null && construction.getArguments.size == 1 &&
                Release.atLeast(site.context, 10) =>
            val source = new TreePath(new TreePath(site.path, construction), construction.getArguments.get(0))
            for {
              (_, kind) <- site.family(source).filter { (family, _) =>
                family == (if copied == "java.util.Map" then Family.Map else Family.Collection)
              }
              if kind.name.startsWith("java.util.")
              (_, built) <- site.family(new TreePath(site.path, construction))
              if !(built.ordered && (built.distinct || built.keyed))
              (_, to) <- site.family(site.path)
            } yield Link(site.invocation, source, site.name, Vector.empty, kind, to, site.typed, walks = false)
          case _ => None
        }
    }

  private def call(site: Site, receiver: TreePath, kind: Option[(Family, Kind)]): Option[Link] =
    if site.method.getModifiers.contains(Modifier.STATIC) || !library(site.method) then None
    else
      kind.flatMap { (family, from) =>
        val name  = site.name
        val size  = site.invocation.getArguments.size
        val table = family match {
          case Family.Stream     => ChainMethods.Streams
          case Family.Collection => ChainMethods.Collections
          case Family.Map        => ChainMethods.Maps
          case Family.Optional   => ChainMethods.Optionals
          case Family.Array      => Map.empty[(String, Int), String]
        }
        def plain(method: String): Link =
          Link(site.invocation, receiver, method, site.arguments, from, site.result, site.typed)
        if family == Family.Stream && name == "collect" then collected(site, receiver, from)
        else if family == Family.Optional && (name == "orElse" || name == "orElseThrow") then
          reduced(site, name).orElse(table.get((name, size)).map(plain))
        else
          table.get((name, size)).map { lowered =>
            if lowered == "size" then comparison(site.path, site.invocation, plain(lowered))
            else if family == Family.Optional && (lowered == "isDefined" || lowered == "isEmpty") then
              negation(site.context, site.path, receiver, from, lowered).getOrElse(plain(lowered))
            else plain(lowered)
          }
      }

  private def collected(site: Site, receiver: TreePath, from: Kind): Option[Link] =
    site.invocation.getArguments.asScala.toList match {
      case List(collector: MethodInvocationTree) =>
        val inner = new TreePath(site.path, collector)
        site.context.trees.getElement(inner) match {
          case method: ExecutableElement if Lints.owner(method).contains("java.util.stream.Collectors") =>
            val args = collector.getArguments
            val step = (method.getSimpleName.toString, args.size) match {
              case ("toList", 0)  => Some(("toList", Vector.empty[Arg]))
              case ("toSet", 0)   => Some(("toSet", Vector.empty[Arg]))
              case ("joining", 0) => Some(("join", Vector.empty[Arg]))
              case ("joining", 1) =>
                Some(("join", Vector(argument(site.context, new TreePath(inner, args.get(0)), args.get(0)))))
              case _ => None
            }
            step.map((name, arguments) =>
              Link(site.invocation, receiver, name, arguments, from, site.result, site.typed)
            )
          case _ => None
        }
      case _ => None
    }

  private def reduced(site: Site, name: String): Option[Link] = {
    val arguments = site.invocation.getArguments.asScala.toList
    val strict    = name == "orElseThrow" && arguments.isEmpty
    val lenient   = name == "orElse" && arguments.size == 1 && Lints.literalText(arguments.head).contains("")
    site.invocation.getMethodSelect match {
      case select: MemberSelectTree if strict || lenient =>
        val reduceTree = bareOf(new TreePath(new TreePath(site.path, select), select.getExpression))
        reduceTree.getLeaf match {
          case reduce: MethodInvocationTree if reduce.getArguments.size == 1 =>
            reduce.getMethodSelect match {
              case reduction: MemberSelectTree if reduction.getIdentifier.contentEquals("reduce") =>
                val receiver = new TreePath(new TreePath(reduceTree, reduction), reduction.getExpression)
                for {
                  separator <- joinSeparator(reduce.getArguments.get(0))
                  if Lints.isType(site.context.trees.getTypeMirror(reduceTree), "java.util.Optional<java.lang.String>")
                  (family, from) <- site.family(receiver)
                  if family == Family.Stream
                } yield Link(
                  site.invocation,
                  receiver,
                  if strict then "joinReduceStrict" else "joinReduce",
                  Vector(argument(site.context, new TreePath(reduceTree, separator), separator)),
                  from,
                  scalar,
                  site.typed
                )
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }
  }

  private def joinSeparator(tree: Tree): Option[ExpressionTree] =
    tree match {
      case lambda: LambdaExpressionTree
          if lambda.getBodyKind == LambdaExpressionTree.BodyKind.EXPRESSION && lambda.getParameters.size == 2 =>
        val Seq(first, second) = lambda.getParameters.asScala.toSeq.map(_.getName.toString)
        Lints.unparenthesised(lambda.getBody.asInstanceOf[ExpressionTree]) match {
          case sum: BinaryTree if sum.getKind == Tree.Kind.PLUS =>
            (Lints.unparenthesised(sum.getLeftOperand), Lints.unparenthesised(sum.getRightOperand)) match {
              case (prefix: BinaryTree, right: IdentifierTree)
                  if prefix.getKind == Tree.Kind.PLUS && right.getName.contentEquals(second) =>
                Lints.unparenthesised(prefix.getLeftOperand) match {
                  case left: IdentifierTree
                      if left.getName.contentEquals(first) && independent(prefix.getRightOperand, Set(first, second)) =>
                    Some(prefix.getRightOperand)
                  case _ => None
                }
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }

  private def independent(tree: Tree, names: Set[String]): Boolean = {
    var free = true
    new com.sun.source.util.TreeScanner[Unit, Unit] {
      override def visitIdentifier(node: IdentifierTree, unused: Unit): Unit =
        if names(node.getName.toString) then free = false
    }.scan(tree, ())
    free
  }

  private def number(tree: ExpressionTree): Option[Int] =
    Lints.unparenthesised(tree) match {
      case literal: LiteralTree =>
        literal.getValue match {
          case value: java.lang.Integer => Some(value.intValue)
          case value: java.lang.Long    => Some(value.intValue).filter(_.toLong == value.longValue)
          case _                        => None
        }
      case _ => None
    }

  private val Mirrored = Map(
    Tree.Kind.LESS_THAN          -> Tree.Kind.GREATER_THAN,
    Tree.Kind.GREATER_THAN       -> Tree.Kind.LESS_THAN,
    Tree.Kind.LESS_THAN_EQUAL    -> Tree.Kind.GREATER_THAN_EQUAL,
    Tree.Kind.GREATER_THAN_EQUAL -> Tree.Kind.LESS_THAN_EQUAL,
    Tree.Kind.EQUAL_TO           -> Tree.Kind.EQUAL_TO,
    Tree.Kind.NOT_EQUAL_TO       -> Tree.Kind.NOT_EQUAL_TO
  )

  private def comparison(path: TreePath, invocation: MethodInvocationTree, plain: Link): Link =
    Lints.enclosing(path.getParentPath).map(parent => (parent, parent.getLeaf)).collect {
      case (parent, binary: BinaryTree) if Mirrored.contains(binary.getKind) =>
        val onLeft = bareOf(new TreePath(parent, binary.getLeftOperand)).getLeaf eq invocation
        val other  = if onLeft then binary.getRightOperand else binary.getLeftOperand
        val kind   = if onLeft then binary.getKind else Mirrored(binary.getKind)
        (binary, kind, number(other))
    }.flatMap { (binary, kind, value) =>
      val method = (kind, value) match {
        case (Tree.Kind.EQUAL_TO, Some(0)) | (Tree.Kind.LESS_THAN_EQUAL, Some(0)) | (Tree.Kind.LESS_THAN, Some(1)) =>
          Some("sizeIsZero")
        case (
              Tree.Kind.NOT_EQUAL_TO,
              Some(0)
            ) | (Tree.Kind.GREATER_THAN, Some(0)) | (Tree.Kind.GREATER_THAN_EQUAL, Some(1)) =>
          Some("sizeNonZero")
        case _ => None
      }
      method.map(lowered => plain.copy(end = binary, method = lowered, args = Vector.empty, to = scalar))
    }.getOrElse(plain)

  private def negation(context: Context, path: TreePath, receiver: TreePath, from: Kind, method: String): Option[Link] =
    Lints.enclosing(path.getParentPath).map(_.getLeaf).collect {
      case unary: UnaryTree if unary.getKind == Tree.Kind.LOGICAL_COMPLEMENT => unary
    }.filter(_ => method != "isDefined" || Release.atLeast(context, 11)).map { unary =>
      Link(unary, receiver, if method == "isDefined" then "isNone" else "isSome", Vector.empty, from, scalar, false)
    }

  private def argument(context: Context, path: TreePath, tree: Tree): Arg =
    new JavaArg(tree.toString, pure(context, path), identity(context, path), literal(tree), comparison(context, path))

  private def parameterTypeIsArray(context: Context, path: TreePath, parameter: VariableTree): Boolean =
    Option(context.trees.getTypeMirror(new TreePath(path, parameter))).exists(_.getKind == TypeKind.ARRAY)

  /** The equality test with a constant a function is: a lambda or a reference to `equals` of a constant, or a lambda of
    * `equals` or `Objects.equals` of the parameter and a constant, negated or not. Arrays are compared by identity, so
    * the elements and the constant are not arrays.
    */
  private def comparison(context: Context, path: TreePath): Option[Comparison] =
    path.getLeaf match {
      case lambda: LambdaExpressionTree
          if lambda.getParameters.size == 1 && lambda.getBodyKind == LambdaExpressionTree.BodyKind.EXPRESSION &&
            !parameterTypeIsArray(context, path, lambda.getParameters.get(0)) =>
        val parameter = lambda.getParameters.get(0).getName.toString
        equality(context, new TreePath(path, lambda.getBody), parameter)
      case reference: MemberReferenceTree
          if reference.getMode == MemberReferenceTree.ReferenceMode.INVOKE &&
            reference.getName.contentEquals("equals") &&
            objectEquals(context.trees.getElement(path)) =>
        val qualifier = new TreePath(path, reference.getQualifierExpression)
        Option.unless(context.trees.getElement(qualifier).isInstanceOf[TypeElement])(qualifier).flatMap(
          constant(context, _, "").map(Comparison(equal = true, _))
        )
      case _ => None
    }

  private def objectEquals(element: Element): Boolean =
    element match {
      case method: ExecutableElement => method.getSimpleName.contentEquals("equals") &&
        method.getParameters.size == 1 && Lints.isType(method.getParameters.get(0).asType, "java.lang.Object")
      case _ => false
    }

  private def equality(context: Context, path: TreePath, parameter: String): Option[Comparison] = {
    def sub(tree: ExpressionTree): TreePath = new TreePath(path, tree)
    def isParameter(tree: ExpressionTree)   = Lints.unparenthesised(tree) match {
      case name: IdentifierTree => name.getName.contentEquals(parameter)
      case _                    => false
    }
    def against(first: ExpressionTree, second: ExpressionTree): Option[Comparison] =
      if isParameter(first) then constant(context, sub(second), parameter).map(Comparison(equal = true, _))
      else if isParameter(second) then constant(context, sub(first), parameter).map(Comparison(equal = true, _))
      else None
    path.getLeaf match {
      case parenthesized: ParenthesizedTree => equality(context, sub(parenthesized.getExpression), parameter)
      case unary: UnaryTree if unary.getKind == Tree.Kind.LOGICAL_COMPLEMENT =>
        equality(context, sub(unary.getExpression), parameter).map(found => found.copy(equal = !found.equal))
      case invocation: MethodInvocationTree if objectEquals(context.trees.getElement(path)) =>
        invocation.getMethodSelect match {
          case select: MemberSelectTree if invocation.getArguments.size == 1 =>
            against(select.getExpression, invocation.getArguments.get(0))
          case _ => None
        }
      case invocation: MethodInvocationTree
          if invocation.getArguments.size == 2 && staticEquals(context.trees.getElement(path)) =>
        against(invocation.getArguments.get(0), invocation.getArguments.get(1))
      case _ => None
    }
  }

  private def staticEquals(element: Element): Boolean =
    element match {
      case method: ExecutableElement => method.getSimpleName.contentEquals("equals") &&
        method.getModifiers.contains(Modifier.STATIC) && Lints.owner(method).contains("java.util.Objects")
      case _ => false
    }

  /** The source of the value an equality test compares with, when it is the same on every call and does not depend on
    * the parameter: not null, not an array, and with no effect.
    */
  private def constant(context: Context, path: TreePath, parameter: String): Option[String] = {
    val tree   = path.getLeaf
    val isNull = Lints.unparenthesised(tree.asInstanceOf[ExpressionTree]) match {
      case literal: LiteralTree => literal.getValue == null
      case _                    => false
    }
    Option.when(
      !isNull && pure(context, path) && independent(tree, Set(parameter)) &&
        !Option(context.trees.getTypeMirror(path)).exists(_.getKind == TypeKind.ARRAY)
    )(tree.toString)
  }

  private def literal(tree: Tree): Option[Any] =
    tree match {
      case literal: LiteralTree => Option(literal.getValue)
      case _                    => None
    }

  private def identity(context: Context, path: TreePath): Boolean =
    path.getLeaf match {
      case lambda: LambdaExpressionTree
          if lambda.getParameters.size == 1 && lambda.getBodyKind == LambdaExpressionTree.BodyKind.EXPRESSION =>
        Lints.unparenthesised(lambda.getBody.asInstanceOf[ExpressionTree]) match {
          case name: IdentifierTree => name.getName.contentEquals(lambda.getParameters.get(0).getName)
          case _                    => false
        }
      case invocation: MethodInvocationTree =>
        invocation.getArguments.isEmpty &&
        (context.trees.getElement(path) match {
          case method: ExecutableElement => method.getSimpleName.contentEquals("identity") &&
            Lints.owner(method).contains("java.util.function.Function")
          case _ => false
        })
      case _ => false
    }

  private val Arithmetic = Set(
    Tree.Kind.PLUS,
    Tree.Kind.MINUS,
    Tree.Kind.MULTIPLY,
    Tree.Kind.DIVIDE,
    Tree.Kind.REMAINDER,
    Tree.Kind.LEFT_SHIFT,
    Tree.Kind.RIGHT_SHIFT,
    Tree.Kind.UNSIGNED_RIGHT_SHIFT,
    Tree.Kind.LESS_THAN,
    Tree.Kind.GREATER_THAN,
    Tree.Kind.LESS_THAN_EQUAL,
    Tree.Kind.GREATER_THAN_EQUAL,
    Tree.Kind.EQUAL_TO,
    Tree.Kind.NOT_EQUAL_TO,
    Tree.Kind.AND,
    Tree.Kind.OR,
    Tree.Kind.XOR,
    Tree.Kind.CONDITIONAL_AND,
    Tree.Kind.CONDITIONAL_OR
  )

  private val Unary =
    Set(Tree.Kind.LOGICAL_COMPLEMENT, Tree.Kind.UNARY_MINUS, Tree.Kind.UNARY_PLUS, Tree.Kind.BITWISE_COMPLEMENT)

  private def variable(element: Element): Boolean =
    element match {
      case field: VariableElement if field.getKind == ElementKind.FIELD => field.getModifiers.contains(Modifier.FINAL)
      case _: VariableElement                                           => true
      case _                                                            => false
    }

  /** Whether evaluating the tree has no effect a rewrite could reorder, repeat or drop: literals, values that cannot
    * change, and operators on those, any call being taken to have one.
    */
  private def pure(context: Context, path: TreePath): Boolean = {
    def sub(tree: Tree): Boolean = pure(context, new TreePath(path, tree))
    path.getLeaf match {
      case _: LiteralTree               => true
      case lambda: LambdaExpressionTree =>
        lambda.getBodyKind == LambdaExpressionTree.BodyKind.EXPRESSION && sub(lambda.getBody)
      case parenthesized: ParenthesizedTree => sub(parenthesized.getExpression)
      case _: IdentifierTree                => Option(context.trees.getElement(path)).exists(variable)
      case select: MemberSelectTree         =>
        Option(context.trees.getElement(path)).exists(variable) &&
        (context.trees.getElement(new TreePath(path, select.getExpression)) match {
          case _: TypeElement => true
          case _              => sub(select.getExpression)
        })
      case unary: UnaryTree if Unary(unary.getKind)         => sub(unary.getExpression)
      case binary: BinaryTree if Arithmetic(binary.getKind) =>
        sub(binary.getLeftOperand) && sub(binary.getRightOperand) && safe(context, path, binary)
      case conditional: ConditionalExpressionTree =>
        sub(conditional.getCondition) && sub(conditional.getTrueExpression) && sub(conditional.getFalseExpression)
      case _ => false
    }
  }

  private def safe(context: Context, path: TreePath, binary: BinaryTree): Boolean =
    binary.getKind match {
      case Tree.Kind.DIVIDE | Tree.Kind.REMAINDER => number(binary.getRightOperand).exists(_ != 0)
      case Tree.Kind.PLUS                         =>
        Option(context.trees.getTypeMirror(path)).exists(tpe => tpe.getKind.isPrimitive) ||
        Lints.isType(context.trees.getTypeMirror(new TreePath(path, binary.getLeftOperand)), "java.lang.String") &&
        Lints.isType(context.trees.getTypeMirror(new TreePath(path, binary.getRightOperand)), "java.lang.String")
      case _ => true
    }
}
