package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.TreePathScanner
import com.sun.source.util.TreeScanner
import com.sun.source.util.Trees
import javax.lang.model.`type`.*
import javax.lang.model.element.*
import net.kurobako.bonsai.collections.*
import net.kurobako.bonsai.java.Javac.*
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** The unevaluated cost of a method of the code: the loops it runs with the collections they run over, the library work
  * it does on collections and the calls it makes outside the JDK. Nothing is evaluated here, as the callees may be
  * written in another language; the analysis reads the tree against their bounds.
  *
  * A collection is named by a path from a parameter (`$0`), `this` or a static field, followed by the fields read from
  * it. What cannot be named this way is unknown.
  */
object CostTrees {
  import Model.Cost.*

  private val lineages = java.util.Collections.synchronizedMap(java.util.WeakHashMap[TypeElement, Set[String]]())

  private val fixedFields = java.util.Collections.synchronizedMap(java.util.WeakHashMap[Element, Boolean]())

  private val Standard = Vector("java.", "javax.", "jdk.", "sun.", "com.sun.", "scala.")

  private val Views        = Set("keySet", "values", "entrySet", "iterator", "listIterator", "descendingIterator")
  private[java] val Visits = Set("forEach", "removeIf", "replaceAll")
  private val Factories    =
    Set("of", "copyOf", "asList", "singletonList", "singleton", "singletonMap", "emptyList", "emptySet", "emptyMap")
  private val Expansions = Set("flatMap", "flatMapToInt", "flatMapToLong", "flatMapToDouble", "mapMulti")
  private val Stages     = Set(
    "filter",
    "map",
    "mapToInt",
    "mapToLong",
    "mapToDouble",
    "mapToObj",
    "boxed",
    "sorted",
    "distinct",
    "peek",
    "limit",
    "skip",
    "sequential",
    "parallel",
    "unordered",
    "takeWhile",
    "dropWhile",
    "asLongStream",
    "asDoubleStream"
  )

  def of(compilation: CompilationUnitTree, path: TreePath, owner: String)(using trees: Trees): Option[Model.Cost] =
    path.getLeaf match {
      case method: MethodTree if method.body.isDefined =>
        trees.getElement(path) match {
          case element: ExecutableElement =>
            val walk = Walk(compilation, path, element)
            walk.scan(TreePath(path, method.getBody), ())
            Some(
              Model.Cost(
                owner,
                Collector.qualified(element),
                key(element),
                walk.line(method),
                Collector.suppressed(element),
                parametersOf(element).map(_.getSimpleName.toString),
                walk.result
              )
            )
          case _ => None
        }
      case _ => None
    }

  /** The qualified owner, the name and the erased JVM descriptor of a method or constructor. */
  private[java] def key(element: ExecutableElement)(using Trees): String = {
    val name       = if (element.getKind == ElementKind.CONSTRUCTOR) "<init>" else element.getSimpleName.toString
    val parameters = synthetic(element) ++ parametersOf(element).map(parameter => descriptor(parameter.asType))
    val owner      = Collector.qualifiedEnclosing(element)
    Descriptors.key(owner, name, parameters, descriptor(element.getReturnType))
  }

  /** The leading parameters javac gives a constructor: the name and ordinal of an enum, the outer instance of an inner
    * class.
    */
  private def synthetic(element: ExecutableElement): Vector[String] =
    if (element.getKind != ElementKind.CONSTRUCTOR) Vector.empty
    else
      element.getEnclosingElement match {
        case cls: TypeElement if cls.getKind == ElementKind.ENUM =>
          Vector(Descriptors.clazz("java.lang.String"), Descriptors.primitive("int"))
        case cls: TypeElement
            if cls.getKind == ElementKind.CLASS && cls.getNestingKind == NestingKind.MEMBER &&
              cls.getEnclosingElement.getKind.isClass && !static(cls) =>
          Vector(Descriptors.clazz(binary(cls.getEnclosingElement.asInstanceOf[TypeElement])))
        case _ => Vector.empty
      }

  private def descriptor(tpe: TypeMirror): String = tpe.getKind match {
    case TypeKind.BOOLEAN      => Descriptors.primitive("boolean")
    case TypeKind.BYTE         => Descriptors.primitive("byte")
    case TypeKind.SHORT        => Descriptors.primitive("short")
    case TypeKind.INT          => Descriptors.primitive("int")
    case TypeKind.LONG         => Descriptors.primitive("long")
    case TypeKind.CHAR         => Descriptors.primitive("char")
    case TypeKind.FLOAT        => Descriptors.primitive("float")
    case TypeKind.DOUBLE       => Descriptors.primitive("double")
    case TypeKind.VOID         => Descriptors.primitive("void")
    case TypeKind.ARRAY        => Descriptors.array(descriptor(tpe.asInstanceOf[ArrayType].getComponentType))
    case TypeKind.TYPEVAR      => descriptor(tpe.asInstanceOf[TypeVariable].getUpperBound)
    case TypeKind.INTERSECTION => descriptor(tpe.asInstanceOf[IntersectionType].getBounds.get(0))
    case TypeKind.DECLARED     =>
      tpe.asInstanceOf[DeclaredType].asElement match {
        case element: TypeElement => Descriptors.clazz(binary(element))
        case _                    => Descriptors.Object
      }
    case _ => Descriptors.Object
  }

  private def binary(element: TypeElement): String = element.getEnclosingElement match {
    case outer: TypeElement => s"${binary(outer)}$$${element.getSimpleName}"
    case _                  => element.getQualifiedName.toString
  }

  private def parametersOf(element: ExecutableElement): Vector[VariableElement] =
    element.getParameters.asScala.toVector

  private def static(element: Element): Boolean = element.getModifiers.contains(Modifier.STATIC)

  private def standard(element: Element)(using Trees): Boolean =
    standard(Collector.qualifiedEnclosing(element))

  private[java] def standard(owner: String): Boolean = Standard.exists(owner.startsWith(_))

  private[java] def collection(tpe: TypeMirror): Boolean = derives(tpe, "java.util.Collection", "java.util.Map")

  private[java] def derives(tpe: TypeMirror, target: String*): Boolean = tpe match {
    case variable: TypeVariable         => derives(variable.getUpperBound, target*)
    case intersection: IntersectionType => intersection.getBounds.asScala.exists(derives(_, target*))
    case other                          => typeElement(other).exists(element => target.exists(lineage(element)))
  }

  private def typeElement(tpe: TypeMirror): Option[TypeElement] = tpe match {
    case declared: DeclaredType =>
      declared.asElement match {
        case element: TypeElement => Some(element)
        case _                    => None
      }
    case _ => None
  }

  private def lineage(element: TypeElement): Set[String] =
    Option(lineages.get(element)).getOrElse {
      val supers = (element.getSuperclass +: element.getInterfaces.asScala.toVector).flatMap(typeElement(_))
      val found  = supers.foldLeft(Set(element.getQualifiedName.toString))(_ ++ lineage(_))
      lineages.put(element, found)
      found
    }

  /** The nodes of a loop or branch being collected; an exit is a branch that ends the recursion. */
  private case class Frame(nodes: mutable.ArrayBuffer[Node], exit: Boolean)

  // A javac visitor is an abstract class, so it is written inline.
  @SuppressWarnings(Array("scalafix:BannedSyntax.anonymous-allocation"))
  private class Walk(compilation: CompilationUnitTree, method: TreePath, element: ExecutableElement)(using trees: Trees)
      extends TreePathScanner[Unit, Unit] {
    private val owner      = element.getEnclosingElement.asInstanceOf[TypeElement]
    private val parameters = parametersOf(element)
    private val frames     = mutable.Stack(Frame(mutable.ArrayBuffer.empty, exit = false))
    private val assigned   = mutable.Set.empty[Element]
    private val aliases    = mutable.Map.empty[Element, String]
    private val lengths    = mutable.Map.empty[Element, String]
    private val iterators  = mutable.Map.empty[Element, String]
    private val shrinks    = mutable.Map.empty[Element, Int]
    private val declared   = mutable.Map.empty[Element, Int]
    private val leaving    = mutable.Set.empty[IfTree]
    private lazy val flow  = CostFlows(trees, method, assigned).run()
    // The state is updated as the walk proceeds.
    @SuppressWarnings(Array("scalafix:DisableSyntax.var"))
    private var depth = 0
    // The state is updated as the walk proceeds.
    @SuppressWarnings(Array("scalafix:DisableSyntax.var"))
    private var floor = 0

    new TreeScanner[Unit, Unit] {
      override def visitAssignment(node: AssignmentTree, unused: Unit): Unit = {
        assign(node.getVariable)
        super.visitAssignment(node, unused)
      }

      override def visitCompoundAssignment(node: CompoundAssignmentTree, unused: Unit): Unit = {
        assign(node.getVariable)
        super.visitCompoundAssignment(node, unused)
      }

      override def visitUnary(node: UnaryTree, unused: Unit): Unit = {
        if (
          Set(
            Tree.Kind.POSTFIX_INCREMENT,
            Tree.Kind.POSTFIX_DECREMENT,
            Tree.Kind.PREFIX_INCREMENT,
            Tree.Kind.PREFIX_DECREMENT
          )(node.getKind)
        ) assign(node.getExpression)
        super.visitUnary(node, unused)
      }

      private def assign(target: ExpressionTree): Unit = target match {
        case identifier: IdentifierTree =>
          Option(trees.getElement(TreePath(method, identifier))).foreach(assigned += _)
        case _ =>
      }
    }.scan(method.getLeaf, ())

    def result: Vector[Node] = frames.top.nodes.toVector

    def line(tree: Tree): Int = {
      val position = trees.getSourcePositions.getStartPosition(compilation, tree)
      if (position < 0) 0 else compilation.getLineMap.getLineNumber(position).toInt
    }

    private def emit(node: Node): Unit = frames.top.nodes += node: Unit

    private def sited(tree: Tree)(step: Sink => Step): Unit = emit(Node(line(tree), step(flow.of(tree))))

    private def looped(tree: Tree, over: (String, Option[Element]), callback: Boolean)(body: => Unit): Unit = {
      val (extent, counter) = over
      val absorbs           = counter.fold(0)(found => (depth - declared.getOrElse(found, 0)).max(0))
      frames.push(Frame(mutable.ArrayBuffer.empty, exit = false))
      depth += 1
      try body
      finally {
        depth -= 1
        val inner = frames.pop().nodes
        emit(Node(line(tree), Step.Loop(extent, callback, absorbs, inner.toVector)))
      }
    }

    private def exiting(tree: Tree)(body: => Unit): Unit = {
      frames.push(Frame(mutable.ArrayBuffer.empty, exit = true))
      try body
      finally {
        val inner = frames.pop().nodes
        if (inner.nonEmpty) emit(Node(line(tree), Step.Exit(inner.toVector)))
      }
    }

    private def once(body: => Unit): Unit = {
      val reachable = frames.size - 1 - floor
      val above     = frames.take(reachable).indexWhere(_.exit) match {
        case -1    => reachable
        case found => found
      }
      val removed = Vector.fill(above)(frames.pop())
      depth -= above
      try body
      finally {
        depth += above
        removed.reverseIterator.foreach(frames.push)
      }
    }

    override def visitReturn(node: ReturnTree, unused: Unit): Unit = once(super.visitReturn(node, unused))

    override def visitThrow(node: ThrowTree, unused: Unit): Unit = once(super.visitThrow(node, unused))

    // The state is updated as the walk proceeds.
    @SuppressWarnings(Array("scalafix:DisableSyntax.var"))
    private def recurs(tree: Tree, parent: TreePath = getCurrentPath): Boolean = {
      var found = false
      new TreePathScanner[Unit, Unit] {
        override def visitMethodInvocation(node: MethodInvocationTree, unused: Unit): Unit = {
          if (trees.getElement(getCurrentPath) == element) found = true
          super.visitMethodInvocation(node, unused)
        }
      }.scan(TreePath(parent, tree), ())
      found
    }

    private def ending(tree: Tree): Boolean = tree match {
      case _: (ReturnTree | ThrowTree) => true
      case block: BlockTree            => block.getStatements.asScala.lastOption.exists(ending(_))
      case _                           => false
    }

    private def branch(tree: Tree, exit: Boolean): Unit =
      if (exit) exiting(tree)(scan(tree, ())) else scan(tree, ())

    override def visitIf(node: IfTree, unused: Unit): Unit = {
      scan(node.getCondition, ())
      val (thenStatement, elseStatement) = (node.getThenStatement, node.elseStatement)
      val (thenRecurs, elseRecurs)       = (recurs(thenStatement), elseStatement.exists(recurs(_)))
      val split                          = elseStatement.nonEmpty && thenRecurs != elseRecurs
      branch(thenStatement, leaving(node) || split && !thenRecurs)
      elseStatement.foreach(branch(_, split && !elseRecurs))
    }

    override def visitConditionalExpression(node: ConditionalExpressionTree, unused: Unit): Unit = {
      scan(node.getCondition, ())
      val (thenRecurs, elseRecurs) = (recurs(node.getTrueExpression), recurs(node.getFalseExpression))
      branch(node.getTrueExpression, thenRecurs != elseRecurs && !thenRecurs)
      branch(node.getFalseExpression, thenRecurs != elseRecurs && !elseRecurs)
    }

    override def visitSwitch(node: SwitchTree, unused: Unit): Unit =
      switched(node.getExpression, node.getCases.asScala.toList)

    override def visitSwitchExpression(node: SwitchExpressionTree, unused: Unit): Unit =
      switched(node.getExpression, node.getCases.asScala.toList)

    private def switched(selector: ExpressionTree, cases: List[CaseTree]): Unit = {
      scan(selector, ())
      val recursing = cases.map(recurs(_))
      val split     = recursing.contains(true) && recursing.contains(false)
      cases.zip(recursing).foreach((branch, recurs) => this.branch(branch, split && !recurs && closed(branch)))
    }

    private def closed(node: CaseTree): Boolean =
      node.getCaseKind == CaseTree.CaseKind.RULE ||
        Option(node.getStatements).flatMap(_.asScala.lastOption).exists {
          case _: BreakTree => true
          case other        => ending(other)
        }

    override def visitBlock(node: BlockTree, unused: Unit): Unit = {
      val statements = node.getStatements.asScala.toList
      val recursing  = statements.scanRight(false)((statement, later) => later || recurs(statement)).tail
      sequence(statements.zip(recursing))
    }

    /** The statements of a block, each with whether a recursive call follows it. */
    private def sequence(statements: List[(StatementTree, Boolean)]): Unit = statements match {
      case (head: IfTree, later) :: rest
          if rest.nonEmpty && head.elseStatement.isEmpty && ending(head.getThenStatement) &&
            recurs(head.getThenStatement, TreePath(getCurrentPath, head)) != later =>
        val thenRecurs = !later
        if (!thenRecurs) leaving += head
        scan(head, ())
        if (thenRecurs) exiting(rest.head._1)(sequence(rest)) else sequence(rest)
      case (head, _) :: rest =>
        scan(head, ())
        sequence(rest)
      case Nil =>
    }

    override def visitLambdaExpression(node: LambdaExpressionTree, unused: Unit): Unit = {
      val saved = floor
      floor = frames.size - 1
      try super.visitLambdaExpression(node, unused)
      finally floor = saved
    }

    private def elementOf(tree: Tree): Element = Lints.elementOf(trees, getCurrentPath, tree)

    private def typeOf(tree: Tree): TypeMirror = Lints.typeOf(trees, getCurrentPath, tree)

    private def path(tree: ExpressionTree): Option[String] = Lint.unwrapped(tree) match {
      case identifier: IdentifierTree =>
        if (identifier.getName.contentEquals("this")) Some("this")
        else
          Option(elementOf(identifier)).flatMap { found =>
            found.getKind match {
              case ElementKind.PARAMETER =>
                Some(parameters.indexOf(found)).filter(_ >= 0).map(index => s"$$$index")
              case ElementKind.FIELD | ElementKind.ENUM_CONSTANT =>
                if (static(found)) Some(Collector.qualified(found))
                else
                  Option.when(derives(
                    owner.asType,
                    Collector.qualifiedEnclosing(found)(using trees)
                  ))(s"this.${found.getSimpleName}")
              case ElementKind.LOCAL_VARIABLE => aliases.get(found)
              case _                          => None
            }
          }
      case select: MemberSelectTree =>
        Option(elementOf(select)).filter(_.getKind.isField).flatMap { found =>
          if (static(found)) Some(Collector.qualified(found))
          else path(select.getExpression).map(base => s"$base.${select.getIdentifier}")
        }
      case _ => None
    }

    private def zero(tree: ExpressionTree): Boolean = Lint.unwrapped(tree) match {
      case number: LiteralTree => number.getValue == 0
      case _                   => false
    }

    private def fixed(tree: ExpressionTree): Boolean = Lint.unwrapped(tree) match {
      case _: (IdentifierTree | MemberSelectTree) =>
        Option(elementOf(tree)).filter(_.getKind.isField).exists { found =>
          Lints.constant(found) &&
          (found.asType.getKind == TypeKind.ARRAY || Memo(fixedFields, found) {
            Option(trees.getTree(found)).exists {
              case field: VariableTree => field.initializer.exists(factory(_))
              case _                   => false
            }
          })
        }
      case _ => false
    }

    private def factory(init: ExpressionTree): Boolean = Lint.unwrapped(init) match {
      case call: MethodInvocationTree =>
        Call.selected(call.getMethodSelect).exists { (receiver, name) =>
          Factories(name) && receiver.exists(target =>
            derives(
              typeOf(target),
              "java.util.List",
              "java.util.Set",
              "java.util.Map",
              "java.util.Arrays",
              "java.util.Collections"
            )
          )
        }
      case _ => false
    }

    private def reach(tree: ExpressionTree): Option[String] = if (fixed(tree)) Some(Fixed) else path(tree)

    @tailrec
    private def extent(tree: ExpressionTree): Option[String] = Lint.unwrapped(tree) match {
      case call: MethodInvocationTree =>
        Call.selected(call.getMethodSelect) match {
          case Some((Some(receiver), name)) if Views(name) && collection(typeOf(receiver)) => extent(receiver)
          case Some((Some(_), "asList"))
              if call.getArguments.size == 1 && typeOf(call.getArguments.get(0)).getKind == TypeKind.ARRAY =>
            extent(call.getArguments.get(0))
          case _ => None
        }
      case other => reach(other)
    }

    private def sized(tree: ExpressionTree): Boolean =
      derives(typeOf(tree), "java.util.Collection", "java.util.Map", "java.lang.CharSequence")

    @tailrec
    private def sizeOf(tree: ExpressionTree): Option[String] = Lint.unwrapped(tree) match {
      case select: MemberSelectTree
          if select.getIdentifier.contentEquals("length") && typeOf(select.getExpression).getKind == TypeKind.ARRAY =>
        reach(select.getExpression)
      case call: MethodInvocationTree if call.getArguments.isEmpty =>
        Call.selected(call.getMethodSelect) match {
          case Some((Some(receiver), "size" | "length")) if sized(receiver) => reach(receiver)
          case _                                                            => None
        }
      case identifier: IdentifierTree => Option(elementOf(identifier)).flatMap(lengths.get)
      case binary: BinaryTree
          if Set(Tree.Kind.PLUS, Tree.Kind.MINUS)(binary.getKind) &&
            Lint.unwrapped(binary.getRightOperand).isInstanceOf[LiteralTree] =>
        sizeOf(binary.getLeftOperand)
      case _ => None
    }

    private def counter(tree: ExpressionTree): Option[Element] = Lint.unwrapped(tree) match {
      case identifier: IdentifierTree => Option(elementOf(identifier))
      case _                          => None
    }

    private def bound(condition: ExpressionTree, init: Option[ExpressionTree]): Option[(String, Option[Element])] =
      Lint.unwrapped(condition) match {
        case call: MethodInvocationTree =>
          Call.selected(call.getMethodSelect) match {
            case Some((Some(receiver), "hasNext")) =>
              counter(receiver).flatMap(found => iterators.get(found).map(_ -> Some(found)))
            case _ => None
          }
        case binary: BinaryTree =>
          binary.getKind match {
            case Tree.Kind.CONDITIONAL_AND =>
              bound(binary.getLeftOperand, init).orElse(bound(binary.getRightOperand, init))
            case Tree.Kind.LESS_THAN | Tree.Kind.LESS_THAN_EQUAL | Tree.Kind.NOT_EQUAL_TO =>
              sizeOf(binary.getRightOperand).map(_ -> counter(binary.getLeftOperand))
            case Tree.Kind.GREATER_THAN | Tree.Kind.GREATER_THAN_EQUAL =>
              sizeOf(binary.getLeftOperand).map(_ -> counter(binary.getRightOperand)).orElse(
                init.flatMap(sizeOf(_)).map(_ -> counter(binary.getLeftOperand))
              )
            case _ => None
          }
        case _ => None
      }

    private def stream(tree: ExpressionTree): Option[String] = Lint.unwrapped(tree) match {
      case call: MethodInvocationTree =>
        Call.selected(call.getMethodSelect) match {
          case Some((Some(receiver), "stream" | "parallelStream")) if collection(typeOf(receiver)) =>
            Some(extent(receiver).getOrElse(Unknown))
          case Some((Some(receiver), "stream"))
              if call.getArguments.size == 1 && derives(typeOf(receiver), "java.util.Arrays") =>
            Some(extent(call.getArguments.get(0)).getOrElse(Unknown))
          case Some((Some(receiver), name)) if Stages(name)     => stream(receiver)
          case Some((Some(receiver), name)) if Expansions(name) => stream(receiver).map(_ => Unknown)
          case _                                                => None
        }
      case _ => None
    }

    override def visitClass(node: ClassTree, unused: Unit): Unit = ()

    override def visitVariable(node: VariableTree, unused: Unit): Unit = {
      trees.namedElement(getCurrentPath).filter(_.getKind == ElementKind.LOCAL_VARIABLE).foreach { found =>
        declared(found) = depth
        node.initializer.filter(_ => !assigned(found)).foreach { init =>
          if (derives(found.asType, "java.util.Iterator")) extent(init).foreach(iterators(found) = _)
          else if (found.asType.getKind == TypeKind.INT) sizeOf(init).foreach(lengths(found) = _)
          else if (!found.asType.getKind.isPrimitive) {
            path(init).foreach(aliases(found) = _)
            shrinking(init).foreach(shrinks(found) = _)
          }
        }
      }
      super.visitVariable(node, unused)
    }

    override def visitEnhancedForLoop(node: EnhancedForLoopTree, unused: Unit): Unit = {
      scan(node.getExpression, ())
      looped(node, extent(node.getExpression).getOrElse(Unknown) -> None, callback = false) {
        scan(node.getVariable, ())
        scan(node.getStatement, ())
      }
    }

    override def visitForLoop(node: ForLoopTree, unused: Unit): Unit = {
      scan(node.getInitializer, ())
      val init = node.getInitializer.asScala.headOption.collect { case variable: VariableTree =>
        variable.getInitializer
      }.flatMap(Option(_))
      val over = Option(node.getCondition).flatMap(bound(_, init)).getOrElse(Unknown -> None)
      looped(node, over, callback = false) {
        scan(node.getCondition, ())
        scan(node.getUpdate, ())
        scan(node.getStatement, ())
      }
    }

    override def visitWhileLoop(node: WhileLoopTree, unused: Unit): Unit =
      looped(node, bound(node.getCondition, None).getOrElse(Unknown -> None), callback = false) {
        scan(node.getCondition, ())
        scan(node.getStatement, ())
      }

    override def visitDoWhileLoop(node: DoWhileLoopTree, unused: Unit): Unit =
      looped(node, bound(node.getCondition, None).getOrElse(Unknown -> None), callback = false) {
        scan(node.getStatement, ())
        scan(node.getCondition, ())
      }

    override def visitCompoundAssignment(node: CompoundAssignmentTree, unused: Unit): Unit = {
      if (node.getKind == Tree.Kind.PLUS_ASSIGNMENT) grown(node, node.getVariable)
      super.visitCompoundAssignment(node, unused)
    }

    override def visitAssignment(node: AssignmentTree, unused: Unit): Unit = {
      @tailrec
      def leftmost(tree: ExpressionTree): Tree = Lint.unwrapped(tree) match {
        case binary: BinaryTree if binary.getKind == Tree.Kind.PLUS => leftmost(binary.getLeftOperand)
        case other                                                  => other
      }
      (Lint.unwrapped(node.getVariable), Lint.unwrapped(node.getExpression)) match {
        case (target: IdentifierTree, sum: BinaryTree) if sum.getKind == Tree.Kind.PLUS =>
          leftmost(sum) match {
            case first: IdentifierTree if elementOf(first) == elementOf(target) => grown(node, target)
            case _                                                              =>
          }
        case _ =>
      }
      super.visitAssignment(node, unused)
    }

    private def grown(node: Tree, target: ExpressionTree): Unit =
      Lint.unwrapped(target) match {
        case identifier: IdentifierTree =>
          Option(elementOf(identifier)).filter(found => derives(found.asType, "java.lang.String")).foreach { found =>
            val since = depth - declared.getOrElse(found, 0)
            if (since > 0) sited(node)(Step.Op(Kind.Concat(since), Static, _))
          }
        case _ =>
      }

    override def visitNewClass(node: NewClassTree, unused: Unit): Unit = {
      scan(node.getEnclosingExpression, ())
      scan(node.getArguments, ())
      trees.getElement(getCurrentPath) match {
        case constructor: ExecutableElement =>
          val arguments   = Lints.arguments(node)
          val constructed = constructor.getEnclosingElement.asType
          if (standard(constructor)) {
            if (collection(constructed) && arguments.sizeIs == 1 && collection(typeOf(arguments.head)))
              sited(node)(Step.Op(Kind.Copy, extent(arguments.head).getOrElse(Unknown), _))
          } else if (node.classBody.isEmpty)
            sited(node)(Step.Call(key(constructor), Static, arguments.map(extent(_).getOrElse(Unknown)), _))
        case _ =>
      }
    }

    override def visitMethodInvocation(node: MethodInvocationTree, unused: Unit): Unit = {
      val found     = Call.of(trees, getCurrentPath)
      val arguments = Lints.arguments(node)
      scan(node.getMethodSelect, ())
      val over =
        found.flatMap { case Call(_, _, _, receiver, name, _) =>
          receiver.flatMap { target =>
            if (Visits(name) && collection(typeOf(target))) Some(extent(target).getOrElse(Unknown))
            else stream(target)
          }
        }
      arguments.foreach {
        case lambda: LambdaExpressionTree if over.isDefined =>
          looped(lambda, over.get -> None, callback = true)(scan(lambda, ()))
        case reference: MemberReferenceTree if over.isDefined =>
          scan(reference, ())
          looped(reference, over.get -> None, callback = true)(referenced(reference))
        case other => scan(other, ())
      }
      found.foreach { case Call(_, _, callee, receiver, name, _) =>
        if (standard(callee)) library(node, callee, receiver, name, arguments)
        else {
          val target =
            if (static(callee) || callee.getKind == ElementKind.CONSTRUCTOR) Static
            else receiver.fold("this")(target => path(target).getOrElse(Unknown))
          sited(node)(Step.Call(key(callee), target, passed(callee, arguments), _))
        }
      }
    }

    private def passed(callee: ExecutableElement, arguments: Vector[ExpressionTree]): Vector[String] =
      arguments.zipWithIndex.map { (argument, index) =>
        (if (callee == element) descent(argument, index) else None).orElse(extent(argument)).getOrElse(Unknown)
      }

    private def positive(tree: ExpressionTree): Boolean = Lint.unwrapped(tree) match {
      case number: LiteralTree =>
        (number.getValue: Any) match {
          case value: Int => value > 0
          case _          => false
        }
      case _ => false
    }

    private def lengthLess(tree: ExpressionTree, of: ExpressionTree): Boolean = Lint.unwrapped(tree) match {
      case binary: BinaryTree =>
        binary.getKind == Tree.Kind.MINUS && positive(binary.getRightOperand) &&
        sizeOf(binary.getLeftOperand).exists(reach(of).contains)
      case _ => false
    }

    private def shrinking(tree: ExpressionTree): Option[Int] = {
      def parameter(of: ExpressionTree): Option[Int] =
        path(of).filter(_.startsWith("$")).flatMap(_.drop(1).toIntOption).filter(parameters.indices.contains)
      def cut(receiver: ExpressionTree, from: ExpressionTree, to: ExpressionTree) =
        if (positive(from) || lengthLess(to, receiver)) parameter(receiver) else None
      Lint.unwrapped(tree) match {
        case call: MethodInvocationTree =>
          val args = Lints.arguments(call)
          Call.selected(call.getMethodSelect) match {
            case Some((Some(receiver), "subList" | "substring")) if args.sizeIs == 2 =>
              cut(receiver, args(0), args(1))
            case Some((Some(receiver), "substring")) if args.sizeIs == 1 && positive(args(0)) => parameter(receiver)
            case Some((Some(_), "copyOfRange")) if args.sizeIs == 3 && positive(args(1))      => parameter(args(0))
            case _                                                                            => None
          }
        case identifier: IdentifierTree => Option(elementOf(identifier)).flatMap(shrinks.get)
        case _                          => None
      }
    }

    private def descent(argument: ExpressionTree, index: Int): Option[String] =
      Lint.unwrapped(argument) match {
        case binary: BinaryTree
            if binary.getKind == Tree.Kind.MINUS && positive(binary.getRightOperand) &&
              typeOf(binary).getKind == TypeKind.INT && path(binary.getLeftOperand).contains(s"$$$index") &&
              parameters(index).asType.getKind == TypeKind.INT =>
          Some(stepped(index))
        case other => shrinking(other).filter(_ == index).map(_ => smaller(index))
      }

    private def referenced(reference: MemberReferenceTree): Unit =
      elementOf(reference) match {
        // `T[]::new` allocates an array, as `new T[n]` does, through a constructor javac makes up for it
        case _ if reference.getQualifierExpression.isInstanceOf[ArrayTypeTree] =>
        case callee: ExecutableElement if !standard(callee)                    =>
          val qualifier = reference.getQualifierExpression
          val receiver  =
            if (static(callee) || callee.getKind == ElementKind.CONSTRUCTOR) Static
            else
              elementOf(qualifier) match {
                case _: TypeElement => Unknown
                case _              =>
                  qualifier match {
                    case identifier: IdentifierTree if identifier.getName.contentEquals("super") => "this"
                    case _ => path(qualifier).getOrElse(Unknown)
                  }
              }
          sited(reference)(Step.Call(key(callee), receiver, Vector.fill(callee.getParameters.size)(Unknown), _))
        case _ =>
      }

    private def library(
        node: MethodInvocationTree,
        callee: ExecutableElement,
        receiver: Option[ExpressionTree],
        name: String,
        arguments: Vector[ExpressionTree]
    ): Unit = {
      def op(kind: Kind, over: Option[String]): Unit = sited(node)(Step.Op(kind, over.getOrElse(Unknown), _))
      val list    = receiver.exists(target => derives(typeOf(target), "java.util.List"))
      val linked  = receiver.exists(target => derives(typeOf(target), "java.util.LinkedList"))
      val onOwner = Collector.qualifiedEnclosing(callee)(using trees)
      val first   = arguments.headOption
      if (Methods.Searches(name) && list) op(Kind.Search, receiver.flatMap(extent(_)))
      else if (name == "remove" && list && arguments.sizeIs == 1) {
        if (callee.getParameters.get(0).asType.getKind == TypeKind.INT) {
          if (zero(arguments.head) && !linked) op(Kind.Shift, receiver.flatMap(extent(_)))
        } else op(Kind.Search, receiver.flatMap(extent(_)))
      } else if (name == "add" && list && !linked && arguments.sizeIs == 2 && zero(arguments.head))
        op(Kind.Shift, receiver.flatMap(extent(_)))
      else if (Methods.Sorts(name) && Set("java.util.Collections", "java.util.Arrays")(onOwner))
        op(Kind.Sort, first.flatMap(extent(_)))
      else if (Methods.Sorts(name) && list) op(Kind.Sort, receiver.flatMap(extent(_)))
      else if (Methods.Sorts(name) && receiver.exists(stream(_).isDefined)) op(Kind.Sort, receiver.flatMap(stream(_)))
      else if (name == "addAll" && arguments.sizeIs == 1 && receiver.map(typeOf(_)).exists(collection(_)))
        op(Kind.Copy, first.flatMap(extent(_)))
      else if (name == "toArray" && receiver.map(typeOf(_)).exists(collection(_)))
        op(Kind.Copy, receiver.flatMap(extent(_)))
      else if (
        Set("copyOf", "copyOfRange")(name) &&
        Set("java.util.Arrays", "java.util.List", "java.util.Set", "java.util.Map")(onOwner)
      )

        op(Kind.Copy, first.flatMap(extent(_)))
    }
  }
}
