package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.TreePathScanner
import com.sun.source.util.TreeScanner
import com.sun.source.util.Trees
import javax.lang.model.`type`.*
import javax.lang.model.element.*
import net.kurobako.bonsai.collections.Methods
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

  private val Standard = Vector("java.", "javax.", "jdk.", "sun.", "com.sun.", "scala.")

  private val Views     = Set("keySet", "values", "entrySet", "iterator", "listIterator", "descendingIterator")
  private val Visits    = Set("forEach", "removeIf", "replaceAll")
  private val Factories =
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
      case method: MethodTree if method.getBody != null =>
        trees.getElement(path) match {
          case element: ExecutableElement =>
            val walk = Walk(compilation, path, element)
            walk.scan(new TreePath(path, method.getBody), ())
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
  private def key(element: ExecutableElement)(using Trees): String = {
    val name       = if element.getKind == ElementKind.CONSTRUCTOR then "<init>" else element.getSimpleName.toString
    val parameters = synthetic(element) ++ parametersOf(element).map(parameter => descriptor(parameter.asType))
    val owner      = Collector.qualified(element.getEnclosingElement)
    Descriptors.key(owner, name, parameters, descriptor(element.getReturnType))
  }

  /** The leading parameters javac gives a constructor: the name and ordinal of an enum, the outer instance of an inner
    * class.
    */
  private def synthetic(element: ExecutableElement): Vector[String] =
    if element.getKind != ElementKind.CONSTRUCTOR then Vector.empty
    else
      element.getEnclosingElement match {
        case cls: TypeElement if cls.getKind == ElementKind.ENUM =>
          Vector(Descriptors.clazz("java.lang.String"), Descriptors.primitive("int"))
        case cls: TypeElement
            if cls.getKind == ElementKind.CLASS && cls.getNestingKind == NestingKind.MEMBER &&
              !static(cls) =>
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

  private def standard(element: Element)(using Trees): Boolean = {
    val owner = Collector.qualified(element.getEnclosingElement)
    Standard.exists(owner.startsWith)
  }

  /** The nodes of a loop or branch being collected; an exit is a branch that ends the recursion. */
  private case class Frame(nodes: mutable.ArrayBuffer[Node], exit: Boolean)

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
    private var depth      = 0
    private var floor      = 0

    private val lineages = mutable.Map.empty[TypeElement, Set[String]]

    private def lineage(element: TypeElement): Set[String] =
      lineages.getOrElseUpdate(
        element, {
          val supers = (element.getSuperclass +: element.getInterfaces.asScala.toVector).flatMap(typeElement)
          supers.foldLeft(Set(element.getQualifiedName.toString))(_ ++ lineage(_))
        }
      )

    private def typeElement(tpe: TypeMirror): Option[TypeElement] = tpe match {
      case declared: DeclaredType =>
        declared.asElement match {
          case element: TypeElement => Some(element)
          case _                    => None
        }
      case _ => None
    }

    private def derives(tpe: TypeMirror, target: String*): Boolean = tpe match {
      case variable: TypeVariable         => derives(variable.getUpperBound, target*)
      case intersection: IntersectionType => intersection.getBounds.asScala.exists(derives(_, target*))
      case other                          => typeElement(other).exists(element => target.exists(lineage(element)))
    }

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
        if Set(
            Tree.Kind.POSTFIX_INCREMENT,
            Tree.Kind.POSTFIX_DECREMENT,
            Tree.Kind.PREFIX_INCREMENT,
            Tree.Kind.PREFIX_DECREMENT
          )(node.getKind)
        then assign(node.getExpression)
        super.visitUnary(node, unused)
      }

      private def assign(target: ExpressionTree): Unit = target match {
        case identifier: IdentifierTree =>
          Option(trees.getElement(new TreePath(method, identifier))).foreach(assigned += _)
        case _ =>
      }
    }.scan(method.getLeaf, ())

    def result: Vector[Node] = frames.top.nodes.toVector

    def line(tree: Tree): Int = {
      val position = trees.getSourcePositions.getStartPosition(compilation, tree)
      if position < 0 then 0 else compilation.getLineMap.getLineNumber(position).toInt
    }

    private def emit(node: Node): Unit = frames.top.nodes += node: Unit

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
        if inner.nonEmpty then emit(Node(line(tree), Step.Exit(inner.toVector)))
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

    private def recurs(tree: Tree, parent: TreePath = getCurrentPath): Boolean = {
      var found = false
      new TreePathScanner[Unit, Unit] {
        override def visitMethodInvocation(node: MethodInvocationTree, unused: Unit): Unit = {
          if trees.getElement(getCurrentPath) == element then found = true
          super.visitMethodInvocation(node, unused)
        }
      }.scan(new TreePath(parent, tree), ())
      found
    }

    private def ending(tree: Tree): Boolean = tree match {
      case _: (ReturnTree | ThrowTree) => true
      case block: BlockTree            => block.getStatements.asScala.lastOption.exists(ending)
      case _                           => false
    }

    private def branch(tree: Tree, exit: Boolean): Unit =
      if exit then exiting(tree)(scan(tree, ())) else scan(tree, ())

    override def visitIf(node: IfTree, unused: Unit): Unit = {
      scan(node.getCondition, ())
      val (thenStatement, elseStatement) = (node.getThenStatement, Option(node.getElseStatement))
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
          if rest.nonEmpty && head.getElseStatement == null && ending(head.getThenStatement) &&
            recurs(head.getThenStatement, new TreePath(getCurrentPath, head)) != later =>
        val thenRecurs = !later
        if !thenRecurs then leaving += head
        scan(head, ())
        if thenRecurs then exiting(rest.head._1)(sequence(rest)) else sequence(rest)
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

    private def elementOf(tree: Tree): Element = trees.getElement(new TreePath(getCurrentPath, tree))

    private def typeOf(tree: Tree): TypeMirror = trees.getTypeMirror(new TreePath(getCurrentPath, tree))

    private def path(tree: ExpressionTree): Option[String] = Lint.unwrapped(tree) match {
      case identifier: IdentifierTree =>
        if identifier.getName.contentEquals("this") then Some("this")
        else
          Option(elementOf(identifier)).flatMap { found =>
            found.getKind match {
              case ElementKind.PARAMETER =>
                Some(parameters.indexOf(found)).filter(_ >= 0).map(index => s"$$$index")
              case ElementKind.FIELD | ElementKind.ENUM_CONSTANT =>
                if static(found) then Some(Collector.qualified(found))
                else
                  Option.when(derives(
                    owner.asType,
                    Collector.qualified(found.getEnclosingElement)
                  ))(s"this.${found.getSimpleName}")
              case ElementKind.LOCAL_VARIABLE => aliases.get(found)
              case _                          => None
            }
          }
      case select: MemberSelectTree =>
        Option(elementOf(select)).filter(_.getKind.isField).flatMap { found =>
          if static(found) then Some(Collector.qualified(found))
          else path(select.getExpression).map(base => s"$base.${select.getIdentifier}")
        }
      case _ => None
    }

    private def zero(tree: ExpressionTree): Boolean = Lint.unwrapped(tree) match {
      case number: LiteralTree => number.getValue == Integer.valueOf(0)
      case _                   => false
    }

    private def collection(tree: ExpressionTree): Boolean =
      derives(typeOf(tree), "java.util.Collection", "java.util.Map")

    private def fixed(tree: ExpressionTree): Boolean = Lint.unwrapped(tree) match {
      case _: (IdentifierTree | MemberSelectTree) =>
        Option(elementOf(tree)).filter(_.getKind.isField).exists { found =>
          Lints.constant(found) &&
          (found.asType.getKind == TypeKind.ARRAY || Option(trees.getTree(found)).exists {
            case field: VariableTree => Option(field.getInitializer).exists(factory)
            case _                   => false
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

    private def reach(tree: ExpressionTree): Option[String] = if fixed(tree) then Some(Fixed) else path(tree)

    private def extent(tree: ExpressionTree): Option[String] = Lint.unwrapped(tree) match {
      case call: MethodInvocationTree =>
        Call.selected(call.getMethodSelect) match {
          case Some((Some(receiver), name)) if Views(name) && collection(receiver) => extent(receiver)
          case Some((Some(_), "asList"))
              if call.getArguments.size == 1 && typeOf(call.getArguments.get(0)).getKind == TypeKind.ARRAY =>
            extent(call.getArguments.get(0))
          case _ => None
        }
      case other => reach(other)
    }

    private def sized(tree: ExpressionTree): Boolean =
      derives(typeOf(tree), "java.util.Collection", "java.util.Map", "java.lang.CharSequence")

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
                init.flatMap(sizeOf).map(_ -> counter(binary.getLeftOperand))
              )
            case _ => None
          }
        case _ => None
      }

    private def stream(tree: ExpressionTree): Option[String] = Lint.unwrapped(tree) match {
      case call: MethodInvocationTree =>
        Call.selected(call.getMethodSelect) match {
          case Some((Some(receiver), "stream" | "parallelStream")) if collection(receiver) =>
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
      val found = trees.getElement(getCurrentPath)
      if found != null && found.getKind == ElementKind.LOCAL_VARIABLE then {
        declared(found) = depth
        Option(node.getInitializer).filter(_ => !assigned(found)).foreach { init =>
          if derives(found.asType, "java.util.Iterator") then extent(init).foreach(iterators(found) = _)
          else if found.asType.getKind == TypeKind.INT then sizeOf(init).foreach(lengths(found) = _)
          else if !found.asType.getKind.isPrimitive then {
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
      if node.getKind == Tree.Kind.PLUS_ASSIGNMENT then grown(node, node.getVariable)
      super.visitCompoundAssignment(node, unused)
    }

    override def visitAssignment(node: AssignmentTree, unused: Unit): Unit = {
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
          val found = elementOf(identifier)
          if found != null && derives(found.asType, "java.lang.String") then {
            val since = depth - declared.getOrElse(found, 0)
            if since > 0 then emit(Node(line(node), Step.Op(Kind.Concat(since), Static)))
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
          if standard(constructor) then {
            if derives(constructed, "java.util.Collection", "java.util.Map") && arguments.sizeIs == 1 &&
              collection(arguments.head)
            then emit(Node(line(node), Step.Op(Kind.Copy, extent(arguments.head).getOrElse(Unknown))))
          } else if node.getClassBody == null then
            emit(Node(line(node), Step.Call(key(constructor), Static, arguments.map(extent(_).getOrElse(Unknown)))))
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
            if Visits(name) && collection(target) then Some(extent(target).getOrElse(Unknown))
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
        if standard(callee) then library(node, callee, receiver, name, arguments)
        else {
          val target =
            if static(callee) || callee.getKind == ElementKind.CONSTRUCTOR then Static
            else receiver.fold("this")(target => path(target).getOrElse(Unknown))
          emit(Node(line(node), Step.Call(key(callee), target, passed(callee, arguments))))
        }
      }
    }

    private def passed(callee: ExecutableElement, arguments: Vector[ExpressionTree]): Vector[String] =
      arguments.zipWithIndex.map { (argument, index) =>
        (if callee == element then descent(argument, index) else None).orElse(extent(argument)).getOrElse(Unknown)
      }

    private def positive(tree: ExpressionTree): Boolean = Lint.unwrapped(tree) match {
      case number: LiteralTree =>
        number.getValue match {
          case value: Integer => value > 0
          case _              => false
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
        if positive(from) || lengthLess(to, receiver) then parameter(receiver) else None
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
        case callee: ExecutableElement if !standard(callee) =>
          val qualifier = reference.getQualifierExpression
          val receiver  =
            if static(callee) || callee.getKind == ElementKind.CONSTRUCTOR then Static
            else
              elementOf(qualifier) match {
                case _: TypeElement => Unknown
                case _              =>
                  qualifier match {
                    case identifier: IdentifierTree if identifier.getName.contentEquals("super") => "this"
                    case _ => path(qualifier).getOrElse(Unknown)
                  }
              }
          emit(Node(line(reference), Step.Call(key(callee), receiver, Vector.fill(callee.getParameters.size)(Unknown))))
        case _ =>
      }

    private def library(
        node: MethodInvocationTree,
        callee: ExecutableElement,
        receiver: Option[ExpressionTree],
        name: String,
        arguments: Vector[ExpressionTree]
    ): Unit = {
      def op(kind: Kind, over: Option[String]): Unit = emit(Node(line(node), Step.Op(kind, over.getOrElse(Unknown))))
      val list    = receiver.exists(target => derives(typeOf(target), "java.util.List"))
      val linked  = receiver.exists(target => derives(typeOf(target), "java.util.LinkedList"))
      val onOwner = Collector.qualified(callee.getEnclosingElement)
      val first   = arguments.headOption
      if Methods.Searches(name) && list then op(Kind.Search, receiver.flatMap(extent))
      else if name == "remove" && list && arguments.sizeIs == 1 then {
        if callee.getParameters.get(0).asType.getKind == TypeKind.INT then {
          if zero(arguments.head) && !linked then op(Kind.Shift, receiver.flatMap(extent))
        } else op(Kind.Search, receiver.flatMap(extent))
      } else if name == "add" && list && !linked && arguments.sizeIs == 2 && zero(arguments.head) then
        op(Kind.Shift, receiver.flatMap(extent))
      else if Methods.Sorts(name) && Set("java.util.Collections", "java.util.Arrays")(onOwner) then
        op(Kind.Sort, first.flatMap(extent))
      else if Methods.Sorts(name) && list then op(Kind.Sort, receiver.flatMap(extent))
      else if Methods.Sorts(name) && receiver.exists(stream(_).isDefined) then op(Kind.Sort, receiver.flatMap(stream))
      else if name == "addAll" && arguments.sizeIs == 1 && receiver.exists(collection) then
        op(Kind.Copy, first.flatMap(extent))
      else if name == "toArray" && receiver.exists(collection) then op(Kind.Copy, receiver.flatMap(extent))
      else if Set("copyOf", "copyOfRange")(name) &&
        Set("java.util.Arrays", "java.util.List", "java.util.Set", "java.util.Map")(onOwner)
      then
        op(Kind.Copy, first.flatMap(extent))
    }
  }
}
