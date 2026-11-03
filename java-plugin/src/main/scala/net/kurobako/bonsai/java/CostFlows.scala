package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.TreeScanner
import com.sun.source.util.Trees
import javax.lang.model.`type`.*
import javax.lang.model.element.*
import net.kurobako.bonsai.collections.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** The sinks of the expressions of a method. */
private[java] class CostFlow(sinks: collection.Map[Tree, Sink]) {
  def of(tree: Tree): Sink = sinks.getOrElse(tree, Sink.Returned)
}

/** Escape analysis over the body of a Java method: for each expression, whether its value is kept past the point it is
  * built at. A value is kept when the method returns it, stores it in a field or a variable of an outer method, or
  * accumulates it in a collection or variable that outlives the turn of a loop. A value that only reaches a call that
  * returns nothing it could hold is dropped.
  *
  * A call holds its arguments in its result, unless it mutates a collection, which keeps them in the receiver. A call
  * of a method of the build is no different, as the callee may be of another compilation. A result that cannot hold a
  * reference, such as a number or a string, holds nothing. The stages of a stream are lazy, so what a function of a
  * stage returns goes where the result of the stage does, rather than being held until the traversal ends.
  */
private[java] class CostFlows(
    trees: Trees,
    root: TreePath,
    assigned: collection.Set[Element]
) {
  private val sinks    = mutable.Map.empty[Tree, Sink]
  private val uses     = mutable.Map.empty[Element, Sink]
  private val declared = mutable.Map.empty[Element, Int]
  private var changed  = false
  private var returns  = Sink.Returned
  private var yields   = Sink.Returned

  private val Voids = Set(TypeKind.VOID, TypeKind.NONE, TypeKind.NULL)
  private val Atoms = Seq("java.lang.String", "java.lang.Number", "java.lang.Boolean", "java.lang.Character")

  def run(): CostFlow = {
    var rounds = 0
    while rounds == 0 || (changed && rounds < 8) do {
      changed = false
      rounds += 1
      flow(root.getLeaf.asInstanceOf[MethodTree].getBody, Sink.Returned, 0)
    }
    CostFlow(sinks)
  }

  private def elementOf(tree: Tree): Element = Lints.elementOf(trees, root, tree)

  private def typeOf(tree: Tree): Option[TypeMirror] = Option(Lints.typeOf(trees, root, tree))

  private def scalar(tree: Tree): Boolean = typeOf(tree).exists { tpe =>
    tpe.getKind.isPrimitive || Voids(tpe.getKind) || CostTrees.derives(tpe, Atoms*)
  }

  private def carried(tree: Tree, sink: Sink): Sink = if (scalar(tree)) Sink.Dropped else sink

  private def use(element: Element, sink: Sink): Unit =
    if (Effects.local(element)) {
      val before = uses.getOrElse(element, Sink.Dropped)
      val joined = before.join(sink)
      if (joined != before) {
        uses(element) = joined
        changed = true
      }
    }

  /** Where what is assigned to or bound as `element` goes: where it is read, and for a variable the loops it lives
    * across.
    */
  private def held(element: Element): Sink = {
    val read = uses.getOrElse(element, Sink.Dropped)
    if (assigned(element)) read.join(Sink(declared.getOrElse(element, 0), read.returned, read.stored)) else read
  }

  private def target(tree: ExpressionTree): Sink = Lint.unwrapped(tree) match {
    case identifier: IdentifierTree if declared.contains(elementOf(identifier)) => held(elementOf(identifier))
    case _                                                                      => Sink.Stored
  }

  private def declare(variable: VariableTree, depth: Int): Element = {
    val element = elementOf(variable)
    declared(element) = depth
    element
  }

  private def children(tree: Tree, sink: Sink, depth: Int): Unit =
    tree.accept(
      new TreeScanner[Unit, Unit] {
        override def scan(node: Tree, unused: Unit): Unit = if (node != null) flow(node, sink, depth)
      },
      ()
    )

  private def flows(trees: java.lang.Iterable[? <: Tree], sink: Sink, depth: Int): Unit =
    trees.asScala.foreach(flow(_, sink, depth))

  private def flow(tree: Tree, sink: Sink, depth: Int): Unit = {
    sinks(tree) = sink
    tree match {
      case block: BlockTree       => flows(block.getStatements, Sink.Dropped, depth)
      case variable: VariableTree =>
        val element = declare(variable, depth)
        Option(variable.getInitializer).foreach(flow(_, held(element), depth))
      case _: ClassTree                       => ()
      case statement: ExpressionStatementTree => flow(statement.getExpression, Sink.Dropped, depth)
      case branch: IfTree                     =>
        flow(branch.getCondition, Sink.Dropped, depth)
        flow(branch.getThenStatement, Sink.Dropped, depth)
        Option(branch.getElseStatement).foreach(flow(_, Sink.Dropped, depth))
      case loop: (WhileLoopTree | DoWhileLoopTree) => children(loop, Sink.Dropped, depth + 1)
      case loop: ForLoopTree                       =>
        flows(loop.getInitializer, Sink.Dropped, depth)
        Option(loop.getCondition).foreach(flow(_, Sink.Dropped, depth + 1))
        flows(loop.getUpdate, Sink.Dropped, depth + 1)
        flow(loop.getStatement, Sink.Dropped, depth + 1)
      case loop: EnhancedForLoopTree =>
        val element = declare(loop.getVariable, depth + 1)
        flow(loop.getExpression, carried(loop.getVariable, held(element)), depth)
        flow(loop.getStatement, Sink.Dropped, depth + 1)
      case attempt: TryTree =>
        flows(attempt.getResources, Sink.Dropped, depth)
        flow(attempt.getBlock, Sink.Dropped, depth)
        attempt.getCatches.forEach { branch =>
          declare(branch.getParameter, depth): Unit
          flow(branch.getBlock, Sink.Dropped, depth)
        }
        Option(attempt.getFinallyBlock).foreach(flow(_, Sink.Dropped, depth))
      case choice: SwitchTree           => switched(choice.getExpression, choice.getCases, Sink.Dropped, depth)
      case choice: SwitchExpressionTree => switched(choice.getExpression, choice.getCases, sink, depth)
      case leave: ReturnTree            => Option(leave.getExpression).foreach(flow(_, returns, depth))
      case leave: YieldTree             => flow(leave.getValue, yields, depth)
      case leave: ThrowTree             => flow(leave.getExpression, Sink.Returned, depth)
      case lock: SynchronizedTree       =>
        flow(lock.getExpression, Sink.Dropped, depth)
        flow(lock.getBlock, Sink.Dropped, depth)
      case labelled: LabeledStatementTree => flow(labelled.getStatement, Sink.Dropped, depth)
      case assertion: AssertTree          => children(assertion, Sink.Dropped, depth)
      case assignment: AssignmentTree     => stored(assignment, assignment.getVariable, assignment.getExpression, depth)
      case assignment: CompoundAssignmentTree =>
        stored(assignment, assignment.getVariable, assignment.getExpression, depth)
      case unary: UnaryTree   => flow(unary.getExpression, Sink.Dropped, depth)
      case binary: BinaryTree =>
        flow(binary.getLeftOperand, carried(binary, sink), depth)
        flow(binary.getRightOperand, carried(binary, sink), depth)
      case wrapped: ParenthesizedTree => flow(wrapped.getExpression, sink, depth)
      case cast: TypeCastTree         => flow(cast.getExpression, carried(cast, sink), depth)
      case test: InstanceOfTree       =>
        flow(test.getExpression, if (test.getPattern != null) Sink.Returned else Sink.Dropped, depth)
      case choice: ConditionalExpressionTree =>
        flow(choice.getCondition, Sink.Dropped, depth)
        flow(choice.getTrueExpression, sink, depth)
        flow(choice.getFalseExpression, sink, depth)
      case identifier: IdentifierTree => use(elementOf(identifier), sink)
      case select: MemberSelectTree   => flow(select.getExpression, carried(select, sink), depth)
      case access: ArrayAccessTree    =>
        flow(access.getExpression, carried(access, sink), depth)
        flow(access.getIndex, Sink.Dropped, depth)
      case array: NewArrayTree =>
        flows(array.getDimensions, Sink.Dropped, depth)
        Option(array.getInitializers).foreach(flows(_, sink, depth))
      case function: LambdaExpressionTree => lambda(function, sink, depth)
      case reference: MemberReferenceTree => flow(reference.getQualifierExpression, sink, depth)
      case call: MethodInvocationTree     => applied(call, sink, depth)
      case creation: NewClassTree         => created(creation, sink, depth)
      case _                              => children(tree, Sink.Returned, depth)
    }
  }

  private def stored(tree: ExpressionTree, lhs: ExpressionTree, rhs: ExpressionTree, depth: Int): Unit = {
    val into = target(lhs)
    sinks(tree) = into
    flow(rhs, carried(tree, into), depth)
    Lint.unwrapped(lhs) match {
      case select: MemberSelectTree => flow(select.getExpression, Sink.Dropped, depth)
      case access: ArrayAccessTree  =>
        flow(access.getExpression, Sink.Dropped, depth)
        flow(access.getIndex, Sink.Dropped, depth)
      case _ =>
    }
  }

  private def switched(selector: ExpressionTree, cases: java.util.List[? <: CaseTree], sink: Sink, depth: Int): Unit = {
    val saved    = yields
    val matching = cases.asScala.exists(_.getLabels.asScala.exists(_.isInstanceOf[PatternCaseLabelTree]))
    yields = sink
    try {
      flow(selector, if (matching) Sink.Returned else Sink.Dropped, depth)
      cases.forEach { branch =>
        Option(branch.getGuard).foreach(flow(_, Sink.Dropped, depth))
        branch.getBody match {
          case body: ExpressionTree => flow(body, sink, depth)
          case null                 => Option(branch.getStatements).foreach(flows(_, Sink.Dropped, depth))
          case body                 => flow(body, Sink.Dropped, depth)
        }
      }
    } finally yields = saved
  }

  private def lambda(function: LambdaExpressionTree, sink: Sink, depth: Int): Unit = {
    val saved = returns
    returns = sink
    try
      function.getBody match {
        case body: ExpressionTree => flow(body, sink, depth)
        case body                 => flow(body, Sink.Dropped, depth)
      }
    finally returns = saved
  }

  private def passed(arguments: Vector[ExpressionTree], into: Sink, function: Sink, inner: Int, depth: Int): Unit =
    arguments.foreach(argument =>
      Lint.unwrapped(argument) match {
        case body: LambdaExpressionTree => lambda(body, function, inner)
        case _                          => flow(argument, into, depth)
      }
    )

  private def created(creation: NewClassTree, sink: Sink, depth: Int): Unit = {
    val result = carried(creation, sink)
    Option(creation.getEnclosingExpression).foreach(flow(_, result, depth))
    passed(Lints.arguments(creation), result, result, depth, depth)
  }

  private def applied(call: MethodInvocationTree, sink: Sink, depth: Int): Unit = {
    val result    = carried(call, sink)
    val selected  = Call.selected(call.getMethodSelect)
    val receiver  = selected.flatMap(_._1)
    val name      = selected.fold("")(_._2)
    val arguments = Lints.arguments(call)
    val kind      = receiver.flatMap(typeOf(_))
    val holder    = kind.exists(CostTrees.collection)
    val mutating  = Methods.Mutators(name) && holder
    val traversal = kind.exists(CostTrees.derives(_, "java.util.stream.BaseStream")) ||
      (CostTrees.Visits(name) && holder)
    val kept = if (mutating) receiver.map(target(_)) else None
    kept.foreach(into => sinks(call) = sink.join(into))
    receiver.foreach(flow(_, result, depth))
    val collected         = if (Methods.Accumulating(name)) result else Sink.Dropped
    val (function, inner) = if (traversal) (collected, depth + 1) else (result, depth)
    passed(arguments, kept.getOrElse(result), function, inner, depth)
  }
}
