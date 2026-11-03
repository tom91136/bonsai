package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.TreePathScanner
import com.sun.source.util.TreeScanner
import com.sun.source.util.Trees
import javax.lang.model.element.*
import net.kurobako.bonsai.java.Lints.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Reads the fields that only the constructors of their class assign, which can be final though they start with no
  * value.
  *
  * A constructor assigns a field once when every path through it ends with the field assigned by one plain assignment,
  * or when it hands its work to a constructor of the class that does and assigns nothing after. The walk counts the
  * assignments over the statements the language lets a constructor assign in: blocks, conditionals, switches and switch
  * expressions, `try`, `synchronized`, loops, and early exits. Conditions are followed as the definite assignment rules
  * of the language do, with a count for when they hold and one for when they do not, through `&&`, `||`, `!` and `?:`,
  * and a branch a constant rules out counts for nothing. A jump out of a `try` with a `finally` block takes what that
  * block does on the way. Any other statement must leave the field alone, and a read of the field before the assignment
  * makes it unfit, as the compiler would refuse a final field read there.
  */
private[java] object Constructors {

  private val Steps = Set(
    Tree.Kind.PREFIX_INCREMENT,
    Tree.Kind.PREFIX_DECREMENT,
    Tree.Kind.POSTFIX_INCREMENT,
    Tree.Kind.POSTFIX_DECREMENT
  )

  /** How many times a path has assigned the field: none, once, or more than once. */
  private type Counts = Set[Int]

  private val Once: Counts = Set(1)
  private val Bad: Counts  = Set(2)

  /** What the jumps pending at a point of the walk carry: for `return`, `break`, `continue` and `yield`, the counts at
    * each, the innermost target first, and for the labelled ones, by label.
    */
  private case class Exits(
      returned: Counts,
      breaks: List[Counts],
      continues: List[Counts],
      yields: List[Counts],
      labelled: Map[String, Counts],
      repeating: Map[String, Counts]
  ) {

    def cleared: Exits = map(_ => Set.empty)

    def map(f: Counts => Counts): Exits =
      Exits(
        f(returned),
        breaks.map(f),
        continues.map(f),
        yields.map(f),
        labelled.view.mapValues(f).toMap,
        repeating.view.mapValues(f).toMap
      )

    def ++(that: Exits): Exits =
      Exits(
        returned ++ that.returned,
        breaks.zip(that.breaks).map(_ ++ _),
        continues.zip(that.continues).map(_ ++ _),
        yields.zip(that.yields).map(_ ++ _),
        Exits.merge(labelled, that.labelled),
        Exits.merge(repeating, that.repeating)
      )

    def all: Counts =
      (returned :: breaks ::: continues ::: yields ::: labelled.values.toList :::
        repeating.values.toList).flatten.toSet
  }

  private object Exits {
    val none: Exits = Exits(Set.empty, Nil, Nil, Nil, Map.empty, Map.empty)

    def merge(left: Map[String, Counts], right: Map[String, Counts]): Map[String, Counts] =
      right.foldLeft(left)((merged, entry) =>
        merged.updated(entry._1, merged.getOrElse(entry._1, Set.empty) ++ entry._2)
      )
  }

  /** Whether the declaration at the path is of an instance field with no initialiser that every constructor of its
    * class assigns once, which the compiler would accept as final if nothing else wrote it.
    */
  def assigned(trees: Trees, path: TreePath): Boolean =
    (path.getLeaf, trees.getElement(path), path.getParentPath) match {
      case (variable: VariableTree, field: VariableElement, parent)
          if parent != null && variable.getInitializer == null && field.getKind == ElementKind.FIELD &&
            Set(Modifier.STATIC, Modifier.FINAL, Modifier.VOLATILE).forall(!field.getModifiers.contains(_)) =>
        (parent.getLeaf, trees.getElement(parent)) match {
          case (declaration: ClassTree, owner: TypeElement)
              if owner.getKind == ElementKind.CLASS || owner.getKind == ElementKind.ENUM =>
            val constructors = declaration.getMembers.asScala.toVector.flatMap {
              case method: MethodTree =>
                val member = child(parent, method)
                trees.getElement(member) match {
                  case constructor: ExecutableElement if constructor.getKind == ElementKind.CONSTRUCTOR =>
                    Some(constructor -> member)
                  case _ => None
                }
              case _ => None
            }
            constructors.nonEmpty && Assignments(trees, field, constructors.toMap).everywhere
          case _ => false
        }
      case _ => false
    }

  /** Whether the target of an assignment names the field of the enclosing instance, which a constructor may assign: a
    * bare name, or the name after `this`.
    */
  def direct(target: ExpressionTree): Boolean = target match {
    case _: IdentifierTree        => true
    case select: MemberSelectTree => Call.isThis(select.getExpression)
    case _                        => false
  }

  /** Whether the code at the path is in a constructor of the owner, and not in a lambda or class inside it. */
  def within(trees: Trees, path: TreePath, owner: Element): Boolean =
    Iterator.iterate(path.getParentPath)(_.getParentPath).takeWhile(_ != null).collectFirst {
      case enclosing if enclosing.getLeaf.isInstanceOf[MethodTree | LambdaExpressionTree | ClassTree] => enclosing
    }.exists { enclosing =>
      enclosing.getLeaf.isInstanceOf[MethodTree] &&
      (trees.getElement(enclosing) match {
        case constructor: ExecutableElement =>
          constructor.getKind == ElementKind.CONSTRUCTOR && constructor.getEnclosingElement == owner
        case _ => false
      })
    }

  /** Whether any code under the root writes the field other than by plain assignments in constructors of its class. */
  def outside(trees: Trees, root: TreePath, field: Element): Boolean =
    Effects.writes(root).exists { write =>
      trees.getElement(child(write.at, write.target)) == field &&
      !(write.plain && direct(write.target) && within(trees, write.at, field.getEnclosingElement))
    }

  private class Assignments(trees: Trees, field: Element, constructors: Map[ExecutableElement, TreePath]) {

    private val settled = mutable.Map.empty[ExecutableElement, Boolean]
    private val running = mutable.Set.empty[ExecutableElement]

    def everywhere: Boolean = constructors.keys.forall(assigns(_))

    private def assigns(constructor: ExecutableElement): Boolean =
      settled.get(constructor).getOrElse {
        val found = !running(constructor) && {
          running += constructor
          try Walk(constructors(constructor)).once
          finally running -= constructor
        }
        settled.update(constructor, found)
        found
      }

    private def names(path: TreePath): Boolean = trees.getElement(path) == field

    private def shift(counts: Counts): Counts =
      if (counts.isEmpty) Once else counts.map(count => (count + 1).min(2))

    /** Whether the code reads or writes the field. */
    private def touches(path: TreePath): (Boolean, Boolean) = {
      var read = false
      new TreePathScanner[Unit, Unit] {
        override def visitIdentifier(node: IdentifierTree, unused: Unit): Unit = {
          if (names(getCurrentPath)) read = true
          super.visitIdentifier(node, unused)
        }

        override def visitMemberSelect(node: MemberSelectTree, unused: Unit): Unit = {
          if (names(getCurrentPath)) read = true
          super.visitMemberSelect(node, unused)
        }
      }.scan(path, ())
      (read, Effects.writes(path).exists(write => names(child(write.at, write.target))))
    }

    private class Walk(constructor: TreePath) {

      private var exits  = Exits.none
      private var failed = false

      private def bad: Counts = {
        failed = true
        Bad
      }

      private def bump(counts: Counts): Counts = {
        val assigned = shift(counts)
        failed ||= assigned.contains(2)
        assigned
      }

      def once: Boolean = {
        val body       = constructor.getLeaf.asInstanceOf[MethodTree].getBody
        val bodyPath   = child(constructor, body)
        val statements = body.getStatements.asScala.toList
        val start      = statements.headOption.flatMap(delegation(bodyPath, _)) match {
          case Some(target) => Option.when(constructors.contains(target) && assigns(target))((Once, statements.tail))
          case None         => Some((Set(0), statements))
        }
        start.exists { (counts, rest) =>
          val ends = block(bodyPath, rest, counts) ++ exits.returned
          !failed && ends == Once
        }
      }

      private def delegation(body: TreePath, first: StatementTree): Option[ExecutableElement] = first match {
        case statement: ExpressionStatementTree =>
          statement.getExpression match {
            case call: MethodInvocationTree if Call.isThis(call.getMethodSelect) =>
              trees.getElement(child(child(body, statement), call)) match {
                case target: ExecutableElement => Some(target)
                case _                         => None
              }
            case _ => None
          }
        case _ => None
      }

      private def returning(counts: Counts): Unit = exits = exits.copy(returned = exits.returned ++ counts)

      private def breaking(counts: Counts): Unit = exits.breaks match {
        case head :: tail => exits = exits.copy(breaks = (head ++ counts) :: tail)
        case Nil          => returning(bad)
      }

      private def continuing(counts: Counts): Unit = exits.continues match {
        case head :: tail => exits = exits.copy(continues = (head ++ counts) :: tail)
        case Nil          => returning(bad)
      }

      private def yielding(counts: Counts): Unit = exits.yields match {
        case head :: tail => exits = exits.copy(yields = (head ++ counts) :: tail)
        case Nil          => ()
      }

      private def leaving(label: String, counts: Counts): Unit =
        exits = exits.copy(labelled = Exits.merge(exits.labelled, Map(label -> counts)))

      private def repeating(label: String, counts: Counts): Unit =
        exits = exits.copy(repeating = Exits.merge(exits.repeating, Map(label -> counts)))

      private def block(parent: TreePath, statements: List[? <: StatementTree], counts: Counts): Counts =
        statements.foldLeft(counts)((current, item) => statement(child(parent, item), current))

      /** The expression leaves the field alone, and reads it only once it is assigned. Where no path reaches it, a
        * write is allowed but still leaves the field assigned, as the compiler counts it.
        */
      private def expression(path: TreePath, counts: Counts): Counts = {
        val (read, wrote) = touches(path)
        if (counts.isEmpty) if (wrote) Once else counts
        else if (!wrote && (!read || counts == Once)) counts
        else bad
      }

      private def plain(path: TreePath, tree: ExpressionTree, counts: Counts): Counts = tree match {
        case assign: AssignmentTree if direct(assign.getVariable) && names(child(path, assign.getVariable)) =>
          bump(valued(child(path, assign.getExpression), assign.getExpression, counts))
        case assign: AssignmentTree if touches(child(path, assign.getVariable)) == (false, false) =>
          valued(child(path, assign.getExpression), assign.getExpression, counts)
        case _ => expression(path, counts)
      }

      private def branching(tree: ExpressionTree): Boolean = tree match {
        case _: ConditionalExpressionTree => true
        case unary: UnaryTree             => unary.getKind == Tree.Kind.LOGICAL_COMPLEMENT
        case binary: BinaryTree           => binary.getKind == Tree.Kind.CONDITIONAL_AND ||
          binary.getKind == Tree.Kind.CONDITIONAL_OR
        case _ => false
      }

      /** The expression, in which operands, conditional arms and switch expression arms may assign the field. */
      private def valued(path: TreePath, tree: ExpressionTree, counts: Counts): Counts = tree match {
        case wrapped: ParenthesizedTree   => valued(child(path, wrapped.getExpression), wrapped.getExpression, counts)
        case choice: SwitchExpressionTree => selection(path, choice, counts)
        case _ if branching(tree)         =>
          val (whenTrue, whenFalse) = test(path, counts)
          whenTrue ++ whenFalse
        case binary: BinaryTree =>
          List(binary.getLeftOperand, binary.getRightOperand).foldLeft(counts)((current, operand) =>
            valued(child(path, operand), operand, current)
          )
        case unary: UnaryTree if !Steps(unary.getKind) =>
          valued(child(path, unary.getExpression), unary.getExpression, counts)
        case cast: TypeCastTree => valued(child(path, cast.getExpression), cast.getExpression, counts)
        case _                  => plain(path, tree, counts)
      }

      /** The counts of the boolean expression when it is true and when it is false, an empty set where a constant rules
        * the outcome out.
        */
      private def test(path: TreePath, counts: Counts): (Counts, Counts) = constant(path) match {
        case Some(true)  => (counts, Set.empty)
        case Some(false) => (Set.empty, counts)
        case _           =>
          path.getLeaf match {
            case tree: ParenthesizedTree => test(child(path, tree.getExpression), counts)
            case tree: UnaryTree if tree.getKind == Tree.Kind.LOGICAL_COMPLEMENT =>
              test(child(path, tree.getExpression), counts).swap
            case tree: BinaryTree if tree.getKind == Tree.Kind.CONDITIONAL_AND =>
              val (leftTrue, leftFalse)   = test(child(path, tree.getLeftOperand), counts)
              val (rightTrue, rightFalse) = test(child(path, tree.getRightOperand), leftTrue)
              (rightTrue, leftFalse ++ rightFalse)
            case tree: BinaryTree if tree.getKind == Tree.Kind.CONDITIONAL_OR =>
              val (leftTrue, leftFalse)   = test(child(path, tree.getLeftOperand), counts)
              val (rightTrue, rightFalse) = test(child(path, tree.getRightOperand), leftFalse)
              (leftTrue ++ rightTrue, rightFalse)
            case tree: ConditionalExpressionTree =>
              val (whenTrue, whenFalse) = test(child(path, tree.getCondition), counts)
              val (thenTrue, thenFalse) = test(child(path, tree.getTrueExpression), whenTrue)
              val (elseTrue, elseFalse) = test(child(path, tree.getFalseExpression), whenFalse)
              (thenTrue ++ elseTrue, thenFalse ++ elseFalse)
            case tree: ExpressionTree =>
              val settled = valued(path, tree, counts)
              (settled, settled)
            case _ => (bad, bad)
          }
      }

      private def constant(path: TreePath): Option[Any] = path.getLeaf match {
        case tree: ParenthesizedTree => constant(child(path, tree.getExpression))
        case tree: LiteralTree       => Option(tree.getValue)
        case tree: UnaryTree if tree.getKind == Tree.Kind.LOGICAL_COMPLEMENT =>
          constant(child(path, tree.getExpression)).collect { case value: Boolean => !value }
        case tree: BinaryTree =>
          for {
            left  <- constant(child(path, tree.getLeftOperand))
            right <- constant(child(path, tree.getRightOperand))
            value <- fold(tree.getKind, left, right)
          } yield value
        case _: (IdentifierTree | MemberSelectTree) =>
          trees.getElement(path) match {
            case variable: VariableElement => Option(variable.getConstantValue)
            case _                         => None
          }
        case _ => None
      }

      private def integral(value: Any): Option[Long] = value match {
        case n: Int       => Some(n.toLong)
        case n: Long      => Some(n)
        case n: Short     => Some(n.toLong)
        case n: Byte      => Some(n.toLong)
        case c: Character => Some(c.toLong)
        case _            => None
      }

      private def fold(kind: Tree.Kind, left: Any, right: Any): Option[Any] = (left, right) match {
        case (a: Boolean, b: Boolean) =>
          kind match {
            case Tree.Kind.CONDITIONAL_AND | Tree.Kind.AND => Some(a && b)
            case Tree.Kind.CONDITIONAL_OR | Tree.Kind.OR   => Some(a || b)
            case Tree.Kind.XOR | Tree.Kind.NOT_EQUAL_TO    => Some(a != b)
            case Tree.Kind.EQUAL_TO                        => Some(a == b)
            case _                                         => None
          }
        case _ =>
          for {
            a     <- integral(left)
            b     <- integral(right)
            value <- kind match {
              case Tree.Kind.EQUAL_TO           => Some(a == b)
              case Tree.Kind.NOT_EQUAL_TO       => Some(a != b)
              case Tree.Kind.LESS_THAN          => Some(a < b)
              case Tree.Kind.LESS_THAN_EQUAL    => Some(a <= b)
              case Tree.Kind.GREATER_THAN       => Some(a > b)
              case Tree.Kind.GREATER_THAN_EQUAL => Some(a >= b)
              case _                            => None
            }
          } yield value
      }

      private def statement(path: TreePath, counts: Counts): Counts = path.getLeaf match {
        case tree: BlockTree               => block(path, tree.getStatements.asScala.toList, counts)
        case tree: ExpressionStatementTree => plain(child(path, tree.getExpression), tree.getExpression, counts)
        case tree: VariableTree            =>
          Option(tree.getInitializer).fold(counts)(value => valued(child(path, value), value, counts))
        case tree: IfTree     => conditional(path, tree, counts)
        case tree: ReturnTree =>
          returning(Option(tree.getExpression).fold(counts)(value => expression(child(path, value), counts)))
          Set.empty
        case tree: ThrowTree =>
          val _ = expression(child(path, tree.getExpression), counts)
          Set.empty
        case tree: BreakTree    => leave(tree, counts)
        case tree: ContinueTree => proceed(tree, counts)
        case tree: YieldTree    =>
          yielding(valued(child(path, tree.getValue), tree.getValue, counts))
          Set.empty
        case tree: LabeledStatementTree =>
          val label = tree.getLabel.toString
          val ended = statement(child(path, tree.getStatement), counts)
          val left  = exits.labelled.getOrElse(label, Set.empty)
          exits = exits.copy(labelled = exits.labelled - label)
          ended ++ left
        case tree: SynchronizedTree =>
          statement(child(path, tree.getBlock), expression(child(path, tree.getExpression), counts))
        case tree: SwitchTree      => switched(path, tree, counts)
        case tree: TryTree         => attempted(path, tree, counts)
        case tree: WhileLoopTree   => cycle(path, counts, tree.getStatement, Nil, guard(path, tree.getCondition), pass)
        case tree: DoWhileLoopTree => cycle(path, counts, tree.getStatement, Nil, pass, guard(path, tree.getCondition))
        case tree: ForLoopTree     =>
          val started = block(path, tree.getInitializer.asScala.toList, counts)
          cycle(path, started, tree.getStatement, tree.getUpdate.asScala.toList, guard(path, tree.getCondition), pass)
        case _ => other(path, counts)
      }

      private def conditional(path: TreePath, tree: IfTree, counts: Counts): Counts = {
        val (whenTrue, whenFalse) = test(child(path, tree.getCondition), counts)
        val taken                 = statement(child(path, tree.getThenStatement), whenTrue)
        taken ++ Option(tree.getElseStatement).fold(whenFalse)(other => statement(child(path, other), whenFalse))
      }

      private val pass: Counts => (Counts, Counts) = from => (from, Set.empty)

      /** The counts of a loop condition, which holds when there is none, as the counts that go on and the ones that
        * leave the loop.
        */
      private def guard(path: TreePath, condition: ExpressionTree)(from: Counts): (Counts, Counts) =
        if (condition == null) pass(from) else test(child(path, condition), from)

      /** A loop assigns once only if nothing it assigns reaches the start of the next round, as the compiler refuses a
        * final field assigned in a loop. The condition comes before the body for `while` and `for`, and after it for
        * `do`; a loop with no condition, or a constant one that holds, is left only by a jump.
        */
      private def cycle(
          path: TreePath,
          counts: Counts,
          body: StatementTree,
          updates: List[StatementTree],
          before: Counts => (Counts, Counts),
          after: Counts => (Counts, Counts)
      ): Counts = {
        val label = Option(path.getParentPath).map(_.getLeaf).collect { case tree: LabeledStatementTree =>
          tree.getLabel.toString
        }
        exits = exits.copy(
          breaks = Set.empty[Int] :: exits.breaks,
          continues = Set.empty[Int] :: exits.continues
        )
        val (entry, skipped) = before(counts)
        val ended            = statement(child(path, body), entry)
        val rejoined         = ended ++ exits.continues.head ++ label.flatMap(exits.repeating.get).getOrElse(Set.empty)
        exits = exits.copy(repeating = exits.repeating -- label)
        val (again, finished) = after(block(path, updates, rejoined))
        val broken            = exits.breaks.head
        exits = exits.copy(breaks = exits.breaks.tail, continues = exits.continues.tail)
        skipped ++ finished ++ broken ++ (if (again.subsetOf(counts)) Set.empty[Int] else bad)
      }

      private def proceed(tree: ContinueTree, counts: Counts): Counts = {
        if (tree.getLabel != null) repeating(tree.getLabel.toString, counts) else continuing(counts)
        Set.empty
      }

      private def selection(path: TreePath, tree: SwitchExpressionTree, counts: Counts): Counts = {
        val selected = expression(child(path, tree.getExpression), counts)
        exits = exits.copy(yields = Set.empty[Int] :: exits.yields)
        var through = Set.empty[Int]
        var ended   = Set.empty[Int]
        tree.getCases.asScala.foreach { arm =>
          val armPath = child(path, arm)
          if (arm.getCaseKind == CaseTree.CaseKind.RULE)
            ended ++=
              (arm.getBody match {
                case value: ExpressionTree => valued(child(armPath, value), value, selected)
                case body                  => statement(child(armPath, body), selected)
              })
          else through = block(armPath, arm.getStatements.asScala.toList, selected ++ through)
        }
        val produced = exits.yields.head
        exits = exits.copy(yields = exits.yields.tail)
        ended ++ through ++ produced
      }

      private def leave(tree: BreakTree, counts: Counts): Counts = {
        if (tree.getLabel != null) leaving(tree.getLabel.toString, counts) else breaking(counts)
        Set.empty
      }

      private def switched(path: TreePath, tree: SwitchTree, counts: Counts): Counts = {
        val selected = expression(child(path, tree.getExpression), counts)
        exits = exits.copy(breaks = Set.empty[Int] :: exits.breaks)
        var through = Set.empty[Int]
        var ended   = Set.empty[Int]
        var covered = false
        tree.getCases.asScala.foreach { arm =>
          val armPath = child(path, arm)
          covered ||= arm.getLabels.asScala.exists(_.isInstanceOf[DefaultCaseLabelTree])
          if (arm.getCaseKind == CaseTree.CaseKind.RULE)
            ended ++=
              (arm.getBody match {
                case value: ExpressionTree => plain(child(armPath, value), value, selected)
                case body                  => statement(child(armPath, body), selected)
              })
          else through = block(armPath, arm.getStatements.asScala.toList, selected ++ through)
        }
        val broken = exits.breaks.head
        exits = exits.copy(breaks = exits.breaks.tail)
        ended ++ through ++ broken ++ (if (covered) Set.empty[Int] else selected)
      }

      /** The jumps out of the try and catch blocks run the `finally` block on the way, so they carry what it does to
        * the count as well.
        */
      private def attempted(path: TreePath, tree: TryTree, counts: Counts): Counts = {
        val opened = tree.getResources.asScala.foldLeft(counts)((current, resource) =>
          expression(child(path, resource), current)
        )
        val outer = exits
        exits = exits.cleared
        val block    = child(path, tree.getBlock)
        val body     = statement(block, opened)
        val entered  = opened ++ body ++ midway(block, opened)
        val handlers = tree.getCatches.asScala.map { handler =>
          val caught = child(child(path, handler), handler.getBlock)
          (statement(caught, entered), midway(caught, entered))
        }
        val finished = handlers.foldLeft(body)(_ ++ _._1)
        val pending  = exits
        exits = outer
        Option(tree.getFinallyBlock) match {
          case None =>
            exits = exits ++ pending
            finished
          case Some(last) =>
            val closing = child(path, last)
            val entry   = handlers.foldLeft(entered ++ finished)(_ ++ _._2) ++ pending.all
            val after   = statement(closing, entry)
            val through =
              if (touches(closing)._2) Some(if (entry.subsetOf(Set(0))) after else bad)
              else if (after == entry) None
              else if (after.isEmpty) Some(after)
              else Some(bad)
            exits = exits ++ through.fold(pending)(settled =>
              pending.map(jumped => if (jumped.isEmpty) jumped else settled)
            )
            through.getOrElse(finished)
        }
      }

      /** What a path through the code can have counted at a point where it stops short, which is one more than at its
        * start when the code assigns the field.
        */
      private def midway(path: TreePath, start: Counts): Counts =
        if (touches(path)._2) shift(start) else Set.empty

      /** A statement the walk does not follow: it leaves the field alone, and the exits it holds are taken as they are.
        */
      private def other(path: TreePath, counts: Counts): Counts = {
        val kept = expression(path, counts)
        new TreeScanner[Unit, Unit] {
          override def visitReturn(node: ReturnTree, unused: Unit): Unit     = returning(kept)
          override def visitYield(node: YieldTree, unused: Unit): Unit       = yielding(kept)
          override def visitContinue(node: ContinueTree, unused: Unit): Unit =
            if (node.getLabel != null) repeating(node.getLabel.toString, kept)
          override def visitLambdaExpression(node: LambdaExpressionTree, unused: Unit): Unit = ()
          override def visitClass(node: ClassTree, unused: Unit): Unit                       = ()
          override def visitBreak(node: BreakTree, unused: Unit): Unit                       =
            if (node.getLabel != null) leaving(node.getLabel.toString, kept)
        }.scan(path.getLeaf, ())
        kept
      }
    }
  }
}
