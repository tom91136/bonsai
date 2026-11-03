package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.tree.Tree.Kind
import com.sun.source.util.TreePath
import com.sun.source.util.TreePathScanner
import javax.lang.model.`type`.ArrayType
import javax.lang.model.`type`.DeclaredType
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.Element
import net.kurobako.bonsai.java.Javac.*
import scala.jdk.CollectionConverters.*

/** The lints of the loops of the Java code whose work a stream, a single loop, or the code before the loop states
  * directly.
  *
  * The lints are of the code of one file, as the trees the compiler gives are of one compilation; the rules that need
  * the evidence of the whole build are of the analysis, which holds the model of the build.
  */
object LintLoops {

  /** The lints of the loops. */
  val lints: Vector[Lint] =
    Vector(LoopToMatch, ConsecutiveIdenticalLoops, TemporaryListIteratedOnce, LoopInvariantCondition)

  /** A loop that returns on the first element that matches and a literal otherwise, which a stream match states. */
  private object LoopToMatch extends Lint {
    val id               = LintId.LoopToMatch.id
    val kinds: Set[Kind] = Set(Tree.Kind.ENHANCED_FOR_LOOP, Tree.Kind.FOR_LOOP)

    def check(context: Context, tree: Tree): Vector[Found] =
      (for {
        following <- context.sibling(1).flatMap(Lint.booleanReturn)
        guard     <- body(tree).map(single(_)).collect { case branch: IfTree if branch.elseStatement.isEmpty => branch }
        exit      <- Lint.booleanReturn(guard.getThenStatement)
        condition = Lints.unparenthesised(guard.getCondition)
        if following == !exit && matchable(context, tree, condition)
      } yield {
        val negated = condition match {
          case unary: UnaryTree => unary.getKind == Tree.Kind.LOGICAL_COMPLEMENT
          case _                => false
        }
        val method = if (exit) "anyMatch" else if (negated) "allMatch" else "noneMatch"
        context.found(s"use $method instead of the loop that returns on the first match")
      }).toVector

    /** Whether the condition can become the predicate of a stream match: it holds in a lambda over the element. */
    private def matchable(context: Context, loop: Tree, condition: ExpressionTree): Boolean = {
      val element = loop match {
        case each: EnhancedForLoopTree =>
          Option.when(streamable(context, context.typeOf(each.getExpression)))(context.elementOf(each.getVariable))
        case plain: ForLoopTree => indexed(context, plain, condition)
        case _                  => None
      }
      element.exists { variable =>
        val method = context.enclosing(Set(Tree.Kind.METHOD))
        !Effects.writes(TreePath(context.path, condition)).nonEmpty && !Effects.throwsChecked(context, condition) &&
        (Effects.locals(context, condition) - variable).forall(captured =>
          method.exists(scope => !Effects.assigns(context, TreePath(context.path, scope), captured))
        )
      }
    }

    private def streamable(context: Context, tpe: TypeMirror): Boolean = tpe match {
      case array: ArrayType => !array.getComponentType.getKind.isPrimitive
      case _                => derives(context, tpe, "java.util.Collection")
    }

    /** The index of a loop that walks a list from some start to its size, when the condition reads the list by it. */
    private def indexed(context: Context, plain: ForLoopTree, condition: ExpressionTree): Option[Element] =
      for {
        index <- plain.getInitializer.asScala.toVector match {
          case Vector(variable: VariableTree) if variable.initializer.isDefined => Some(variable)
          case _                                                                => None
        }
        bound <- plain.condition.map(Lints.unparenthesised).collect {
          case binary: BinaryTree if binary.getKind == Tree.Kind.LESS_THAN => binary
        }
        if named(bound.getLeftOperand, index.getName.toString)
        list <- sized(bound.getRightOperand)
        if plain.getUpdate.asScala.toVector match {
          case Vector(step) => stepped(step.getExpression, index.getName.toString)
          case _            => false
        }
        if derives(context, context.typeOf(list), "java.util.List")
        if readsBy(condition, index.getName.toString, list.toString)
      } yield context.elementOf(index)

    private def named(expression: ExpressionTree, name: String): Boolean = Lints.unparenthesised(expression) match {
      case identifier: IdentifierTree => identifier.getName.contentEquals(name)
      case _                          => false
    }

    private def sized(expression: ExpressionTree): Option[ExpressionTree] = expression match {
      case call: MethodInvocationTree if call.getArguments.isEmpty =>
        call.getMethodSelect match {
          case select: MemberSelectTree if select.getIdentifier.contentEquals("size") => Some(select.getExpression)
          case _                                                                      => None
        }
      case _ => None
    }

    private def stepped(expression: ExpressionTree, name: String): Boolean = expression match {
      case unary: UnaryTree =>
        (unary.getKind == Tree.Kind.POSTFIX_INCREMENT || unary.getKind == Tree.Kind.PREFIX_INCREMENT) &&
        named(unary.getExpression, name)
      case _ => false
    }

    /** Whether the condition uses the index only to read the list. */
    private def readsBy(condition: ExpressionTree, index: String, list: String): Boolean = {
      def read(node: Tree): Boolean = node match {
        case call: MethodInvocationTree =>
          call.getMethodSelect match {
            case select: MemberSelectTree =>
              select.getIdentifier.contentEquals("get") && select.getExpression.toString == list &&
              call.getArguments.size == 1 && named(call.getArguments.get(0), index)
            case _ => false
          }
        case _ => false
      }
      val reads = Scan.exists(condition, !read(_)) { case node => read(node) }
      val other = Scan.exists(condition, !read(_)) { case node: IdentifierTree => node.getName.contentEquals(index) }
      reads && !other
    }
  }

  /** A loop that has the same body as the loop before it, which one loop over both sources runs. */
  private object ConsecutiveIdenticalLoops extends Lint {
    val id               = LintId.ConsecutiveIdenticalLoops.id
    val kinds: Set[Kind] = Set(Tree.Kind.ENHANCED_FOR_LOOP)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val current = tree.asInstanceOf[EnhancedForLoopTree]
      context.sibling(-1).collect {
        case previous: EnhancedForLoopTree if merges(context, previous, current) =>
          context.found("the loop has the same body as the loop before it; iterate over both sources in one loop")
      }.toVector
    }

    private def merges(context: Context, previous: EnhancedForLoopTree, current: EnhancedForLoopTree): Boolean =
      previous.getVariable.getType.toString == current.getVariable.getType.toString &&
        previous.getExpression.toString != current.getExpression.toString &&
        !current.getStatement.isInstanceOf[EmptyStatementTree] &&
        normalised(previous) == normalised(current) &&
        lacks(current.getStatement, Set(Tree.Kind.BREAK)) &&
        !Effects.changes(context, previous, Effects.locals(context, current.getExpression))

    /** The body of the loop with the name of its variable replaced, so that loops that differ only by it are equal. */
    private def normalised(loop: EnhancedForLoopTree): String =
      loop.getStatement.toString.replaceAll(
        "\\b" + java.util.regex.Pattern.quote(loop.getVariable.getName.toString) + "\\b",
        "#variable#"
      )
  }

  /** A list that one loop fills and the next loop iterates once, which the first loop can use directly. */
  private object TemporaryListIteratedOnce extends Lint {
    val id               = LintId.TemporaryListIteratedOnce.id
    val kinds: Set[Kind] = Set(Tree.Kind.VARIABLE)

    private val lists = Set("java.util.ArrayList", "java.util.LinkedList")

    def check(context: Context, tree: Tree): Vector[Found] = {
      val declaration = tree.asInstanceOf[VariableTree]
      val found       = for {
        (statements, index) <- context.siblings
        created             <- declaration.initializer.collect {
          case created: NewClassTree if created.getArguments.isEmpty && created.classBody.isEmpty => created
        }
        if context.typeOf(created) match {
          case declared: DeclaredType => lists(declared.asElement.toString)
          case _                      => false
        }
        list  = context.element
        later = statements.drop(index + 1)
        users = later.zipWithIndex.filter((statement, _) => uses(context, statement, list).nonEmpty)
        (fill, read, between) <- users match {
          case Vector((fill: EnhancedForLoopTree, first), (read: EnhancedForLoopTree, second)) =>
            Some((fill, read, later.slice(first + 1, second)))
          case _ => None
        }
        if fills(context, fill, list) && reads(context, read, list) && independent(context, fill, read, between, list)
      } yield context.found(
        s"${declaration.getName} is filled by one loop and iterated by the next only once; " +
          "handle the elements where they are added"
      )
      found.toVector
    }

    // A javac visitor is an abstract class, so it is written inline.
    @SuppressWarnings(Array("scalafix:BannedSyntax.anonymous-allocation"))
    private def uses(context: Context, tree: Tree, element: Element): Vector[TreePath] = {
      val paths = Vector.newBuilder[TreePath]
      new TreePathScanner[Unit, Unit] {
        override def visitIdentifier(node: IdentifierTree, unused: Unit): Unit =
          if (context.trees.getElement(getCurrentPath) == element) paths += getCurrentPath
      }.scan(TreePath(context.path, tree), ())
      paths.result()
    }

    /** Whether the loop only adds to the list, and never leaves the method before the loop after it runs. */
    private def fills(context: Context, loop: EnhancedForLoopTree, list: Element): Boolean =
      uses(context, loop, list).forall { path =>
        path.getParentPath.getLeaf match {
          case select: MemberSelectTree if select.getIdentifier.contentEquals("add") =>
            path.getParentPath.getParentPath.getLeaf match {
              case call: MethodInvocationTree => (call.getMethodSelect eq select) && call.getArguments.size == 1
              case _                          => false
            }
          case _ => false
        }
      } && lacks(loop.getStatement, Set(Tree.Kind.RETURN))

    /** Whether the loop iterates the list and does nothing else with it, and does not leave the loop early. */
    private def reads(context: Context, loop: EnhancedForLoopTree, list: Element): Boolean = {
      val paths = uses(context, loop, list)
      paths.size == 1 && (paths.head.getLeaf eq loop.getExpression) &&
      lacks(loop.getStatement, Set(Tree.Kind.RETURN, Tree.Kind.BREAK))
    }

    /** Whether the two loops can run in one: neither changes what the other, or the code between them, reads. */
    private def independent(
        context: Context,
        fill: EnhancedForLoopTree,
        read: EnhancedForLoopTree,
        between: Vector[StatementTree],
        list: Element
    ): Boolean = {
      def outside(tree: Tree): Set[Element] =
        Effects.locals(context, tree) -- Effects.declared(context, tree) - list
      val fields =
        Effects.variables(context, fill).exists(variable => !Effects.local(variable) && !Lints.constant(variable))
      val shared = outside(fill).intersect(outside(read))
      !fields && between.forall(_.isInstanceOf[VariableTree]) &&
      !shared.exists(variable =>
        Effects.changes(context, fill, Set(variable)) || Effects.changes(context, read, Set(variable))
      ) &&
      between.forall(statement => !Effects.changes(context, fill, outside(statement)))
    }

  }

  /** A condition that guards the whole body of a loop and that nothing the loop does changes, which can be tested once
    * outside the loop.
    */
  private object LoopInvariantCondition extends Lint {
    val id               = LintId.LoopInvariantCondition.id
    val kinds: Set[Kind] = Set(Tree.Kind.IF)

    private val boundaries = Set(
      Tree.Kind.LAMBDA_EXPRESSION,
      Tree.Kind.METHOD,
      Tree.Kind.CLASS,
      Tree.Kind.INTERFACE,
      Tree.Kind.ENUM,
      Tree.Kind.RECORD,
      Tree.Kind.ANNOTATION_TYPE
    )

    def check(context: Context, tree: Tree): Vector[Found] = {
      val guard     = tree.asInstanceOf[IfTree]
      val condition = guard.getCondition
      (for {
        loop <- context.ancestors.takeWhile(ancestor => !boundaries(ancestor.getKind)).find(a => Lints.loops(a.getKind))
        if guard.elseStatement.isEmpty && body(loop).map(single(_)).exists(_ eq guard)
        reads <- Effects.reads(context, condition)
        if reads.variables.nonEmpty && reads.variables.forall(Effects.local) &&
          !Effects.changes(context, loop, reads.all)
      } yield context.found(
        s"the condition ${Lints.unparenthesised(condition)} does not change across the turns of the loop; " +
          "test it once before the loop"
      )).toVector
    }
  }

  /** Whether the code of the tree holds no statement of the kinds. */
  private def lacks(body: Tree, kinds: Set[Tree.Kind]): Boolean = !Scan.exists(body) {
    case node => kinds(node.getKind)
  }

  private def single(statement: StatementTree): StatementTree = statement match {
    case block: BlockTree if block.getStatements.size == 1 => block.getStatements.get(0)
    case _                                                 => statement
  }

  private def body(loop: Tree): Option[StatementTree] = loop match {
    case each: EnhancedForLoopTree => Some(each.getStatement)
    case plain: ForLoopTree        => Some(plain.getStatement)
    case waiting: WhileLoopTree    => Some(waiting.getStatement)
    case doing: DoWhileLoopTree    => Some(doing.getStatement)
    case _                         => None
  }

  private def derives(context: Context, tpe: TypeMirror, name: String): Boolean = tpe match {
    case declared: DeclaredType =>
      context.elements.typeNamed(name).exists { element =>
        Lints.subtype(context, context.types.erasure(declared), context.types.erasure(element.asType))
      }
    case _ => false
  }
}
