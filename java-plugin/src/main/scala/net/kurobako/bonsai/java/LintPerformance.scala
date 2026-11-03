package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.tree.Tree.Kind
import javax.lang.model.`type`.DeclaredType
import net.kurobako.bonsai.java.Javac.*

/** The lints of the performance of the Java code, which the code of a type holds.
  *
  * The lints are of the code of one file, as the trees the compiler gives are of one compilation; the rules that need
  * the evidence of the whole build are of the analysis, which holds the model of the build.
  */
object LintPerformance {

  /** The lints of the performance. */
  val lints: Vector[Lint] =
    Vector(
      StringConcatenationInLoop,
      CollectionSizeInLoop,
      BoxingInLoop,
      StringBufferInsteadOfBuilder,
      NestedLoopOverSameCollection,
      ListSearchInLoop,
      ListFrontEditInLoop
    )

  /** A text that is built anew on every turn of a loop. */
  private object StringConcatenationInLoop extends Lint {
    val id               = LintId.StringConcatenationInLoop.id
    val kinds: Set[Kind] = Set(Tree.Kind.ASSIGNMENT, Tree.Kind.PLUS_ASSIGNMENT)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val variable = tree match {
        case assignment: AssignmentTree       => Some(assignment.getVariable)
        case compound: CompoundAssignmentTree => Some(compound.getVariable)
        case _                                => None
      }
      variable.fold(Vector.empty[Found]) { variable =>
        val text = Lints.isType(context.typeOf(variable), "java.lang.String")
        // A text that is added to on every turn of a loop is built anew on every turn of it, whether it is added to
        // with a plus of the code or with a plus of the assignment.
        val concatenation = tree match {
          case assignment: AssignmentTree =>
            assignment.getExpression match {
              case binary: BinaryTree => binary.getKind == Tree.Kind.PLUS
              case _                  => false
            }
          case _: CompoundAssignmentTree => true
          case _                         => false
        }
        if (concatenation && text && context.inLoop)
          Vector(context.found("the text is built anew on every turn of the loop"))
        else Vector.empty
      }
    }
  }

  /** The size of a collection that is asked for on every turn of a loop. */
  private object CollectionSizeInLoop extends Lint {
    val id               = LintId.CollectionSizeInLoop.id
    val kinds: Set[Kind] = Set(Tree.Kind.METHOD_INVOCATION)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val invocation = tree.asInstanceOf[MethodInvocationTree]
      val method     = invocation.getMethodSelect
      val sized      = Call.named(method, "size")
      // The collection is the receiver of the call, whose type is the one of the collection.
      val collection = method match {
        case select: MemberSelectTree => LintPerformance.isCollection(context.typeOf(select.getExpression))
        case _                        => false
      }
      if (sized && collection && context.inLoop && !madeInLoop(context, method))
        Vector(context.found("the size of the collection is asked for on every turn of the loop"))
      else Vector.empty
    }
  }

  /** Whether the collection a call is on is a variable the innermost loop declares, so that every turn has its own. */
  private def madeInLoop(context: Context, method: ExpressionTree): Boolean = {
    val receiver = method match {
      case select: MemberSelectTree => Some(select.getExpression)
      case _                        => None
    }
    val positions = context.trees.getSourcePositions
    val made      = for {
      identifier  <- receiver.collect { case identifier: IdentifierTree => identifier }
      loop        <- context.enclosing(Lints.loops)
      element     <- context.namedElement(identifier)
      declaration <- Option(context.trees.getTree(element))
    } yield {
      val start = positions.getStartPosition(context.unit, declaration)
      start >= positions.getStartPosition(context.unit, loop) && start < positions.getEndPosition(context.unit, loop)
    }
    made.getOrElse(false)
  }

  /** Whether the type is a collection, which the lints that care of the size of a collection read. */
  private def isCollection(tpe: javax.lang.model.`type`.TypeMirror): Boolean = tpe match {
    case declared: DeclaredType =>
      Set("Collection", "List", "Set", "Map", "ArrayList", "HashMap", "HashSet", "LinkedList", "TreeMap")
        .contains(declared.asElement().getSimpleName.toString)
    case _ => false
  }

  /** A value that is boxed on every turn of a loop. */
  private object BoxingInLoop extends Lint {
    val id               = LintId.BoxingInLoop.id
    val kinds: Set[Kind] = Set(Tree.Kind.METHOD_INVOCATION)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val invocation = tree.asInstanceOf[MethodInvocationTree]
      val method     = invocation.getMethodSelect
      if (LintRedundant.boxing(context, method) && context.inLoop)
        Vector(context.found("the value is boxed on every turn of the loop"))
      else Vector.empty
    }
  }

  /** A text that is built in a way that another thread may see half of. */
  private object StringBufferInsteadOfBuilder extends Lint {
    val id               = LintId.StringBufferInsteadOfBuilder.id
    val kinds: Set[Kind] = Set(Tree.Kind.NEW_CLASS)

    def check(context: Context, tree: Tree): Vector[Found] = {
      // The text is built in a buffer, which the type of the code names.
      val buffer = context.namedElement
        .map(_.getEnclosingElement)
        .map(_.toString)
        .getOrElse("")
      if (buffer == "java.lang.StringBuffer")
        Vector(context.found("the text is built in a way that another thread may see half of"))
      else Vector.empty
    }
  }

  /** A loop over a collection that a loop around it runs over as well, which takes time quadratic in its size. */
  private object NestedLoopOverSameCollection extends Lint {
    val id               = LintId.NestedLoopOverSameCollection.id
    val kinds: Set[Kind] = Set(Tree.Kind.ENHANCED_FOR_LOOP, Tree.Kind.FOR_LOOP, Tree.Kind.WHILE_LOOP)

    def check(context: Context, tree: Tree): Vector[Found] =
      iterated(tree).filter(collection =>
        context.ancestors.exists(outer => Lints.loops.contains(outer.getKind) && iterated(outer).contains(collection))
      ).map(collection =>
        context.found(s"the loop runs over $collection inside another loop over it, which takes quadratic time")
      ).toVector

    /** The collection a loop runs over, when it names one by a variable or a field. */
    private def iterated(loop: Tree): Option[String] = loop match {
      case each: EnhancedForLoopTree => named(each.getExpression)
      case plain: ForLoopTree        => plain.condition.flatMap(bounded(_))
      case waiting: WhileLoopTree    => bounded(waiting.getCondition)
      case _                         => None
    }

    private def bounded(condition: ExpressionTree): Option[String] = condition match {
      case parenthesized: ParenthesizedTree                                  => bounded(parenthesized.getExpression)
      case binary: BinaryTree if binary.getKind == Tree.Kind.CONDITIONAL_AND =>
        bounded(binary.getLeftOperand).orElse(bounded(binary.getRightOperand))
      case binary: BinaryTree if bounds.contains(binary.getKind) => sized(binary.getRightOperand)
      case _                                                     => None
    }

    // a counter below the size is a walk over the collection, where a size above zero only tests for what is left
    private val bounds = Set(Tree.Kind.LESS_THAN, Tree.Kind.LESS_THAN_EQUAL)

    private def sized(expression: ExpressionTree): Option[String] = expression match {
      case call: MethodInvocationTree if call.getArguments.isEmpty =>
        call.getMethodSelect match {
          case select: MemberSelectTree if Set("size", "length").contains(select.getIdentifier.toString) =>
            named(select.getExpression)
          case _ => None
        }
      case select: MemberSelectTree if select.getIdentifier.toString == "length" => named(select.getExpression)
      case _                                                                     => None
    }
  }

  /** A search of a list that is made on every turn of a loop, which reads the whole list each time. */
  private object ListSearchInLoop extends Lint {
    val id               = LintId.ListSearchInLoop.id
    val kinds: Set[Kind] = Set(Tree.Kind.METHOD_INVOCATION)

    private val searches = Set("contains", "indexOf", "lastIndexOf", "containsAll")

    def check(context: Context, tree: Tree): Vector[Found] =
      tree.asInstanceOf[MethodInvocationTree].getMethodSelect match {
        case select: MemberSelectTree
            if searches.contains(select.getIdentifier.toString) && context.inLoop &&
              named(select.getExpression).isDefined && isList(context.typeOf(select.getExpression)) =>
          Vector(context.found(s"${select.getIdentifier} reads the whole list on every turn of the loop"))
        case _ => Vector.empty
      }
  }

  /** An edit at the front of an array list on every turn of a loop, which moves every element after it. */
  private object ListFrontEditInLoop extends Lint {
    val id               = LintId.ListFrontEditInLoop.id
    val kinds: Set[Kind] = Set(Tree.Kind.METHOD_INVOCATION)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val invocation = tree.asInstanceOf[MethodInvocationTree]
      val arguments  = Lints.arguments(invocation)
      invocation.getMethodSelect match {
        case select: MemberSelectTree if context.inLoop && isList(context.typeOf(select.getExpression)) =>
          val name  = select.getIdentifier.toString
          val front = (name == "remove" && arguments.size == 1 && zero(arguments.head)) ||
            (name == "add" && arguments.size == 2 && zero(arguments.head))
          if (front)
            Vector(context.found(s"$name at the front of the list moves every element on each turn of the loop"))
          else Vector.empty
        case _ => Vector.empty
      }
    }

    private def zero(expression: ExpressionTree): Boolean = expression match {
      case literal: LiteralTree => literal.getValue == 0
      case _                    => false
    }
  }

  /** The name of a variable or a field, which a loop or a call can be matched to another by. */
  private def named(expression: ExpressionTree): Option[String] = expression match {
    case identifier: IdentifierTree => Some(identifier.getName.toString)
    case select: MemberSelectTree   =>
      named(select.getExpression).map(base => s"$base.${select.getIdentifier}").map(_.stripPrefix("this."))
    case parenthesized: ParenthesizedTree => named(parenthesized.getExpression)
    case _                                => None
  }

  private val lists =
    Set(
      "java.util.List",
      "java.util.ArrayList",
      "java.util.Vector",
      "java.util.Stack",
      "java.util.concurrent.CopyOnWriteArrayList"
    )

  /** Whether the type is a list that reads the elements it passes in order, as an array list does. */
  private def isList(tpe: javax.lang.model.`type`.TypeMirror): Boolean = tpe match {
    case declared: DeclaredType => lists.contains(declared.asElement().toString)
    case _                      => false
  }
}
