package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.tree.Tree.Kind
import com.sun.source.util.TreeScanner
import javax.lang.model.`type`.DeclaredType
import javax.lang.model.`type`.TypeKind
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.ElementKind
import javax.lang.model.element.ExecutableElement
import javax.lang.model.element.Modifier
import javax.lang.model.element.TypeElement
import javax.lang.model.element.VariableElement
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** The lints of the control flow of the Java code, which the code of a type holds.
  *
  * The lints are of the code of one file, as the trees the compiler gives are of one compilation; the rules that need
  * the evidence of the whole build are of the analysis, which holds the model of the build.
  */
object LintControlFlow {

  /** The lints of the control flow. */
  val lints: Vector[Lint] =
    Vector(
      MissingDefaultInSwitch,
      FallthroughInSwitch,
      ConstantCondition,
      ReturnInFinally,
      CloseInFinally,
      IfCanBeSwitch,
      IdenticalBranches,
      TrivialIf,
      UnnecessaryReturn,
      PointlessArithmetic,
      PointlessBoolean,
      EmptyMethod,
      EmptyStatementBody,
      SimplifiableConditional,
      NullDefault
    )

  /** A switch of an enumeration that does not handle every case of it. */
  private object MissingDefaultInSwitch extends Lint {
    val id               = JavaLint.MissingDefaultInSwitch.id
    val kinds: Set[Kind] = Set(Tree.Kind.SWITCH)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val switch = tree.asInstanceOf[SwitchTree]
      val cases  = switch.getCases.asScala.toVector
      // The default case of a switch of the newer Java is a label of the case that holds it, beside `null`.
      val hasDefault = cases.exists(case_ =>
        case_.getLabels.isEmpty ||
          case_.getLabels.asScala.exists {
            case _: DefaultCaseLabelTree => true
            case _                       => false
          }
      )
      // A switch of an enumeration that has no default case misses the cases the enumeration gains later, unless it
      // handles every case the enumeration has now.
      val ofEnumeration = context.typeOf(switch.getExpression) match {
        case declared: DeclaredType => declared.asElement() match {
            case element if element.getKind == ElementKind.ENUM => Some(element)
            case _                                              => None
          }
        case _ => None
      }
      val handled = cases.view
        .flatMap(_.getLabels.asScala)
        .flatMap {
          // The label of a case wraps the constant it names, which the older compilers gave as the label itself.
          case label: ConstantCaseLabelTree =>
            label.getConstantExpression match {
              case identifier: IdentifierTree => Vector(identifier.getName.toString)
              case _                          => Vector.empty
            }
          case identifier: IdentifierTree => Vector(identifier.getName.toString)
          case _                          => Vector.empty
        }
        .toSet
      val uncovered = ofEnumeration
        .map(_.getEnclosedElements.asScala.toVector.collect {
          case constant if constant.getKind == ElementKind.ENUM_CONSTANT => constant.getSimpleName.toString
        })
        .getOrElse(Vector.empty)
        .filterNot(handled)
      if (!hasDefault && uncovered.nonEmpty)
        Vector(context.found("the switch does not handle every case of the enumeration"))
      else Vector.empty
    }
  }

  /** A case of a switch that falls through to the case after it. */
  private object FallthroughInSwitch extends Lint {
    val id               = JavaLint.FallthroughInSwitch.id
    val kinds: Set[Kind] = Set(Tree.Kind.SWITCH)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val cases = tree.asInstanceOf[SwitchTree].getCases.asScala.toVector
      // The last case falls through to nothing, so only the others are cases that fall through.
      val falling = cases.init.filter { case_ =>
        case_.getCaseKind == CaseTree.CaseKind.STATEMENT && {
          val statements = case_.getStatements.asScala.toVector
          statements.nonEmpty && !endsTheCase(statements.last)
        }
      }
      if (falling.isEmpty) Vector.empty
      else Vector(context.found(s"${falling.size} case(s) of the switch fall through to the case after them"))
    }

    /** Whether a statement ends the case it is in, which a break, a return or a throw does. */
    private def endsTheCase(statement: StatementTree): Boolean = statement match {
      case _: BreakTree     => true
      case _: ReturnTree    => true
      case _: ThrowTree     => true
      case _: ContinueTree  => true
      case block: BlockTree => block.getStatements.asScala.lastOption.exists(endsTheCase(_))
      case _                => false
    }
  }

  /** An if chain that compares one value with constants, which a switch states more directly. */
  private object IfCanBeSwitch extends Lint {
    val id               = JavaLint.IfCanBeSwitch.id
    val kinds: Set[Kind] = Set(Tree.Kind.IF)

    // Two branches are a choice, which a switch of two cases does not state more directly.
    private val MinimumBranches = 3

    def check(context: Context, tree: Tree): Vector[Found] = {
      val head = tree.asInstanceOf[IfTree]
      // The chain is read at its head, which is where the one finding of it is made.
      val chained = context.ancestors.headOption.exists {
        case outer: IfTree => outer.getElseStatement eq head
        case _             => false
      }
      if (chained) Vector.empty
      else {
        val branches    = chain(head)
        val comparisons = branches.flatMap((condition, _) => comparison(context, condition))
        if (branches.size < MinimumBranches || comparisons.size != branches.size) Vector.empty
        else {
          val first     = comparisons.head
          val constants = comparisons.map(_.constant)
          val same      =
            comparisons.forall(compared => compared.name == first.name && compared.equalsKind == first.equalsKind)
          if (same && constants.distinct.size == constants.size && switchable(context, first))
            Vector(context.found(s"the if chain on '${first.name}' can be replaced with a switch statement"))
          else Vector.empty
        }
      }
    }

    /** A comparison of a value with a constant, and whether the code compares them as texts. */
    private case class Comparison(expression: ExpressionTree, name: String, constant: String, equalsKind: Boolean)

    private def chain(head: IfTree): Vector[(ExpressionTree, StatementTree)] = {
      val branches        = Vector.newBuilder[(ExpressionTree, StatementTree)]
      var current: IfTree = head
      var more            = true
      while more do {
        branches += ((current.getCondition, current.getThenStatement))
        current.getElseStatement match {
          case next: IfTree => current = next
          case _            => more = false
        }
      }
      branches.result()
    }

    @tailrec
    private def comparison(context: Context, condition: ExpressionTree): Option[Comparison] =
      condition match {
        case parenthesized: ParenthesizedTree => comparison(context, parenthesized.getExpression)
        case invocation: MethodInvocationTree =>
          invocation.getMethodSelect match {
            case select: MemberSelectTree
                if select.getIdentifier.contentEquals("equals") && invocation.getArguments.size == 1 =>
              for {
                name     <- selector(select.getExpression)
                constant <- constant(context, invocation.getArguments.get(0))
              } yield Comparison(select.getExpression, name, constant, equalsKind = true)
            case _ => None
          }
        case binary: BinaryTree if binary.getKind == Tree.Kind.EQUAL_TO =>
          val left = for {
            constant <- constant(context, binary.getLeftOperand)
            name     <- selector(binary.getRightOperand)
          } yield Comparison(binary.getRightOperand, name, constant, equalsKind = false)
          left.orElse {
            for {
              constant <- constant(context, binary.getRightOperand)
              name     <- selector(binary.getLeftOperand)
            } yield Comparison(binary.getLeftOperand, name, constant, equalsKind = false)
          }
        case _ => None
      }

    @tailrec
    private def selector(tree: ExpressionTree): Option[String] = tree match {
      case _: IdentifierTree | _: MemberSelectTree => Some(tree.toString)
      case parenthesized: ParenthesizedTree        => selector(parenthesized.getExpression)
      case _                                       => None
    }

    @tailrec
    private def constant(context: Context, tree: ExpressionTree): Option[String] = tree match {
      case parenthesized: ParenthesizedTree => constant(context, parenthesized.getExpression)
      case literal: LiteralTree if literal.getKind != Tree.Kind.NULL_LITERAL => Some(literal.toString)
      case select: MemberSelectTree                                          =>
        val element = context.elementOf(select)
        if (element == null) None
        else if (
          element.getKind == ElementKind.ENUM_CONSTANT ||
          (element.getKind == ElementKind.FIELD && Lints.constant(element))
        ) Some(select.toString)
        else None
      case _ => None
    }

    /** Whether a switch can hold the value, which strings, the integral types and enumerations can. */
    private def switchable(context: Context, compared: Comparison): Boolean =
      context.typeOf(compared.expression).getKind match {
        case TypeKind.CHAR | TypeKind.BYTE | TypeKind.SHORT | TypeKind.INT | TypeKind.LONG => true
        case TypeKind.DECLARED                                                             =>
          context.typeOf(compared.expression) match {
            case declared: DeclaredType =>
              declared.asElement.getKind == ElementKind.ENUM ||
              (compared.equalsKind &&
                (declared.asElement match {
                  case element: TypeElement => element.getQualifiedName.contentEquals("java.lang.String")
                  case _                    => false
                }))
            case _ => false
          }
        case _ => false
      }
  }

  /** The branches of an if that hold the same code, or end or start with it. */
  private object IdenticalBranches extends Lint {
    val id               = JavaLint.IdenticalBranches.id
    val kinds: Set[Kind] = Set(Tree.Kind.IF)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val ifTree     = tree.asInstanceOf[IfTree]
      val thenBranch = statements(ifTree.getThenStatement)
      val elseBranch = Option(ifTree.getElseStatement).map(statements(_)).getOrElse(Vector.empty)
      if (thenBranch.isEmpty || elseBranch.isEmpty) Vector.empty
      else if (
        thenBranch.size == elseBranch.size && thenBranch.zip(elseBranch).forall((s1, s2) => same(context, s1, s2))
      )

        Vector(context.found("the branches of the if statement are identical"))
      else if (same(context, thenBranch.last, elseBranch.last))
        Vector(context.found("the branches of the if statement end with the same statement"))
      else if (same(context, thenBranch.head, elseBranch.head))
        Vector(context.found("the branches of the if statement start with the same statement"))
      else Vector.empty
    }

    private def same(context: Context, s1: StatementTree, s2: StatementTree): Boolean =
      if (s1.toString != s2.toString) false
      else {
        val ids1 = identifiers(s1)
        val ids2 = identifiers(s2)
        if (ids1.size != ids2.size) false
        else
          ids1.zip(ids2).forall { (id1, id2) =>
            val e1 = context.elementOf(id1)
            val e2 = context.elementOf(id2)
            (e1, e2) match {
              case (v1: javax.lang.model.element.VariableElement, v2: javax.lang.model.element.VariableElement)
                  if v1.getKind == ElementKind.LOCAL_VARIABLE || v2.getKind == ElementKind.LOCAL_VARIABLE =>
                val t1 = v1.asType()
                val t2 = v2.asType()
                t1 != null && t2 != null && context.types.isSameType(t1, t2)
              case _ => true
            }
          }
      }

    private def identifiers(tree: Tree): Vector[IdentifierTree] = {
      val result  = mutable.ArrayBuffer.empty[IdentifierTree]
      val scanner = new TreeScanner[Unit, Unit] {
        override def visitIdentifier(node: IdentifierTree, unused: Unit): Unit = {
          result += node
          super.visitIdentifier(node, unused)
        }
      }
      scanner.scan(tree, ())
      result.toVector
    }

    /** The statements of a branch, which tells the same code apart from other code. */
    private def statements(statement: StatementTree): Vector[StatementTree] = statement match {
      case block: BlockTree => block.getStatements.asScala.toVector
      case other            => Vector(other)
    }
  }

  /** An if that returns two boolean literals, which a return of the condition says in one statement. */
  private object TrivialIf extends Lint {
    val id               = JavaLint.TrivialIf.id
    val kinds: Set[Kind] = Set(Tree.Kind.IF)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val ifTree     = tree.asInstanceOf[IfTree]
      val thenReturn = Lint.booleanReturn(ifTree.getThenStatement)
      Option(ifTree.getElseStatement).flatMap(Lint.booleanReturn) match {
        case Some(other) =>
          if (thenReturn.exists(_ != other)) Vector(context.found("the if statement can be simplified"))
          else Vector.empty
        case None =>
          // The if returns one literal, and the code after it returns the other.
          thenReturn match {
            case Some(value) if context.sibling(1).flatMap(Lint.booleanReturn).contains(!value) =>
              Vector(context.found("the if statement can be simplified"))
            case _ => Vector.empty
          }
      }
    }
  }

  /** A return that is unnecessary as the last statement of a void method. */
  private object UnnecessaryReturn extends Lint {
    val id               = JavaLint.UnnecessaryReturn.id
    val kinds: Set[Kind] = Set(Tree.Kind.RETURN)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val ret = tree.asInstanceOf[ReturnTree]
      if (ret.getExpression != null) Vector.empty
      else
        context.ancestors.collectFirst { case method: MethodTree => method } match {
          case Some(method) if voidMethod(method) =>
            method.getBody match {
              case block: BlockTree if block.getStatements.asScala.lastOption.contains(ret) =>
                Vector(context.found("the return is unnecessary as the last statement of a void method"))
              case _ => Vector.empty
            }
          case _ => Vector.empty
        }
    }

    private def voidMethod(method: MethodTree): Boolean = method.getReturnType match {
      case null                         => method.getName.contentEquals("<init>")
      case primitive: PrimitiveTypeTree => primitive.getPrimitiveTypeKind == TypeKind.VOID
      case _                            => false
    }
  }

  /** An arithmetic expression with the identity of its operator, which the other operand alone says. */
  private object PointlessArithmetic extends Lint {
    val id               = JavaLint.PointlessArithmetic.id
    val kinds: Set[Kind] = Set(
      Tree.Kind.PLUS,
      Tree.Kind.MINUS,
      Tree.Kind.MULTIPLY,
      Tree.Kind.DIVIDE,
      Tree.Kind.OR,
      Tree.Kind.XOR,
      Tree.Kind.AND,
      Tree.Kind.LEFT_SHIFT,
      Tree.Kind.RIGHT_SHIFT,
      Tree.Kind.UNSIGNED_RIGHT_SHIFT
    )

    def check(context: Context, tree: Tree): Vector[Found] = {
      val binary = tree.asInstanceOf[BinaryTree]
      comparable(context, binary).map { other =>
        context.found(s"$binary can be simplified to $other")
      }.toVector
    }

    /** The other operand, when the expression is the operator's identity around it. */
    private def comparable(context: Context, binary: BinaryTree): Option[ExpressionTree] = {
      val left     = binary.getLeftOperand
      val right    = binary.getRightOperand
      val kind     = binary.getKind
      val identity =
        if (kind == Tree.Kind.PLUS)
          if (zero(left)) Some(right) else if (zero(right)) Some(left) else None
        else if (kind == Tree.Kind.MINUS)
          if (zero(right)) Some(left) else None
        else if (kind == Tree.Kind.MULTIPLY)
          if (one(left)) Some(right) else if (one(right)) Some(left) else None
        else if (kind == Tree.Kind.DIVIDE)
          if (one(right)) Some(left) else None
        else if (kind == Tree.Kind.OR || kind == Tree.Kind.XOR)
          if (zero(left)) Some(right) else if (zero(right)) Some(left) else None
        else if (kind == Tree.Kind.AND)
          if (minusOne(left)) Some(right) else if (minusOne(right)) Some(left) else None
        else if (
          kind == Tree.Kind.LEFT_SHIFT || kind == Tree.Kind.RIGHT_SHIFT ||
          kind == Tree.Kind.UNSIGNED_RIGHT_SHIFT
        )

          if (zero(right)) Some(left) else None
        else None
      identity.filter(other => numeric(context, other) && number(other).isEmpty)
    }

    /** Whether the type of the expression is one the arithmetic operators work on. */
    private def numeric(context: Context, tree: ExpressionTree): Boolean =
      context.typeOf(tree) != null && Set(
        TypeKind.BYTE,
        TypeKind.SHORT,
        TypeKind.INT,
        TypeKind.LONG,
        TypeKind.FLOAT,
        TypeKind.DOUBLE,
        TypeKind.CHAR
      ).contains(context.typeOf(tree).getKind)

    private def number(tree: ExpressionTree): Option[Double] = tree match {
      case literal: LiteralTree =>
        literal.getValue match {
          case number: java.lang.Number       => Some(number.doubleValue)
          case character: java.lang.Character => Some(character.charValue.toDouble)
          case _                              => None
        }
      case unary: UnaryTree if unary.getKind == Tree.Kind.UNARY_MINUS =>
        number(unary.getExpression).map(-_)
      case parenthesized: ParenthesizedTree => number(parenthesized.getExpression)
      case _                                => None
    }

    private def zero(tree: ExpressionTree): Boolean     = number(tree).contains(0.0)
    private def one(tree: ExpressionTree): Boolean      = number(tree).contains(1.0)
    private def minusOne(tree: ExpressionTree): Boolean = number(tree).contains(-1.0)
  }

  /** A boolean expression with the identity of its operator, which the other operand alone says. */
  private object PointlessBoolean extends Lint {
    val id               = JavaLint.PointlessBoolean.id
    val kinds: Set[Kind] = Set(
      Tree.Kind.METHOD_INVOCATION,
      Tree.Kind.CONDITIONAL_AND,
      Tree.Kind.CONDITIONAL_OR,
      Tree.Kind.AND,
      Tree.Kind.OR,
      Tree.Kind.EQUAL_TO,
      Tree.Kind.NOT_EQUAL_TO,
      Tree.Kind.CONDITIONAL_EXPRESSION
    )

    def check(context: Context, tree: Tree): Vector[Found] = tree match {
      case invocation: MethodInvocationTree       => boxed(context, invocation)
      case binary: BinaryTree                     => binaryExpression(context, binary)
      case conditional: ConditionalExpressionTree => conditionalExpression(context, conditional)
      case _                                      => Vector.empty
    }

    /** `Boolean.TRUE.equals(value)` and `Boolean.FALSE.equals(value)`, which the value alone says. */
    private def boxed(context: Context, invocation: MethodInvocationTree): Vector[Found] = {
      val element  = context.element
      val constant = invocation.getMethodSelect match {
        case select: MemberSelectTree
            if select.getIdentifier.contentEquals("equals") &&
              invocation.getArguments.size == 1 =>
          select.getExpression.toString match {
            case "Boolean.TRUE" | "java.lang.Boolean.TRUE"   => Some((invocation.getArguments.get(0), false))
            case "Boolean.FALSE" | "java.lang.Boolean.FALSE" => Some((invocation.getArguments.get(0), true))
            case _                                           => None
          }
        case _ => None
      }
      if (element == null || element.getEnclosingElement == null) Vector.empty
      else
        element.getEnclosingElement match {
          case owner: TypeElement if owner.getQualifiedName.contentEquals("java.lang.Boolean") =>
            constant.filter { (argument, _) =>
              val mirror      = context.typeOf(argument)
              val isPrimitive = mirror != null && mirror.getKind.isPrimitive
              isPrimitive || LintFlow.nonNull(context, argument)
            }.map((argument, negated) =>
              context.found(s"$invocation can be simplified to ${if (negated) "!" else ""}$argument")
            ).toVector
          case _ => Vector.empty
        }
    }

    /** `true && x`, `false || x` and the comparisons with the boolean literals, which the other operand alone says. */
    private def binaryExpression(context: Context, binary: BinaryTree): Vector[Found] = {
      val left        = boolean(binary.getLeftOperand)
      val right       = boolean(binary.getRightOperand)
      val replacement = binary.getKind match {
        case Tree.Kind.CONDITIONAL_AND | Tree.Kind.AND =>
          if (left.contains(true)) Some(binary.getRightOperand)
          else if (right.contains(true)) Some(binary.getLeftOperand)
          else None
        case Tree.Kind.CONDITIONAL_OR | Tree.Kind.OR =>
          if (left.contains(false)) Some(binary.getRightOperand)
          else if (right.contains(false)) Some(binary.getLeftOperand)
          else None
        case Tree.Kind.EQUAL_TO =>
          if (left.isDefined) Some(binary.getRightOperand)
          else if (right.isDefined) Some(binary.getLeftOperand)
          else None
        case Tree.Kind.NOT_EQUAL_TO =>
          if (left.isDefined) Some(binary.getRightOperand)
          else if (right.contains(true)) Some(binary.getLeftOperand)
          else None
        case _ => None
      }
      replacement.filter(other => boolean(other).isEmpty).map { other =>
        val negated = binary.getKind match {
          case Tree.Kind.NOT_EQUAL_TO => left.contains(true) || right.contains(true)
          case Tree.Kind.EQUAL_TO     => left.contains(false) || right.contains(false)
          case _                      => false
        }
        context.found(s"$binary can be simplified to ${if (negated) "!" else ""}$other")
      }.toVector
    }

    private def conditionalExpression(context: Context, conditional: ConditionalExpressionTree): Vector[Found] = {
      val thenBranch = boolean(conditional.getTrueExpression)
      val elseBranch = boolean(conditional.getFalseExpression)
      (thenBranch, elseBranch) match {
        case (Some(true), Some(false)) =>
          Vector(context.found(s"$conditional can be simplified to ${conditional.getCondition}"))
        case (Some(false), Some(true)) =>
          Vector(context.found(s"$conditional can be simplified to !${conditional.getCondition}"))
        case _ => Vector.empty
      }
    }

    private def boolean(tree: ExpressionTree): Option[Boolean] =
      value(tree).collect { case flag: java.lang.Boolean => flag.booleanValue }
  }

  /** A method whose body is empty, and so is that of every method that overrides it. */
  private object EmptyMethod extends Lint {
    val id               = JavaLint.EmptyMethod.id
    val kinds: Set[Kind] = Set(Tree.Kind.METHOD)

    def check(context: Context, tree: Tree): Vector[Found] = {
      tree match {
        case method: MethodTree if !method.getName.contentEquals("<init>") =>
          Option(context.element).collect { case executable: ExecutableElement => executable }.foreach { element =>
            val empty  = Option.when(method.getBody != null && method.getBody.getStatements.isEmpty)(Registry.Empty)
            val parent = Hierarchy.overridden(context, element)
            if (parent.nonEmpty) context.registry.overrides(Registry.Empty, parent, empty)
            else if (Hierarchy.closed(element) && (empty.isDefined || method.getBody == null))
              context.registry.declares(
                Registry.Empty,
                element,
                empty,
                detail => Linter.found(this, context, Found(detail, method)),
                context.suppressed
              )
          }
        case _ => ()
      }
      Vector.empty
    }
  }

  /** A loop or conditional whose body is the empty statement. */
  private object EmptyStatementBody extends Lint {
    val id               = JavaLint.EmptyStatementBody.id
    val kinds: Set[Kind] = Set(
      Tree.Kind.IF,
      Tree.Kind.WHILE_LOOP,
      Tree.Kind.DO_WHILE_LOOP,
      Tree.Kind.FOR_LOOP,
      Tree.Kind.ENHANCED_FOR_LOOP
    )

    def check(context: Context, tree: Tree): Vector[Found] = {
      val body = tree match {
        case ifTree: IfTree            => Some(ifTree.getThenStatement)
        case loop: WhileLoopTree       => Some(loop.getStatement)
        case loop: DoWhileLoopTree     => Some(loop.getStatement)
        case loop: ForLoopTree         => Some(loop.getStatement)
        case loop: EnhancedForLoopTree => Some(loop.getStatement)
        case _                         => None
      }
      // a loop with braces and nothing in them does as little as one with a semicolon, whatever the comment in it says
      val empty = body.exists {
        case _: EmptyStatementTree => true
        case block: BlockTree      => block.getStatements.isEmpty && tree.getKind != Tree.Kind.IF
        case _                     => false
      }
      if (empty) Vector(context.found("the body of the statement is empty"))
      else Vector.empty
    }
  }

  /** A conditional expression that a call or a boolean states more directly. */
  private object SimplifiableConditional extends Lint {
    val id               = JavaLint.SimplifiableConditional.id
    val kinds: Set[Kind] = Set(Tree.Kind.CONDITIONAL_EXPRESSION)

    def check(context: Context, tree: Tree): Vector[Found] = tree match {
      case conditional: ConditionalExpressionTree =>
        (
          Lints.literalText(conditional.getTrueExpression),
          Lints.literalText(conditional.getFalseExpression)
        ) match {
          case (Some("true"), Some("false")) =>
            Vector(context.found(s"use Boolean.toString(${conditional.getCondition}) instead of the conditional"))
          case (Some("false"), Some("true")) =>
            Vector(context.found(s"use Boolean.toString(!${conditional.getCondition}) instead of the conditional"))
          case _ => Vector.empty
        }
      case _ => Vector.empty
    }
  }

  /** A check for null that picks a default, which `Objects.requireNonNullElse` states in one call. */
  private object NullDefault extends Lint {
    val id               = JavaLint.NullDefault.id
    val kinds: Set[Kind] = Set(Tree.Kind.CONDITIONAL_EXPRESSION, Tree.Kind.IF)

    private val Factories =
      Set("java.util.List", "java.util.Set", "java.util.Map", "java.util.Collections", "java.util.Optional")
    private val Empties = Set("of", "empty", "emptyList", "emptySet", "emptyMap")

    def check(context: Context, tree: Tree): Vector[Found] =
      if (!Release.atLeast(context, 9)) Vector.empty
      else
        (tree match {
          case conditional: ConditionalExpressionTree =>
            choice(conditional.getCondition, conditional.getTrueExpression, conditional.getFalseExpression)
          case ifTree: IfTree =>
            for {
              whenTrue  <- returned(ifTree.getThenStatement)
              otherwise <- Option(ifTree.getElseStatement).orElse(context.sibling(1))
              whenFalse <- returned(otherwise)
              chosen    <- choice(ifTree.getCondition, whenTrue, whenFalse)
            } yield chosen
          case _ => None
        }).flatMap((kept, fallback) => suggestion(context, kept, fallback)).map(context.found(_)).toVector

    private def returned(statement: StatementTree): Option[ExpressionTree] = Lint.single(statement) match {
      case ret: ReturnTree => Option(ret.getExpression)
      case _               => None
    }

    /** The value that is checked and the default that stands for it, when the branches return the value or a default by
      * the check.
      */
    private def choice(
        condition: ExpressionTree,
        whenTrue: ExpressionTree,
        whenFalse: ExpressionTree
    ): Option[(ExpressionTree, ExpressionTree)] =
      Lints.unparenthesised(condition) match {
        case binary: BinaryTree if binary.getKind == Tree.Kind.NOT_EQUAL_TO || binary.getKind == Tree.Kind.EQUAL_TO =>
          val value = (nulled(binary.getLeftOperand), nulled(binary.getRightOperand)) match {
            case (true, false) => Some(binary.getRightOperand)
            case (false, true) => Some(binary.getLeftOperand)
            case _             => None
          }
          val present = binary.getKind == Tree.Kind.NOT_EQUAL_TO
          value.flatMap { value =>
            val (kept, fallback) = if (present) (whenTrue, whenFalse) else (whenFalse, whenTrue)
            Option.when(Lints.unparenthesised(kept).toString == Lints.unparenthesised(value).toString)(
              (value, fallback)
            )
          }
        case _ => None
      }

    private def nulled(expression: ExpressionTree): Boolean =
      Lints.unparenthesised(expression).getKind == Tree.Kind.NULL_LITERAL

    private def suggestion(context: Context, value: ExpressionTree, fallback: ExpressionTree): Option[String] = {
      val unchanged = Effects.reads(context, value).isDefined && !context.typeOf(value).getKind.isPrimitive
      if (!unchanged || !related(context, value, fallback)) None
      else
        eager(context, fallback).map { lazily =>
          if (lazily)
            s"use Objects.requireNonNullElseGet($value, () -> $fallback) instead of the null check"
          else s"use Objects.requireNonNullElse($value, $fallback) instead of the null check"
        }
    }

    /** Whether the default is built only when the value is null, and nothing when it may be null itself or may fail. */
    private def eager(context: Context, fallback: ExpressionTree): Option[Boolean] =
      Lints.unparenthesised(fallback) match {
        case literal: LiteralTree => Option.when(literal.getKind != Tree.Kind.NULL_LITERAL)(false)
        case created: NewClassTree
            if created.getClassBody == null && jdk(context.typeOf(created)) &&
              Lints.arguments(created).forall(Effects.reads(context, _).isDefined) =>
          Some(true)
        case call: MethodInvocationTree if call.getArguments.isEmpty && factory(context, call) => Some(false)
        case expression if Effects.reads(context, expression).isDefined && LintFlow.nonNull(context, expression) =>
          Some(!cheap(context, expression))
        case _ => None
      }

    /** Whether the default has the type of the value, or one it converts to without the arithmetic of a conditional. */
    private def related(context: Context, value: ExpressionTree, fallback: ExpressionTree): Boolean = {
      val types                               = context.types
      def erased(tpe: TypeMirror): TypeMirror =
        types.erasure(if (tpe.getKind.isPrimitive) types.boxedClass(types.getPrimitiveType(tpe.getKind)).asType
        else tpe)
      val (left, right) = (erased(context.typeOf(value)), erased(context.typeOf(fallback)))
      Lints.subtype(context, left, right) || Lints.subtype(context, right, left)
    }

    /** Whether the expression names a variable, which reading cannot fail. */
    private def cheap(context: Context, expression: ExpressionTree): Boolean = expression match {
      case _: IdentifierTree        => true
      case select: MemberSelectTree =>
        context.elementOf(select) match {
          case field: VariableElement => field.getModifiers.contains(Modifier.STATIC)
          case _                      => false
        }
      case _ => false
    }

    private def jdk(tpe: TypeMirror): Boolean = tpe match {
      case declared: DeclaredType =>
        val name = declared.asElement.toString
        name.startsWith("java.util.") || name.startsWith("java.lang.")
      case _ => false
    }

    private def factory(context: Context, call: MethodInvocationTree): Boolean =
      context.elementOf(call) match {
        case method: ExecutableElement =>
          method.getModifiers.contains(Modifier.STATIC) && Empties(method.getSimpleName.toString) &&
          Option(method.getEnclosingElement).exists(owner => Factories(owner.toString))
        case _ => false
      }
  }

  /** A condition that is always the same. */
  private object ConstantCondition extends Lint {
    val id               = JavaLint.ConstantCondition.id
    val kinds: Set[Kind] = Set(Tree.Kind.IF, Tree.Kind.WHILE_LOOP, Tree.Kind.DO_WHILE_LOOP)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val (condition, loop) = tree match {
        case ifTree: IfTree           => (ifTree.getCondition, false)
        case whileTree: WhileLoopTree => (whileTree.getCondition, true)
        case doTree: DoWhileLoopTree  => (doTree.getCondition, true)
        case _                        => (null, false)
      }
      LintControlFlow.value(condition) match {
        // A loop whose condition is true is how the code writes a loop that runs until something in it leaves, so only
        // a condition that is false, or an if whose condition is a constant, is a condition that is always the same.
        case Some(value: java.lang.Boolean) if !(loop && value) =>
          Vector(context.found(s"the condition is always $value"))
        case _ => Vector.empty
      }
    }
  }

  /** The value of a literal, which the parentheses of the code hold as a tree of their own. */
  private def value(tree: Tree): Option[Any] = tree match {
    case parenthesized: ParenthesizedTree => LintControlFlow.value(parenthesized.getExpression)
    case literal: LiteralTree             => Option(literal.getValue)
    case _                                => None
  }

  /** A method that returns from its `finally` block, which hides the exception the method was to report. */
  private object ReturnInFinally extends Lint {
    val id               = JavaLint.ReturnInFinally.id
    val kinds: Set[Kind] = Set(Tree.Kind.RETURN)

    def check(context: Context, tree: Tree): Vector[Found] = {
      // The blocks the code of the return is in, the finally block of a try being one of them when it holds the code.
      val blocks    = context.ancestors.collect { case block: BlockTree => block }.toSet
      val inFinally = context.ancestors.exists {
        case tryTree: TryTree => tryTree.getFinallyBlock != null && blocks.contains(tryTree.getFinallyBlock)
        case _                => false
      }
      if (inFinally)
        Vector(context.found("the return hides the exception the method was to report"))
      else Vector.empty
    }
  }

  /** A resource that the finally block closes, which the try statement can close instead. */
  private object CloseInFinally extends Lint {
    val id               = JavaLint.CloseInFinally.id
    val kinds: Set[Kind] = Set(Tree.Kind.TRY)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val tryTree      = tree.asInstanceOf[TryTree]
      val finallyBlock = tryTree.getFinallyBlock
      // A try that already holds its resources closes them in the order it lists them, which the finally block does
      // not, so the code that closes a resource there is only the code of a try without resources.
      if (finallyBlock == null || !tryTree.getResources.isEmpty) Vector.empty
      else
        finallyBlock.getStatements.asScala.toVector.flatMap {
          case statement: ExpressionStatementTree => statement.getExpression match {
              case call: MethodInvocationTree => call.getMethodSelect match {
                  case select: MemberSelectTree
                      if select.getIdentifier.toString == "close" && call.getArguments.isEmpty =>
                    resource(context, tryTree, select.getExpression) match {
                      case Some(name) => Vector(Found(
                          s"the resource $name is closed in a finally block; let the try statement close it",
                          statement
                        ))
                      case None => Vector.empty
                    }
                  case _ => Vector.empty
                }
              case _ => Vector.empty
            }
          case _ => Vector.empty
        }
    }

    /** The name of the resource the code closes, when it is a closeable variable that the code does not give a value.
      */
    private def resource(context: Context, tryTree: TryTree, expression: ExpressionTree): Option[String] = {
      val element = context.elementOf(expression)
      if (element == null) None
      else
        element.getKind match {
          // a variable the try declared elsewhere is a resource of the try from Java 9 on
          case ElementKind.LOCAL_VARIABLE | ElementKind.PARAMETER
              if element.getKind == ElementKind.LOCAL_VARIABLE || Release.atLeast(context, 9) =>
            if (closeable(context.typeOf(expression)) && !assigned(context, tryTree, element))
              Some(expression.toString)
            else None
          // try-with-resources names only final fields, from Java 9 on
          case ElementKind.FIELD
              if Release.atLeast(context, 9) && element.getModifiers.contains(Modifier.FINAL) &&
                closeable(context.typeOf(expression)) =>
            Some(expression.toString)
          case _ => None
        }
    }

    /** Whether the type is a closeable one, which the try statement can hold as a resource. */
    private def closeable(tpe: TypeMirror): Boolean = tpe match {
      case declared: DeclaredType =>
        val element = declared.asElement().asInstanceOf[TypeElement]
        element.getQualifiedName.contentEquals("java.lang.AutoCloseable") ||
        element.getInterfaces.asScala.exists(closeable(_)) ||
        closeable(element.getSuperclass)
      case _ => false
    }

    /** Whether the code gives the variable a value, which the try statement cannot hold as a resource then. */
    private def assigned(context: Context, tree: Tree, element: javax.lang.model.element.Element): Boolean = {
      var found                     = false
      def same(node: Tree): Boolean = {
        val path = context.trees.getPath(context.unit, node)
        path != null && context.trees.getElement(path) == element
      }
      val scanner = new TreeScanner[Unit, Unit] {
        override def visitAssignment(node: AssignmentTree, unused: Unit): Unit = {
          if (same(node.getVariable)) found = true
          super.visitAssignment(node, unused)
        }

        override def visitCompoundAssignment(node: CompoundAssignmentTree, unused: Unit): Unit = {
          if (same(node.getVariable)) found = true
          super.visitCompoundAssignment(node, unused)
        }

        override def visitUnary(node: UnaryTree, unused: Unit): Unit = {
          node.getKind match {
            case Tree.Kind.PREFIX_INCREMENT | Tree.Kind.PREFIX_DECREMENT |
                Tree.Kind.POSTFIX_INCREMENT | Tree.Kind.POSTFIX_DECREMENT =>
              if (same(node.getExpression)) found = true
            case _ => ()
          }
          super.visitUnary(node, unused)
        }
      }
      scanner.scan(tree, ())
      found
    }
  }
}
