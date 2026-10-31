package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.Trees
import javax.lang.model.`type`.DeclaredType
import javax.lang.model.`type`.TypeKind
import javax.lang.model.element.*
import net.kurobako.bonsai.java.Model.Guard
import net.kurobako.bonsai.java.Model.Probe
import scala.jdk.CollectionConverters.*

/** Reads how a type is constructed: who may call its constructors, and what the constructor requires of its arguments
  * before it stores them, which is the invariant of the values it builds.
  */
private[java] object Guards {

  private val Comparisons = Map(
    Tree.Kind.LESS_THAN          -> "<",
    Tree.Kind.LESS_THAN_EQUAL    -> "<=",
    Tree.Kind.GREATER_THAN       -> ">",
    Tree.Kind.GREATER_THAN_EQUAL -> ">=",
    Tree.Kind.EQUAL_TO           -> "==",
    Tree.Kind.NOT_EQUAL_TO       -> "!="
  )

  /** Where a constructor keeps the value of a field, in the field of the type it is given as the parameter at `index`,
    * or as a constant of the delegation to another constructor.
    */
  enum Stored {
    case Parameter(index: Int)
    case Constant(argument: String)
  }

  private val Flipped = Map("<" -> ">", "<=" -> ">=", ">" -> "<", ">=" -> "<=", "==" -> "==", "!=" -> "!=")

  /** The constructors or the methods the class declares, with the elements they declare. */
  private def declared(
      trees: Trees,
      path: TreePath,
      declaration: ClassTree,
      kind: ElementKind
  ): Vector[(TreePath, ExecutableElement)] =
    declaration.getMembers.asScala.toVector.flatMap {
      case method: MethodTree =>
        val member = new TreePath(path, method)
        trees.getElement(member) match {
          case element: ExecutableElement if element.getKind == kind => Some(member -> element)
          case _                                                     => None
        }
      case _ => None
    }

  private def instantiable(owner: TypeElement): Boolean =
    owner.getKind == ElementKind.CLASS || owner.getKind == ElementKind.RECORD

  /** The construction of the class or record at the path, unless it is built freely with nothing required of it. */
  def construction(trees: Trees, path: TreePath): Option[Model.Construction] =
    (path.getLeaf, trees.getElement(path)) match {
      case (declaration: ClassTree, owner: TypeElement) if instantiable(owner) =>
        val constructors = declared(trees, path, declaration, ElementKind.CONSTRUCTOR)
        val restricted   = constructors.nonEmpty && constructors.forall(_._2.getModifiers.contains(Modifier.PRIVATE))
        val factories    = if restricted then Vector.empty else factoriesOf(owner)
        val guard        = guardOf(trees, owner, constructors)
        val access       = if restricted then "private" else if factories.nonEmpty then "factory" else "open"
        Option.when(access != "open" || guard != Guard.Known(true))(
          Model.Construction(Collector.qualified(owner)(using trees), access, factories, guard)
        )
      case _ => None
    }

  /** For each constructor of the class, what it keeps in each field of the type: the parameter it stores in the field
    * unchanged, or the constant a constructor it delegates to keeps there. A constructor with a variable number of
    * arguments keeps nothing that way.
    */
  def stores(trees: Trees, path: TreePath): Vector[(ExecutableElement, Map[String, Stored])] =
    (path.getLeaf, trees.getElement(path)) match {
      case (declaration: ClassTree, owner: TypeElement) if owner.getKind == ElementKind.CLASS =>
        val constructors = declared(trees, path, declaration, ElementKind.CONSTRUCTOR)
        val readers      =
          constructors.map((constructor, element) => element -> Reader(trees, owner, constructor, element)).toMap
        val kept = scala.collection.mutable.Map.empty[ExecutableElement, Map[String, Stored]]
        def keeps(element: ExecutableElement): Map[String, Stored] =
          kept.getOrElseUpdate(
            element,
            readers.get(element).filterNot(_ => element.isVarArgs).fold(Map.empty[String, Stored])(_.stores(keeps))
          )
        constructors.map((_, element) => element -> keeps(element))
      case _ => Vector.empty
    }

  /** The guards the methods of the class start with, for the methods that callers reach by their name: those that are
    * static, private or final, or are in a record or a final class, that do not override another method, and that no
    * other method of the class shares a name with. A guard that is only a probe of one parameter makes the method a
    * check of its argument rather than a function of it, so it is no precondition.
    */
  def preconditions(trees: Trees, path: TreePath): Vector[Model.Precondition] =
    (path.getLeaf, trees.getElement(path)) match {
      case (declaration: ClassTree, owner: TypeElement) if instantiable(owner) =>
        val methods = declared(trees, path, declaration, ElementKind.METHOD)
        val names   = methods.groupBy(_._2.getSimpleName.toString).view.mapValues(_.size)
        methods.flatMap { (method, element) =>
          val modifiers = element.getModifiers
          val fixed     = modifiers.contains(Modifier.STATIC) || modifiers.contains(Modifier.PRIVATE) ||
            modifiers.contains(Modifier.FINAL) || owner.getKind == ElementKind.RECORD ||
            owner.getModifiers.contains(Modifier.FINAL)
          if !fixed || names(element.getSimpleName.toString) > 1 || Collector.overriding(element) then None
          else
            Reader(trees, owner, method, element).guard.filter(guard => informative(guard) && !probe(guard))
              .map(Model.Precondition(Collector.qualified(element)(using trees), _))
        }
      case _ => Vector.empty
    }

  private def probe(guard: Guard): Boolean = guard match {
    case _: Guard.Holds   => true
    case Guard.Not(inner) => probe(inner)
    case _                => false
  }

  private def informative(guard: Guard): Boolean = guard match {
    case _: (Guard.Holds | Guard.Is | Guard.Compare) => true
    case Guard.Not(inner)                            => informative(inner)
    case Guard.Connective(_, left, right)            => informative(left) || informative(right)
    case _                                           => false
  }

  /** The guard every constructor that does not delegate to another one starts with, which is not known when they
    * differ.
    */
  private def guardOf(
      trees: Trees,
      owner: TypeElement,
      constructors: Vector[(TreePath, ExecutableElement)]
  ): Guard =
    constructors.flatMap((path, element) => Reader(trees, owner, path, element).guard).distinct match {
      case Vector()     => Guard.Known(true)
      case Vector(only) => only
      case _            => Guard.Unknown
    }

  /** The static methods of the type that return an `Optional` of it and take what its fields hold, which are the
    * factories that validate what the constructor would not.
    */
  private def factoriesOf(owner: TypeElement): Vector[String] = {
    val members                             = owner.getEnclosedElements.asScala.toVector
    def shapes(elements: Seq[? <: Element]) = elements.map { element =>
      val parameter = Collector.parameter(element)
      (parameter.kind, parameter.shape)
    }.sorted
    val stored = shapes(members.filter(member =>
      member.getKind == ElementKind.FIELD && !member.getModifiers.contains(Modifier.STATIC)
    ))
    def optionalOfOwner(method: ExecutableElement) = method.getReturnType match {
      case declared: DeclaredType =>
        declared.asElement.toString == "java.util.Optional" && declared.getTypeArguments.asScala.headOption.exists {
          case argument: DeclaredType => argument.asElement == owner
          case _                      => false
        }
      case _ => false
    }
    members.collect {
      case method: ExecutableElement
          if method.getKind == ElementKind.METHOD && method.getModifiers.contains(Modifier.STATIC) &&
            !method.getModifiers.contains(Modifier.PRIVATE) && optionalOfOwner(method) &&
            shapes(method.getParameters.asScala.toVector) == stored =>
        s"${owner.getSimpleName}.${method.getSimpleName}"
    }.distinct
  }

  private object Reader {
    private val Validates = Set("org.apache.commons.lang3.Validate", "org.apache.commons.lang.Validate")
    private val Spring    = "org.springframework.util.Assert"

    private val NullChecks = Set(
      "java.util.Objects"                    -> "requireNonNull",
      "com.google.common.base.Preconditions" -> "checkNotNull",
      "com.google.common.base.Verify"        -> "verifyNotNull"
    ) ++ Validates.flatMap(owner => Set("notNull", "notEmpty", "notBlank").map(owner -> _)) ++
      Set("notNull", "notEmpty", "hasLength", "hasText").map(Spring -> _)

    private val Assertions = Set(
      "com.google.common.base.Preconditions" -> "checkArgument",
      "com.google.common.base.Preconditions" -> "checkState",
      "com.google.common.base.Verify"        -> "verify"
    ) ++ Validates.flatMap(owner => Set("isTrue", "validState").map(owner -> _)) ++
      Set("isTrue", "state").map(Spring -> _)

    private val IndexCheck = "java.util.Objects" -> "checkIndex"

    private val RangeChecks = Validates.flatMap(owner => Set("inclusiveBetween", "exclusiveBetween").map(owner -> _))
  }

  private class Reader(trees: Trees, owner: TypeElement, constructor: TreePath, element: ExecutableElement) {
    import Lints.child
    import Reader.*

    private val method     = constructor.getLeaf.asInstanceOf[MethodTree]
    private val body       = method.getBody
    private val bodyPath   = Option(body).map(new TreePath(constructor, _))
    private val statements = Option(body).fold(Vector.empty[StatementTree])(_.getStatements.asScala.toVector)

    private def variable(path: TreePath): Option[VariableElement] = trees.getElement(path) match {
      case variable: VariableElement => Some(variable)
      case _                         => None
    }

    private def isCall(statement: StatementTree, name: String): Boolean = statement match {
      case expression: ExpressionStatementTree =>
        expression.getExpression match {
          case call: MethodInvocationTree =>
            call.getMethodSelect match {
              case identifier: IdentifierTree => identifier.getName.contentEquals(name)
              case _                          => false
            }
          case _ => false
        }
      case _ => false
    }

    /** The parameters the constructor assigns after it is given them. */
    private val reassigned: Set[Element] = bodyPath.fold(Set.empty[Element])(root =>
      Effects.writes(root).collect { case Effects.Write(at, identifier: IdentifierTree, _) =>
        variable(child(at, identifier))
      }.flatten.toSet
    )

    /** The parameters and the fields they are stored in, unchanged: a record stores its components by name, a class
      * stores a parameter in a final field it assigns it to.
      */
    private val stored: Map[VariableElement, String] = {
      val parameters = element.getParameters.asScala.toVector.filterNot(reassigned)
      if element.getKind == ElementKind.METHOD || owner.getKind == ElementKind.RECORD then
        parameters.map(parameter => parameter -> parameter.getSimpleName.toString).toMap
      else {
        val pairs       = bodyPath.toVector.flatMap(root => statements.flatMap(stored(root, _)))
        val byParameter = pairs.groupBy(_._1).filter(_._2.size == 1)
        val byField     = pairs.groupBy(_._2).filter(_._2.size == 1)
        pairs.filter((parameter, field) =>
          parameters.contains(parameter) && byParameter.contains(parameter) && byField.contains(field)
        ).toMap
      }
    }

    private def fields: Vector[String] = element.getParameters.asScala.toVector.map(stored.getOrElse(_, ""))

    /** What the constructor keeps in each field, given what the constructors it may delegate to keep. */
    def stores(keeps: ExecutableElement => Map[String, Stored]): Map[String, Stored] =
      delegation match {
        case None =>
          fields.zipWithIndex.collect {
            case (field, index) if field.nonEmpty => field -> Stored.Parameter(index)
          }.toMap
        case Some((invocation, call, target)) =>
          val arguments = call.getArguments.asScala.toVector
          keeps(target).flatMap { (field, kept) =>
            kept match {
              case constant: Stored.Constant => Some(field -> constant)
              case Stored.Parameter(index)   =>
                arguments.lift(index).flatMap(delegated(invocation, _)).map(field -> _)
            }
          }
      }

    /** The `this(..)` call the constructor starts with, and the constructor it calls. */
    private def delegation: Option[(TreePath, MethodInvocationTree, ExecutableElement)] =
      for {
        root  <- bodyPath
        first <- statements.headOption.collect { case call: ExpressionStatementTree if Call.isThisCall(call) => call }
        invocation = child(child(root, first), first.getExpression)
        call   <- Option(invocation.getLeaf).collect { case call: MethodInvocationTree => call }
        target <- Option(trees.getElement(invocation)).collect { case target: ExecutableElement => target }
      } yield (invocation, call, target)

    /** What the constructor keeps when it passes the argument on: its own parameter, or a constant. */
    private def delegated(invocation: TreePath, argument: ExpressionTree): Option[Stored] =
      variable(child(invocation, argument)).map(element.getParameters.indexOf(_)).filter(_ >= 0)
        .filterNot(index => reassigned(element.getParameters.get(index))).map(Stored.Parameter(_)).orElse(
          Some(Collector.argument(trees, invocation, argument)).filter(constant).map(Stored.Constant(_))
        )

    private def constant(argument: String): Boolean =
      Set("true", "false", "absent")(argument) || Set("number:", "text:", "symbol:").exists(argument.startsWith)

    private def stored(path: TreePath, kind: String): Option[VariableElement] =
      variable(path).filter(held => stored.contains(held) && Collector.parameter(held).kind == kind)

    private def stored(root: TreePath, statement: StatementTree): Option[(VariableElement, String)] =
      assignment(child(root, statement))

    /** The parameter a statement stores in a final field of the type, with the field's name. */
    private def assignment(path: TreePath): Option[(VariableElement, String)] = path.getLeaf match {
      case statement: ExpressionStatementTree =>
        val expression = child(path, statement.getExpression)
        expression.getLeaf match {
          case assign: AssignmentTree =>
            val value = child(expression, assign.getExpression)
            for {
              field <- variable(child(expression, assign.getVariable)).filter(field =>
                field.getKind == ElementKind.FIELD && field.getEnclosingElement == owner &&
                  field.getModifiers.contains(Modifier.FINAL)
              )
              parameter <- required(value).orElse(variable(value)).filter(element.getParameters.contains)
            } yield parameter -> field.getSimpleName.toString
          case _ => None
        }
      case _ => None
    }

    private def called(path: TreePath): (String, String) = trees.getElement(path) match {
      case callee: ExecutableElement => (Lints.owner(callee).getOrElse(""), callee.getSimpleName.toString)
      case _                         => ("", "")
    }

    /** The parameter a call that throws on null, or on an index out of bounds, checks and gives back. */
    private def required(path: TreePath): Option[VariableElement] = path.getLeaf match {
      case call: MethodInvocationTree
          if call.getArguments.size >= 1 && (NullChecks(called(path)) || called(path) == IndexCheck) =>
        variable(child(path, call.getArguments.get(0)))
      case _ => None
    }

    /** What a call that throws unless its first argument holds requires. */
    private def asserted(path: TreePath): Option[Guard] = path.getLeaf match {
      case call: MethodInvocationTree if call.getArguments.size >= 1 && Assertions(called(path)) =>
        Some(condition(child(path, call.getArguments.get(0))))
      case _ => None
    }

    /** What a call that throws unless its first argument is an index within the length its second gives requires. */
    private def indexed(path: TreePath): Option[Guard] = path.getLeaf match {
      case call: MethodInvocationTree if call.getArguments.size == 2 && called(path) == IndexCheck =>
        val (index, length) = (child(path, call.getArguments.get(0)), child(path, call.getArguments.get(1)))
        Some(
          stored(index, "number").fold[Guard](Guard.Unknown)(held =>
            Guard.Compare(stored(held), ">=", BigDecimal(0)).and(relation("<", index, length))
          )
        )
      case _ => None
    }

    /** What a call that throws unless its third argument is between the first two, which are constants, requires. */
    private def ranged(path: TreePath): Option[Guard] = path.getLeaf match {
      case call: MethodInvocationTree if call.getArguments.size >= 3 && RangeChecks(called(path)) =>
        val inclusive = called(path)._2 == "inclusiveBetween"
        val bounds    = (0 to 2).map(index => child(path, call.getArguments.get(index)))
        Some(
          (stored(bounds(2), "number"), number(bounds(0)), number(bounds(1))) match {
            case (Some(held), Some(low), Some(high)) =>
              Guard.Compare(stored(held), if inclusive then ">=" else ">", low)
                .and(Guard.Compare(stored(held), if inclusive then "<=" else "<", high))
            case _ => Guard.Unknown
          }
        )
      case _ => None
    }

    /** The guard of the constructor, unless it hands its work to another constructor of the type. */
    def guard: Option[Guard] =
      if statements.headOption.exists(Call.isThisCall) then None
      else
        Some(
          bodyPath.fold(Guard.Known(true)) { root =>
            statements.iterator.map(statement => read(child(root, statement))).takeWhile(_.isDefined).flatten
              .foldLeft[Guard](Guard.Known(true)) {
                case (Guard.Known(true), next)     => next
                case (previous, Guard.Known(true)) => previous
                case (previous, next)              => previous.and(next)
              }
          }
        )

    /** What a statement at the start of a constructor requires, if it is one that belongs there. */
    private def read(path: TreePath): Option[Guard] = path.getLeaf match {
      case statement: StatementTree if isCall(statement, "super") => Some(Guard.Known(true))
      case statement: IfTree if statement.getElseStatement == null && throws(statement.getThenStatement) =>
        Some(Guard.Not(condition(child(path, statement.getCondition))))
      case statement: ExpressionStatementTree =>
        val expression = child(path, statement.getExpression)
        val checked    = required(expression).orElse(expression.getLeaf match {
          case assign: AssignmentTree if assignment(path).isDefined => required(child(expression, assign.getExpression))
          case _                                                    => None
        })
        val indexing = indexed(expression).orElse(expression.getLeaf match {
          case assign: AssignmentTree if assignment(path).isDefined => indexed(child(expression, assign.getExpression))
          case _                                                    => None
        })
        asserted(expression).orElse(ranged(expression)).orElse(indexing).orElse(
          if checked.isDefined then checked.map(nonNull(_, true)) else assignment(path).map(_ => Guard.Known(true))
        )
      case _ => None
    }

    private def throws(statement: StatementTree): Boolean = statement match {
      case _: ThrowTree     => true
      case block: BlockTree => block.getStatements.asScala.toList match {
          case List(_: ThrowTree) => true
          case _                  => false
        }
      case _ => false
    }

    private def nonNull(parameter: VariableElement, wanted: Boolean): Guard = stored.get(parameter) match {
      case None       => Guard.Unknown
      case Some(name) =>
        val shape = Collector.parameter(parameter)
        if shape.kind == "optional" && Collector.optionalValue(parameter.asType).isEmpty then
          if wanted then Guard.Holds(name, Probe.Present) else Guard.Not(Guard.Holds(name, Probe.Present))
        else Guard.Known(wanted)
    }

    private def condition(path: TreePath): Guard = path.getLeaf match {
      case parenthesized: ParenthesizedTree => condition(child(path, parenthesized.getExpression))
      case literal: LiteralTree             =>
        literal.getValue match {
          case value: java.lang.Boolean => Guard.Known(value)
          case _                        => Guard.Unknown
        }
      case unary: UnaryTree if unary.getKind == Tree.Kind.LOGICAL_COMPLEMENT =>
        Guard.Not(condition(child(path, unary.getExpression)))
      case binary: BinaryTree =>
        val (left, right) = (child(path, binary.getLeftOperand), child(path, binary.getRightOperand))
        binary.getKind match {
          case Tree.Kind.CONDITIONAL_AND | Tree.Kind.AND => condition(left).and(condition(right))
          case Tree.Kind.CONDITIONAL_OR | Tree.Kind.OR   => condition(left).or(condition(right))
          case kind if Comparisons.contains(kind)        => relation(Comparisons(kind), left, right)
          case _                                         => Guard.Unknown
        }
      case _: IdentifierTree =>
        stored(path, "flag").fold[Guard](Guard.Unknown)(held => Guard.Holds(stored(held), Probe.Flag))
      case call: MethodInvocationTree => invocation(path, call)
      case _                          => Guard.Unknown
    }

    private def invocation(path: TreePath, call: MethodInvocationTree): Guard =
      trees.getElement(path) match {
        case callee: ExecutableElement =>
          val arguments = call.getArguments.asScala.toVector
          (Lints.owner(callee).getOrElse(""), callee.getSimpleName.toString) match {
            case ("java.util.Objects", "isNull") if arguments.size == 1 =>
              variable(child(path, arguments.head)).fold[Guard](Guard.Unknown)(nonNull(_, false))
            case ("java.util.Objects", "nonNull") if arguments.size == 1 =>
              variable(child(path, arguments.head)).fold[Guard](Guard.Unknown)(nonNull(_, true))
            case (owner, name @ ("isPresent" | "isEmpty"))
                if owner.startsWith("java.util.Optional") && arguments.isEmpty =>
              call.getMethodSelect match {
                case select: MemberSelectTree =>
                  val receiver = child(child(path, select), select.getExpression)
                  variable(receiver).flatMap(stored.get).fold[Guard](Guard.Unknown) { field =>
                    if name == "isPresent" then Guard.Holds(field, Probe.Present)
                    else Guard.Not(Guard.Holds(field, Probe.Present))
                  }
                case _ => Guard.Unknown
              }
            case _ => Guard.Unknown
          }
        case _ => Guard.Unknown
      }

    private def isNull(path: TreePath): Boolean = path.getLeaf match {
      case parenthesized: ParenthesizedTree => isNull(child(path, parenthesized.getExpression))
      case literal: LiteralTree             => literal.getValue == null
      case _                                => false
    }

    private def number(path: TreePath): Option[BigDecimal] = path.getLeaf match {
      case parenthesized: ParenthesizedTree => number(child(path, parenthesized.getExpression))
      case literal: LiteralTree             =>
        literal.getValue match {
          case value: (java.lang.Integer | java.lang.Long | java.lang.Double | java.lang.Float) =>
            Some(BigDecimal(value.toString))
          case _ => None
        }
      case unary: UnaryTree if unary.getKind == Tree.Kind.UNARY_MINUS =>
        number(child(path, unary.getExpression)).map(-_)
      case _ => None
    }

    private def enumCase(path: TreePath): Option[String] = path.getLeaf match {
      case _: IdentifierTree | _: MemberSelectTree =>
        Option(trees.getElement(path)).filter(_.getKind == ElementKind.ENUM_CONSTANT).map(_.getSimpleName.toString)
      case _ => None
    }

    private def relation(operator: String, left: TreePath, right: TreePath): Guard = {
      val equality                  = operator == "==" || operator == "!="
      def parameter(path: TreePath) = variable(path).filter(stored.contains)
      def negate(guard: Guard)      = if operator == "!=" then Guard.Not(guard) else guard
      def comparison(path: TreePath, value: TreePath, operator: String) =
        for {
          held  <- parameter(path)
          known <- number(value)
          if Collector.parameter(held).kind == "number"
        } yield Guard.Compare(stored(held), operator, known)
      def tag(path: TreePath, value: TreePath) =
        for {
          held <- parameter(path)
          name <- enumCase(value)
          if Collector.parameter(held).kind == "tag"
        } yield negate(Guard.Is(stored(held), name))
      val booleans = trees.getTypeMirror(left).getKind == TypeKind.BOOLEAN
      if equality && isNull(right) then parameter(left).fold[Guard](Guard.Unknown)(nonNull(_, operator == "!="))
      else if equality && isNull(left) then parameter(right).fold[Guard](Guard.Unknown)(nonNull(_, operator == "!="))
      else if equality && booleans then negate(same(condition(left), condition(right)))
      else
        comparison(left, right, operator).orElse(comparison(right, left, Flipped(operator))).orElse(
          if equality then tag(left, right).orElse(tag(right, left)) else None
        ).getOrElse(Guard.Unknown)
    }

    private def same(left: Guard, right: Guard): Guard = (left, right) match {
      case (Guard.Unknown, _) | (_, Guard.Unknown) => Guard.Unknown
      case (Guard.Known(true), other)              => other
      case (Guard.Known(false), other)             => Guard.Not(other)
      case (other, Guard.Known(true))              => other
      case (other, Guard.Known(false))             => Guard.Not(other)
      case _                                       => left.and(right).or(Guard.Not(left).and(Guard.Not(right)))
    }
  }
}
