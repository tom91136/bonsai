package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.tree.Tree.Kind
import javax.lang.model.`type`.*
import javax.lang.model.element.*
import javax.tools.JavaFileObject
import net.kurobako.bonsai.java.Javac.*
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

/** The lints that read the facts of the data flow of a method, which `DataFlow` collects once for all of them.
  *
  * A nullness lint, a lint of the conditions the code makes constant and a lint of the nulls that reach a contract read
  * the same values, so that a check that settles a value for one settles it for the others.
  */
object LintFlow {

  val lints: Vector[Lint] = Vector(NullableUse, ConstantValue, DataFlowNull, NullableProblem, NullableReturn)

  private def facts(context: Context, tree: Tree): Option[(MethodTree, DataFlow.Facts)] = tree match {
    case method: MethodTree if method.body.isDefined => Some(method -> DataFlow.of(context, method))
    case _                                           => None
  }

  private def enclosingMethod(context: Context): Option[MethodTree] =
    context.enclosing(Set(Tree.Kind.METHOD)).collect { case method: MethodTree if method.body.isDefined => method }

  /** Whether the data flow of the method that holds the code shows the expression is known to be non-null. */
  def nonNull(context: Context, expression: ExpressionTree): Boolean =
    enclosingMethod(context).exists(method => DataFlow.of(context, method).value(expression).nonNull)

  @tailrec
  private def isNullLiteral(tree: ExpressionTree): Boolean = tree match {
    case literal: LiteralTree             => literal.getKind == Tree.Kind.NULL_LITERAL
    case parenthesized: ParenthesizedTree => isNullLiteral(parenthesized.getExpression)
    case _                                => false
  }

  private def executable(context: Context, method: MethodTree): Option[ExecutableElement] =
    context.namedElement(method).collect {
      case executable: ExecutableElement => executable
    }

  /** A nullable value used where a non-null one is required. */
  private object NullableUse extends Lint {
    val id               = LintId.NullableUse.id
    val kinds: Set[Kind] = Set(Tree.Kind.METHOD)

    def check(context: Context, tree: Tree): Vector[Found] =
      facts(context, tree).toVector.flatMap { (method, flow) =>
        val uses = flow.uses.flatMap { use =>
          val value = flow.value(use.tree)
          if (value.mayBeNull && !isNullLiteral(use.tree) || value.definitelyNull && isNullLiteral(use.tree))
            Some(context.found(s"${use.tree} may be null and is used as ${use.what}", use.tree))
          else None
        }
        val passed = flow.passes.flatMap { pass =>
          val value = flow.value(pass.argument)
          if (
            value.mayBeNull && !isNullLiteral(pass.argument) &&
            Nullness.nonNullParameter(pass.parameter)
          )

            Some(
              context.found(
                s"${pass.argument} may be null and is used as the parameter ${pass.parameter.getSimpleName} of " +
                  s"${pass.callee.getSimpleName}",
                pass.argument
              )
            )
          else None
        }
        val returned = executable(context, method).toVector.flatMap { element =>
          if (Nullness.marked(element) && Nullness.nonNullMethod(element))
            flow.returns.view.filter(_.method.exists(_ eq method)).flatMap { ret =>
              val value = flow.value(ret.expression)
              if (value.mayBeNull && !isNullLiteral(ret.expression))
                Some(
                  context.found(
                    s"${ret.expression} may be null but ${method.getName} is not annotated @Nullable",
                    ret.expression
                  )
                )
              else None
            }.toVector
          else Vector.empty
        }
        uses ++ passed ++ returned
      }
  }

  /** A method whose result can be null, however its result is annotated: an Optional result says that instead. */
  private object NullableReturn extends Lint {
    val id               = LintId.NullableReturn.id
    val kinds: Set[Kind] = Set(Tree.Kind.METHOD)

    private val Message =
      "the method can return null: return Optional<T> (or OptionalInt, OptionalLong, OptionalDouble) instead"

    def check(context: Context, tree: Tree): Vector[Found] = tree match {
      case method: MethodTree if method.returnType.isDefined =>
        executable(context, method).toVector.flatMap { element =>
          if (!returnsReference(element) || fixedByLibrary(context, element)) Vector.empty
          else if (declaredNullable(element)) Vector(context.found(Message, method))
          else if (element.getReturnType.getKind == TypeKind.TYPEVAR)
            returned(context, method) { (expression, flow) =>
              nullBranch(expression) || flow.value(expression).definitelyNull
            }
          else
            returned(context, method) { (expression, flow) =>
              nullBranch(expression) || flow.value(expression).mayBeNull
            }
        }
      case _ => Vector.empty
    }

    /** The first value the method returns that the nullness test accepts. */
    private def returned(context: Context, method: MethodTree)(
        nullable: (ExpressionTree, DataFlow.Facts) => Boolean
    ): Vector[Found] =
      facts(context, method).toVector.flatMap { (_, flow) =>
        flow.returns.filter(_.method.exists(_ eq method)).collectFirst {
          case ret if nullable(ret.expression, flow) => context.found(Message, ret.expression)
        }.toVector
      }

    /** Whether a branch of the expression is the null literal. The null of a type variable comes from the type argument
      * of the caller, so only this null is one the method returns itself.
      */
    private def nullBranch(expression: ExpressionTree): Boolean = expression match {
      case conditional: ConditionalExpressionTree =>
        nullBranch(conditional.getTrueExpression) || nullBranch(conditional.getFalseExpression)
      case parenthesized: ParenthesizedTree => nullBranch(parenthesized.getExpression)
      case _                                => isNullLiteral(expression)
    }

    /** Whether the method returns a reference other than `Void`, which a generic framework asks for a null of. */
    private def returnsReference(method: ExecutableElement): Boolean = {
      val result = method.getReturnType
      result.getKind match {
        case TypeKind.VOID            => false
        case kind if kind.isPrimitive => false
        case TypeKind.DECLARED        =>
          result.asInstanceOf[DeclaredType].asElement match {
            case owner: TypeElement => !owner.getQualifiedName.contentEquals("java.lang.Void")
            case _                  => true
          }
        case _ => true
      }
    }

    /** Whether a method outside the analysed code declares the signature, which the code cannot change. */
    private def fixedByLibrary(context: Context, method: ExecutableElement): Boolean =
      Hierarchy.overridden(context, method).exists(parent => !analysed(context, parent))

    /** Whether the code of the element is among the code the compiler compiles, rather than read from a library. */
    private def analysed(context: Context, element: Element): Boolean =
      try Option(context.elements.getFileObjectOf(element)).exists(_.getKind == JavaFileObject.Kind.SOURCE)
      catch {
        // older compilers do not say which file declares an element; their trees hold the code still being compiled
        case _: NoSuchMethodError => Option(context.trees.getTree(element)).isDefined
      }

    /** Whether the method or its result type carries an annotation named `Nullable`, from whichever library. */
    private def declaredNullable(method: ExecutableElement): Boolean =
      Nullness.hasSimpleName(method, "Nullable") || Nullness.hasSimpleName(method.getReturnType, "Nullable")
  }

  /** A condition that is always the same, by the declarations and the checks the code has made. */
  private object ConstantValue extends Lint {
    val id               = LintId.ConstantValue.id
    val kinds: Set[Kind] = Set(Tree.Kind.METHOD)

    def check(context: Context, tree: Tree): Vector[Found] =
      facts(context, tree).toVector.flatMap { (_, flow) =>
        val comparisons = flow.comparisons.flatMap { comparison =>
          flow.value(comparison.tree).constant.collect { case always: Boolean =>
            val subject = flow.value(comparison.subject)
            val slot    = Lints.unparenthesised(comparison.subject) match {
              case access: ArrayAccessTree => Some(access.getIndex.toString)
              case _                       => None
            }
            val reason =
              if (subject.definitelyNull) slot.fold("as the value is null")(s => s"as slot $s is unassigned")
              else if (isFieldAssignedAfterConstruction(context, comparison.subject))
                "as the field is declared non-null but read before it is assigned; annotate it @Nullable or assign it in the constructor"
              else "as the value is not nullable"
            context.found(s"the condition is always $always, $reason", comparison.tree)
          }
        }
        val defaults = flow.defaults.flatMap { default =>
          if (flow.value(default.argument).nonNull)
            Some(
              context.found(
                s"requireNonNullElse returns ${default.argument}, as the value is not nullable",
                default.invocation
              )
            )
          else None
        }
        comparisons ++ defaults
      }

    private def isFieldAssignedAfterConstruction(context: Context, subject: ExpressionTree): Boolean = {
      val leaf = Lints.unparenthesised(subject)
      if (leaf.isInstanceOf[ArrayAccessTree]) false
      else {
        val elementOpt = context.namedElement(leaf)
        elementOpt match {
          case Some(field: VariableElement) if field.getKind == ElementKind.FIELD =>
            val isMarkedNonNull = Nullness.marked(field) && !Nullness.nullableElement(field)
            if (isMarkedNonNull) {
              val ownerTree =
                Option(context.trees.getTree(field.getEnclosingElement)).collect { case c: ClassTree => c }
                  .orElse(context.ancestors.collectFirst { case c: ClassTree => c })
              ownerTree.exists { classTree =>
                val fieldDecl = classTree.getMembers.asScala.collectFirst {
                  case v: VariableTree if v.getName.contentEquals(field.getSimpleName) => v
                }
                val hasInit = fieldDecl.exists(_.initializer.isDefined)
                if (hasInit) false
                else {
                  val bodies = classTree.getMembers.asScala.flatMap {
                    case m: MethodTree if m.getName.contentEquals("<init>") => m.body
                    case b: BlockTree                                       => Some(b)
                    case _                                                  => None
                  }
                  !bodies.exists { body =>
                    Scan.exists(body) {
                      case node: AssignmentTree =>
                        Lints.unparenthesised(node.getVariable) match {
                          case id: IdentifierTree if id.getName.contentEquals(field.getSimpleName)           => true
                          case sel: MemberSelectTree if sel.getIdentifier.contentEquals(field.getSimpleName) => true
                          case _                                                                             => false
                        }
                    }
                  }
                }
              }
            } else false
          case _ => false
        }
      }
    }
  }

  /** A null that reaches a parameter or an array of non-null elements, or that a reference holder starts with where its
    * value type is not nullable.
    */
  private object DataFlowNull extends Lint {
    val id               = LintId.DataFlowNull.id
    val kinds: Set[Kind] = Set(Tree.Kind.METHOD, Tree.Kind.NEW_CLASS)

    /** The holders that start as null, by the types of the constructors that give them no value. */
    private val EmptyHolders = Map(
      "java.util.concurrent.atomic.AtomicReference"      -> Set(Vector.empty[String]),
      "java.util.concurrent.atomic.AtomicReferenceArray" -> Set(Vector("int")),
      "java.lang.ThreadLocal"                            -> Set(Vector.empty[String]),
      "javafx.beans.property.SimpleObjectProperty"       ->
        Set(Vector.empty[String], Vector("java.lang.Object", "java.lang.String")),
      "javafx.beans.property.ReadOnlyObjectWrapper" ->
        Set(Vector.empty[String], Vector("java.lang.Object", "java.lang.String"))
    )

    def check(context: Context, tree: Tree): Vector[Found] = tree match {
      case creation: NewClassTree => emptyHolder(context, creation)
      case _                      =>
        facts(context, tree).toVector.flatMap { (_, flow) =>
          val passed = flow.passes.flatMap { pass =>
            if (flow.value(pass.argument).definitelyNull && Nullness.nonNullParameter(pass.parameter))
              Some(context.found("null is passed where the parameter is not nullable", pass.argument))
            else None
          }
          val stored = flow.stores.flatMap { store =>
            if (
              flow.value(store.expression).definitelyNull && !Nullness.nullableType(store.component) &&
              context.namedElement.exists(Nullness.marked)
            ) Some(context.found("null is stored in an array of elements that are not nullable", store.expression))
            else None
          }
          passed ++ stored
        }
    }

    private def emptyHolder(context: Context, creation: NewClassTree): Vector[Found] =
      if (creation.classBody.isDefined) Vector.empty
      else
        context.namedElement(creation).toVector.collect { case constructor: ExecutableElement => constructor }.flatMap {
          constructor =>
            val parameters =
              constructor.getParameters.asScala.map(p => context.types.erasure(p.asType).toString).toVector
            val owner = constructor.getEnclosingElement.toString
            val scope = context.ancestors.view.flatMap(context.namedElement(_)).headOption
            if (!EmptyHolders.get(owner).exists(_.contains(parameters)) || !scope.exists(Nullness.marked)) Vector.empty
            else
              holderValue(context, creation).filterNot(Nullness.nullableType(_)).toVector.map(value =>
                context.found(s"the holder starts as null but its value type $value is not nullable", creation)
              )
        }

    /** The type of the value the holder keeps, read from the declaration the holder is given to, as the type of a
      * diamond keeps no annotation.
      */
    private def holderValue(context: Context, creation: NewClassTree): Option[TypeMirror] = {
      val diamond = creation.getIdentifier match {
        case parameterized: ParameterizedTypeTree => parameterized.getTypeArguments.isEmpty
        case _                                    => false
      }
      val holder = context.ancestors.headOption match {
        case Some(variable: VariableTree)     => context.namedElement(variable).map(_.asType)
        case Some(assignment: AssignmentTree) => context.namedElement(assignment.getVariable).map(_.asType)
        case _ if !diamond                    => context.attributedType(creation)
        case _                                => None
      }
      holder.collect { case declared: DeclaredType => declared }.flatMap(_.getTypeArguments.asScala.headOption)
    }
  }

  /** A null returned from a method that is not nullable, a nullable method that never returns null, and an override
    * that breaks the nullness of the method it overrides.
    */
  private object NullableProblem extends Lint {
    val id               = LintId.NullableProblem.id
    val kinds: Set[Kind] = Set(Tree.Kind.METHOD)

    def check(context: Context, tree: Tree): Vector[Found] =
      facts(context, tree).toVector.flatMap { (method, flow) =>
        executable(context, method).toVector.flatMap { element =>
          val marked = Nullness.marked(element)
          val own    = flow.returns.filter(_.method.exists(_ eq method))
          val nulls  =
            if (marked && Nullness.nonNullMethod(element))
              own.view.filter(ret => flow.value(ret.expression).definitelyNull).map(ret =>
                context.found("null is returned from a method that is not nullable", ret.expression)
              ).toVector
            else Vector.empty
          val overridden = Hierarchy.overridden(context, element)
          val never      =
            if (
              marked && Nullness.nullableResult(element) && overridden.isEmpty && own.nonEmpty && closed(element) &&
              element.getReturnType.getKind != TypeKind.TYPEVAR &&
              own.forall(ret => flow.value(ret.expression).nonNull)
            ) Vector(context.found("the method is annotated nullable but never returns null", method))
            else Vector.empty
          nulls ++ never ++ breaks(context, method, element, overridden)
        }
      }

    /** Whether no subclass can override the method, whose nullable result then states what it returns. */
    private def closed(method: ExecutableElement): Boolean =
      method.getModifiers.asScala.exists(modifier =>
        modifier == Modifier.PRIVATE || modifier == Modifier.STATIC || modifier == Modifier.FINAL
      ) || method.getEnclosingElement.getModifiers.contains(Modifier.FINAL)

    private def breaks(
        context: Context,
        method: MethodTree,
        element: ExecutableElement,
        overridden: Vector[ExecutableElement]
    ): Vector[Found] =
      if (!Nullness.marked(element)) Vector.empty
      else {
        val ours = element.getParameters.asScala.toVector
        overridden.view.filter(Nullness.marked(_)).flatMap { parent =>
          val parameters = parent.getParameters.asScala.toVector.zip(ours).zip(method.getParameters.asScala).collect {
            case ((inherited, own), parameterTree)
                if Nullness.nullableElement(inherited) && !Nullness.nullableElement(own) =>
              context.found(
                s"the parameter ${own.getSimpleName} must stay nullable, as in the method it overrides",
                parameterTree
              )
          }
          val result =
            if (!Nullness.nullableResult(parent) && Nullness.nullableResult(element))
              Vector(context.found("the result must stay non-null, as in the method it overrides", method))
            else Vector.empty
          parameters ++ result
        }.toVector
      }
  }
}
