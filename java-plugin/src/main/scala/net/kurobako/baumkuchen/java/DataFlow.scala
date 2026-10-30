package net.kurobako.baumkuchen.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.TreePathScanner
import javax.lang.model.`type`.*
import javax.lang.model.element.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** The facts of the data flow of the Java code of a method, which the lints that read values rather than syntax share.
  *
  * The analysis walks the trees the compiler attributed, keeping for each local and parameter what the code has shown
  * of its value: whether it is null, and the constant it holds. Checks refine the values in the code they dominate, the
  * branches join where they meet, and a loop is walked until its values settle. A lint reads the facts the walk
  * collects and decides which of them are findings, so that a nullness lint, a lint of constant conditions and a lint
  * of a pointless operation agree on what a value is.
  */
private[java] object DataFlow {

  /** What is known of whether a value is null: it is, it is not, the scope says a parameter is not but the code does
    * not trust it, the declarations say it may be, or nothing is said.
    */
  enum Null {
    case Null, NonNull, Contract, Maybe, Unknown
  }

  /** What is known of a value: its nullness, and the constant it holds if the code fixes one. */
  final case class Value(nullness: Null, constant: Option[Any] = None) {
    def join(other: Value): Value = {
      val joined =
        if nullness == other.nullness then nullness
        else if nullness == Null.Unknown || other.nullness == Null.Unknown then Null.Unknown
        else if Set(nullness, other.nullness) == Set(Null.NonNull, Null.Contract) then Null.Contract
        else Null.Maybe
      Value(joined, if constant == other.constant then constant else None)
    }

    def nonNull: Boolean        = nullness == Null.NonNull || nullness == Null.Contract
    def mayBeNull: Boolean      = nullness == Null.Maybe || nullness == Null.Null
    def definitelyNull: Boolean = nullness == Null.Null
  }

  object Value {
    val Unknown: Value              = Value(Null.Unknown)
    val NonNull: Value              = Value(Null.NonNull)
    val Nil: Value                  = Value(Null.Null)
    def constant(value: Any): Value = Value(Null.NonNull, Some(value))
  }

  /** A variable, or a chain of fields read from one, whose value the walk follows. */
  final case class Ref(root: Element, fields: List[Element] = Nil, index: Option[String] = None)

  type Env = Map[Ref, Value]

  /** A value that the code dereferences or unboxes, where a null fails. */
  final case class Use(tree: ExpressionTree, what: String)

  /** An argument the code passes to a parameter of a callee. */
  final case class Pass(argument: ExpressionTree, parameter: VariableElement, callee: ExecutableElement)

  /** A value the code returns; `method` is the method it returns from, or empty when it returns from a lambda. */
  final case class Return(tree: ReturnTree, expression: ExpressionTree, method: Option[MethodTree])

  /** A value the code stores in an array whose elements have the component type. */
  final case class Store(expression: ExpressionTree, component: TypeMirror)

  /** A comparison of a subject with null. */
  final case class Compare(tree: BinaryTree, subject: ExpressionTree, equal: Boolean)

  /** A call of `Objects.requireNonNullElse` with the value it is given first. */
  final case class Default(invocation: MethodInvocationTree, argument: ExpressionTree)

  /** What the walk of a method found. */
  final class Facts(
      private val values: java.util.IdentityHashMap[Tree, Value],
      val uses: Vector[Use],
      val passes: Vector[Pass],
      val returns: Vector[Return],
      val stores: Vector[Store],
      val comparisons: Vector[Compare],
      val defaults: Vector[Default]
  ) {

    /** The value of an expression of the method, which is unknown for code the walk did not reach. */
    def value(tree: Tree): Value = Option(values.get(tree)).getOrElse(Value.Unknown)
  }

  private val cache        = new java.util.WeakHashMap[MethodTree, Facts]
  private val capturedEnvs = new java.util.WeakHashMap[MethodTree, Env]

  /** The facts of the method, which are computed once. */
  def of(context: Context, method: MethodTree): Facts =
    Option(cache.get(method)).getOrElse {
      context.ancestors.collect { case m: MethodTree => m }.reverse.foreach { outer =>
        if outer != method && !cache.containsKey(outer) then of(context, outer): Unit
      }
      val facts = new DataFlow(context).run(method)
      cache.put(method, facts)
      facts
    }
}

private[java] final class DataFlow(context: Context) {
  import DataFlow.*

  /** A place the code can jump to with `break`, `continue` or `yield`. */
  private final class Target(val label: Option[String], val loop: Boolean) {
    val breaks    = mutable.ArrayBuffer.empty[Env]
    val continues = mutable.ArrayBuffer.empty[Env]
    val yields    = mutable.ArrayBuffer.empty[Value]
  }

  private val values      = new java.util.IdentityHashMap[Tree, Value]
  private val seen        = new java.util.IdentityHashMap[Tree, java.lang.Boolean]
  private val uses        = mutable.ArrayBuffer.empty[Use]
  private val passes      = mutable.ArrayBuffer.empty[Pass]
  private val returns     = mutable.ArrayBuffer.empty[Return]
  private val stores      = mutable.ArrayBuffer.empty[Store]
  private val comparisons = mutable.ArrayBuffer.empty[Compare]
  private val defaults    = mutable.ArrayBuffer.empty[Default]
  private var targets     = List.empty[Target]
  private var functions   = List.empty[Option[MethodTree]]
  private var lambdas     = List.empty[LambdaExpressionTree]
  private var root        = Option.empty[MethodTree]

  private val Limit = 4

  def run(method: MethodTree): Facts = {
    functions = List(Some(method))
    root = Some(method)
    val captured = Option(capturedEnvs.get(method)).getOrElse(Map.empty)
    val start    = method.getParameters.asScala.foldLeft(captured)((env, parameter) =>
      element(parameter).fold(env) { element =>
        val value = declared(element)
        env + (Ref(element) -> (if value.nullness == Null.NonNull then Value(Null.Contract) else value))
      }
    )
    Option(method.getBody).foreach(body => statement(body, Some(start)))
    new Facts(
      values,
      uses.toVector,
      passes.toVector,
      returns.toVector,
      stores.toVector,
      comparisons.toVector,
      defaults.toVector
    )
  }

  private def once(tree: Tree)(action: => Unit): Unit =
    if seen.put(tree, true) == null then action

  private def record(tree: Tree, value: Value): Value = {
    val previous = values.get(tree)
    values.put(tree, if previous == null then value else previous.join(value))
    value
  }

  private def join(left: Option[Env], right: Option[Env]): Option[Env] = (left, right) match {
    case (None, other)               => other
    case (other, None)               => other
    case (Some(first), Some(second)) =>
      Some(first.collect {
        case (key, value) if second.contains(key) => key -> value.join(second(key))
      })
  }

  private def joined(envs: Iterable[Env]): Option[Env] =
    envs.foldLeft(Option.empty[Env])((all, env) => join(all, Some(env)))

  // -- declarations

  private def element(tree: Tree): Option[Element] =
    Option(context.trees.getPath(context.unit, tree)).flatMap(path => Option(context.trees.getElement(path)))

  private def typeOf(tree: Tree): TypeMirror =
    Option(context.trees.getPath(context.unit, tree)).flatMap(path => Option(context.trees.getTypeMirror(path))).orNull

  /** The array type of an expression, preferring the declaration of a variable, which keeps the annotations of its
    * elements that the type of its uses may not.
    */
  private def arrayType(tree: ExpressionTree): Option[ArrayType] = {
    val declaredType = stripped(tree) match {
      case _: IdentifierTree | _: MemberSelectTree => element(stripped(tree)).map(_.asType)
      case _                                       => None
    }
    declaredType.orElse(Option(typeOf(tree))).collect { case array: ArrayType => array }
  }

  /** The type a variable declares for the array its initializer lists. */
  private def initialized(newArray: NewArrayTree): Option[TypeMirror] =
    Option(context.trees.getPath(context.unit, newArray)).map(_.getParentPath).flatMap { parent =>
      parent.getLeaf match {
        case variable: VariableTree => element(variable).map(_.asType)
        case _                      => None
      }
    }

  private def marked(element: Element): Boolean = Nullness.marked(element)

  private def lambdaParameter(element: Element): Boolean =
    element.getKind == ElementKind.PARAMETER &&
      Option(context.trees.getPath(element)).exists(path =>
        path.getParentPath != null && path.getParentPath.getLeaf.isInstanceOf[LambdaExpressionTree]
      )

  /** The value a variable or member has by its declaration alone. */
  private def declared(element: Element): Value =
    element.getKind match {
      case ElementKind.ENUM_CONSTANT                                                                    => Value.NonNull
      case ElementKind.PARAMETER if lambdaParameter(element)                                            => Value.Unknown
      case ElementKind.LOCAL_VARIABLE | ElementKind.EXCEPTION_PARAMETER | ElementKind.RESOURCE_VARIABLE =>
        if element.getKind == ElementKind.LOCAL_VARIABLE && Nullness.nullableElement(element) then Value(Null.Maybe)
        else Value.Unknown
      case ElementKind.PARAMETER | ElementKind.FIELD | ElementKind.RECORD_COMPONENT =>
        element match {
          case variable: VariableElement if variable.getConstantValue != null =>
            Value.constant(variable.getConstantValue)
          case _ =>
            if Nullness.nullableElement(element) then Value(Null.Maybe)
            else if marked(element) then
              if element.getKind == ElementKind.FIELD && filledBySetup(element) then Value(Null.Maybe)
              else Value.NonNull
            else Value.Unknown
        }
      case _ => Value.Unknown
    }

  /** Whether the code reads a field that a setup method fills where the field may still be null: a teardown method runs
    * even when the setup failed, and a lambda or local class made before the setup assigns the field can run before it.
    */
  private def filledBySetup(field: Element): Boolean =
    Lifecycle.assignedInSetup(context.trees, field) && {
      val method = functions.head.flatMap(element(_))
      method.exists(Lifecycle.isTeardown) || creation.exists(
        Lifecycle.createdBeforeSetup(context.trees, context.unit, _, field)
      )
    }

  /** Where the code being walked is created if it is a lambda or in a local class, which may run apart from its
    * surroundings.
    */
  private def creation: Option[TreePath] =
    root
      .flatMap(method => Option(context.trees.getPath(context.unit, method)))
      .flatMap(Lifecycle.enclosingLocalClass)
      .orElse(lambdas.lastOption.flatMap(lambda => Option(context.trees.getPath(context.unit, lambda))))

  /** The variables whose values the walk follows: the code checks and assigns fields as it does locals. */
  private def trackable(element: Element): Boolean = element.getKind match {
    case ElementKind.LOCAL_VARIABLE | ElementKind.PARAMETER | ElementKind.RESOURCE_VARIABLE |
        ElementKind.EXCEPTION_PARAMETER =>
      true
    case ElementKind.FIELD => !element.getModifiers.contains(Modifier.VOLATILE)
    case _                 => false
  }

  /** A call may null or replace a field the walk saw hold a value, but it rarely empties one the code has filled, so
    * the walk keeps the fields it knows are not null and forgets the rest.
    */
  private def forgetFields(env: Env): Env =
    env.filter((ref, value) => (ref.root.getKind != ElementKind.FIELD && ref.fields.isEmpty) || value.nonNull)

  /** The environment once the variable or the chain of fields is assigned, which changes what is known of everything
    * read through it.
    */
  private def invalidate(env: Env, ref: Ref): Env =
    env.filterNot { (key, _) =>
      (key.root == ref.root && key.fields.startsWith(ref.fields)) ||
      key.index.exists(idx => ref.root.getSimpleName.contentEquals(idx))
    }

  private def assign(env: Env, ref: Ref, value: Value): Env = invalidate(env, ref) + (ref -> value)

  private def dropAssigned(env: Env, assigned: Set[Element]): Env =
    env.filterNot { (ref, _) =>
      assigned(ref.root) || ref.fields.exists(assigned) ||
      ref.index.exists(idx => assigned.exists(_.getSimpleName.contentEquals(idx)))
    }

  // -- statements

  private def statements(trees: java.lang.Iterable[? <: StatementTree], state: Option[Env]): Option[Env] =
    trees.asScala.foldLeft(state)((current, tree) => statement(tree, current))

  private def statement(tree: StatementTree, state: Option[Env]): Option[Env] =
    state match {
      case None      => None
      case Some(env) =>
        tree match {
          case block: BlockTree => statements(block.getStatements, state)

          case variable: VariableTree =>
            val (value, after) = Option(variable.getInitializer) match {
              case Some(initializer) =>
                val (value, after) = expression(initializer, env)
                unboxing(initializer, typeOf(variable))
                (Some(value), after)
              case None => (None, env)
            }
            Some(element(variable) match {
              case Some(local) =>
                value match {
                  case Some(value) =>
                    val kept =
                      if local.asType.getKind.isPrimitive then Value.NonNull
                      else if value.nullness == Null.Unknown && Nullness.nullableElement(local) then Value(Null.Maybe)
                      else value
                    assign(after, Ref(local), kept)
                  case None => invalidate(after, Ref(local))
                }
              case None => after
            })

          case expressionStatement: ExpressionStatementTree =>
            Some(expression(expressionStatement.getExpression, env)._2)

          case ifTree: IfTree =>
            val (_, whenTrue, whenFalse) = condition(ifTree.getCondition, env)
            val thenOut                  = statement(ifTree.getThenStatement, whenTrue)
            val elseOut                  = Option(ifTree.getElseStatement).fold(whenFalse)(statement(_, whenFalse))
            join(thenOut, elseOut)

          case whileTree: WhileLoopTree =>
            loop(env, None, Some(whileTree.getCondition), whileTree.getStatement, Nil, check = true)

          case doWhile: DoWhileLoopTree =>
            loop(env, None, Some(doWhile.getCondition), doWhile.getStatement, Nil, check = false)

          case forTree: ForLoopTree =>
            val initialized =
              forTree.getInitializer.asScala.foldLeft(state)((current, init) => statement(init, current))
            initialized.flatMap(entry =>
              loop(
                entry,
                None,
                Option(forTree.getCondition),
                forTree.getStatement,
                forTree.getUpdate.asScala.toList.map(_.getExpression),
                check = true
              )
            )

          case enhanced: EnhancedForLoopTree =>
            val (_, after) = expression(enhanced.getExpression, env)
            use(enhanced.getExpression, "an enhanced for loop")
            val variable = element(enhanced.getVariable)
            val entry    = variable.fold(after)(el => invalidate(after, Ref(el)))
            loop(entry, variable, None, enhanced.getStatement, Nil, check = true, exhaustible = true)

          case ret: ReturnTree =>
            Option(ret.getExpression).foreach { returned =>
              expression(returned, env): Unit
              once(ret)(returns += Return(ret, returned, functions.head))
              functions.head.flatMap(element(_)).collect { case executable: ExecutableElement => executable }
                .foreach(executable => unboxing(returned, executable.getReturnType))
            }
            None

          case thrown: ThrowTree =>
            expression(thrown.getExpression, env): Unit
            use(thrown.getExpression, "a throw")
            None

          case jump: BreakTree =>
            findTarget(Option(jump.getLabel).map(_.toString), loopOnly = false).foreach(_.breaks += env)
            None

          case jump: ContinueTree =>
            findTarget(Option(jump.getLabel).map(_.toString), loopOnly = true).foreach(_.continues += env)
            None

          case yielded: YieldTree =>
            val (value, _) = expression(yielded.getValue, env)
            targets.find(!_.loop).foreach(_.yields += value)
            None

          case assertTree: AssertTree =>
            val (_, whenTrue, _) = condition(assertTree.getCondition, env)
            whenTrue.orElse(Some(env))

          case synchronizedTree: SynchronizedTree =>
            val (_, after) = expression(synchronizedTree.getExpression, env)
            use(synchronizedTree.getExpression, "a synchronized block")
            statement(synchronizedTree.getBlock, Some(after))

          case labeled: LabeledStatementTree =>
            val target = new Target(Some(labeled.getLabel.toString), loop = false)
            targets = target :: targets
            val out = statement(labeled.getStatement, state)
            targets = targets.tail
            join(out, joined(target.breaks))

          case tryTree: TryTree => attempt(tryTree, env)

          case switchTree: SwitchTree =>
            val (_, after) = expression(switchTree.getExpression, env)
            switchCases(switchTree.getCases.asScala.toList, after, expressionSwitch = false)._2

          case _ => state
        }
    }

  private def findTarget(label: Option[String], loopOnly: Boolean): Option[Target] =
    label match {
      case Some(name) => targets.find(_.label.contains(name))
      case None       => targets.find(target => target.loop || (!loopOnly && target.label.isEmpty))
    }

  /** The environment after a loop, whose head is walked again until the values that reach it stop changing. */
  private def loop(
      entry: Env,
      variable: Option[Element],
      test: Option[ExpressionTree],
      body: StatementTree,
      update: List[ExpressionTree],
      check: Boolean,
      exhaustible: Boolean = false
  ): Option[Env] = {
    var head    = entry
    var exit    = Option.empty[Env]
    var turn    = 0
    var settled = false
    while !settled && turn < Limit do {
      turn += 1
      val target = new Target(None, loop = true)
      targets = target :: targets
      var leaving = Option.empty[Env]
      val start   =
        if check then
          test match {
            case Some(tree) =>
              val (_, whenTrue, whenFalse) = condition(tree, head)
              leaving = whenFalse
              whenTrue
            case None =>
              if exhaustible then leaving = Some(head)
              Some(head)
          }
        else Some(head)
      val bodyOut = statement(body, start.map(env => variable.fold(env)(el => invalidate(env, Ref(el)))))
      val back    = join(bodyOut, joined(target.continues))
      val looped  =
        if check then back
        else
          back.flatMap(env =>
            test match {
              case Some(tree) =>
                val (_, whenTrue, whenFalse) = condition(tree, env)
                leaving = whenFalse
                whenTrue
              case None => Some(env)
            }
          )
      val updated = looped.map(env => update.foldLeft(env)((current, tree) => expression(tree, current)._2))
      targets = targets.tail
      val next = join(Some(entry), updated).getOrElse(entry)
      exit = join(leaving, joined(target.breaks))
      if next == head then settled = true else head = next
    }
    exit
  }

  private def attempt(tryTree: TryTree, env: Env): Option[Env] = {
    val opened = tryTree.getResources.asScala.foldLeft(Option(env)) {
      case (Some(current), resource: StatementTree)  => statement(resource, Some(current))
      case (Some(current), resource: ExpressionTree) => Some(expression(resource, current)._2)
      case (state, _)                                => state
    }
    val bodyOut  = statement(tryTree.getBlock, opened)
    val assigned = assignedIn(tryTree.getBlock)
    val widened  = dropAssigned(env, assigned)
    val catches  = tryTree.getCatches.asScala.toList.map(catchTree => statement(catchTree.getBlock, Some(widened)))
    val merged   = catches.foldLeft(bodyOut)(join)
    Option(tryTree.getFinallyBlock) match {
      case None          => merged
      case Some(finalOf) =>
        val every   = assigned ++ tryTree.getCatches.asScala.flatMap(catchTree => assignedIn(catchTree.getBlock))
        val handled = statement(finalOf, Some(dropAssigned(env, every)))
        handled.flatMap(_ => merged.map(dropAssigned(_, assignedIn(finalOf))))
    }
  }

  private def assignedIn(tree: Tree): Set[Element] = {
    val found = mutable.Set.empty[Element]
    val root  = Option(context.trees.getPath(context.unit, tree))
    root.foreach { path =>
      new TreePathScanner[Unit, Unit] {
        private def note(target: ExpressionTree): Unit =
          Option(context.trees.getElement(new TreePath(getCurrentPath, target))).foreach(found += _)

        override def visitAssignment(node: AssignmentTree, unused: Unit): Unit = {
          note(node.getVariable)
          super.visitAssignment(node, unused)
        }

        override def visitCompoundAssignment(node: CompoundAssignmentTree, unused: Unit): Unit = {
          note(node.getVariable)
          super.visitCompoundAssignment(node, unused)
        }

        override def visitUnary(node: UnaryTree, unused: Unit): Unit = {
          if Set(
              Tree.Kind.PREFIX_INCREMENT,
              Tree.Kind.PREFIX_DECREMENT,
              Tree.Kind.POSTFIX_INCREMENT,
              Tree.Kind.POSTFIX_DECREMENT
            )
              .contains(node.getKind)
          then note(node.getExpression)
          super.visitUnary(node, unused)
        }
      }.scan(path, ())
    }
    found.toSet
  }

  /** Walks the cases of a switch, returning the values it yields and the environment after it. */
  private def switchCases(cases: List[CaseTree], entry: Env, expressionSwitch: Boolean): (Value, Option[Env]) = {
    val target = new Target(None, loop = false)
    targets = target :: targets
    var previous   = Option.empty[Env]
    var hasDefault = false
    val arms       = mutable.ArrayBuffer.empty[Value]
    cases.foreach { caseTree =>
      if caseTree.getExpressions.isEmpty then hasDefault = true
      val start = if caseTree.getCaseKind == CaseTree.CaseKind.RULE then Some(entry) else join(Some(entry), previous)
      caseTree.getCaseKind match {
        case CaseTree.CaseKind.RULE =>
          caseTree.getBody match {
            case body: ExpressionTree =>
              val (value, after) = expression(body, start.getOrElse(entry))
              arms += value
              target.breaks += after
              previous = None
            case body: StatementTree =>
              val out = statement(body, start)
              out.foreach(target.breaks += _)
              previous = None
            case _ => previous = None
          }
        case _ =>
          previous = statements(caseTree.getStatements, start)
      }
    }
    targets = targets.tail
    val exits = join(joined(target.breaks), previous)
    val all   = if hasDefault || expressionSwitch then exits else join(exits, Some(entry))
    val value = (arms ++ target.yields).reduceOption(_.join(_)).getOrElse(Value.Unknown)
    (value, all)
  }

  // -- expressions

  private def use(tree: ExpressionTree, what: String): Unit = once(tree)(uses += Use(tree, what))

  /** Notes that a boxed value is unboxed where a primitive of the target type is required. */
  private def unboxing(tree: ExpressionTree, target: TypeMirror): Unit =
    if target != null && target.getKind.isPrimitive then unbox(tree)

  private def unbox(tree: ExpressionTree): Unit =
    if Nullness.boxed(typeOf(tree)) then use(tree, "an unboxing conversion")

  private def isTypeName(tree: ExpressionTree): Boolean =
    element(tree).exists(element =>
      element.getKind.isClass || element.getKind.isInterface || element.getKind == ElementKind.PACKAGE
    )

  /** Evaluates an expression, returning its value and the environment after it. */
  private def expression(tree: ExpressionTree, env: Env): (Value, Env) = {
    val (value, after) = evaluate(tree, env)
    (record(tree, value), after)
  }

  /** The variable an expression names, or the chain of fields it reads from one. */
  private def variableOf(tree: ExpressionTree): Option[Ref] =
    stripped(tree) match {
      case identifier: IdentifierTree =>
        if identifier.getName.contentEquals("this") then None else element(identifier).filter(trackable).map(Ref(_))
      case assignment: AssignmentTree => variableOf(assignment.getVariable)
      case select: MemberSelectTree   =>
        element(select).filter(el => el.getKind == ElementKind.FIELD && trackable(el)).flatMap { field =>
          stripped(select.getExpression) match {
            case owner: IdentifierTree if owner.getName.contentEquals("this") => Some(Ref(field))
            case owner                                                        =>
              variableOf(owner).map(ref => Ref(ref.root, ref.fields :+ field))
          }
        }
      case access: ArrayAccessTree =>
        variableOf(access.getExpression).map { base =>
          val idx = stripped(access.getIndex) match {
            case id: IdentifierTree => id.getName.toString
            case lit: LiteralTree   => lit.getValue.toString
            case other              => other.toString
          }
          Ref(base.root, base.fields, Some(idx))
        }
      case _ => None
    }

  /** Records what a check shows of a variable, which leaves what is known of the fields read through it alone. */
  private def refine(tree: ExpressionTree, env: Env, value: Value): Env =
    variableOf(tree).fold(env)(ref => env + (ref -> value))

  private def stripped(tree: ExpressionTree): ExpressionTree = tree match {
    case parenthesized: ParenthesizedTree => stripped(parenthesized.getExpression)
    case other                            => other
  }

  private def evaluate(tree: ExpressionTree, env: Env): (Value, Env) = tree match {
    case parenthesized: ParenthesizedTree => expression(parenthesized.getExpression, env)

    case literal: LiteralTree =>
      val value = literal.getKind match {
        case Tree.Kind.NULL_LITERAL    => Value.Nil
        case Tree.Kind.STRING_LITERAL  => Value.constant(literal.getValue)
        case Tree.Kind.BOOLEAN_LITERAL => Value.constant(literal.getValue)
        case Tree.Kind.CHAR_LITERAL    => Value.constant(literal.getValue.asInstanceOf[Char].toLong)
        case Tree.Kind.INT_LITERAL | Tree.Kind.LONG_LITERAL =>
          Value.constant(literal.getValue.asInstanceOf[Number].longValue)
        case _ => Value.NonNull
      }
      (value, env)

    case identifier: IdentifierTree =>
      if identifier.getName.contentEquals("this") || identifier.getName.contentEquals("super") then (Value.NonNull, env)
      else
        element(identifier) match {
          case Some(el) =>
            (env.getOrElse(Ref(el), declared(el)), env)
          case None => (Value.Unknown, env)
        }

    case select: MemberSelectTree =>
      if select.getIdentifier.contentEquals("class") then (Value.NonNull, env)
      else if variableOf(select).exists(env.contains) then (env(variableOf(select).get), env)
      else if isTypeName(select.getExpression) then
        (element(select).map(declared).getOrElse(Value.Unknown), env)
      else {
        val (_, after) = expression(select.getExpression, env)
        val what       =
          if select.getIdentifier.contentEquals("length") && typeOf(select.getExpression) != null &&
            typeOf(select.getExpression).getKind == TypeKind.ARRAY
          then "an array"
          else "a field"
        use(select.getExpression, what)
        val narrowed = refine(select.getExpression, after, Value.NonNull)
        (element(select).map(declared).getOrElse(Value.Unknown), narrowed)
      }

    case invocation: MethodInvocationTree => call(invocation, env)

    case newClass: NewClassTree =>
      val inner       = Option(newClass.getEnclosingExpression).fold(env)(expression(_, env)._2)
      val constructor = element(newClass).collect { case executable: ExecutableElement => executable }
      val after       = forgetFields(arguments(newClass.getArguments.asScala.toList, constructor, inner))
      if newClass.getClassBody != null then {
        val nonNullCaptures = inner.filter { case (ref, v) =>
          v.nonNull && (ref.root.getKind == ElementKind.LOCAL_VARIABLE || ref.root.getKind == ElementKind.PARAMETER)
        }
        if nonNullCaptures.nonEmpty then {
          newClass.getClassBody.getMembers.asScala.foreach {
            case m: MethodTree => capturedEnvs.put(m, nonNullCaptures)
            case _             => ()
          }
        }
      }
      (Value.NonNull, after)

    case newArray: NewArrayTree =>
      val dimensions =
        newArray.getDimensions.asScala.foldLeft(env)((current, dimension) => expression(dimension, current)._2)
      val component =
        (if newArray.getType == null then initialized(newArray) else None).orElse(Option(typeOf(newArray))) match {
          case Some(array: ArrayType) => array.getComponentType
          case _                      => null
        }
      val after = Option(newArray.getInitializers).map(_.asScala.toList).getOrElse(Nil).foldLeft(dimensions) {
        (current, initializer) =>
          val (_, next) = expression(initializer, current)
          if component != null then once(initializer)(stores += Store(initializer, component))
          next
      }
      (Value.NonNull, after)

    case array: ArrayAccessTree =>
      val (_, first) = expression(array.getExpression, env)
      use(array.getExpression, "an array")
      val (_, second) = expression(array.getIndex, first)
      val owner       = element(stripped(array.getExpression))
      val component   = arrayType(array.getExpression).map(_.getComponentType).orNull
      val value       = variableOf(array).flatMap(second.get).getOrElse {
        owner match {
          case Some(el) if component != null =>
            if Nullness.nullableType(component) || Nullness.hasAnnotation(el, Nullness.Nullable) then
              Value(Null.Maybe)
            else if el.getKind == ElementKind.PARAMETER || el.getKind == ElementKind.FIELD then
              if marked(el) then Value.NonNull else Value.Unknown
            else Value.Unknown
          case _ => Value.Unknown
        }
      }
      (value, refine(array.getExpression, second, Value.NonNull))

    case assignment: AssignmentTree =>
      val (value, afterRight) =
        stripped(assignment.getVariable) match {
          case access: ArrayAccessTree =>
            val (_, a) = expression(access.getExpression, env)
            use(access.getExpression, "an array")
            val (_, b)      = expression(access.getIndex, a)
            val (stored, c) = expression(assignment.getExpression, b)
            arrayType(access.getExpression).foreach(arrayType =>
              once(assignment.getExpression)(stores += Store(assignment.getExpression, arrayType.getComponentType))
            )
            val after = variableOf(access).fold(c)(ref => assign(c, ref, stored))
            (stored, after)
          case select: MemberSelectTree =>
            val a = if isTypeName(select.getExpression) then env
            else {
              val (_, a) = expression(select.getExpression, env)
              use(select.getExpression, "a field")
              a
            }
            expression(assignment.getExpression, a)
          case _ => expression(assignment.getExpression, env)
        }
      unboxing(assignment.getExpression, typeOf(assignment.getVariable))
      val after = variableOf(assignment.getVariable).fold(afterRight) { el =>
        val target = el.fields.lastOption.getOrElse(el.root)
        val kept   =
          if target.asType.getKind.isPrimitive then Value.NonNull
          else if value.nullness == Null.Unknown && Nullness.nullableElement(target) &&
            target.getKind == ElementKind.LOCAL_VARIABLE
          then Value(Null.Maybe)
          else value
        assign(afterRight, el, kept)
      }
      (value, after)

    case compound: CompoundAssignmentTree =>
      val (_, first)  = expression(compound.getVariable, env)
      val (_, second) = expression(compound.getExpression, first)
      if !(compound.getKind == Tree.Kind.PLUS_ASSIGNMENT && isText(compound.getVariable)) then {
        unbox(compound.getVariable)
        unbox(compound.getExpression)
      }
      val after = variableOf(compound.getVariable).fold(second)(invalidate(second, _))
      (Value.Unknown, after)

    case unary: UnaryTree =>
      unary.getKind match {
        case Tree.Kind.LOGICAL_COMPLEMENT =>
          val (value, whenTrue, whenFalse) = condition(unary, env)
          (value, join(whenTrue, whenFalse).getOrElse(env))
        case Tree.Kind.PREFIX_INCREMENT | Tree.Kind.PREFIX_DECREMENT | Tree.Kind.POSTFIX_INCREMENT |
            Tree.Kind.POSTFIX_DECREMENT =>
          val (_, after) = expression(unary.getExpression, env)
          unbox(unary.getExpression)
          val dropped = variableOf(unary.getExpression).fold(after)(invalidate(after, _))
          (Value.Unknown, dropped)
        case _ =>
          val (value, after) = expression(unary.getExpression, env)
          unbox(unary.getExpression)
          val folded = (unary.getKind, value.constant) match {
            case (Tree.Kind.UNARY_MINUS, Some(number: Long)) => Value.constant(-number)
            case (Tree.Kind.UNARY_PLUS, Some(number: Long))  => Value.constant(number)
            case _                                           => Value.Unknown
          }
          (folded, after)
      }

    case binary: BinaryTree => binaryExpression(binary, env)

    case conditional: ConditionalExpressionTree =>
      val (_, whenTrue, whenFalse) = condition(conditional.getCondition, env)
      val left                     = whenTrue.map(expression(conditional.getTrueExpression, _))
      val right                    = whenFalse.map(expression(conditional.getFalseExpression, _))
      val value = (left.map(_._1) ++ right.map(_._1)).reduceOption(_.join(_)).getOrElse(Value.Unknown)
      (value, join(left.map(_._2), right.map(_._2)).getOrElse(env))

    case cast: TypeCastTree =>
      val (value, after) = expression(cast.getExpression, env)
      unboxing(cast.getExpression, typeOf(cast))
      (value, after)

    case instanceOf: InstanceOfTree =>
      val (_, after) = expression(instanceOf.getExpression, env)
      (Value.Unknown, after)

    case lambda: LambdaExpressionTree =>
      val saved = (targets, functions, lambdas)
      targets = Nil
      functions = List(None)
      lambdas = lambda :: lambdas
      lambda.getBody match {
        case body: ExpressionTree => expression(body, env): Unit
        case body: StatementTree  => statement(body, Some(env)): Unit
        case _                    => ()
      }
      targets = saved._1
      functions = saved._2
      lambdas = saved._3
      (Value.NonNull, env)

    case reference: MemberReferenceTree =>
      if !isTypeName(reference.getQualifierExpression) then {
        expression(reference.getQualifierExpression, env): Unit
        use(reference.getQualifierExpression, "a method reference")
      }
      (Value.NonNull, env)

    case switchTree: SwitchExpressionTree =>
      val (_, after)   = expression(switchTree.getExpression, env)
      val (value, out) = switchCases(switchTree.getCases.asScala.toList, after, expressionSwitch = true)
      (value, out.getOrElse(after))

    case _ => (Value.Unknown, env)
  }

  private def arguments(args: List[ExpressionTree], callee: Option[Element], env: Env): Env = {
    val parameters = callee.collect { case executable: ExecutableElement => executable }
    args.zipWithIndex.foldLeft(env) { case (current, (argument, index)) =>
      val (_, next) = expression(argument, current)
      parameters.foreach { executable =>
        val declaredParameters = executable.getParameters.asScala.toVector
        val parameter          =
          if executable.isVarArgs && index >= declaredParameters.size - 1 then None
          else declaredParameters.lift(index)
        parameter.foreach { parameter =>
          once(argument)(passes += Pass(argument, parameter, executable))
          unboxing(argument, parameter.asType)
        }
      }
      next
    }
  }

  private def call(invocation: MethodInvocationTree, env: Env): (Value, Env) = {
    val executable    = element(invocation.getMethodSelect).collect { case executable: ExecutableElement => executable }
    val afterReceiver = invocation.getMethodSelect match {
      case select: MemberSelectTree if !isTypeName(select.getExpression) =>
        val (_, a) = expression(select.getExpression, env)
        use(select.getExpression, s"the method ${select.getIdentifier}")
        refine(select.getExpression, a, Value.NonNull)
      case _ => env
    }
    val afterArguments = forgetFields(arguments(invocation.getArguments.asScala.toList, executable, afterReceiver))
    val objects        =
      executable.exists(method => Option(method.getEnclosingElement).exists(_.toString == "java.util.Objects"))
    val named = executable.map(_.getSimpleName.toString).getOrElse("")
    if objects && named == "requireNonNullElse" then
      invocation.getArguments.asScala.headOption.foreach(argument =>
        once(invocation)(defaults += Default(invocation, argument))
      )
    val narrowed =
      if objects && named == "requireNonNull" then
        invocation.getArguments.asScala.headOption.fold(afterArguments)(refine(_, afterArguments, Value.NonNull))
      else afterArguments
    (executable.map(result(invocation, _)).getOrElse(Value.Unknown), narrowed)
  }

  /** The value a call returns, by what its declaration and the type arguments of the call say. */
  private def result(invocation: MethodInvocationTree, method: ExecutableElement): Value =
    if Nullness.knownNonNull(method) then Value.NonNull
    else if method.getModifiers.contains(Modifier.NATIVE) then Value.Unknown
    else if Nullness.hasAnnotation(method, Nullness.Nullable) then Value(Null.Maybe)
    else if Nullness.hasBoundary(method) then
      if method.getReturnType.getKind == TypeKind.TYPEVAR then Value(Null.Maybe) else Value.Unknown
    else if method.getReturnType.getKind == TypeKind.TYPEVAR then {
      val bound = binding(invocation, method)
      if Nullness.hasAnnotation(method.getReturnType, Nullness.Nullable) then Value(Null.Maybe)
      else if bound.exists(Nullness.nullableType) || Nullness.nullableType(typeOf(invocation)) then Value(Null.Maybe)
      else if bound.exists(tpe => tpe.getKind != TypeKind.TYPEVAR) && marked(method) then Value.NonNull
      else Value.Unknown
    } else if Nullness.nullableType(method.getReturnType) then Value(Null.Maybe)
    else if method.getReturnType.getKind.isPrimitive || marked(method) then Value.NonNull
    else Value.Unknown

  /** The type argument a call gives the type variable its method returns, read from the declared types of the
    * arguments, which keep the annotations of the type arguments that the inferred type of the call loses.
    */
  private def binding(invocation: MethodInvocationTree, method: ExecutableElement): Option[TypeMirror] = {
    val variable = method.getReturnType.asInstanceOf[TypeVariable].asElement
    def unify(parameter: TypeMirror, argument: TypeMirror): Option[TypeMirror] =
      (parameter, argument) match {
        case (found: TypeVariable, _) if found.asElement == variable                                  => Some(argument)
        case (declared: DeclaredType, actual: DeclaredType) if declared.asElement == actual.asElement =>
          declared.getTypeArguments.asScala.zip(actual.getTypeArguments.asScala).iterator
            .flatMap((expected, actualArgument) => unify(expected, actualArgument)).nextOption()
        case (array: ArrayType, actual: ArrayType) => unify(array.getComponentType, actual.getComponentType)
        case (wildcard: WildcardType, _)           => Option(wildcard.getExtendsBound).flatMap(unify(_, argument))
        case (_, wildcard: WildcardType)           => Option(wildcard.getExtendsBound).flatMap(unify(parameter, _))
        case _                                     => None
      }
    method.getParameters.asScala.zip(invocation.getArguments.asScala).iterator.flatMap { (parameter, argument) =>
      val actual = stripped(argument) match {
        case _: IdentifierTree | _: MemberSelectTree =>
          element(stripped(argument)).map(_.asType).orElse(Option(typeOf(argument)))
        case other => Option(typeOf(other))
      }
      actual.flatMap(unify(parameter.asType, _))
    }.nextOption()
  }

  private def binaryExpression(binary: BinaryTree, env: Env): (Value, Env) =
    binary.getKind match {
      case Tree.Kind.CONDITIONAL_AND | Tree.Kind.CONDITIONAL_OR | Tree.Kind.EQUAL_TO | Tree.Kind.NOT_EQUAL_TO =>
        val (value, whenTrue, whenFalse) = condition(binary, env)
        (value, join(whenTrue, whenFalse).getOrElse(env))
      case kind =>
        val (left, first)  = expression(binary.getLeftOperand, env)
        val (right, after) = expression(binary.getRightOperand, first)
        val textual        = kind == Tree.Kind.PLUS &&
          (isText(binary.getLeftOperand) || isText(binary.getRightOperand))
        if !textual then {
          unbox(binary.getLeftOperand)
          unbox(binary.getRightOperand)
        }
        val folded = (left.constant, right.constant) match {
          case (Some(a: Long), Some(b: Long)) => fold(kind, a, b)
          case _ if textual                   => Value.NonNull
          case _                              => Value.Unknown
        }
        (folded, after)
    }

  private def isText(tree: ExpressionTree): Boolean = {
    val tpe = typeOf(tree)
    tpe != null && tpe.getKind == TypeKind.DECLARED && tpe.toString == "java.lang.String"
  }

  private def fold(kind: Tree.Kind, a: Long, b: Long): Value = kind match {
    case Tree.Kind.PLUS                => Value.constant(a + b)
    case Tree.Kind.MINUS               => Value.constant(a - b)
    case Tree.Kind.MULTIPLY            => Value.constant(a * b)
    case Tree.Kind.DIVIDE if b != 0    => Value.constant(a / b)
    case Tree.Kind.REMAINDER if b != 0 => Value.constant(a % b)
    case Tree.Kind.LESS_THAN           => Value.constant(a < b)
    case Tree.Kind.LESS_THAN_EQUAL     => Value.constant(a <= b)
    case Tree.Kind.GREATER_THAN        => Value.constant(a > b)
    case Tree.Kind.GREATER_THAN_EQUAL  => Value.constant(a >= b)
    case _                             => Value.Unknown
  }

  // -- conditions

  private def booleanValue(constant: Option[Boolean]): Value =
    constant.fold(Value.Unknown)(Value.constant(_))

  /** Evaluates a condition, returning its value and the environments where it holds and where it does not; an
    * environment is empty where a literal settles that the condition cannot take that value.
    */
  private def condition(tree: ExpressionTree, env: Env): (Value, Option[Env], Option[Env]) = {
    val (value, whenTrue, whenFalse) = branch(tree, env)
    (record(tree, value), whenTrue, whenFalse)
  }

  private def branch(tree: ExpressionTree, env: Env): (Value, Option[Env], Option[Env]) =
    tree match {
      case parenthesized: ParenthesizedTree => condition(parenthesized.getExpression, env)

      case unary: UnaryTree if unary.getKind == Tree.Kind.LOGICAL_COMPLEMENT =>
        val (value, whenTrue, whenFalse) = condition(unary.getExpression, env)
        (booleanValue(value.constant.collect { case flag: Boolean => !flag }), whenFalse, whenTrue)

      case binary: BinaryTree if binary.getKind == Tree.Kind.CONDITIONAL_AND =>
        val (left, leftTrue, leftFalse) = condition(binary.getLeftOperand, env)
        leftTrue match {
          case None        => (left, None, leftFalse)
          case Some(entry) =>
            val (right, rightTrue, rightFalse) = condition(binary.getRightOperand, entry)
            val constant                       = (left.constant, right.constant) match {
              case (Some(false), _) | (_, Some(false)) => Some(false)
              case (Some(true), Some(true))            => Some(true)
              case _                                   => None
            }
            (booleanValue(constant), rightTrue, join(leftFalse, rightFalse))
        }

      case binary: BinaryTree if binary.getKind == Tree.Kind.CONDITIONAL_OR =>
        val (left, leftTrue, leftFalse) = condition(binary.getLeftOperand, env)
        leftFalse match {
          case None        => (left, leftTrue, None)
          case Some(entry) =>
            val (right, rightTrue, rightFalse) = condition(binary.getRightOperand, entry)
            val constant                       = (left.constant, right.constant) match {
              case (Some(true), _) | (_, Some(true)) => Some(true)
              case (Some(false), Some(false))        => Some(false)
              case _                                 => None
            }
            (booleanValue(constant), join(leftTrue, rightTrue), rightFalse)
        }

      case binary: BinaryTree
          if binary.getKind == Tree.Kind.EQUAL_TO || binary.getKind == Tree.Kind.NOT_EQUAL_TO =>
        val equal   = binary.getKind == Tree.Kind.EQUAL_TO
        val subject = (stripped(binary.getLeftOperand), stripped(binary.getRightOperand)) match {
          case (left, literal: LiteralTree) if literal.getKind == Tree.Kind.NULL_LITERAL  => Some(left)
          case (literal: LiteralTree, right) if literal.getKind == Tree.Kind.NULL_LITERAL => Some(right)
          case _                                                                          => None
        }
        subject match {
          case Some(operand) =>
            val (value, after) = expression(operand, env)
            val outcome        = value.nullness match {
              case Null.Null    => Some(equal)
              case Null.NonNull => Some(!equal)
              case _            => None
            }
            once(binary)(comparisons += Compare(binary, operand, equal))
            val isNull                = refine(operand, after, Value.Nil)
            val notNull               = refine(operand, after, Value.NonNull)
            val (whenTrue, whenFalse) = if equal then (isNull, notNull) else (notNull, isNull)
            (booleanValue(outcome), Some(whenTrue), Some(whenFalse))
          case None =>
            val (left, first)  = expression(binary.getLeftOperand, env)
            val (right, after) = expression(binary.getRightOperand, first)
            val lt             = typeOf(binary.getLeftOperand)
            val rt             = typeOf(binary.getRightOperand)
            if lt != null && rt != null then {
              if lt.getKind.isPrimitive then unbox(binary.getRightOperand)
              if rt.getKind.isPrimitive then unbox(binary.getLeftOperand)
            }
            val constant = (left.constant, right.constant) match {
              case (Some(a), Some(b)) if a.isInstanceOf[Long] == b.isInstanceOf[Long] => Some((a == b) == equal)
              case _                                                                  => None
            }
            (booleanValue(constant), Some(after), Some(after))
        }

      case instanceOf: InstanceOfTree =>
        val (_, after) = expression(instanceOf.getExpression, env)
        (Value.Unknown, Some(refine(instanceOf.getExpression, after, Value.NonNull)), Some(after))

      case literal: LiteralTree if literal.getKind == Tree.Kind.BOOLEAN_LITERAL =>
        val flag = literal.getValue.asInstanceOf[Boolean]
        (Value.constant(flag), if flag then Some(env) else None, if flag then None else Some(env))

      case invocation: MethodInvocationTree
          if element(invocation.getMethodSelect).exists(el =>
            el.getSimpleName.toString.matches("isNull|nonNull") &&
              Option(el.getEnclosingElement).exists(_.toString == "java.util.Objects")
          ) =>
        val (value, after) = call(invocation, env)
        val checked        = invocation.getArguments.asScala.headOption
        val positive       = element(invocation.getMethodSelect).exists(_.getSimpleName.contentEquals("nonNull"))
        checked.fold((value, Some(after), Some(after))) { operand =>
          val isNull  = refine(operand, after, Value.Nil)
          val notNull = refine(operand, after, Value.NonNull)
          (Value.Unknown, Some(if positive then notNull else isNull), Some(if positive then isNull else notNull))
        }

      case other =>
        val (value, after) = expression(other, env)
        (value, Some(after), Some(after))
    }
}
