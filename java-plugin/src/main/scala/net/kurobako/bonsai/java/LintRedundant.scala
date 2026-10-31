package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.TreePathScanner
import com.sun.source.util.TreeScanner
import javax.lang.model.`type`.DeclaredType
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.Element
import javax.lang.model.element.ElementKind
import javax.lang.model.element.ExecutableElement
import javax.lang.model.element.Modifier
import javax.lang.model.element.TypeElement
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** The lints of the verbose and redundant code of the Java, which the code of a type holds.
  *
  * The lints are of the code of one file, as the trees the compiler gives are of one compilation; the rules that need
  * the evidence of the whole build are of the analysis, which holds the model of the build.
  */
object LintRedundant {

  /** The lints of the verbose and redundant code of a type. */
  val lints: Vector[Lint] = Vector(
    RedundantModifier,
    RedundantThis,
    UnnecessaryBoxing,
    MissingOverride,
    UnusedAssignment,
    FieldCanBeLocal,
    RedundantMethodOverride,
    CanBeFinal,
    Convert2Diamond,
    UnnecessaryUnboxing
  )

  /** The lints of the verbose and redundant code of the file as a whole. */
  val fileLints: Vector[Lint] = Vector(UnusedImport, UnnecessaryUnicodeEscape, DanglingDocComment)

  /** A modifier that the code around it already implies. */
  private object RedundantModifier extends Lint {
    val id    = JavaLint.RedundantModifier.id
    val kinds = Set(Tree.Kind.CLASS, Tree.Kind.INTERFACE, Tree.Kind.METHOD, Tree.Kind.VARIABLE)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val element = context.element
      if element == null then Vector.empty
      else {
        // The modifiers of the element hold the ones the code around it implies (the members of an interface are public
        // and abstract already), so the ones the code writes are read of the tree.
        val modifiers = tree match {
          case method: MethodTree     => method.getModifiers.getFlags.asScala.toSet
          case clazz: ClassTree       => clazz.getModifiers.getFlags.asScala.toSet
          case variable: VariableTree => variable.getModifiers.getFlags.asScala.toSet
          case _                      => Set.empty[Modifier]
        }
        // The members of an interface are public and abstract already, and the fields of one are static and final.
        val inInterface = Option(element.getEnclosingElement).exists(_.getKind == ElementKind.INTERFACE)
        val redundant   = if inInterface then
          element.getKind match {
            case ElementKind.METHOD => modifiers.contains(Modifier.PUBLIC) || modifiers.contains(Modifier.ABSTRACT)
            case ElementKind.FIELD  =>
              modifiers.contains(Modifier.PUBLIC) || modifiers.contains(Modifier.STATIC) ||
              modifiers.contains(Modifier.FINAL)
            case ElementKind.CONSTRUCTOR => modifiers.contains(Modifier.PUBLIC)
            case _                       => false
          }
        else false
        if redundant then Vector(context.found("the modifier is already implied by the code around it"))
        else Vector.empty
      }
    }
  }

  /** A qualifier of a member that the code around it already holds. */
  private object RedundantThis extends Lint {
    val id    = JavaLint.RedundantThis.id
    val kinds = Set(Tree.Kind.MEMBER_SELECT)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val select = tree.asInstanceOf[MemberSelectTree]
      val target = select.getExpression
      val isThis = Call.isThis(target)
      if !isThis then Vector.empty
      else {
        // A local variable or a parameter of the same name hides the member, so the qualifier is needed then.
        val name  = select.getIdentifier.toString
        val hides = context.enclosing(Set(Tree.Kind.METHOD)).exists { method =>
          val methodTree = method.asInstanceOf[MethodTree]
          val body       = methodTree.getBody
          methodTree.getParameters.asScala.exists(_.getName.toString == name) ||
          (body != null && LintRedundant.locals(body).contains(name))
        }
        // The block around the qualifier writes its members with the qualifier or without it, and the qualifier it
        // writes is the way of the block: removing it would leave one statement that does not name `this` among the
        // ones that do. The qualifier is the odd one out only when the block writes more than twice as many members
        // without it, or when it names no other member and there is no way of the block to keep.
        val kind                           = Option(context.element).map(_.getKind).orNull
        val (qualified, unqualified, bare) =
          context.enclosing(Set(Tree.Kind.BLOCK)).fold((0, 0, 0))(block => memberStyle(context, block, kind))
        val odd = (qualified > 0 && unqualified >= 2 * qualified) ||
          (qualified == 1 && unqualified == 0 && bare == 0)
        if hides || !odd then Vector.empty else Vector(context.found(s"the qualifier of $name is not needed"))
      }
    }
  }

  /** The numbers of the members the block writes with the qualifier and without it, which is the way it writes them,
    * and the number of times it names `this` on its own, which shows that it writes the receiver out.
    */
  private def memberStyle(context: Context, block: Tree, kind: ElementKind): (Int, Int, Int) = {
    val blockPath = context.trees.getPath(context.unit, block)
    if blockPath == null then (0, 0, 0)
    else {
      var qualified   = 0
      var unqualified = 0
      var bare        = 0
      val scanner     = new TreeScanner[Unit, Unit] {
        override def visitMemberSelect(node: MemberSelectTree, unused: Unit): Unit =
          node.getExpression match {
            case identifier: IdentifierTree if Call.isThis(identifier) =>
              qualified += 1
            // The expression of the select is the receiver, which the scanner would read as a name of its own.
            case _ => super.visitMemberSelect(node, unused)
          }

        override def visitIdentifier(node: IdentifierTree, unused: Unit): Unit = {
          val name = node.getName.toString
          if name == "this" then bare += 1
          else {
            // `super` is not a member of the type, although the compiler models the receiver as a field of it.
            val element =
              if name == "super" then null else context.trees.getElement(new TreePath(blockPath, node))
            if element != null && element.getKind == kind then unqualified += 1
          }
          super.visitIdentifier(node, unused)
        }
      }
      scanner.scan(block, ())
      (qualified, unqualified, bare)
    }
  }

  /** The names the locals of a block have, which the lints that care of what hides a member read. */
  private def locals(body: Tree): Set[String] = {
    val names   = mutable.Set.empty[String]
    val scanner = new TreeScanner[Unit, Unit] {
      override def visitVariable(node: VariableTree, unused: Unit): Unit = {
        names += node.getName.toString
        super.visitVariable(node, unused)
      }
    }
    scanner.scan(body, ())
    names.toSet
  }

  /** A value that is boxed although the code does not need it. */
  private object UnnecessaryBoxing extends Lint {
    val id    = JavaLint.UnnecessaryBoxing.id
    val kinds = Set(Tree.Kind.METHOD_INVOCATION)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val invocation = tree.asInstanceOf[MethodInvocationTree]
      val method     = invocation.getMethodSelect
      // The value of a box is boxed again by the call of valueOf, which the code does not need.
      if LintRedundant.boxing(context, method) && LintRedundant.unneeded(context) then
        Vector(context.found("the value is boxed although the code does not need it"))
      else Vector.empty
    }
  }

  /** Whether the value of a box is wanted as a number, which the box of it is of no use to. */
  private def unneeded(context: Context): Boolean =
    LintRedundant.holder(context) match {
      // A number the code holds does not need the box of the value.
      case Some(variable: VariableTree)     => LintRedundant.number(context.typeOf(variable))
      case Some(assignment: AssignmentTree) => LintRedundant.number(context.typeOf(assignment.getVariable))
      // The method returns a number, which the box of the value is of no use to.
      case Some(_: ReturnTree) =>
        context.enclosing(Set(Tree.Kind.METHOD)).exists { method =>
          method.asInstanceOf[MethodTree].getReturnType.isInstanceOf[PrimitiveTypeTree]
        }
      // The number the code counts with is a number, which the box of the value is of no use to.
      case Some(binary: BinaryTree) =>
        val other = if binary.getLeftOperand == context.tree then binary.getRightOperand else binary.getLeftOperand
        LintRedundant.number(context.typeOf(other))
      // The parameter of the call is a number, which the box of the value is of no use to.
      case Some(invocation: MethodInvocationTree) =>
        LintRedundant.parameter(context, invocation).exists(LintRedundant.number)
      case _ => false
    }

  /** The code the value of a box is used by, which the parentheses of the code hold as a tree of their own. */
  private def holder(context: Context): Option[Tree] = Lints.enclosing(context.path.getParentPath).map(_.getLeaf)

  /** The type of the parameter the value of the box is passed as, which the call of the value holds. */
  private def parameter(context: Context, invocation: MethodInvocationTree): Option[TypeMirror] = {
    val index = Lints.arguments(invocation).indexWhere(argument => argument == context.tree)
    if index < 0 then None
    else
      context.element match {
        case executable: ExecutableElement =>
          executable.getParameters.asScala.toVector.lift(index).map(_.asType())
        case _ => None
      }
  }

  /** Whether the type is a number or a flag, which the code holds without a box. */
  private def number(tpe: TypeMirror): Boolean = tpe != null && tpe.getKind.isPrimitive

  /** The types whose values are boxes, which the lints that care of the boxing of a value read. */
  private val boxes: Set[String] = Set("Integer", "Long", "Short", "Byte", "Character", "Boolean", "Double", "Float")

  /** Whether the call is `valueOf` of one of the boxes. */
  def boxing(context: Context, method: ExpressionTree): Boolean =
    Call.named(method, "valueOf") &&
      boxes.contains(Option(context.element).map(_.getEnclosingElement).map(_.getSimpleName.toString).getOrElse(""))

  val qualifiedBoxes: Set[String] = boxes.map(name => s"java.lang.$name")

  /** A method that overrides a method of a supertype and does not say so. */
  private object MissingOverride extends Lint {
    val id    = JavaLint.MissingOverride.id
    val kinds = Set(Tree.Kind.METHOD)

    def check(context: Context, tree: Tree): Vector[Found] =
      context.element match {
        // A constructor is not a method, and it overrides no method of a supertype.
        case method: ExecutableElement if method.getKind == ElementKind.METHOD =>
          // A static method hides the method of the supertype, and a private one is of its own type, so neither of them
          // overrides a method of a supertype.
          val hides = method.getModifiers.contains(Modifier.STATIC) || method.getModifiers.contains(Modifier.PRIVATE)
          // The compiler reads the method as one of the supertype, but the code says nothing of it.
          if !hides && Collector.overriding(method) && !LintRedundant.overridden(method) then
            Vector(context.found("the method overrides the one of the supertype and does not say so"))
          else Vector.empty
        case _ => Vector.empty
      }
  }

  /** Whether a method says that it overrides a method of a supertype, which the annotation of it says. */
  private def overridden(element: Element): Boolean =
    element.getAnnotationMirrors.asScala.exists(_.getAnnotationType.toString == "java.lang.Override")

  /** An initializer of a field that gives it the value it has anyway. */
  private object UnusedAssignment extends Lint {
    val id    = JavaLint.UnusedAssignment.id
    val kinds = Set(Tree.Kind.VARIABLE)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val variable = tree.asInstanceOf[VariableTree]
      // A local needs its initializer, and a final field cannot be left without one. The field of a type that is not
      // final starts with the default value of its type, which an initializer that writes that value repeats.
      val field = context.ancestors.headOption.exists(_.isInstanceOf[ClassTree])
      val fixed = Option(context.element).exists(_.getModifiers.contains(Modifier.FINAL))
      // MissingFieldInitializer asks for the start value of the state of the type to be written.
      if !field || fixed || LintBugs.MissingFieldInitializer.required(context.element) then Vector.empty
      else
        Option(variable.getInitializer).filter(default).map { initializer =>
          context.found(s"the initializer $initializer of the field ${variable.getName} is redundant")
        }.toVector
    }

    /** Whether the expression is the value a field of its type has before the code gives it one. */
    private def default(expression: ExpressionTree): Boolean = expression match {
      case literal: LiteralTree =>
        literal.getKind match {
          case Tree.Kind.NULL_LITERAL    => true
          case Tree.Kind.BOOLEAN_LITERAL => literal.getValue == java.lang.Boolean.FALSE
          case Tree.Kind.CHAR_LITERAL    => literal.getValue == java.lang.Character.valueOf('\u0000')
          case Tree.Kind.INT_LITERAL | Tree.Kind.LONG_LITERAL | Tree.Kind.FLOAT_LITERAL |
              Tree.Kind.DOUBLE_LITERAL =>
            literal.getValue.asInstanceOf[java.lang.Number].doubleValue == 0.0
          case _ => false
        }
      case _ => false
    }
  }

  /** An override that only calls the method it overrides. */
  private object RedundantMethodOverride extends Lint {
    val id    = JavaLint.RedundantMethodOverride.id
    val kinds = Set(Tree.Kind.METHOD)

    def check(context: Context, tree: Tree): Vector[Found] = tree match {
      case method: MethodTree if !method.getName.contentEquals("<init>") && method.getBody != null =>
        called(method) match {
          case Some(invocation) if callsSuper(method, invocation) =>
            Vector(context.found(s"the method ${method.getName} only calls the method it overrides"))
          case _ => Vector.empty
        }
      case _ => Vector.empty
    }

    /** The one call of the body, when the body is only that call or its return. */
    private def called(method: MethodTree): Option[MethodInvocationTree] = method.getBody match {
      case block: BlockTree if block.getStatements.size == 1 =>
        block.getStatements.get(0) match {
          case ret: ReturnTree => ret.getExpression match {
              case invocation: MethodInvocationTree => Some(invocation)
              case _                                => None
            }
          case statement: ExpressionStatementTree => statement.getExpression match {
              case invocation: MethodInvocationTree => Some(invocation)
              case _                                => None
            }
          case _ => None
        }
      case _ => None
    }

    /** Whether the call is `super` with the method's own parameters, in their order. */
    private def callsSuper(method: MethodTree, invocation: MethodInvocationTree): Boolean =
      invocation.getMethodSelect match {
        case select: MemberSelectTree
            if select.getExpression.toString == "super" &&
              select.getIdentifier.contentEquals(method.getName) =>
          val parameters = method.getParameters.asScala.toVector.map(_.getName.toString)
          val arguments  = Lints.arguments(invocation)
          arguments.size == parameters.size && arguments.zip(parameters).forall {
            case (argument: IdentifierTree, name) => argument.getName.contentEquals(name)
            case _                                => false
          }
        case _ => false
      }
  }

  /** A private field whose every use is in one method where every read is preceded by a write on every path. */
  private object FieldCanBeLocal extends Lint {
    val id    = JavaLint.FieldCanBeLocal.id
    val kinds = Set(Tree.Kind.VARIABLE)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val variable = tree.asInstanceOf[VariableTree]
      val field    = context.element
      val owner    = context.ancestors.collectFirst { case clazz: ClassTree => clazz }
      if field == null || owner.isEmpty || variable.getInitializer != null ||
        !field.getModifiers.contains(Modifier.PRIVATE) ||
        field.getModifiers.contains(Modifier.STATIC) ||
        field.getModifiers.contains(Modifier.VOLATILE)
      then Vector.empty
      else {
        val uses = mutable.ArrayBuffer.empty[(Option[Long], Boolean, String, Option[MethodTree])]
        val root = new TreePath(new TreePath(context.unit), owner.get)
        new TreePathScanner[Unit, Unit] {
          private def checkLeaf(): Unit = {
            val path = getCurrentPath
            if context.trees.getElement(path) == field then {
              var method: Option[MethodTree] = None
              var nested                     = false
              var cursor                     = path.getParentPath
              while cursor != null && method.isEmpty do {
                cursor.getLeaf match {
                  case found: MethodTree                      => method = Some(found)
                  case _: LambdaExpressionTree                => nested = true
                  case clazz: ClassTree if clazz ne owner.get => nested = true
                  case _                                      => ()
                }
                cursor = cursor.getParentPath
              }
              val start = method.flatMap(found =>
                Option(context.trees.getSourcePositions.getStartPosition(context.unit, found))
                  .filter(_ >= 0)
                  .map(_.toLong)
              )
              val name = method.map(found =>
                if found.getName.contentEquals("<init>") then "the constructor" else found.getName.toString
              ).getOrElse("an initializer")
              uses += ((start, nested, name, method))
            }
          }

          override def visitIdentifier(node: IdentifierTree, unused: Unit): Unit = {
            checkLeaf()
            super.visitIdentifier(node, unused)
          }

          override def visitMemberSelect(node: MemberSelectTree, unused: Unit): Unit = {
            checkLeaf()
            super.visitMemberSelect(node, unused)
          }
        }.scan(root, ())
        val methods = uses.map(_._1).distinct
        val names   = uses.map(_._3).distinct
        if uses.isEmpty || uses.exists(_._2) || methods.size != 1 || methods.head.isEmpty ||
          names.head == "the constructor"
        then
          Vector.empty
        else {
          val targetMethod = uses.head._4
          if targetMethod.isEmpty || targetMethod.get.getBody == null then Vector.empty
          else {
            val (readBeforeWrite, readCount) = analyzeMethod(context, field, targetMethod.get.getBody)
            if !readBeforeWrite && readCount > 0 then
              Vector(context.found(
                s"the field ${variable.getName} is only used in ${names.head}, so it can be a local variable"
              ))
            else Vector.empty
          }
        }
      }
    }

    private final case class State(written: Boolean, dead: Boolean)

    private def join(s1: State, s2: State): State =
      if s1.dead && s2.dead then State(written = true, dead = true)
      else if s1.dead then s2
      else if s2.dead then s1
      else State(written = s1.written && s2.written, dead = false)

    private def analyzeMethod(context: Context, field: Element, body: BlockTree): (Boolean, Int) = {
      var readBeforeWrite = false
      var readCount       = 0

      def names(tree: Tree): Boolean =
        Option(context.trees.getPath(
          context.unit,
          tree
        )).flatMap(p => Option(context.trees.getElement(p))).contains(field)

      def isField(tree: ExpressionTree): Boolean =
        if tree == null then false
        else
          Lints.unparenthesised(tree) match {
            case id: IdentifierTree    => id.getName.contentEquals(field.getSimpleName) && names(id)
            case sel: MemberSelectTree => sel.getIdentifier.contentEquals(field.getSimpleName) && names(sel)
            case _                     => false
          }

      def checkExpr(tree: ExpressionTree, s: State): State =
        if tree == null || s.dead then s
        else
          Lint.unwrapped(tree) match {
            case assignment: AssignmentTree =>
              if isField(assignment.getVariable) then {
                val sAfterRhs = checkExpr(assignment.getExpression, s)
                sAfterRhs.copy(written = true)
              } else {
                val sLeft = checkExpr(assignment.getVariable, s)
                checkExpr(assignment.getExpression, sLeft)
              }
            case compound: CompoundAssignmentTree =>
              val sLeft = checkExpr(compound.getVariable, s)
              val sRhs  = checkExpr(compound.getExpression, sLeft)
              if isField(compound.getVariable) then sRhs.copy(written = true) else sRhs
            case unary: UnaryTree =>
              val sExpr = checkExpr(unary.getExpression, s)
              if isField(unary.getExpression) then sExpr.copy(written = true) else sExpr
            case binary: BinaryTree =>
              val sLeft = checkExpr(binary.getLeftOperand, s)
              checkExpr(binary.getRightOperand, sLeft)
            case method: MethodInvocationTree =>
              val sSelect = checkExpr(method.getMethodSelect, s)
              Lints.arguments(method).foldLeft(sSelect)((curr, arg) => checkExpr(arg, curr))
            case newClass: NewClassTree =>
              val sEncl = Option(newClass.getEnclosingExpression).fold(s)(checkExpr(_, s))
              Lints.arguments(newClass).foldLeft(sEncl)((curr, arg) => checkExpr(arg, curr))
            case cond: ConditionalExpressionTree =>
              val sCond  = checkExpr(cond.getCondition, s)
              val sTrue  = checkExpr(cond.getTrueExpression, sCond)
              val sFalse = checkExpr(cond.getFalseExpression, sCond)
              join(sTrue, sFalse)
            case inst: InstanceOfTree =>
              checkExpr(inst.getExpression, s)
            case arr: ArrayAccessTree =>
              val sArr = checkExpr(arr.getExpression, s)
              checkExpr(arr.getIndex, sArr)
            case newArr: NewArrayTree =>
              val sDims = newArr.getDimensions.asScala.foldLeft(s)((curr, d) => checkExpr(d, curr))
              Option(newArr.getInitializers).map(_.asScala).getOrElse(Nil).foldLeft(sDims)((curr, init) =>
                checkExpr(init, curr)
              )
            case sel: MemberSelectTree =>
              if isField(sel) then {
                if !s.written then readBeforeWrite = true
                else readCount += 1
                s
              } else checkExpr(sel.getExpression, s)
            case id: IdentifierTree =>
              if isField(id) then {
                if !s.written then readBeforeWrite = true
                else readCount += 1
              }
              s
            case _ => s
          }

      def checkStmt(tree: StatementTree, s: State): State =
        if tree == null || s.dead then s
        else
          tree match {
            case block: BlockTree =>
              block.getStatements.asScala.foldLeft(s)((curr, stmt) => checkStmt(stmt, curr))
            case exprStmt: ExpressionStatementTree =>
              checkExpr(exprStmt.getExpression, s)
            case varTree: VariableTree =>
              Option(varTree.getInitializer).fold(s)(checkExpr(_, s))
            case ifTree: IfTree =>
              val sCond = checkExpr(ifTree.getCondition, s)
              val sThen = checkStmt(ifTree.getThenStatement, sCond)
              val sElse = if ifTree.getElseStatement != null then checkStmt(ifTree.getElseStatement, sCond) else sCond
              join(sThen, sElse)
            case whileLoop: WhileLoopTree =>
              val _ = checkExpr(whileLoop.getCondition, s)
              val _ = checkStmt(whileLoop.getStatement, s)
              s
            case forLoop: ForLoopTree =>
              val sInit = forLoop.getInitializer.asScala.foldLeft(s)((curr, init) => checkStmt(init, curr))
              if forLoop.getCondition != null then { val _ = checkExpr(forLoop.getCondition, sInit) }
              val _ = checkStmt(forLoop.getStatement, sInit)
              forLoop.getUpdate.asScala.foreach(up => checkExpr(up.getExpression, sInit))
              s
            case enhancedFor: EnhancedForLoopTree =>
              val sExpr = checkExpr(enhancedFor.getExpression, s)
              val _     = checkStmt(enhancedFor.getStatement, sExpr)
              sExpr
            case doWhile: DoWhileLoopTree =>
              val sBody = checkStmt(doWhile.getStatement, s)
              val _     = checkExpr(doWhile.getCondition, sBody)
              sBody
            case tryTree: TryTree =>
              val sRes = tryTree.getResources.asScala.foldLeft(s)((curr, res) =>
                res match {
                  case v: VariableTree   => Option(v.getInitializer).fold(curr)(checkExpr(_, curr))
                  case e: ExpressionTree => checkExpr(e, curr)
                  case _                 => curr
                }
              )
              val _ = checkStmt(tryTree.getBlock, sRes)
              tryTree.getCatches.asScala.foreach(c => checkStmt(c.getBlock, sRes))
              if tryTree.getFinallyBlock != null then checkStmt(tryTree.getFinallyBlock, sRes)
              else sRes
            case returnTree: ReturnTree =>
              val sRet = Option(returnTree.getExpression).fold(s)(checkExpr(_, s))
              sRet.copy(dead = true)
            case throwTree: ThrowTree =>
              val sThrow = checkExpr(throwTree.getExpression, s)
              sThrow.copy(dead = true)
            case switchTree: SwitchTree =>
              val sExpr = checkExpr(switchTree.getExpression, s)
              switchTree.getCases.asScala.foreach { c =>
                if c.getCaseKind == CaseTree.CaseKind.RULE then
                  c.getBody match {
                    case body: StatementTree  => checkStmt(body, sExpr)
                    case body: ExpressionTree => checkExpr(body, sExpr)
                    case _                    =>
                  }
                else c.getStatements.asScala.foreach(checkStmt(_, sExpr))
              }
              sExpr
            case _ => s
          }

      val _ = checkStmt(body, State(written = false, dead = false))
      (readBeforeWrite, readCount)
    }
  }

  /** A field or local variable that is never assigned again, so it can be final; a field with no initialiser is one
    * that only the constructors of its class assign, each of them once.
    */
  private object CanBeFinal extends Lint {
    val id    = JavaLint.CanBeFinal.id
    val kinds = Set(Tree.Kind.VARIABLE)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val variable = tree.asInstanceOf[VariableTree]
      val element  = context.element
      val kind     = if element == null then ElementKind.OTHER else element.getKind
      val fixed    = element != null && element.getModifiers.contains(Modifier.FINAL)
      val volatile = element != null && element.getModifiers.contains(Modifier.VOLATILE)
      val visible  = kind == ElementKind.FIELD && !element.getModifiers.contains(Modifier.PRIVATE)
      val valued   = variable.getInitializer != null
      if element == null ||
        (kind != ElementKind.LOCAL_VARIABLE && kind != ElementKind.FIELD) ||
        visible || fixed || volatile ||
        !(valued || (kind == ElementKind.FIELD && Constructors.assigned(context.trees, context.path))) ||
        assigned(context, element, valued)
      then Vector.empty
      else {
        val noun = if kind == ElementKind.FIELD then "field" else "local variable"
        Vector(context.found(s"the $noun ${variable.getName} can be final"))
      }
    }

    /** Whether the code assigns the variable after its declaration, or for a field with no initialiser, other than
      * where its constructors do.
      */
    private def assigned(context: Context, variable: Element, valued: Boolean): Boolean =
      if variable.getKind == ElementKind.FIELD then {
        // a field is written through any qualifier by any class of the file, not only the one that declares it
        val root = new TreePath(context.unit)
        if valued then Effects.assigns(context, root, variable) else Constructors.outside(context.trees, root, variable)
      } else
        context.ancestors.collectFirst {
          case scope: (MethodTree | ClassTree) => new TreePath(new TreePath(context.unit), scope)
        }.exists(root => Effects.assigns(context, root, variable))
  }

  /** An explicit constructor type argument that the target type already gives, which `<>` says. */
  private object Convert2Diamond extends Lint {
    val id    = JavaLint.Convert2Diamond.id
    val kinds = Set(Tree.Kind.NEW_CLASS)

    def check(context: Context, tree: Tree): Vector[Found] = tree match {
      case newClass: NewClassTree =>
        newClass.getIdentifier match {
          case parameterized: ParameterizedTypeTree if explicitTypeArguments(context, parameterized) =>
            context.ancestors.headOption.collect {
              case variable: VariableTree
                  if (newClass.getClassBody == null || Release.atLeast(context, 9)) &&
                    variable.getType.isInstanceOf[ParameterizedTypeTree] &&
                    LintRedundant.rawType(context, variable.getType) ==
                    LintRedundant.rawType(context, newClass.getIdentifier) =>
                context.found("the constructor type arguments are inferred from the target type; use <>")
            }.toVector
          case _ => Vector.empty
        }
      case _ => Vector.empty
    }

    private def explicitTypeArguments(context: Context, parameterized: ParameterizedTypeTree): Boolean =
      !parameterized.getTypeArguments.isEmpty && {
        val positions = context.trees.getSourcePositions
        val typeEnd   = positions.getEndPosition(context.unit, parameterized.getType)
        val paramEnd  = positions.getEndPosition(context.unit, parameterized)
        if typeEnd >= 0 && paramEnd > typeEnd then
          parameterized.getTypeArguments.asScala.forall { arg =>
            val argStart = positions.getStartPosition(context.unit, arg)
            val argEnd   = positions.getEndPosition(context.unit, arg)
            argStart > typeEnd && argEnd <= paramEnd
          }
        else true
      }
  }

  private def rawType(context: Context, tree: Tree): Option[String] =
    context.mirrorOf(tree)
      .collect {
        case declared: javax.lang.model.`type`.DeclaredType =>
          declared.asElement match {
            case element: javax.lang.model.element.TypeElement => element.getQualifiedName.toString
            case _                                             => ""
          }
      }

  /** An explicit unboxing conversion that the context performs anyway. */
  private object UnnecessaryUnboxing extends Lint {
    val id    = JavaLint.UnnecessaryUnboxing.id
    val kinds = Set(Tree.Kind.METHOD_INVOCATION)

    private val Methods =
      Set("booleanValue", "byteValue", "charValue", "doubleValue", "floatValue", "intValue", "longValue", "shortValue")

    def check(context: Context, tree: Tree): Vector[Found] = {
      val invocation = tree.asInstanceOf[MethodInvocationTree]
      val unboxing   = context.element match {
        case method: ExecutableElement if Methods(method.getSimpleName.toString) && invocation.getArguments.isEmpty =>
          invocation.getMethodSelect match {
            case select: MemberSelectTree =>
              Option(context.typeOf(select.getExpression)).collect {
                case declared: DeclaredType
                    if LintRedundant.qualifiedBoxes(declared.asElement match {
                      case element: TypeElement => element.getQualifiedName.toString
                      case _                    => ""
                    }) =>
                  true
              }.getOrElse(false)
            case _ => false
          }
        case _ => false
      }
      if unboxing then Vector(context.found("the unboxing conversion is unnecessary")) else Vector.empty
    }
  }

  /** A Unicode escape that the source can hold as the character itself. */
  private object UnnecessaryUnicodeEscape extends Lint {
    val id    = JavaLint.UnnecessaryUnicodeEscape.id
    val kinds = Set.empty[Tree.Kind]

    private val Escape = java.util.regex.Pattern.compile("\\\\u+([0-9a-fA-F]{4})")

    /** Whether the character can be written in the source as it is: control, format, separator, private use and
      * unassigned characters cannot be seen, or would end the line.
      */
    private def printable(code: Int): Boolean =
      Character.getType(code) match {
        case Character.CONTROL | Character.FORMAT | Character.SURROGATE | Character.PRIVATE_USE |
            Character.UNASSIGNED | Character.LINE_SEPARATOR | Character.PARAGRAPH_SEPARATOR =>
          false
        case _ => true
      }

    def check(context: Context, tree: Tree): Vector[Found] = {
      val text    = context.unit.getSourceFile.getCharContent(true).toString
      val matcher = Escape.matcher(text)
      val found   = Vector.newBuilder[Found]
      while matcher.find() do {
        val code = Integer.parseInt(matcher.group(1), 16)
        // a backslash that an odd number of backslashes precede is itself escaped and starts no escape
        val escaped = (matcher.start - 1 to 0 by -1).takeWhile(text.charAt(_) == '\\').size % 2 == 1
        if !escaped && code >= 0x20 && printable(code) && !"\\\"'\n\r".contains(code.toChar) then
          found += context.found(
            s"Unicode escape ${matcher.group(0)} can be written as ${code.toChar}",
            LintRedundant.containing(context, matcher.start)
          )
      }
      found.result()
    }
  }

  /** A doc comment that documents nothing. */
  private object DanglingDocComment extends Lint {
    val id    = JavaLint.DanglingDocComment.id
    val kinds = Set.empty[Tree.Kind]

    def check(context: Context, tree: Tree): Vector[Found] =
      LintRedundant.danglingDocComments(context.unit.getSourceFile.getCharContent(true).toString).map(offset =>
        Found("the doc comment documents nothing", context.unit, offset)
      )
  }

  /** Where the doc comments of the text start that another doc comment, a closing brace or the end of the text follows,
    * with the comments that are no doc comments skipped as well as the literals that hold the marker of one.
    */
  private def danglingDocComments(text: String): Vector[Int] = {
    val orphans         = Vector.newBuilder[Int]
    var pending         = -1
    var index           = 0
    def release(): Unit = {
      if pending >= 0 then orphans += pending
      pending = -1
    }
    def quoted(quote: Char): Int = {
      var end = index + 1
      while end < text.length && text.charAt(end) != quote && text.charAt(end) != '\n' do
        end += (if text.charAt(end) == '\\' then 2 else 1)
      end + 1
    }
    while index < text.length do {
      val char = text.charAt(index)
      if char.isWhitespace then index += 1
      else if text.startsWith("//", index) then {
        val end = text.indexOf('\n', index)
        index = if end < 0 then text.length else end
      } else if text.startsWith("/*", index) then {
        val close = text.indexOf("*/", index + 2)
        val end   = if close < 0 then text.length else close + 2
        if text.startsWith("/**", index) && close > index + 2 then {
          release()
          pending = index
        }
        index = end
      } else if text.startsWith("\"\"\"", index) then {
        var end = index + 3
        while end < text.length && !text.startsWith("\"\"\"", end) do end += (if text.charAt(end) == '\\' then 2 else 1)
        pending = -1
        index = end + 3
      } else {
        if char == '}' then release() else pending = -1
        index = if char == '"' || char == '\'' then quoted(char) else index + 1
      }
    }
    release()
    orphans.result()
  }

  private def containing(context: Context, offset: Int): Tree = {
    var best      = context.unit: Tree
    var width     = Int.MaxValue
    val positions = context.trees.getSourcePositions
    new TreeScanner[Unit, Unit] {
      override def scan(node: Tree, unused: Unit): Unit =
        if node != null then {
          val start = positions.getStartPosition(context.unit, node)
          val end   = positions.getEndPosition(context.unit, node)
          if start >= 0 && end > start && start <= offset && offset < end && end - start < width then {
            best = node
            width = (end - start).toInt
          }
          super.scan(node, unused)
        }
    }.scan(context.unit, ())
    best
  }

  /** An import that the file never uses. */
  private object UnusedImport extends Lint {
    val id    = JavaLint.UnusedImport.id
    val kinds = Set(Tree.Kind.COMPILATION_UNIT)

    def check(context: Context, tree: Tree): Vector[Found] = {
      val unit = tree.asInstanceOf[CompilationUnitTree]
      val used = LintRedundant.used(unit)
      unit.getImports.asScala.toVector.collect {
        case importTree: ImportTree
            if !importTree.isStatic && !importTree.getQualifiedIdentifier.toString.endsWith("*") =>
          val name = importTree.getQualifiedIdentifier match {
            case select: MemberSelectTree => select.getIdentifier.toString
            case other                    => other.toString
          }
          if !used.contains(name) then
            Some(context.found(s"the import of $name is never used", importTree))
          else None
      }.flatten
    }
  }

  /** The names the code of the file uses, which the lints that care of the imports of the file read. */
  private def used(unit: CompilationUnitTree): Set[String] = {
    val names   = mutable.Set.empty[String]
    val scanner = new TreeScanner[Unit, Unit] {
      override def visitImport(node: ImportTree, unused: Unit): Unit         = ()
      override def visitIdentifier(node: IdentifierTree, unused: Unit): Unit = {
        names += node.getName.toString
        super.visitIdentifier(node, unused)
      }
      override def visitMemberSelect(node: MemberSelectTree, unused: Unit): Unit = {
        names += node.getIdentifier.toString
        super.visitMemberSelect(node, unused)
      }
    }
    scanner.scan(unit, ())
    names.toSet
  }
}
