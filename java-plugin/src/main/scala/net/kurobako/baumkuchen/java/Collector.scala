package net.kurobako.baumkuchen.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.TreePathScanner
import com.sun.source.util.Trees
import javax.lang.model.`type`.DeclaredType
import javax.lang.model.element.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** The collector of the model of a compilation: it walks the trees the compiler gives it and records the types it
  * declares, the methods and the fields they hold, and the calls between them.
  *
  * The names of the nodes are the qualified names the analysis uses, which are the package, the types that hold a node
  * and its own name, joined by dots. A node is a type, as it is in the graph of the Scala code of a build: the members
  * of a type are held by the type, and the references between the types are the ones the members make.
  */
class Collector(trees: Trees) {

  private val nodes       = mutable.LinkedHashMap.empty[String, Model.Node]
  private val edges       = mutable.LinkedHashMap.empty[String, mutable.LinkedHashSet[String]]
  private val methods     = mutable.LinkedHashMap.empty[String, Model.Method]
  private val fields      = mutable.LinkedHashMap.empty[String, Vector[Model.Field]]
  private val sites       = mutable.ArrayBuffer.empty[Model.Site]
  private val clones      = new Clones(trees)
  private val products    = mutable.LinkedHashMap.empty[String, Model.Product]
  private val sums        = mutable.LinkedHashMap.empty[String, Model.Sum]
  private val lints       = mutable.ArrayBuffer.empty[Model.Lint]
  private val reflections = mutable.ArrayBuffer.empty[Model.Reflection]

  /** The model of the compilation, which holds the nodes of every unit the compiler compiled. */
  def model: Model.Model = {
    val found = edges.toVector.flatMap { case (from, tos) => tos.toVector.map(to => Edge(from, to)) }
    Model.Model(
      nodes.values.toVector,
      methods.values.toVector,
      fields.values.toVector.flatten,
      sites.toVector,
      found,
      clones.fragments,
      products.values.toVector,
      sums.values.toVector,
      lints.toVector,
      reflections.toVector
    )
  }

  /** The place a node is defined, which the findings name. */
  def location(compilation: CompilationUnitTree, node: Tree): String = {
    val position = trees.getSourcePositions.getStartPosition(compilation, node)
    val line     = if position < 0 then 0 else compilation.getLineMap.getLineNumber(position).toInt
    s"${Option(compilation.getSourceFile).map(_.toUri.getPath).getOrElse("")}:$line"
  }

  /** Adds the lints the linter found in the code, which the model holds. */
  def lint(found: Vector[Model.Lint]): Unit = lints ++= found

  /** Adds the nodes of the class the compiler has just attributed, which holds the types it declares and the ones it
    * holds. The compiler attributes one type at a time, so the types of the other types of the file are not resolved
    * yet and are left to the events that follow.
    */
  def unit(compilation: CompilationUnitTree, typeElement: TypeElement): Unit =
    Lints.declaration(trees, compilation, typeElement).foreach { declaration =>
      collect(compilation, declaration)
    }

  /** Adds the nodes of the class and of the types it holds, which the compiler has just attributed. The walk is of the
    * declaration alone, as the types of the other types of the file are not resolved yet, and their code is walked when
    * the compiler attributes them.
    */
  private def collect(compilation: CompilationUnitTree, declaration: ClassTree): Unit = {
    val types   = mutable.Stack.empty[String]
    val owners  = mutable.Stack.empty[String]
    val scanner = new TreePathScanner[Unit, Unit] {

      override def visitClass(node: ClassTree, unused: Unit): Unit = {
        val path    = getCurrentPath
        val element = trees.getElement(path)
        val name    = Collector.qualified(element)
        if name.isEmpty then super.visitClass(node, unused)
        else {
          nodes.getOrElseUpdate(
            name,
            Model.Node(
              name,
              Collector.source(compilation),
              Collector.line(trees, compilation, node),
              Collector.exported(element),
              Collector.outer(element),
              Collector.suppressed(element),
              generated = Collector.generated(element)
            )
          ): Unit
          edges.getOrElseUpdate(name, mutable.LinkedHashSet.empty): Unit
          // A record is a product of its components, which the analysis reads alike with a case class.
          if element.getKind == ElementKind.RECORD then
            products.update(
              name,
              Model.Product(name, name, Collector.components(element), Set.empty, Collector.exported(element))
            ): Unit
          // A sealed type is a sum of the types that extend it, which the analysis reads alike with a sealed trait.
          if element.getModifiers.contains(Modifier.SEALED) then
            sums.update(name, Model.Sum(name, name, Collector.variants(element))): Unit
          types.push(name)
          owners.push(name)
          try super.visitClass(node, unused)
          finally {
            owners.pop(): Unit
            types.pop(): Unit
          }
        }
      }

      override def visitMethod(node: MethodTree, unused: Unit): Unit = {
        val path    = getCurrentPath
        val element = trees.getElement(path)
        // The owner of a method is the type that holds it, while the stack of owners holds the methods.
        val owner = types.headOption.getOrElse("")
        val name  = Collector.qualified(element)
        if name.isEmpty then super.visitMethod(node, unused)
        else {
          // A native method is called by the native code, which names it, and a main method is called by the code that
          // runs the build, so the type that holds either is entered from outside the build.
          if Collector.enters(element) then
            nodes.get(owner).foreach(found => nodes.update(owner, found.copy(entry = true)))
          val parameters = element.asInstanceOf[ExecutableElement].getParameters.asScala.toVector.map { parameter =>
            val shape = Collector.shape(parameter.asType(), parameter)
            Model.Parameter(
              parameter.getSimpleName.toString,
              shape.kind,
              shape.name,
              shape.cases,
              shape.primitive
            )
          }
          methods.getOrElseUpdate(
            name,
            Model.Method(
              owner,
              name,
              Collector.exported(element),
              Set.empty,
              parameters,
              location(compilation, node),
              Collector.returns(element.asInstanceOf[ExecutableElement]),
              Collector.overriding(element.asInstanceOf[ExecutableElement]),
              Collector.suppressed(element)
            )
          ): Unit
          // The body of the method is what recurs, so the fragments are taken of it.
          val body = node.getBody
          if body != null then clones.add(body, path, owner)
          owners.push(name)
          try super.visitMethod(node, unused)
          finally owners.pop(): Unit
        }
      }

      override def visitVariable(node: VariableTree, unused: Unit): Unit = {
        val path    = getCurrentPath
        val element = trees.getElement(path)
        // A field of the type is a field of the product the analysis makes of it; a parameter of a method is one of
        // the parameters of the method, which the method itself holds.

        if element != null && element.getKind == ElementKind.FIELD then {
          val owner = owners.headOption.getOrElse("")
          val shape = Collector.shape(element.asType(), element)
          fields.update(
            owner,
            fields.getOrElse(owner, Vector.empty) :+ Model.Field(
              owner,
              Model.Parameter(
                element.getSimpleName.toString,
                shape.kind,
                shape.name,
                shape.cases,
                shape.primitive
              )
            )
          ): Unit
        }
        super.visitVariable(node, unused)
      }

      override def visitMethodInvocation(node: MethodInvocationTree, unused: Unit): Unit = {
        val path    = getCurrentPath
        val element = trees.getElement(path)
        val caller  = owners.headOption.getOrElse("")
        if element != null && element.getKind == ElementKind.METHOD then {
          val callee = Collector.qualified(element)
          if callee.nonEmpty then {
            val arguments =
              node.getArguments.asScala.toVector.map(argument => Collector.argument(trees, getCurrentPath, argument))
            val discarded = Collector.discarded(getCurrentPath)
            val negated   = Collector.negated(getCurrentPath)
            sites += Model.Site(caller, callee, arguments, location(compilation, node), discarded, negated): Unit
            // A class the code looks up by name is not read by the compiler, so a module build has to require its module
            // for the code to work at run time, which the analysis checks over the whole build.
            if Collector.forName(element) then
              node.getArguments.asScala.headOption.flatMap(Collector.text).foreach { target =>
                reflections += Model.Reflection(caller, target, location(compilation, node)): Unit
              }
          }
        }
        super.visitMethodInvocation(node, unused)
      }

      override def visitMemberReference(node: MemberReferenceTree, unused: Unit): Unit = {
        val path    = getCurrentPath
        val element = trees.getElement(path)
        val caller  = owners.headOption.getOrElse("")
        if element != null && (element.getKind == ElementKind.METHOD || element.getKind == ElementKind.CONSTRUCTOR)
        then {
          val callee = Collector.qualified(element)
          if callee.nonEmpty then {
            val executable = element.asInstanceOf[ExecutableElement]
            val arguments  = Vector.fill(executable.getParameters.size)("unknown")
            sites += Model.Site(caller, callee, arguments, location(compilation, node)): Unit
          }
        }
        super.visitMemberReference(node, unused)
      }

      override def visitMemberSelect(node: MemberSelectTree, unused: Unit): Unit = {
        val path    = getCurrentPath
        val element = trees.getElement(path)
        val owner   = types.headOption.getOrElse("")
        // a type named through the type that holds it, as in a class literal of an annotation
        if element != null && (element.getKind.isClass || element.getKind.isInterface) then {
          val target = Collector.reference(trees, path)
          if target.isDefined && target.get != owner then
            edges.getOrElseUpdate(owner, mutable.LinkedHashSet.empty) += target.get: Unit
        }
        super.visitMemberSelect(node, unused)
      }

      override def visitIdentifier(node: IdentifierTree, unused: Unit): Unit = {
        val path    = getCurrentPath
        val element = trees.getElement(path)
        val owner   = types.headOption.getOrElse("")
        if element != null then {
          val target = Collector.reference(trees, path)
          if target.isDefined && target.get != owner then
            edges.getOrElseUpdate(owner, mutable.LinkedHashSet.empty) += target.get: Unit
        }
        super.visitIdentifier(node, unused)
      }
    }
    // The walk begins at the declaration, which the compilation unit holds, so that the paths of the code are the
    // ones the compiler attributed.
    scanner.scan(new TreePath(new TreePath(compilation), declaration), ())
  }
}

object Collector {

  /** The name of a type, without the packages and the type arguments it holds, which is what the analysis reads to know
    * the type of a value alike with the type of the Scala code.
    */
  def name(tpe: javax.lang.model.`type`.TypeMirror): String = {
    val erased = tpe.toString.takeWhile(_ != '<')
    erased.split('.').lastOption.getOrElse("").stripSuffix("[]")
  }

  /** The type a method returns, which the analysis reads to know whether a call to it gives a value. */
  def returns(element: ExecutableElement): String = name(element.getReturnType)

  /** Whether a method overrides a method of a supertype, which the analysis reads as a method it inherits rather than
    * declares.
    */
  def overriding(element: ExecutableElement): Boolean = {
    def signature(method: ExecutableElement): Vector[String] =
      method.getParameters.asScala.map(parameter => name(parameter.asType())).toVector
    // A method that is static or private does not override the one of the supertype, it hides it, and the code of the
    // build does not inherit it.
    def hidden(method: ExecutableElement): Boolean =
      method.getModifiers.contains(Modifier.STATIC) || method.getModifiers.contains(Modifier.PRIVATE)
    def overrides(typeElement: TypeElement): Boolean =
      typeElement.getEnclosedElements.asScala.exists {
        case member: ExecutableElement =>
          member.getKind == ElementKind.METHOD && member.getSimpleName == element.getSimpleName &&
          !hidden(member) && signature(member) == signature(element)
        case _ => false
      }
    !hidden(element) && supertypes(element.getEnclosingElement).exists(overrides)
  }

  /** The types a type extends, which the methods of it may override. */
  private def supertypes(element: Element): Vector[TypeElement] = {
    val visited                                                = mutable.Set.empty[String]
    def collect(typeElement: TypeElement): Vector[TypeElement] =
      if visited(Collector.qualified(typeElement)) then Vector.empty
      else {
        visited += Collector.qualified(typeElement)
        val parents = Option(typeElement.getSuperclass).toVector ++ typeElement.getInterfaces.asScala.toVector
        parents
          .collect { case declared: DeclaredType => declared.asElement() }
          .collect { case typeElement: TypeElement => typeElement }
          .flatMap(parent => parent +: collect(parent))
      }
    element match {
      case typeElement: TypeElement => collect(typeElement)
      case _                        => Vector.empty
    }
  }

  /** Whether the method is `Class.forName`, which looks a class up by the name it is given. */
  def forName(element: Element): Boolean = {
    val owner = element.getEnclosingElement
    element.getSimpleName.toString == "forName" &&
    owner != null && owner.getKind == ElementKind.CLASS &&
    owner.asInstanceOf[TypeElement].getQualifiedName.toString == "java.lang.Class"
  }

  /** The text of a string constant, which the compiler folds from the literals the expression holds. */
  def text(tree: Tree): Option[String] = tree match {
    case literal: LiteralTree =>
      Option(literal.getValue).collect { case value: String => value }
    case parenthesized: ParenthesizedTree                       => text(parenthesized.getExpression)
    case binary: BinaryTree if binary.getKind == Tree.Kind.PLUS =>
      for {
        left  <- text(binary.getLeftOperand)
        right <- text(binary.getRightOperand)
      } yield left + right
    case _ => None
  }

  /** Whether the code that makes a call at the path drops the value the call gives, which is how the analysis knows
    * that nothing uses it.
    */
  def discarded(path: TreePath): Boolean = {
    def parent(tree: Tree): Option[Tree] = tree match {
      case parenthesized: ParenthesizedTree => parent(parenthesized.getExpression)
      case _                                => None
    }
    val parentPath = path.getParentPath
    if parentPath == null then false
    else {
      val tree = parentPath.getLeaf
      tree.getKind == Tree.Kind.EXPRESSION_STATEMENT || parent(tree).exists(_.getKind == Tree.Kind.EXPRESSION_STATEMENT)
    }
  }

  /** Whether the code that makes a call at the path inverts the value the call gives with `!`, which is how the
    * analysis knows that every caller reads a boolean method as the opposite of what it returns.
    */
  def negated(path: TreePath): Boolean = {
    def ancestor(candidate: TreePath): Boolean =
      if candidate == null then false
      else
        candidate.getLeaf match {
          case parenthesized: ParenthesizedTree => ancestor(candidate.getParentPath)
          case unary: UnaryTree                 => unary.getKind == Tree.Kind.LOGICAL_COMPLEMENT
          case _                                => false
        }
    ancestor(path.getParentPath)
  }

  /** The shape of a type: its kind, the name that gives it its shape, the cases of an enumeration, and whether it is a
    * primitive.
    */
  case class Shape(kind: String, name: String, cases: Vector[String], primitive: Boolean)

  /** The qualified name of the element, which is the package, the types that hold it and its own name joined by dots.
    */
  def qualified(element: Element): String =
    if element == null then ""
    else
      element match {
        case e: TypeElement                                               => e.getQualifiedName.toString
        case e: PackageElement                                            => e.getQualifiedName.toString
        case e: ExecutableElement if e.getKind == ElementKind.CONSTRUCTOR =>
          val owner = qualified(e.getEnclosingElement)
          if owner.isEmpty then e.getSimpleName.toString else s"$owner.${e.getEnclosingElement.getSimpleName}"
        case e: ExecutableElement => member(e)
        case e: VariableElement   => member(e)
        case other                => other.getSimpleName.toString
      }

  /** The qualified name of a member, which is the name of the type that holds it and its own name joined by dots. */
  private def member(element: Element): String = {
    val owner = qualified(element.getEnclosingElement)
    if owner.isEmpty then element.getSimpleName.toString else s"$owner.${element.getSimpleName}"
  }

  /** The enclosing type of the element, if it has one. */
  def outer(element: Element): Option[String] =
    if element == null then None
    else
      Option(element.getEnclosingElement)
        .filter(_.getKind.isClass)
        .map(qualified)
        .filter(_.nonEmpty)

  /** The name a reference to the element at the path has. */
  def reference(trees: Trees, path: TreePath): Option[String] = {
    val element = trees.getElement(path)
    if element == null then None
    else
      element.getKind match {
        case ElementKind.PACKAGE   => None
        case ElementKind.PARAMETER => None
        case _                     =>
          val name = qualified(element)
          if name.isEmpty then None else Some(name)
      }
  }

  /** Whether the element carries an annotation of the simple name, whichever package it is in. */
  def annotated(element: Element, simpleName: String): Boolean =
    element.getAnnotationMirrors.asScala.exists(_.getAnnotationType.asElement.getSimpleName.contentEquals(simpleName))

  /** Whether a tool generated the type, or the type that holds it, which the annotation says. */
  def generated(element: Element): Boolean =
    element != null && (annotated(element, "Generated") || generated(element.getEnclosingElement))

  /** Whether the build enters the code through the element, which is the case of a native method and of a main method.
    */
  def enters(element: Element): Boolean =
    element.getModifiers.contains(Modifier.NATIVE) ||
      (element.getKind == ElementKind.METHOD && element.getSimpleName.toString == "main")

  /** Whether the element is part of the API other code may use. A package-private element is not, as the private one of
    * a package is not in Scala.
    */
  def exported(element: Element): Boolean =
    if element == null then false
    else {
      val modifiers = element.getModifiers
      modifiers.contains(Modifier.PUBLIC) || modifiers.contains(Modifier.PROTECTED)
    }

  /** The components of a record, which are the fields of the product the analysis makes of it. */
  def components(element: Element): Vector[Model.Parameter] =
    element match {
      case typeElement: TypeElement =>
        typeElement.getRecordComponents.asScala.toVector.map { component =>
          val shape = Collector.shape(component.asType(), component)
          Model.Parameter(
            component.getSimpleName.toString,
            shape.kind,
            shape.name,
            shape.cases,
            shape.primitive
          )
        }
      case _ => Vector.empty
    }

  /** The variants of a sealed type, which are the types it allows to extend it. */
  def variants(element: Element): Vector[Model.Variant] =
    element match {
      case typeElement: TypeElement =>
        typeElement.getPermittedSubclasses.asScala.toVector.flatMap { permitted =>
          // The permitted subclasses are the types, which the analysis reads as the variants of a sealed trait.
          permitted match {
            case declared: javax.lang.model.`type`.DeclaredType =>
              declared.asElement() match {
                case null      => Vector.empty
                case permitted =>
                  val name = Collector.qualified(permitted)
                  // A record that extends a sealed type is a variant with fields, which the analysis reads as a case
                  // class.
                  val fields =
                    if permitted.getKind == ElementKind.RECORD then Collector.components(permitted)
                    else Vector.empty
                  Vector(Model.Variant(name, fields))
              }
            case _ => Vector.empty
          }
        }
      case _ => Vector.empty
    }

  /** The shape of the type of the element, which the analysis compares the parameters of the methods by. */
  def shape(tpe: javax.lang.model.`type`.TypeMirror, element: Element): Shape = {
    val kind = tpe.getKind match {
      case javax.lang.model.`type`.TypeKind.BOOLEAN => "flag"
      case javax.lang.model.`type`.TypeKind.BYTE | javax.lang.model.`type`.TypeKind.SHORT |
          javax.lang.model.`type`.TypeKind.INT | javax.lang.model.`type`.TypeKind.LONG |
          javax.lang.model.`type`.TypeKind.FLOAT | javax.lang.model.`type`.TypeKind.DOUBLE |
          javax.lang.model.`type`.TypeKind.CHAR => "number"
      // A reference that may be absent is an optional, which the analysis reads alike with an option in Scala.
      case _ if Collector.nullable(tpe, element) => "optional"
      case _                                     => "other"
    }
    val name = tpe.toString
    // the cases are those of the enum the type is, which the declaration of a parameter or a field does not show
    val enumType = tpe match {
      case declared: DeclaredType => Some(declared.asElement).filter(_.getKind == ElementKind.ENUM)
      case _                      => Option(element).filter(_.getKind == ElementKind.ENUM)
    }
    val cases =
      if kind == "other" then
        enumType.fold(Vector.empty[String])(
          _.getEnclosedElements.asScala.toVector.collect {
            case constant: VariableElement if constant.getSimpleName.toString != "$VALUES" =>
              constant.getSimpleName.toString
          }
        )
      else Vector.empty
    val primitive = kind == "number" || kind == "flag"
    Shape(if cases.nonEmpty then "tag" else kind, name, cases, primitive)
  }

  /** Whether the element may be absent, which the JSpecify annotations say. The annotation is on the type, as it says
    * how the value may be used, not only whether it is there.
    */
  def nullable(tpe: javax.lang.model.`type`.TypeMirror, element: Element): Boolean = {
    def marked(mirrors: java.util.List[? <: javax.lang.model.element.AnnotationMirror]): Boolean =
      mirrors.asScala.exists { mirror =>
        mirror.getAnnotationType.toString == "org.jspecify.annotations.Nullable"
      }
    marked(tpe.getAnnotationMirrors) || (element != null && marked(element.getAnnotationMirrors))
  }

  /** The argument a call passes, with the constants and the enum cases it names read as their symbols. */
  def argument(trees: Trees, call: TreePath, tree: ExpressionTree): String = {
    val symbol = tree match {
      case _: IdentifierTree | _: MemberSelectTree =>
        Option(trees.getElement(new TreePath(call, tree))).filter(element =>
          element.getKind == ElementKind.ENUM_CONSTANT ||
            (element.getKind == ElementKind.FIELD &&
              element.getModifiers.contains(javax.lang.model.element.Modifier.STATIC) &&
              element.getModifiers.contains(javax.lang.model.element.Modifier.FINAL))
        ).map(element => s"symbol:${Collector.qualified(element)}")
      case _ => None
    }
    symbol.getOrElse(argument(tree))
  }

  /** The argument a call passes, which the analysis reads the values the callers pass by. */
  def argument(tree: ExpressionTree): String = tree match {
    case literal: LiteralTree => literal.getValue match {
        case value: java.lang.Boolean => if value then "true" else "false"
        case value: java.lang.Integer => s"number:$value"
        case value: java.lang.Long    => s"number:$value"
        case value: java.lang.Double  => s"number:$value"
        case value: String            => s"text:$value"
        case null                     => "absent"
        case _                        => "unknown"
      }
    case identifier: IdentifierTree =>
      identifier.getName.toString match {
        case "null" => "absent"
        case name   => s"present:$name"
      }
    case _ => "unknown"
  }

  /** The checks the element suppresses, which the annotations name. */
  def suppressed(element: Element): Set[String] =
    if element == null then Set.empty
    else
      element.getAnnotationMirrors.asScala.toVector.flatMap { mirror =>
        if mirror.getAnnotationType.toString != "java.lang.SuppressWarnings" then Vector.empty
        else
          mirror.getElementValues.asScala.toVector.flatMap { case (key, value) =>
            if key.getSimpleName.toString != "value" then Vector.empty
            else
              value.getValue match {
                case items: java.util.List[?] =>
                  items.asScala.toVector.flatMap {
                    // The value of an element of the array holds the string, which the annotation value wraps.
                    case item: AnnotationValue =>
                      item.getValue match {
                        case text: String => Vector(text)
                        case _            => Vector.empty
                      }
                    case text: String => Vector(text)
                    case _            => Vector.empty
                  }
                case item: String => Vector(item)
                case _            => Vector.empty
              }
          }
      }.flatMap(name => JavaLint.resolve(name).toVector).toSet

  /** The path of the file the node is defined in. */
  def source(compilation: CompilationUnitTree): String =
    Option(compilation.getSourceFile).map(_.toUri.getPath).getOrElse("")

  /** The line the node is defined on. */
  def line(trees: Trees, compilation: CompilationUnitTree, node: Tree): Int = {
    val position = trees.getSourcePositions.getStartPosition(compilation, node)
    if position < 0 then 0 else compilation.getLineMap.getLineNumber(position).toInt
  }

}
