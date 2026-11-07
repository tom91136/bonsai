package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.util.TreePath
import com.sun.source.util.TreePathScanner
import com.sun.source.util.Trees
import javax.lang.model.`type`.DeclaredType
import javax.lang.model.element.*
import net.kurobako.bonsai.java.Javac.*
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object Collector {

  /** The name of a type, without the packages and the type arguments it holds, which is what the analysis reads to know
    * the type of a value alike with the type of the Scala code.
    */
  private def name(tpe: javax.lang.model.`type`.TypeMirror): String = {
    val erased = tpe.toString.takeWhile(_ != '<')
    erased.split('.').lastOption.getOrElse("").stripSuffix("[]")
  }

  /** The type a method returns, which the analysis reads to know whether a call to it gives a value. */
  private def returns(element: ExecutableElement): String = name(element.getReturnType)

  /** Whether the constructor of a record is its canonical one, which takes the components in their order. */
  private def canonical(record: TypeElement, constructor: ExecutableElement): Boolean =
    record.getKind == ElementKind.RECORD && {
      val components = record.getRecordComponents.asScala.toVector
      val parameters = constructor.getParameters.asScala.toVector
      components.size == parameters.size && components.zip(parameters).forall((component, parameter) =>
        component.getSimpleName == parameter.getSimpleName && component.asType.toString == parameter.asType.toString
      )
    }

  /** Whether a method overrides a method of a supertype, which the analysis reads as a method it inherits rather than
    * declares.
    */
  def overriding(element: ExecutableElement): Boolean = {
    def signature(method: ExecutableElement): Vector[String] =
      method.getParameters.asScala.map(parameter => name(parameter.asType())).toVector
    // A method that is static or private does not override the one of the supertype, it hides it, and the code of the
    // build does not inherit it.
    def visible(method: ExecutableElement): Boolean =
      !method.getModifiers.contains(Modifier.STATIC) && !method.getModifiers.contains(Modifier.PRIVATE)
    def overrides(typeElement: TypeElement): Boolean =
      typeElement.getEnclosedElements.asScala.exists {
        case member: ExecutableElement =>
          member.getKind == ElementKind.METHOD && member.getSimpleName == element.getSimpleName &&
          visible(member) && signature(member) == signature(element)
        case _ => false
      }
    visible(element) && supertypes(element.getEnclosingElement).exists(overrides(_))
  }

  /** The types a type extends, which the methods of it may override. */
  private def supertypes(element: Element): Vector[TypeElement] = {
    val visited                                                = mutable.Set.empty[TypeElement]
    def collect(typeElement: TypeElement): Vector[TypeElement] =
      if (visited(typeElement)) Vector.empty
      else {
        visited += typeElement
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
  private def forName(element: Element): Boolean =
    element.getSimpleName.toString == "forName" &&
      element.enclosing.exists { owner =>
        owner.getKind == ElementKind.CLASS &&
        owner.asInstanceOf[TypeElement].getQualifiedName.toString == "java.lang.Class"
      }

  /** The text of a string constant, which the compiler folds from the literals the expression holds. */
  private def text(tree: ExpressionTree): Option[String] = Lints.unparenthesised(tree) match {
    case binary: BinaryTree if binary.getKind == Tree.Kind.PLUS =>
      for {
        left  <- text(binary.getLeftOperand)
        right <- text(binary.getRightOperand)
      } yield left + right
    case other => Lints.literalText(other)
  }

  /** Whether the code that makes a call at the path drops the value the call gives, which is how the analysis knows
    * that nothing uses it.
    */
  def discarded(path: TreePath): Boolean =
    path.parent.exists(_.getLeaf.getKind == Tree.Kind.EXPRESSION_STATEMENT)

  /** Whether the code that makes a call at the path inverts the value the call gives with `!`, which is how the
    * analysis knows that every caller reads a boolean method as the opposite of what it returns.
    */
  private def negated(path: TreePath): Boolean =
    Lints.enclosing(path.getParentPath).map(_.getLeaf).exists {
      case unary: UnaryTree => unary.getKind == Tree.Kind.LOGICAL_COMPLEMENT
      case _                => false
    }

  /** The shape of a type: its kind, the name that gives it its shape, the cases of an enumeration, and whether it is a
    * primitive.
    */
  private case class Shape(kind: String, name: String, cases: Vector[String], primitive: Boolean)

  /** The qualified name of the element, which is the package, the types that hold it and its own name joined by dots.
    */
  def qualified(element: Element)(using Trees): String =
    element match {
      case e: TypeElement if e.getNestingKind == NestingKind.LOCAL      => local(e)
      case e: TypeElement                                               => e.getQualifiedName.toString
      case e: PackageElement                                            => e.getQualifiedName.toString
      case e: ExecutableElement if e.getKind == ElementKind.CONSTRUCTOR =>
        val owner = qualifiedEnclosing(e)
        if (owner.isEmpty) e.getSimpleName.toString else s"$owner.${e.getEnclosingElement.getSimpleName}"
      case e: (ExecutableElement | VariableElement) => member(e)
      case other                                    => other.getSimpleName.toString
    }

  /** The qualified name of the element the tree at the path names, which is empty where the compiler names none. */
  def qualified(trees: Trees, path: TreePath): String = trees.namedElement(path).fold("")(qualified(_)(using trees))

  /** The qualified name of the element that holds the element, which is empty where nothing holds it. */
  def qualifiedEnclosing(element: Element)(using Trees): String = element.enclosing.fold("")(qualified(_))

  /** The qualified name of a local class, which is a member of the method that holds it. A second local class of the
    * same name in that method takes its place among them as well, as the classes of one method are told apart by where
    * they are declared.
    */
  // A javac visitor is an abstract class, so it is written inline.
  // The state is updated as the walk proceeds.
  @SuppressWarnings(Array("scalafix:BannedSyntax.anonymous-allocation", "scalafix:DisableSyntax.var"))
  private def local(element: TypeElement)(using trees: Trees): String = {
    val holder = Option(trees.getPath(element)).flatMap(path =>
      path.parents
        .find(_.getLeaf.isInstanceOf[MethodTree | ClassTree])
    )
    var seen     = 0
    var position = 1
    holder.foreach { root =>
      new TreePathScanner[Unit, Unit] {
        override def visitClass(node: ClassTree, unused: Unit): Unit = {
          trees.getElement(getCurrentPath) match {
            case found: TypeElement
                if found.getNestingKind == NestingKind.LOCAL && found.getSimpleName == element.getSimpleName &&
                  found.getEnclosingElement == element.getEnclosingElement =>
              seen += 1
              if (found == element) position = seen
            case _ =>
          }
          super.visitClass(node, unused)
        }
      }.scan(root, ())
    }
    if (position == 1) member(element) else s"${member(element)}$$$position"
  }

  /** The qualified name of a member, which is the name of the type that holds it and its own name joined by dots. */
  private def member(element: Element)(using Trees): String = {
    val owner = qualifiedEnclosing(element)
    if (owner.isEmpty) element.getSimpleName.toString else s"$owner.${element.getSimpleName}"
  }

  /** The enclosing type of the element, if it has one. */
  private def outer(element: Element)(using Trees): Option[String] =
    element.enclosing
      .filter(_.getKind.isClass)
      .map(qualified(_))
      .filter(_.nonEmpty)

  /** The name a reference to the element at the path has. */
  private def reference(trees: Trees, path: TreePath): Option[String] =
    trees.namedElement(path).flatMap { element =>
      element.getKind match {
        case ElementKind.PACKAGE   => None
        case ElementKind.PARAMETER => None
        case _                     =>
          val name = qualified(element)(using trees)
          if (name.isEmpty) None else Some(name)
      }
    }

  /** Whether the element carries an annotation of the simple name, whichever package it is in. */
  private def annotated(element: Element, simpleName: String): Boolean =
    element.getAnnotationMirrors.asScala.exists(_.getAnnotationType.asElement.getSimpleName.contentEquals(simpleName))

  /** Whether a tool generated the type, or the type that holds it, which the annotation says. */
  private[java] def generated(element: Element): Boolean =
    annotated(element, "Generated") || element.enclosing.exists(generated(_))

  /** Whether the build enters the code through the element, which is the case of a native method and of a main method.
    */
  private def enters(element: Element): Boolean =
    element.getModifiers.contains(Modifier.NATIVE) ||
      (element.getKind == ElementKind.METHOD && element.getSimpleName.toString == "main")

  private def field(element: Element): Boolean = element.getKind == ElementKind.FIELD

  private def instanceField(element: Element): Boolean =
    field(element) && !element.getModifiers.contains(Modifier.STATIC)

  private val Steps = Set(
    Tree.Kind.PREFIX_INCREMENT,
    Tree.Kind.PREFIX_DECREMENT,
    Tree.Kind.POSTFIX_INCREMENT,
    Tree.Kind.POSTFIX_DECREMENT
  )

  /** Whether the declaration is of a field that is not private, final or volatile and starts with a value or is
    * assigned by every constructor, which carries no annotation but the suppression of a check.
    */
  private def reassignable(node: VariableTree, element: Element, constructed: => Boolean): Boolean =
    field(element) && {
      val modifiers = element.getModifiers
      !modifiers.contains(Modifier.PRIVATE) && !modifiers.contains(Modifier.FINAL) &&
      !modifiers.contains(Modifier.VOLATILE) &&
      element.getAnnotationMirrors.asScala.forall(_.getAnnotationType.toString == "java.lang.SuppressWarnings")
    } && (node.initializer.isDefined || constructed)

  /** Whether the element is part of the API other code may use. A package-private element is not, as the private one of
    * a package is not in Scala.
    */
  private def exported(element: Element): Boolean = {
    val modifiers = element.getModifiers
    modifiers.contains(Modifier.PUBLIC) || modifiers.contains(Modifier.PROTECTED)
  }

  /** Whether other code can call a constructor of the type: any that is public or protected, or for a record the
    * canonical one.
    */
  private def constructible(element: Element): Boolean =
    element match {
      case typeElement: TypeElement =>
        typeElement.getEnclosedElements.asScala.exists {
          case constructor: ExecutableElement if constructor.getKind == ElementKind.CONSTRUCTOR =>
            exported(constructor) && (typeElement.getKind != ElementKind.RECORD || canonical(typeElement, constructor))
          case _ => false
        }
      case _ => false
    }

  private[java] def parameter(element: Element): Model.Parameter = {
    val shape = Collector.shape(element.asType(), element)
    Model.Parameter(element.getSimpleName.toString, shape.kind, shape.name, shape.cases, shape.primitive)
  }

  /** The components of a record, which are the fields of the product the analysis makes of it. */
  private def components(element: Element): Vector[Model.Parameter] =
    element match {
      case typeElement: TypeElement =>
        typeElement.getRecordComponents.asScala.toVector.map(Collector.parameter(_))
      case _ => Vector.empty
    }

  /** The variants of a sealed type, which are the types it allows to extend it. */
  private def variants(element: Element)(using Trees): Vector[Model.Variant] =
    element match {
      case typeElement: TypeElement =>
        // The permitted subclasses are the types, which the analysis reads as the variants of a sealed trait.
        typeElement.getPermittedSubclasses.asScala.toVector.flatMap {
          case declared: javax.lang.model.`type`.DeclaredType =>
            Option(declared.asElement()).map(subclass => Model.Variant(Collector.qualified(subclass)))
          case _ => None
        }
      case _ => Vector.empty
    }

  /** The shape of the type of the element, which the analysis compares the parameters of the methods by. */
  private def shape(tpe: javax.lang.model.`type`.TypeMirror, element: Element): Shape = {
    val held = Collector.optionalValue(tpe)
    val kind = tpe.getKind match {
      case javax.lang.model.`type`.TypeKind.BOOLEAN => "flag"
      case javax.lang.model.`type`.TypeKind.BYTE | javax.lang.model.`type`.TypeKind.SHORT |
          javax.lang.model.`type`.TypeKind.INT | javax.lang.model.`type`.TypeKind.LONG |
          javax.lang.model.`type`.TypeKind.FLOAT | javax.lang.model.`type`.TypeKind.DOUBLE |
          javax.lang.model.`type`.TypeKind.CHAR => "number"
      // A reference that may be absent is an optional, which the analysis reads alike with an option in Scala.
      case _ if held.isDefined                   => "optional"
      case _ if Collector.nullable(tpe, element) => "optional"
      case _                                     => "other"
    }
    val name = held.getOrElse(tpe.toString)
    // the cases are those of the enum the type is, which the declaration of a parameter or a field does not show
    val enumType = tpe match {
      case declared: DeclaredType => Some(declared.asElement).filter(_.getKind == ElementKind.ENUM)
      case _                      => Some(element).filter(_.getKind == ElementKind.ENUM)
    }
    val cases =
      if (kind == "other")
        enumType.fold(Vector.empty[String])(
          _.getEnclosedElements.asScala.toVector.collect {
            case constant: VariableElement if constant.getSimpleName.toString != "$VALUES" =>
              constant.getSimpleName.toString
          }
        )
      else Vector.empty
    val primitive = kind == "number" || kind == "flag"
    Shape(if (cases.nonEmpty) "tag" else kind, name, cases, primitive)
  }

  /** The values of the `java.util` types that hold at most one, which are optionals like an option in Scala. */
  private val Optionals = Map(
    "java.util.Optional"       -> None,
    "java.util.OptionalInt"    -> Some("int"),
    "java.util.OptionalLong"   -> Some("long"),
    "java.util.OptionalDouble" -> Some("double")
  )

  /** The type an optional of `java.util` holds, if the type is one. */
  private[java] def optionalValue(tpe: javax.lang.model.`type`.TypeMirror): Option[String] = tpe match {
    case declared: DeclaredType =>
      declared.asElement match {
        case owner: TypeElement =>
          Optionals.get(owner.getQualifiedName.toString).map(_.getOrElse(
            declared.getTypeArguments.asScala.headOption.fold("java.lang.Object")(_.toString)
          ))
        case _ => None
      }
    case _ => None
  }

  /** Whether the method is a factory of an optional of `java.util`, which is the name of the factory. */
  private def optionalFactory(element: Element): Option[String] =
    Option(element).filter(_.getKind == ElementKind.METHOD).flatMap { method =>
      method.getEnclosingElement match {
        case owner: TypeElement if Optionals.contains(owner.getQualifiedName.toString) =>
          Some(method.getSimpleName.toString)
        case _ => None
      }
    }

  /** Whether the element may be absent, which the JSpecify annotations say. The annotation is on the type, as it says
    * how the value may be used, not only whether it is there.
    */
  private def nullable(tpe: javax.lang.model.`type`.TypeMirror, element: Element): Boolean = {
    def marked(mirrors: java.util.List[? <: javax.lang.model.element.AnnotationMirror]): Boolean =
      mirrors.asScala.exists { mirror =>
        mirror.getAnnotationType.toString == "org.jspecify.annotations.Nullable"
      }
    marked(tpe.getAnnotationMirrors) || marked(element.getAnnotationMirrors)
  }

  /** The parameters, by index, that the body of the method passes only to constructors. */
  // A javac visitor is an abstract class, so it is written inline.
  @SuppressWarnings(Array("scalafix:BannedSyntax.anonymous-allocation"))
  private def stored(trees: Trees, method: TreePath): Set[Int] =
    Option(method.getLeaf.asInstanceOf[MethodTree])
      .filterNot(_.getParameters.isEmpty)
      .flatMap(declared => declared.body)
      .fold(Set.empty[Int]) { body =>
        val uses   = mutable.Map.empty[Element, Int].withDefaultValue(0)
        val passes = mutable.Map.empty[Element, Int].withDefaultValue(0)
        new TreePathScanner[Unit, Unit] {
          private def pass(arguments: java.util.List[? <: ExpressionTree]): Unit =
            arguments.asScala.foreach { argument =>
              Lint.unwrapped(argument) match {
                case identifier: IdentifierTree =>
                  Option(trees.getElement(Lints.child(getCurrentPath, identifier)))
                    .foreach(element => passes(element) += 1)
                case _ =>
              }
            }

          override def visitIdentifier(node: IdentifierTree, unused: Unit): Unit = {
            trees.namedElement(getCurrentPath).foreach(element => uses(element) += 1)
            super.visitIdentifier(node, unused)
          }

          override def visitNewClass(node: NewClassTree, unused: Unit): Unit = {
            pass(node.getArguments)
            super.visitNewClass(node, unused)
          }

          override def visitMethodInvocation(node: MethodInvocationTree, unused: Unit): Unit = {
            if (trees.namedElement(getCurrentPath).exists(_.getKind == ElementKind.CONSTRUCTOR))
              pass(node.getArguments)
            super.visitMethodInvocation(node, unused)
          }
        }.scan(TreePath(method, body), ())
        trees.getElement(method).asInstanceOf[ExecutableElement].getParameters.asScala.zipWithIndex.collect {
          case (parameter, index) if uses(parameter) > 0 && uses(parameter) == passes(parameter) => index
        }.toSet
      }

  /** The argument a call passes, with the constants and the enum cases it names read as their symbols. */
  private[java] def argument(trees: Trees, call: TreePath, tree: ExpressionTree): String = {
    val path = TreePath(call, tree)
    tree match {
      case literal: LiteralTree       => literalArgument(literal)
      case identifier: IdentifierTree =>
        identifier.getName.toString match {
          case "null" => "absent"
          case "this" => "present:this"
          case name   =>
            trees.namedElement(path).fold("unknown") { element =>
              symbol(element)(using trees).getOrElse(if (present(element)) s"present:$name" else "unknown")
            }
        }
      case _: MemberSelectTree =>
        trees.namedElement(path).flatMap(symbol(_)(using trees)).getOrElse("unknown")
      case unary: UnaryTree if unary.getKind == Tree.Kind.UNARY_MINUS =>
        argument(trees, path, unary.getExpression) match {
          case number if number.startsWith("number:") => s"number:${-BigDecimal(number.drop("number:".length))}"
          case _                                      => "unknown"
        }
      case _: MethodInvocationTree =>
        optionalFactory(trees.getElement(path)) match {
          case Some("empty") => "absent"
          case Some("of")    => "present:"
          case _             => "unknown"
        }
      case _ => "unknown"
    }
  }

  /** The symbol of a constant or an enum case, which the argument names. */
  private def symbol(element: Element)(using Trees): Option[String] = {
    val constant = element.getKind match {
      case ElementKind.ENUM_CONSTANT => true
      case ElementKind.FIELD         => Lints.constant(element)
      case _                         => false
    }
    Option.when(constant)(s"symbol:${Collector.qualified(element)}")
  }

  /** Whether the variable cannot be null, which is the case of a primitive and of a parameter or field of a null-marked
    * scope that is not annotated as nullable. The nullness of a local variable is inferred from what it is assigned.
    */
  private def present(element: Element): Boolean =
    element.asType.getKind.isPrimitive ||
      (Set(ElementKind.PARAMETER, ElementKind.FIELD)(element.getKind) &&
        Nullness.marked(element) && !Nullness.nullableElement(element))

  /** The argument a call passes when it is a literal, which the analysis reads the values the callers pass by. */
  private def literalArgument(literal: LiteralTree): String = (literal.getValue: Any) match {
    case value: Boolean               => if (value) "true" else "false"
    case value: (Int | Long | Double) => s"number:$value"
    case value: String                => s"text:$value"
    case _ if literal.value.isEmpty   => "absent"
    case _                            => "unknown"
  }

  /** The checks the element suppresses, which the annotations name. */
  def suppressed(element: Element): Set[String] =
    element.getAnnotationMirrors.asScala.view.flatMap { mirror =>
      if (mirror.getAnnotationType.toString != "java.lang.SuppressWarnings") Vector.empty
      else
        mirror.getElementValues.asScala.toVector.flatMap { case (key, value) =>
          if (key.getSimpleName.toString != "value") Vector.empty
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
    }.flatMap(name => LintId.resolve(name).toVector).toSet

  /** The path of the file the node is defined in. */
  def source(compilation: CompilationUnitTree): String =
    sources.computeIfAbsent(compilation, _ => Option(compilation.getSourceFile).map(_.toUri.getPath).getOrElse(""))

  private val sources = java.util.Collections.synchronizedMap(java.util.WeakHashMap[CompilationUnitTree, String]())

  /** The line the node is defined on. */
  private def line(trees: Trees, compilation: CompilationUnitTree, node: Tree): Int = {
    val position = trees.getSourcePositions.getStartPosition(compilation, node)
    if (position < 0) 0 else compilation.getLineMap.getLineNumber(position).toInt
  }

}

/** The collector of the model of a compilation: it walks the trees the compiler gives it and records the types it
  * declares, the methods and the fields they hold, and the calls between them.
  *
  * The names of the nodes are the qualified names the analysis uses, which are the package, the types that hold a node
  * and its own name, joined by dots. A node is a type, as it is in the graph of the Scala code of a build: the members
  * of a type are held by the type, and the references between the types are the ones the members make.
  */
class Collector(trees: Trees) {

  private given Trees = trees

  private val nodes         = mutable.LinkedHashMap.empty[String, Model.Node]
  private val edges         = mutable.LinkedHashMap.empty[(String, String), mutable.LinkedHashSet[String]]
  private val methods       = mutable.LinkedHashMap.empty[String, Model.Method]
  private val fields        = mutable.LinkedHashMap.empty[String, Vector[Model.Field]]
  private val sites         = mutable.ArrayBuffer.empty[Model.Site]
  private val clones        = Clones(trees)
  private val products      = mutable.LinkedHashMap.empty[String, Model.Product]
  private val sums          = mutable.LinkedHashMap.empty[String, Model.Sum]
  private val lints         = mutable.ArrayBuffer.empty[Model.Lint]
  private val reflections   = mutable.ArrayBuffer.empty[Model.Reflection]
  private val constructions = mutable.LinkedHashMap.empty[String, Model.Construction]
  private val preconditions = mutable.ArrayBuffer.empty[Model.Precondition]
  private val reassignables = mutable.ArrayBuffer.empty[Model.Reassignable]
  private val writes        = mutable.LinkedHashSet.empty[Model.Write]
  private val stores        = mutable.Map.empty[String, Vector[(String, Map[String, Model.Stored])]]
  private val instances     = mutable.ArrayBuffer.empty[Model.Instance]
  private val costs         = mutable.ArrayBuffer.empty[Model.Cost]

  /** The model of the compilation, which holds the nodes of every unit the compiler compiled. */
  def model: Model.Model = {
    val found = edges.toVector.flatMap { case ((from, method), tos) => tos.toVector.map(to => Edge(from, to, method)) }
    Model.Model(
      nodes.values.toVector,
      methods.values.toVector,
      fields.values.toVector.flatten,
      sites.toVector ++ instances.toVector.flatMap(aligned(_)),
      found,
      clones.fragments,
      products.values.toVector,
      sums.values.toVector,
      lints.toVector,
      reflections.toVector,
      constructions.values.toVector,
      preconditions.toVector,
      reassignables.toVector,
      writes.toVector,
      costs.toVector,
      instances.toVector.filterNot(instance => nodes.contains(instance.owner) || CostTrees.standard(instance.owner)),
      constructions.keys.toVector.flatMap(owner =>
        stores.getOrElse(owner, Vector.empty).flatMap((constructor, kept) =>
          kept.map((field, held) => Model.Store(owner, constructor, field, held))
        )
      )
    )
  }

  /** A construction of a class, which is a site of it once the fields of the class are all known: its arguments are
    * those of the fields the constructor stores them in, and unknown for the other fields.
    */
  private def aligned(instance: Model.Instance): Option[Model.Site] =
    for {
      _      <- constructions.get(instance.owner)
      fields <- this.fields.get(instance.owner)
    } yield {
      val kept = stores.get(instance.owner).flatMap(_.collectFirst {
        case (constructor, kept) if constructor == instance.constructor => kept
      }).getOrElse(Map.empty)
      val values = Model.aligned(fields, kept, instance.arguments)
      Model.Site(instance.caller, instance.owner, values, instance.location)
    }

  /** The place a node is defined, which the findings name. */
  private def location(compilation: CompilationUnitTree, node: Tree): String =
    s"${Collector.source(compilation)}:${Collector.line(trees, compilation, node)}"

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
  // A javac visitor is an abstract class, so it is written inline.
  // The state is updated as the walk proceeds.
  @SuppressWarnings(Array("scalafix:BannedSyntax.anonymous-allocation", "scalafix:DisableSyntax.var"))
  private def collect(compilation: CompilationUnitTree, declaration: ClassTree): Unit = {
    val types   = mutable.Stack.empty[String]
    val owners  = mutable.Stack.empty[String]
    var unnamed = 0
    val scanner = new TreePathScanner[Unit, Unit] {

      override def visitClass(node: ClassTree, unused: Unit): Unit = {
        val path  = getCurrentPath
        val named =
          trees.namedElement(path).map(element => element -> Collector.qualified(element)).filter(_._2.nonEmpty)
        named match {
          case None =>
            unnamed += 1
            try super.visitClass(node, unused)
            finally unnamed -= 1
          case Some((element, name)) =>
            val _ = nodes.getOrElseUpdate(
              name,
              Model.Node(
                name,
                Collector.source(compilation),
                Collector.line(trees, compilation, node),
                Collector.exported(element),
                Collector.outer(element),
                Collector.suppressed(element),
                generated = Collector.generated(element),
                constructible = Collector.constructible(element)
              )
            )
            val _ = edges.getOrElseUpdate((name, ""), mutable.LinkedHashSet.empty)
            // A record is a product of its components, which the analysis reads alike with a case class.
            if (element.getKind == ElementKind.RECORD)
              products.update(name, Model.Product(name, name, Collector.components(element), Set.empty)): Unit
            // A sealed type is a sum of the types that extend it, which the analysis reads alike with a sealed trait.
            if (element.getModifiers.contains(Modifier.SEALED))
              sums.update(name, Model.Sum(name, name, Collector.variants(element))): Unit
            Guards.construction(trees, path).foreach(constructions.update(name, _))
            preconditions ++= Guards.preconditions(trees, path)
            stores.update(
              name,
              Guards.stores(trees, path).map((constructor, kept) => CostTrees.key(constructor) -> kept)
            )
            types.push(name)
            owners.push(name)
            try super.visitClass(node, unused)
            finally {
              val _ = owners.pop()
              val _ = types.pop()
            }
        }
      }

      override def visitMethod(node: MethodTree, unused: Unit): Unit = {
        val path  = getCurrentPath
        val named =
          trees.namedElement(path).map(element => element -> Collector.qualified(element)).filter(_._2.nonEmpty)
        // The owner of a method is the type that holds it, while the stack of owners holds the methods.
        val owner = types.headOption.getOrElse("")
        named match {
          case None                  => super.visitMethod(node, unused)
          case Some((element, name)) =>
            val executable = element.asInstanceOf[ExecutableElement]
            // A native method is called by the native code, which names it, and a main method is called by the code that
            // runs the build, so the type that holds either is entered from outside the build.
            if (Collector.enters(element))
              nodes.get(owner).foreach(found => nodes.update(owner, found.copy(entry = true)))
            val parameters = executable.getParameters.asScala.toVector.map(Collector.parameter(_))
            val key        = CostTrees.key(executable)
            val _          = methods.getOrElseUpdate(
              key,
              Model.Method(
                owner,
                name,
                key,
                Collector.exported(element),
                Collector.stored(trees, path),
                parameters,
                location(compilation, node),
                Collector.returns(executable),
                Collector.overriding(executable),
                Collector.suppressed(element)
              )
            )
            // The body of the method is what recurs, so the fragments are taken of it.
            node.body.foreach(clones.add(_, path, owner, executable, Collector.overriding(executable)))
            CostTrees.of(compilation, path, owner).foreach(costs += _)
            owners.push(key)
            try super.visitMethod(node, unused)
            finally { val _ = owners.pop() }
        }
      }

      override def visitVariable(node: VariableTree, unused: Unit): Unit = {
        val path = getCurrentPath
        // An instance field of the type is a field of the product the analysis makes of it; a parameter of a method is
        // one of the parameters of the method, which the method itself holds.
        trees.namedElement(path).filter(_ => unnamed == 0).foreach { element =>
          if (Collector.instanceField(element)) {
            val owner = owners.headOption.getOrElse("")
            fields.update(
              owner,
              fields.getOrElse(owner, Vector.empty) :+ Model.Field(owner, Collector.parameter(element))
            ): Unit
          }
          if (Collector.reassignable(node, element, Constructors.assigned(trees, path)))
            reassignables += Model.Reassignable(
              Collector.qualifiedEnclosing(element)(using trees),
              element.getSimpleName.toString,
              Collector.exported(element),
              Collector.source(compilation),
              Collector.line(trees, compilation, node),
              Collector.suppressed(element),
              node.initializer.isDefined
            ): Unit
        }
        super.visitVariable(node, unused)
      }

      override def visitAssignment(node: AssignmentTree, unused: Unit): Unit = {
        written(node.getVariable, plain = true)
        super.visitAssignment(node, unused)
      }

      override def visitCompoundAssignment(node: CompoundAssignmentTree, unused: Unit): Unit = {
        written(node.getVariable, plain = false)
        super.visitCompoundAssignment(node, unused)
      }

      override def visitUnary(node: UnaryTree, unused: Unit): Unit = {
        if (Collector.Steps(node.getKind)) written(node.getExpression, plain = false)
        super.visitUnary(node, unused)
      }

      @tailrec
      private def written(target: ExpressionTree, plain: Boolean): Unit = target match {
        case parenthesized: ParenthesizedTree       => written(parenthesized.getExpression, plain = false)
        case _: (IdentifierTree | MemberSelectTree) =>
          trees.namedElement(TreePath(getCurrentPath, target)).filter(Collector.field(_)).foreach { element =>
            val owner       = element.getEnclosingElement
            val constructor = plain && Constructors.direct(target) && Constructors.within(trees, getCurrentPath, owner)
            writes += Model.Write(Collector.qualified(owner), element.getSimpleName.toString, constructor): Unit
          }
        case _ =>
      }

      override def visitMethodInvocation(node: MethodInvocationTree, unused: Unit): Unit = {
        val path   = getCurrentPath
        val caller = owners.headOption.getOrElse("")
        Call.of(trees, path).filter(_.method.getKind == ElementKind.METHOD).foreach { call =>
          if (Collector.qualified(call.method).nonEmpty) {
            val callee    = CostTrees.key(call.method)
            val values    = call.arguments
            val arguments = values.map(Collector.argument(trees, path, _))
            val discarded = Collector.discarded(path)
            val negated   = Collector.negated(path)
            sites += Model.Site(caller, callee, arguments, location(compilation, node), discarded, negated): Unit
            // A class the code looks up by name is not read by the compiler, so a module build has to require its module
            // for the code to work at run time, which the analysis checks over the whole build.
            if (Collector.forName(call.method))
              values.headOption.flatMap(Collector.text(_)).foreach { target =>
                reflections += Model.Reflection(caller, target, location(compilation, node)): Unit
              }
          }
        }
        super.visitMethodInvocation(node, unused)
      }

      override def visitNewClass(node: NewClassTree, unused: Unit): Unit = {
        val path   = getCurrentPath
        val caller = owners.headOption.getOrElse("")
        trees.getElement(path) match {
          case constructor: ExecutableElement if node.classBody.isEmpty =>
            val arguments = Lints.arguments(node).map(Collector.argument(trees, path, _))
            constructor.getEnclosingElement match {
              case record: TypeElement if Collector.canonical(record, constructor) =>
                sites += Model.Site(caller, Collector.qualified(record), arguments, location(compilation, node)): Unit
              case declared: TypeElement if declared.getKind == ElementKind.CLASS =>
                instances += Model.Instance(
                  caller,
                  Collector.qualified(declared),
                  CostTrees.key(constructor),
                  arguments,
                  location(compilation, node)
                ): Unit
              case _ =>
            }
          case _ =>
        }
        super.visitNewClass(node, unused)
      }

      override def visitMemberReference(node: MemberReferenceTree, unused: Unit): Unit = {
        val path   = getCurrentPath
        val caller = owners.headOption.getOrElse("")
        trees.namedElement(path).filter(e =>
          e.getKind == ElementKind.METHOD || e.getKind == ElementKind.CONSTRUCTOR
        ).foreach {
          element =>
            if (Collector.qualified(element).nonEmpty) {
              val executable = element.asInstanceOf[ExecutableElement]
              val arguments  = Vector.fill(executable.getParameters.size)("unknown")
              sites += Model.Site(caller, CostTrees.key(executable), arguments, location(compilation, node)): Unit
            }
        }
        super.visitMemberReference(node, unused)
      }

      private def link(path: TreePath): Unit = {
        val owner  = types.headOption.getOrElse("")
        val target = Collector.reference(trees, path)
        if (target.isDefined && target.get != owner)
          edges.getOrElseUpdate(
            (owner, owners.headOption.filter(_ != owner).getOrElse("")),
            mutable.LinkedHashSet.empty
          ) +=
            target.get: Unit
      }

      // The walk reaches every node, so the missing element is tested for null rather than wrapped.
      @SuppressWarnings(Array("scalafix:DisableSyntax.null"))
      override def visitMemberSelect(node: MemberSelectTree, unused: Unit): Unit = {
        val path    = getCurrentPath
        val element = trees.getElement(path)
        // a type named through the type that holds it, as in a class literal of an annotation
        if ((element ne null) && (element.getKind.isClass || element.getKind.isInterface)) link(path)
        super.visitMemberSelect(node, unused)
      }

      override def visitIdentifier(node: IdentifierTree, unused: Unit): Unit = {
        link(getCurrentPath)
        super.visitIdentifier(node, unused)
      }
    }
    // The walk begins at the declaration, which the compilation unit holds, so that the paths of the code are the
    // ones the compiler attributed.
    scanner.scan(TreePath(TreePath(compilation), declaration), ())
  }
}
