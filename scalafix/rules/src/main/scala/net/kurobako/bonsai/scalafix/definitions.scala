package net.kurobako.bonsai.scalafix

import scala.meta.*
import scala.util.control.NonFatal
import scalafix.v1.*

private object definitions {
  def public(mods: List[Mod]): Boolean = !mods.exists {
    case _: Mod.Private | _: Mod.Protected => true
    case _                                 => false
  }

  /** Whether code outside the definitions enclosing `definition`, which carries `mods`, can reach it: neither it nor an
    * enclosing definition is private, or private to an enclosing definition, and none of them is local.
    */
  def reachable(definition: Tree, mods: List[Mod]): Boolean = {
    val enclosing = ancestors(definition)
    val names     = enclosing.collect {
      case owner: Defn.Class  => owner.name.value
      case owner: Defn.Trait  => owner.name.value
      case owner: Defn.Object => owner.name.value
    }.toSet
    def hidden(mods: List[Mod]) = mods.exists {
      case Mod.Private(Name.Anonymous() | Term.This(_)) => true
      case Mod.Private(qualifier)                       => names(qualifier.toString)
      case _                                            => false
    }
    !hidden(mods) && !enclosing.exists {
      case owner: Defn.Class                       => hidden(owner.mods)
      case owner: Defn.Trait                       => hidden(owner.mods)
      case owner: Defn.Object                      => hidden(owner.mods)
      case block: Term.Block                       => !extensionBody(block)
      case _: Defn.Def | _: Defn.Val | _: Defn.Var => true
      case _                                       => false
    }
  }

  /** Comparisons of a size with zero that state emptiness. */
  val zeroComparisons: Set[String] = Set(">", "==", "!=")

  /** Whether `size` is compared with zero, which states emptiness rather than counting. */
  def zeroComparison(size: Term): Boolean = size.parent.exists {
    case comparison: Term.ApplyInfix =>
      comparison.argClause.values match {
        case List(Lit.Int(0)) => zeroComparisons(comparison.op.value)
        case _                => false
      }
    case _ => false
  }

  /** Whether `mods` carry an annotation named `name`, written bare or qualified. */
  def annotated(mods: List[Mod], name: String): Boolean = mods.exists {
    case Mod.Annot(init) =>
      init.tpe match {
        case Type.Name(annotation)                 => annotation == name
        case Type.Select(_, Type.Name(annotation)) => annotation == name
        case _                                     => false
      }
    case _ => false
  }

  /** The trees enclosing `tree`, innermost first. */
  def ancestors(tree: Tree): List[Tree] =
    Iterator.iterate(tree.parent)(_.flatMap(_.parent)).takeWhile(_.isDefined).flatten.toList

  /** Whether `block` holds the methods of an extension group, which belong to the scope around it. */
  def extensionBody(block: Term.Block): Boolean = block.parent.exists(_.isInstanceOf[Defn.ExtensionGroup])

  def has[M <: Mod](mods: List[Mod])(using tag: scala.reflect.ClassTag[M]): Boolean =
    mods.exists(tag.runtimeClass.isInstance(_))

  def parameters(definition: Defn.Def): List[Term.Param] =
    definition.paramClauseGroups.flatMap(_.paramClauses).filterNot(_.mod.nonEmpty).flatMap(_.values)

  /** The symbol of a declared type, looking through application and annotations. */
  @annotation.tailrec
  def symbol(tpe: Type)(using SemanticDocument): Symbol = tpe match {
    case application: Type.Apply => symbol(application.tpe)
    case Type.Annotate(base, _)  => symbol(base)
    case other                   => other.symbol
  }

  private val mutableJava =
    Set("java/util/ArrayList#", "java/util/HashMap#", "java/util/HashSet#", "java/util/LinkedList#")

  def mutableType(symbol: Symbol): Boolean =
    symbol.value.startsWith("scala/collection/mutable/") || symbol.value == "scala/Array#" ||
      mutableJava(symbol.value) || MutableLibraries(symbol.value)

  // ujson's values hold mutable maps and buffers.
  private val MutableLibraries = Set("ujson/Value#", "ujson/Obj#", "ujson/Arr#")

  def function(tpe: Type): Boolean = tpe match {
    case _: Type.Function | _: Type.ContextFunction => true
    case _                                          => false
  }

  /** The information of a symbol, or none when the symbol table cannot read its class file, as for JDK classes on a JDK
    * newer than the ASM the symbol table reads them with.
    */
  def info(symbol: Symbol)(using SemanticDocument): Option[SymbolInformation] =
    try symbol.info
    catch {
      case NonFatal(_) => None
    }

  /** Whether the term names a method that takes arguments, which the compiler eta expands where a function is wanted.
    */
  def methodReference(term: Term)(using SemanticDocument): Boolean = term match {
    case reference: (Term.Name | Term.Select) =>
      info(reference.symbol).map(_.signature).exists {
        case MethodSignature(_, parameterLists, _) =>
          parameterLists.filterNot(_.exists(parameter => parameter.isImplicit || parameter.isGiven)).exists(_.nonEmpty)
        case _ => false
      }
    case _ => false
  }

  /** The type constructor of a value or parameterless method, through aliases. */
  def valueType(symbol: Symbol)(using SemanticDocument): Option[Symbol] = {
    @annotation.tailrec
    def resolve(tpe: SemanticType, visited: Set[Symbol]): Option[Symbol] = tpe match {
      case ExistentialType(inner, _)                       => resolve(inner, visited)
      case AnnotatedType(_, inner)                         => resolve(inner, visited)
      case ByNameType(inner)                               => resolve(inner, visited)
      case TypeRef(_, reference, _) if !visited(reference) =>
        info(reference).map(_.signature) match {
          case Some(TypeSignature(_, lower, upper)) if lower == upper => resolve(upper, visited + reference)
          case _                                                      => Some(reference)
        }
      case _ => None
    }
    info(symbol).map(_.signature).flatMap {
      case ValueSignature(tpe)          => resolve(tpe, Set.empty)
      case MethodSignature(_, Nil, tpe) => resolve(tpe, Set.empty)
      case _                            => None
    }
  }

  /** The type constructor of a named value, through aliases. */
  def typeSymbol(term: Term)(using SemanticDocument): Option[String] = (term match {
    case reference: (Term.Name | Term.Select) => Some(reference.symbol)
    case _                                    => None
  }).flatMap(valueType(_)).map(_.value)

  def mapType(term: Term)(using SemanticDocument): Boolean = typeSymbol(term).exists(_.endsWith("Map#"))

  def isNone(tree: Term)(using SemanticDocument): Boolean = tree match {
    case Term.Name("None") | Term.Select(_, Term.Name("None")) => tree.symbol.value == "scala/None."
    case _                                                     => false
  }

  /** Statements of an object, class or trait body. */
  def members(template: Template): List[Stat] = template.body.stats
}
