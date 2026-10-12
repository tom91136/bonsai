package net.kurobako.baumkuchen.scalafix

import scala.meta.*
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

  /** The type constructor of a value or parameterless method, through aliases. */
  def valueType(symbol: Symbol)(using SemanticDocument): Option[Symbol] = {
    @annotation.tailrec
    def resolve(tpe: SemanticType, visited: Set[Symbol]): Option[Symbol] = tpe match {
      case ExistentialType(inner, _)                       => resolve(inner, visited)
      case AnnotatedType(_, inner)                         => resolve(inner, visited)
      case TypeRef(_, reference, _) if !visited(reference) =>
        reference.info.map(_.signature) match {
          case Some(TypeSignature(_, lower, upper)) if lower == upper => resolve(upper, visited + reference)
          case _                                                      => Some(reference)
        }
      case _ => None
    }
    symbol.info.map(_.signature).flatMap {
      case ValueSignature(tpe)          => resolve(tpe, Set.empty)
      case MethodSignature(_, Nil, tpe) => resolve(tpe, Set.empty)
      case _                            => None
    }
  }

  /** Statements of an object, class or trait body. */
  def members(template: Template): List[Stat] = template.body.stats
}
