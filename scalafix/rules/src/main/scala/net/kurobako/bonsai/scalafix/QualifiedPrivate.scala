package net.kurobako.bonsai.scalafix

import scala.meta.*
import scalafix.v1.*

/** Access qualified by an enclosing type, such as `private[Outer]`, that plain `private` already grants: every use is
  * inside the definition's owner or its companion. An enclosing type is declared in one file, so its uses are all here.
  */
class QualifiedPrivate extends SemanticRule("QualifiedPrivate") {
  override def fix(using document: SemanticDocument): Patch = {
    // `new C(...)` records the class rather than its constructor, a creator application `C(...)` the class, and one of
    // a generic class nothing, so that falls back to the classes of the same name here.
    def constructor(cls: Symbol) = Symbol(cls.value.stripSuffix("#") + "#`<init>`().")
    val classes = document.tree.collect { case owner: Defn.Class => owner.name.value -> owner.name.symbol }
      .groupMap(_._1)(_._2)
    @annotation.tailrec
    def created(function: Term): List[Symbol] = function match {
      case application: Term.ApplyType                     => created(application.fun)
      case reference: Term.Name if reference.symbol.isNone =>
        classes.getOrElse(reference.value, Nil).map(constructor(_))
      case select: Term.Select if select.symbol.isNone =>
        classes.getOrElse(select.name.value, Nil).map(constructor(_))
      case reference: (Term.Name | Term.Select) if reference.symbol.value.endsWith("#") =>
        List(constructor(reference.symbol))
      case _ => Nil
    }
    val uses = document.tree.collect {
      case init: Init              => List(constructor(definitions.symbol(init.tpe)) -> init.pos)
      case application: Term.Apply => created(application.fun).map(_ -> application.pos)
      case name: Name              => List(name.symbol -> name.pos)
    }.flatten.groupMap(_._1)(_._2)
    document.tree.collect {
      case modifier @ Mod.Private(within: Name) if within.value.nonEmpty && enclosingType(modifier, within.value) =>
        target(modifier).map { (symbol, owner) =>
          if (uses.getOrElse(symbol, Nil).forall(use => scope(owner).exists(contains(_, use))))
            lint(
              "redundant-qualifier",
              s"private[${within.value}] grants nothing plain private does not; every use is within its own type",
              modifier.pos
            )
          else Patch.empty
        }.asPatch
    }.asPatch
  }

  // A qualifier names an enclosing type or package, the innermost first; SemanticDB omits it on primary constructors.
  private def enclosingType(modifier: Mod.Private, qualifier: String): Boolean =
    ancestry.ancestors(modifier).collectFirst {
      case owner: Defn.Class if owner.name.value == qualifier        => true
      case owner: Defn.Trait if owner.name.value == qualifier        => true
      case owner: Defn.Object if owner.name.value == qualifier       => true
      case pkg: Pkg if pkg.ref.syntax.split('.').contains(qualifier) => false
    }.contains(true)

  /** The qualified definition and the type whose members plain `private` would leave it visible to. Overrides keep the
    * access of what they override, `@publicInBinary` needs a qualifier, and a type may appear in wider signatures.
    */
  private def target(modifier: Mod.Private)(using SemanticDocument): Option[(Symbol, Defn)] =
    modifier.parent.filterNot {
      case definition: Stat.WithMods =>
        definitions.has[Mod.Override](definition.mods) || binary(definition.mods)
      case _ => false
    }.flatMap {
      case constructor: Ctor.Primary => constructor.parent.collect { case owner: Defn.Class =>
          Symbol(owner.name.symbol.value + "`<init>`().") -> owner
        }
      case parameter: Term.Param => parameter.parent.flatMap(_.parent).flatMap(_.parent).collect {
          case owner: Defn.Class => Symbol(owner.name.symbol.value + parameter.name.value + ".") -> owner
        }
      case _: (Defn.Class | Defn.Trait | Defn.Object | Defn.Type | Defn.Enum) => None
      case definition: Defn => named(definition).zip(owner(definition))
      case _                => None
    }

  private def named(definition: Defn)(using SemanticDocument): Option[Symbol] = definition match {
    case member: Member  => Some(member.name.symbol)
    case value: Defn.Val => value.pats.collectFirst { case Pat.Var(name) => name.symbol }
    case value: Defn.Var => value.pats.collectFirst { case Pat.Var(name) => name.symbol }
    case _               => None
  }

  private def owner(definition: Defn): Option[Defn] = definition.parent.flatMap(_.parent).flatMap(_.parent).collect {
    case owner: (Defn.Class | Defn.Trait | Defn.Object) => owner
  }

  // Plain private reaches the owner and its companion.
  private def scope(owner: Defn): List[Defn] = owner :: owner.parent.toList.flatMap(_.children.collect {
    case companion: Defn.Object if !owner.isInstanceOf[Defn.Object] && companion.name.value == name(owner) => companion
    case companion: Defn.Class if owner.isInstanceOf[Defn.Object] && companion.name.value == name(owner)   => companion
    case companion: Defn.Trait if owner.isInstanceOf[Defn.Object] && companion.name.value == name(owner)   => companion
  })

  private def name(definition: Defn): String = definition match {
    case member: Member => member.name.value
    case _              => ""
  }

  private def binary(mods: List[Mod])(using SemanticDocument): Boolean = mods.exists {
    case Mod.Annot(init) => definitions.symbol(init.tpe).value == "scala/annotation/publicInBinary#"
    case _               => false
  }

  private def contains(tree: Tree, position: Position): Boolean =
    tree.pos.start <= position.start && position.end <= tree.pos.end
}
