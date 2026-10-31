package net.kurobako.bonsai.scalafix

import scala.meta.*

private object ancestry {

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
      case block: Term.Block                       => localBlock(block)
      case _: Defn.Def | _: Defn.Val | _: Defn.Var => true
      case _                                       => false
    }
  }

  /** The trees enclosing `tree`, innermost first. */
  def ancestors(tree: Tree): List[Tree] =
    Iterator.iterate(tree.parent)(_.flatMap(_.parent)).takeWhile(_.isDefined).flatten.toList

  /** Whether `tree` is outside every import and export. */
  def outsideImports(tree: Tree): Boolean = ancestors(tree).forall {
    case _: Import | _: Export => false
    case _                     => true
  }

  /** Whether `block` is a scope of its own, not the methods of an extension group, which belong to the scope around it.
    */
  def localBlock(block: Term.Block): Boolean = !block.parent.exists(_.isInstanceOf[Defn.ExtensionGroup])
}
