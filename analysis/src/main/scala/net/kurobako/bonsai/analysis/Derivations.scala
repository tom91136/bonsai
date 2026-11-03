package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.StdNames.tpnme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.RefinedType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeTraverser
import dotty.tools.dotc.inlines.Inlines
import scala.annotation.tailrec
import scala.collection.mutable

private[analysis] object Derivations {

  /** The case classes a type is built from. */
  def cases(tpe: Type)(using Context): Set[Symbol] = classes(tpe)(_.is(Flags.CaseClass))

  private def classes(tpe: Type)(accept: Symbol => Boolean)(using Context): Set[Symbol] = {
    val found = mutable.Set.empty[Symbol]
    new TypeTraverser {
      def traverse(tpe: Type): Unit = {
        val cls = tpe.dealias.classSymbol
        if (cls.isClass && accept(cls)) found += cls
        traverseChildren(tpe.dealias)
      }
    }.traverse(tpe.widen)
    found.toSet
  }

  private def built(tpe: Type)(using Context): Set[Symbol] = classes(tpe)(structured)

  private def structured(cls: Symbol)(using Context): Boolean = cls.is(Flags.CaseClass) || cls.is(Flags.Sealed)

  // The classes a derived instance of `cls` recurses into: the types of its fields and the cases of a sum.
  private def components(cls: Symbol)(using Context): Set[Symbol] = {
    val fields =
      if (cls.is(Flags.CaseClass))
        cls.info.decls.toList.filter(_.is(Flags.ParamAccessor)).flatMap(field =>
          classes(field.info.widenExpr)(structured)
        )
      else Nil
    val subclasses =
      if (cls.is(Flags.Sealed))
        cls.children.map(child => if (child.isTerm) child.moduleClass else child).filter(_.isClass)
      else Nil
    (fields ++ subclasses).toSet
  }

  // Generic derivation of an instance reads every field of a class, which no use shows; `CanEqual` reads none.
  private def declared(cls: Symbol)(using Context): Boolean =
    cls.companionModule.exists && cls.companionModule.moduleClass.info.decls.toList.exists { member =>
      val name = member.name.toString
      name.startsWith("derived$") && name != "derived$CanEqual"
    }

  // The type a summoned mirror stands for is a refinement of the mirror type, which no other expression has.
  @tailrec private def mirrored(tpe: Type)(using Context): Option[Type] = tpe.dealias match {
    case refined: RefinedType =>
      if (refined.refinedName == tpnme.MirroredType) Some(refined.refinedInfo.bounds.hi) else mirrored(refined.parent)
    case _ => None
  }
}

/** The case classes that code the build does not show builds and reads field by field: those with a `derives` clause,
  * those a generic derivation reaches through an inline call or a summoned mirror, and everything such a derivation
  * recurses into: the case classes among the field types and the cases of a sealed type. The walk stops at classes the
  * build does not define, as no finding concerns their fields. A class can be reached from any file of the build, so
  * the evidence is complete only once every file is traversed.
  */
private[analysis] class Derivations(defined: Symbol => Context ?=> Boolean) {
  import Derivations.*

  private val reached  = mutable.Set.empty[Symbol]
  private val generics = mutable.Map.empty[Symbol, Boolean]
  private var spread   = Option.empty[Set[Symbol]]

  // Expansion is invisible after pickling, so a method derives only if its body can depend on a type argument beyond
  // what typing it shows: a macro, a mirror, or compile-time inspection.
  private def generic(method: Symbol)(using Context): Boolean =
    method.is(Flags.Inline) && generics.getOrElseUpdate(
      method,
      method.is(Flags.Macro) ||
        method.paramSymss.flatten.exists(_.info.widen.derivesFrom(requiredClass("scala.deriving.Mirror"))) ||
        Inlines.hasBodyToInline(method) && Inlines.bodyToInline(method).existsSubTree {
          case tpd.Splice(_)                  => true
          case tree: (tpd.Ident | tpd.Select) =>
            tree.symbol.exists && tree.symbol.owner.fullName.toString.startsWith("scala.compiletime")
          case tree: tpd.Match => tree.isInline
          case tree: tpd.If    => tree.isInline
          case _               => false
        }
    )

  def derived(cls: Symbol)(using Context): Boolean = spread.getOrElse(spreading()).contains(cls)

  private def spreading()(using Context): Set[Symbol] = {
    val visited = mutable.Set.empty[Symbol]
    val pending = mutable.Stack.from(reached)
    while (pending.nonEmpty) {
      val cls = pending.pop()
      if (defined(cls) && visited.add(cls)) pending.pushAll(components(cls))
    }
    spread = Some(visited.toSet)
    visited.toSet
  }

  /** Records what a tree derives: the type arguments of an inline or macro call, which survive pickling although the
    * expansion does not, and the type of a mirror.
    */
  def observe(tree: tpd.Tree)(using Context): Unit = {
    tree match {
      case call: tpd.TypeApply if generic(call.fun.symbol) =>
        call.args.foreach(argument => reached ++= built(argument.tpe))
      case defn: tpd.TypeDef if defn.symbol.isClass && declared(defn.symbol) => reached += defn.symbol
      case _                                                                 => ()
    }
    if (tree.isTerm && tree.tpe != null) mirrored(tree.tpe.widen).foreach(tpe => reached ++= built(tpe))
  }
}
