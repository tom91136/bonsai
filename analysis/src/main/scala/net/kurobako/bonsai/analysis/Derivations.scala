package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.StdNames.tpnme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Symbols.requiredPackage
import dotty.tools.dotc.core.Types.RefinedType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeTraverser
import dotty.tools.dotc.inlines.Inlines
import net.kurobako.bonsai.lifting.*
import scala.annotation.tailrec
import scala.collection.mutable

private[analysis] object Derivations {

  /** The case classes a type is built from. */
  def cases(tpe: Type)(using Context): Set[Symbol] = classes(tpe)(_.is(Flags.CaseClass))

  private def classes(tpe: Type)(accept: Symbol => Boolean)(using Context): Set[Symbol] = {
    val traverser = Classes(accept)
    traverser.traverse(tpe.widen)
    traverser.found.toSet
  }

  private class Classes(accept: Symbol => Boolean)(using Context) extends TypeTraverser {
    val found: mutable.Set[Symbol] = mutable.Set.empty[Symbol]

    def traverse(tpe: Type): Unit = {
      val cls = tpe.dealias.classSymbol
      if (cls.isClass && accept(cls)) found += cls
      traverseChildren(tpe.dealias)
    }
  }

  private def built(tpe: Type)(using Context): Set[Symbol] = classes(tpe)(structured(_))

  private def structured(cls: Symbol)(using Context): Boolean = cls.is(Flags.CaseClass) || cls.is(Flags.Sealed)

  // The classes a derived instance of `cls` recurses into: the types of its fields and the cases of a sum.
  private def components(cls: Symbol)(using Context): Set[Symbol] = {
    val fields =
      if (cls.is(Flags.CaseClass))
        cls.info.decls.toList.filter(_.is(Flags.ParamAccessor)).flatMap(field =>
          classes(field.info.widenExpr)(structured(_))
        )
      else Nil
    val subclasses =
      if (cls.is(Flags.Sealed))
        Symbols.childClasses(cls).filter(_.isClass)
      else Nil
    (fields ++ subclasses).toSet
  }

  // Generic derivation of an instance reads every field of a class, which no use shows; `CanEqual` reads none.
  private def declared(cls: Symbol)(using Context): Boolean =
    cls.companionModule.exists && cls.companionModule.moduleClass.info.decls.exists { member =>
      member.name.toString.startsWith("derived$") && member.info.finalResultType.classSymbol != defn.CanEqualClass
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
private[analysis] class Derivations(symbols: Symbols, foreign: Set[Output]) {
  import Derivations.*

  private def defined(cls: Symbol)(using Context): Boolean = symbols.key(cls).isDefined

  private val reached  = mutable.Set.empty[Symbol]
  private val generics = mutable.Map.empty[Symbol, Boolean]
  private val spread   = mutable.ArrayBuffer.empty[Set[Symbol]]

  // Expansion is invisible after pickling, so a method derives only if its body can depend on a type argument beyond
  // what typing it shows: a macro, a mirror, or compile-time inspection.
  private def generic(method: Symbol)(using Context): Boolean =
    method.is(Flags.Inline) && generics.getOrElseUpdate(
      method,
      method.is(Flags.Macro) ||
        method.paramSymss.flatten.exists(_.info.widen.derivesFrom(Classes.required("scala.deriving.Mirror"))) ||
        Inlines.hasBodyToInline(method) && Inlines.bodyToInline(method).existsSubTree {
          case tpd.Splice(_)                  => true
          case tree: (tpd.Ident | tpd.Select) =>
            tree.symbol.exists &&
            tree.symbol.owner.ownersIterator.contains(requiredPackage("scala.compiletime").moduleClass)
          case tree: tpd.Match => tree.isInline
          case tree: tpd.If    => tree.isInline
          case _               => false
        }
    )

  private var received = Set.empty[String] // scalafix:ok DisableSyntax.var

  def receive(others: Set[String]): Unit = received = others

  def derived(cls: Symbol)(using Context): Boolean =
    spread.headOption.getOrElse(spreading()).contains(cls) || received(symbols.identified(cls))

  /** The classes this run did not read that its code derives, by name. */
  def evidence(using Context): Set[String] =
    if (foreign.isEmpty) Set.empty
    else spread.headOption.getOrElse(
      spreading()
    ).filter(cls => symbols.output(cls).exists(foreign)).map(symbols.identified(_))

  private def spreading()(using Context): Set[Symbol] = {
    val visited = mutable.Set.empty[Symbol]
    val pending = mutable.Stack.from(reached)
    while (pending.nonEmpty) {
      val cls = pending.pop()
      if (defined(cls) && visited.add(cls)) pending.pushAll(components(cls))
    }
    val found = visited.toSet
    spread += found
    found
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
    if (tree.isTerm) Option(tree.tpe).foreach(tpe => mirrored(tpe.widen).foreach(mirror => reached ++= built(mirror)))
  }
}
