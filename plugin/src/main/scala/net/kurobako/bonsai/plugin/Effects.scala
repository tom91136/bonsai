package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.AppliedType
import dotty.tools.dotc.core.Types.MethodicType
import dotty.tools.dotc.core.Types.TypeBounds
import net.kurobako.bonsai.effects.*
import net.kurobako.bonsai.lifting.*
import scala.collection.mutable

private object Effects {
  val Checks: Set[Check] = Set(Check.EagerEffect)
}

/** Reports side effects a method returning an effect performs when it is called rather than when the effect runs, so
  * running the effect again, or discarding it, neither repeats nor skips them. An effect is a value of an abstract type
  * constructor, as in tagless final code, or of a known effect type; an opaque type is abstract only outside its scope,
  * and a type constructor bounded by a collection, as in `T[X] <: Iterable[X]`, holds values rather than effects. The
  * effects of calls are those the analysis infers, from the same axioms; calls returning `Unit` that nothing describes
  * as pure are side effects by definition, and methods of the unit that perform side effects pass them on to their
  * callers.
  */
private class Effects(checks: Checks)(using Context) {
  // A side effect, described, or a call to a method of the unit that may perform some.
  private case class Site(tree: tpd.Tree, effect: Either[Symbol, String])

  private val Known = Set("cats.effect.IO", "cats.effect.SyncIO", "zio.ZIO", "monix.eval.Task")
  // Reading is no side effect, and a call that may do anything is one only where its result is dropped.
  private val Eager   = Set(Kind.Writes, Kind.Output, Kind.Random)
  private val Dropped = Set(Kind.Reads, Kind.Unknown)
  private val axioms  = Calls()

  def run(tree: tpd.Tree): Unit = {
    val methods = tree.filterSubTrees {
      case definition: tpd.DefDef => !definition.symbol.is(Flags.Synthetic) && !definition.rhs.isEmpty
      case _                      => false
    }.collect { case definition: tpd.DefDef => definition.symbol -> definition }.toMap
    val sites = methods.view.mapValues { definition =>
      val walker = Sites(methods.contains)
      walker.traverse(definition.rhs)
      walker.found.toList
    }.toMap
    val causes = mutable.Map.empty[Symbol, Option[String]]
    // A method returning an effect only describes its callee's effects, and a recursive call adds none.
    def cause(method: Symbol): Option[String] = causes.getOrElse(
      method, {
        causes(method) = None
        val found =
          if (effect(methods(method)).isDefined) None
          else sites(method).view.flatMap(describe(_)).headOption
        causes(method) = found
        found
      }
    )
    def describe(site: Site): Option[String] =
      site.effect.fold(callee => cause(callee).map(inner => s"calls ${callee.name}, which $inner"), Some(_))
    methods.values.foreach(definition =>
      effect(definition).foreach(constructor =>
        sites(definition.symbol).foreach(site =>
          describe(site).foreach(effect =>
            checks(
              Check.EagerEffect,
              s"${definition.name} returns $constructor but $effect when called, not when the $constructor runs; " +
                "suspend it in the effect",
              site.tree.srcPos
            )
          )
        )
      )
    )
  }

  private def effect(definition: tpd.DefDef): Option[String] = definition.tpt.tpe.dealias match {
    case AppliedType(constructor, _)
        if constructor.typeSymbol.isAbstractOrParamType && !constructor.typeSymbol.is(Flags.Opaque) &&
          !container(constructor.typeSymbol) || Known(constructor.typeSymbol.fullName.toString) =>
      Some(constructor.typeSymbol.name.toString)
    case _ => None
  }

  private def container(symbol: Symbol): Boolean = symbol.info match {
    case bounds: TypeBounds => Classes.derives(bounds.hi.finalResultType, "scala.collection.IterableOnce")
    case _                  => false
  }

  /** The side effects and calls to methods of the unit that run when a definition is called: not those in nested
    * definitions, in functions that run later, or in arguments passed by name.
    */
  private class Sites(unit: Symbol => Boolean) extends tpd.TreeTraverser {
    val found: mutable.ListBuffer[Site] = mutable.ListBuffer.empty

    // Locals holding a value the method allocated, which mutating is invisible to its callers.
    private val allocated = mutable.Set.empty[Symbol]

    override def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
      case _: tpd.DefDef | _: tpd.TypeDef                   => ()
      case value: tpd.ValDef if value.symbol.is(Flags.Lazy) => ()
      case value: tpd.ValDef                                =>
        val symbol = value.symbol
        if (symbol.exists && !symbol.owner.isClass && !symbol.is(Flags.Mutable) && Calls.allocates(value.rhs))
          allocated += symbol
        traverseChildren(tree)
      case tpd.Assign(target, value) =>
        val symbol = target.symbol
        if (symbol.exists && symbol.owner.isClass)
          Calls.field(target, Kind.Writes, "", allocated).foreach(_ =>
            found += Site(tree, Right(s"assigns ${symbol.name}"))
          )
        traverse(value)
      case application @ tpd.Apply(function, arguments) =>
        call(application)
        traverse(function)
        val parameters = Trees.parameters(application)
        val eager      = Trees.eager(function)
        arguments.zipWithIndex.foreach((argument, index) =>
          Calls.lambda(argument) match {
            case Some(lambda) => if (eager) traverse(lambda.rhs)
            case None         => if (eager || !parameters.lift(index).exists(Trees.byName(_))) traverse(argument)
          }
        )
      case _ => traverseChildren(tree)
    }

    private def call(application: tpd.Apply): Unit = {
      val symbol   = application.fun.symbol
      val receiver = Calls.selected(application.fun).map(_.qualifier)
      if (symbol.exists && !symbol.isConstructor && !application.tpe.widen.isInstanceOf[MethodicType])
        if (unit(symbol)) found += Site(application, Left(symbol))
        else effects(application, symbol, receiver).foreach(kind =>
          found += Site(application, Right(described(kind, symbol)))
        )
    }

    private def effects(application: tpd.Apply, symbol: Symbol, receiver: Option[tpd.Tree]): Option[Kind] = {
      val unseen  = Calls.receiver(receiver, allocated) == Receiver.Fresh
      val dropped = application.tpe.widen.isRef(defn.UnitClass)
      Axioms.effects(axioms.of(symbol, receiver)).view.filterNot(effect => unseen && silent(effect)).map(_.kind)
        .find(kind => Eager(kind) || dropped && Dropped(kind))
    }

    // Acting on a value the method allocated is invisible to its callers, whatever the call does besides.
    private def silent(effect: Effect[Nothing]): Boolean = effect.onThis || effect.kind == Kind.Unknown

    private def described(kind: Kind, symbol: Symbol): String = {
      val name = symbol.name.show
      if (kind != Kind.Random) s"calls $name" else if (name == "randomUUID") "draws a UUID" else s"draws $name"
    }
  }
}
