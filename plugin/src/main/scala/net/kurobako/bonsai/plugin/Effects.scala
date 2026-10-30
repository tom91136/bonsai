package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.AppliedType
import dotty.tools.dotc.core.Types.MethodicType
import scala.collection.mutable

/** Reports side effects a method returning an effect performs when it is called rather than when the effect runs, so
  * running the effect again, or discarding it, neither repeats nor skips them. An effect is a value of an abstract type
  * constructor, as in tagless final code, or of a known effect type; an opaque type is abstract only outside its scope.
  * Calls returning `Unit` are side effects by definition; methods of the unit that perform side effects pass them on to
  * their callers.
  */
private class Effects(checks: Checks)(using Context) {
  // A side effect, described, or a call to a method of the unit that may perform some.
  private case class Site(tree: tpd.Tree, effect: Either[Symbol, String])

  private val Known      = Set("cats.effect.IO", "cats.effect.SyncIO", "zio.ZIO", "monix.eval.Task")
  private val Validation = Set("scala.Predef.require", "scala.Predef.assert", "scala.Predef.assume")
  private val Updates = "(set|lazySet|compare|weakCompare|getAnd|increment|decrement|add|update|accumulate|reset).*".r
  private val Draws   = "(next|between|shuffle|ints|longs|doubles|alphanumeric).*".r
  private val Randoms = List(requiredClass("java.util.Random"), requiredClass("scala.util.Random"))

  def run(tree: tpd.Tree): Unit = {
    val methods = tree.filterSubTrees {
      case definition: tpd.DefDef => !definition.symbol.is(Flags.Synthetic) && !definition.rhs.isEmpty
      case _                      => false
    }.collect { case definition: tpd.DefDef => definition.symbol -> definition }.toMap
    val sites = methods.view.mapValues { definition =>
      val walker = Sites(definition, methods.contains)
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
        if constructor.typeSymbol.isAbstractOrParamType && !constructor.typeSymbol.is(Flags.Opaque) ||
          Known(constructor.typeSymbol.fullName.toString) =>
      Some(constructor.typeSymbol.name.toString)
    case _ => None
  }

  /** The side effects and calls to methods of the unit that run when `definition` is called: not those in nested
    * definitions, in functions that run later, or in arguments passed by name.
    */
  private class Sites(definition: tpd.DefDef, unit: Symbol => Boolean) extends tpd.TreeTraverser {
    val found: mutable.ListBuffer[Site] = mutable.ListBuffer.empty

    override def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
      case _: tpd.DefDef | _: tpd.TypeDef                   => ()
      case value: tpd.ValDef if value.symbol.is(Flags.Lazy) => ()
      case tpd.Assign(target, value)                        =>
        if (target.symbol.exists && target.symbol.owner.isClass) found +=
          Site(tree, Right(s"assigns ${target.symbol.name}"))
        traverse(value)
      case application @ tpd.Apply(function, arguments) =>
        call(application)
        traverse(function)
        val parameters = Trees.parameters(application)
        val eager      = Trees.eager(function)
        arguments.zipWithIndex.foreach((argument, index) =>
          Trees.lambda(argument) match {
            case Some(lambda) => if (eager) traverse(lambda.rhs)
            case None         => if (eager || !parameters.lift(index).exists(Trees.byName(_))) traverse(argument)
          }
        )
      case _ => traverseChildren(tree)
    }

    private def call(application: tpd.Apply): Unit = {
      val symbol   = application.fun.symbol
      val name     = symbol.name.toString
      val receiver = Trees.selected(application.fun).map(_.qualifier)
      if (symbol.exists && !symbol.isConstructor && !application.tpe.widen.isInstanceOf[MethodicType])
        if (Trees.qualified(symbol.owner).startsWith("java.util.concurrent.atomic.") && Updates.matches(name))
          found += Site(application, Right(s"calls $name"))
        else if (receiver.exists(tree => Randoms.exists(tree.tpe.widen.derivesFrom(_))) && Draws.matches(name))
          found += Site(application, Right(s"draws $name"))
        else if (Trees.qualified(symbol) == "java.util.UUID.randomUUID") found +=
          Site(application, Right("draws a UUID"))
        else if (unit(symbol)) found += Site(application, Left(symbol))
        else if (
          application.tpe.widen.isRef(defn.UnitClass) && !Validation(Trees.qualified(symbol)) &&
          !receiver.exists(created(_))
        ) found += Site(application, Right(s"calls $name"))
    }

    // Mutating a value the method itself created is invisible to its callers.
    private def created(receiver: tpd.Tree): Boolean = {
      val symbol = receiver.symbol
      symbol.exists && symbol.isTerm && !symbol.is(Flags.Param) && !symbol.owner.isClass &&
      symbol.ownersIterator.contains(definition.symbol)
    }
  }
}
