package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.ClassSymbol
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.Type

private object Equality {
  val Checks: Set[Check] = Set(Check.UnstableEquality)
}

/** Reports case class fields whose types compare by reference or change after construction, so `equals` and `hashCode`
  * stop describing the value: arrays, functions, mutable collections, and open traits whose one abstract method lambdas
  * implement, unless an object of the unit implements the trait, which makes it a strategy.
  */
private class Equality(checks: Checks)(using Context) {
  private val Strategies = Set(
    "scala.math.Ordering",
    "scala.math.PartialOrdering",
    "scala.math.Equiv",
    "java.util.Comparator",
    "java.lang.Comparable"
  )

  private val MutableLibraries = Set("ujson.Value", "ujson.Obj", "ujson.Arr")

  def run(tree: tpd.Tree): Unit = {
    val classes = tree.filterSubTrees {
      case definition: tpd.TypeDef => definition.symbol.isClass && definition.rhs.isInstanceOf[tpd.Template]
      case _                       => false
    }.collect { case definition @ tpd.TypeDef(_, template: tpd.Template) => (definition, template) }
    val objects =
      classes.collect { case (definition, _) if definition.symbol.is(Flags.Module) => definition.symbol.asClass }
    classes.filter((definition, _) => definition.symbol.is(Flags.Case) && !definition.symbol.is(Flags.Module)).foreach {
      (definition, template) =>
        template.constr.termParamss.headOption.toList.flatten.foreach(parameter =>
          reason(parameter.tpt.tpe, objects).foreach(why =>
            checks(
              Check.UnstableEquality,
              s"${parameter.name}: $why, so ${definition.name} has unstable equality",
              parameter.srcPos
            )
          )
        )
    }
  }

  private def reason(tpe: Type, objects: List[ClassSymbol]): Option[String] = {
    val widened = tpe.widen.dealias
    val symbol  = widened.classSymbol
    if (widened.isRef(defn.ArrayClass)) Some("arrays compare by reference")
    else if (defn.isFunctionType(widened)) Some("functions compare by reference")
    else if (lambdaShaped(widened, symbol, objects))
      Some("single-method traits that lambdas implement compare by reference")
    else if (mutable(symbol)) Some("mutable fields change the value after construction")
    else None
  }

  private def mutable(symbol: Symbol): Boolean = {
    val name = symbol.fullName.toString
    symbol.exists &&
    (name.startsWith("scala.collection.mutable.") || name == "java.util.ArrayList" ||
      name == "java.util.HashMap" || name == "java.util.HashSet" || name == "java.util.LinkedList" ||
      MutableLibraries(name))
  }

  private def lambdaShaped(tpe: Type, symbol: Symbol, objects: List[ClassSymbol]): Boolean =
    symbol.exists && symbol.is(Flags.Trait) && !symbol.is(Flags.Sealed) && !Strategies(symbol.fullName.toString) &&
      !objects.exists(_.derivesFrom(symbol)) &&
      (tpe.possibleSamMethods match {
        case Seq(method) => method.info.paramInfoss.nonEmpty
        case _           => false
      })
}
