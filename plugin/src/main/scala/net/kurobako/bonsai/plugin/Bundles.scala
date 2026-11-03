package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.Symbols.Symbol
import net.kurobako.bonsai.lifting.Calls
import scala.collection.mutable

private object Bundles {
  val Checks: Set[Check] = Set(Check.ParameterObjectUnpacked)
}

/** Reports a private case class that is built at one call, passed to a method, and unpacked there at once into its
  * fields, so that nothing but the parameter count of that method needed it. The class must have at least four fields,
  * appear in no other signature, value or construction, and the method must not touch the parameter after unpacking.
  */
private class Bundles(checks: Checks)(using Context) {
  private val MinimumFields = 4

  def run(tree: tpd.Tree): Unit = {
    val classes = mutable.ListBuffer.empty[tpd.TypeDef]
    tree.foreachSubTree {
      case definition: tpd.TypeDef if definition.rhs.isInstanceOf[tpd.Template] && candidate(definition.symbol) =>
        classes += definition: Unit
      case _ => ()
    }
    if (classes.nonEmpty) {
      val scan = Scan(classes.map(_.symbol).toSet)
      scan.traverse(tree)
      classes.foreach(definition => bundle(definition, scan))
    }
  }

  private def candidate(cls: Symbol): Boolean =
    cls.isClass && cls.is(Flags.CaseClass) && (cls.is(Flags.Private) || cls.owner.isTerm) &&
      cls.caseAccessors.sizeIs >= MinimumFields

  private def bundle(definition: tpd.TypeDef, scan: Scan): Unit = {
    val cls = definition.symbol
    (scan.constructions.getOrElse(cls, Nil), scan.consumers.getOrElse(cls, Nil)) match {
      case (List(site), List((method, parameter)))
          if scan.mentions.getOrElse(cls, 0) == 1 && scan.passed.get(site).contains(method.symbol) &&
            unpacks(method, parameter.symbol, cls) =>
        checks(
          Check.ParameterObjectUnpacked,
          s"${cls.name} is built at one call and unpacked by ${method.symbol.name} at once; pass its fields as " +
            "parameters",
          definition.srcPos
        )
      case _ => ()
    }
  }

  // The leading statements bind the fields of the parameter, which the rest of the body leaves alone.
  private def unpacks(method: tpd.DefDef, parameter: Symbol, cls: Symbol): Boolean = {
    val (stats, result) = method.rhs match {
      case tpd.Block(stats, expression) => (stats, expression)
      case other                        => (Nil, other)
    }
    val accessors                            = cls.caseAccessors.toSet
    val holders                              = mutable.Set.empty[Symbol]
    def unpacked(value: tpd.ValDef): Boolean = Calls.peelInlined(value.rhs) match {
      case select: tpd.Select if select.qualifier.symbol == parameter => accessors(select.symbol)
      case select: tpd.Select                                         => holders(select.qualifier.symbol)
      case tpd.Match(selector, _) if selector.symbol == parameter     => holders += value.symbol: Unit; true
      case _                                                          => false
    }
    val prefix = stats.takeWhile {
      case value: tpd.ValDef => unpacked(value)
      case _                 => false
    }
    prefix.nonEmpty && !(stats.drop(prefix.size) :+ result).exists(_.existsSubTree(_.symbol == parameter))
  }

  private class Scan(candidates: Set[Symbol]) extends tpd.TreeTraverser {
    val constructions = mutable.Map.empty[Symbol, List[tpd.Apply]]
    val consumers     = mutable.Map.empty[Symbol, List[(tpd.DefDef, tpd.ValDef)]]
    val mentions      = mutable.Map.empty[Symbol, Int]
    val passed        = mutable.Map.empty[tpd.Apply, Symbol]

    private def built(application: tpd.Apply)(using Context): Symbol = {
      val function = application.fun.symbol
      if (function.isPrimaryConstructor) function.owner
      else if (function.is(Flags.Synthetic) && function.name == nme.apply && function.owner.is(Flags.ModuleClass))
        function.owner.companionClass
      else if (function.is(Flags.Synthetic) && function.name == nme.copy) function.owner
      else NoSymbol
    }

    private def mention(tpe: dotty.tools.dotc.core.Types.Type)(using Context): Unit =
      candidates.foreach(cls => if (tpe.existsPart(_.typeSymbol == cls)) mentions(cls) = mentions.getOrElse(cls, 0) + 1)

    override def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
      case method: tpd.DefDef if method.symbol.is(Flags.Synthetic) => ()
      case method: tpd.DefDef                                      =>
        mention(method.tpt.tpe)
        method.termParamss.flatten.foreach { parameter =>
          val cls = parameter.tpt.tpe.widen.classSymbol
          if (candidates(cls)) consumers(cls) = consumers.getOrElse(cls, Nil) :+ (method -> parameter)
        }
        traverseChildren(tree)
      case value: tpd.ValDef =>
        mention(value.tpt.tpe)
        traverseChildren(tree)
      case application: tpd.Apply =>
        val cls = built(application)
        if (candidates(cls)) constructions(cls) = constructions.getOrElse(cls, Nil) :+ application
        application.args.foreach(argument =>
          Calls.peelInlined(argument) match {
            case inner: tpd.Apply if candidates(built(inner)) => passed(inner) = application.fun.symbol
            case _                                            => ()
          }
        )
        traverseChildren(tree)
      case _ => traverseChildren(tree)
    }
  }
}
