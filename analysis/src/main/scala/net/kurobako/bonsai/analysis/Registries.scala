package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.Symbol
import net.kurobako.bonsai.lifting.Calls

/** Values of one type that a collection literal of their siblings, all of one type constructor, registers. */
private object Registries {
  private val Collections     = Set("Vector", "Seq", "List", "Set", "IndexedSeq")
  private val MinimumRegistry = 2

  /** The values of a class of the kind such a registry collects that none of the registries lists. */
  def unlisted(template: tpd.Template)(using Context): Set[Symbol] = {
    val values = template.body.collect {
      case value: tpd.ValDef
          if !value.symbol.isOneOf(Flags.Synthetic | Flags.Mutable | Flags.ParamAccessor | Flags.Module) => value
    }
    val kinds = values.flatMap { value =>
      val kind = value.symbol.info.widen.dealias.typeSymbol
      Option.when(kind.exists)(value.symbol -> kind)
    }.toMap
    val listed = values.flatMap(value => registered(value.rhs, kinds)).groupMapReduce(_._1)(_._2)(_ ++ _)
    kinds.collect { case (symbol, kind) if listed.get(kind).exists(!_(symbol)) => symbol }.toSet
  }

  private def registered(tree: tpd.Tree, kinds: Map[Symbol, Symbol])(using Context): Option[(Symbol, Set[Symbol])] =
    tree match {
      case tpd.Apply(function, arguments) if collection(function) =>
        val members = Predicates.spread(arguments).map(element => Calls.peel(element).symbol)
        Option.when(
          members.size >= MinimumRegistry && members.forall(kinds.contains) && members.map(kinds).distinct.size == 1
        )(kinds(members.head) -> members.toSet)
      case _ => None
    }

  @annotation.tailrec
  private def collection(function: tpd.Tree)(using Context): Boolean = function match {
    case tpd.TypeApply(inner, _)                          => collection(inner)
    case tpd.Select(qualifier, name) if name == nme.apply =>
      val factory = qualifier.symbol
      Collections(factory.name.toString) && factory.fullName.toString.startsWith("scala.")
    case _ => false
  }
}
