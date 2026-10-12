package net.kurobako.baumkuchen.plugin

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.util.NoSource

private class AcyclicPhase(analysis: Analysis, checks: Checks) extends PluginPhase {
  val phaseName: String                 = "acyclic"
  override val runsAfter: Set[String]   = Set("posttyper")
  override val runsBefore: Set[String]  = Set("pickler")
  override def run(using Context): Unit = ()

  override def runOn(units: List[CompilationUnit])(using ctx: Context): List[CompilationUnit] = {
    val sources = units.filterNot(_.isJava).sortBy(_.source.path)
    // Macro expansion can resume suspended files in further runs of the same compilation.
    if (!ctx.run.isCompilingSuspended) analysis.clear()
    analysis.include(ctx.run.files.map(_.path))
    analysis.forget(sources.map(_.source.path).toSet)
    val paths = analysis.paths
    val edges = analysis.edges

    def valid(symbol: Symbol): Boolean =
      symbol != NoSymbol && Option(symbol.source).exists(source => source != NoSource && paths(source.path))

    sources.foreach(unit => Edges(unit.source.path, valid(_), analysis).traverse(unit.tpdTree))

    val byPair  = edges.toVector.groupBy { case ((from, to, _), _) => (from, to) }
    val checked = byPair.filterNot { case (_, dependencies) =>
      // Removing compile-time edges exempts exactly the cycles that cross a macro boundary.
      dependencies.forall(_._1._3)
    }
    val graph = paths.map(path => path -> Set.empty[String]).toMap ++
      checked.keys.toVector.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
    for (group <- DependencyGraph.components(graph).filter(_.size > 1)) {
      val members  = group.toSet
      val internal = checked.toVector.filter { case ((from, to), _) => members(from) && members(to) }.sortBy(_._1)
      val details  = internal.map { case ((from, to), dependencies) =>
        s"  $from:${dependencies.minBy(_._2.start)._2.line + 1} -> $to"
      }
      val remainder = if (details.size > 12) s"\n  ${details.size - 12} more dependencies" else ""
      val message   = s"Cyclic source dependency (${group.size} files):\n${details.take(12).mkString("\n")}$remainder"
      val position  = internal.head._2.head._2
      checks(Check.Acyclic, message, position)
    }
    units
  }
}
