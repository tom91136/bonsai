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
    // One shortest cycle shows a dependency to break; the other files of the component follow, to break theirs.
    for (group <- DependencyGraph.components(graph).filter(_.size > 1)) {
      val cycle = DependencyGraph.shortest(graph, group)
      val steps = (cycle :+ cycle.head).sliding(2).toVector.map(pair => (pair(0), pair(1)))
        .map(step => step -> checked(step).minBy(_._2.start)._2)
      val details = steps.map { case ((from, to), position) => s"  $from:${position.line + 1} -> $to" }
      val others  = group.filterNot(cycle.toSet)
      val files   = if (others.size == 1) "file" else "files"
      val rest    =
        if (others.isEmpty) "" else s"\nand ${others.size} more $files on cycles with it:\n  ${others.mkString("\n  ")}"
      val message =
        s"Cyclic source dependency (${group.size} files), through the shortest cycle:\n${details.mkString("\n")}$rest"
      checks(Check.Acyclic, message, steps.head._2)
    }
    units
  }
}
