package net.kurobako.bonsai.plugin

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.report
import dotty.tools.dotc.util.NoSource
import scala.collection.mutable

private class AcyclicPhase(analysis: Analysis, checks: Checks, problems: List[String]) extends PluginPhase {
  val phaseName: String                 = "acyclic"
  override val runsAfter: Set[String]   = Set("posttyper")
  override val runsBefore: Set[String]  = Set("pickler")
  override def run(using Context): Unit = ()

  // 3.4.0 does not suspend compilations, and does not say so.
  private def suspended(using Context): Boolean =
    try ctx.run.isCompilingSuspended
    catch { case _: NoSuchMethodError => false }

  override def runOn(units: List[CompilationUnit])(using ctx: Context): List[CompilationUnit] = {
    problems.foreach(report.error(_))
    val sources = units.filterNot(_.isJava).sortBy(_.source.path)
    // Macro expansion can resume suspended files in further runs of the same compilation.
    if (!suspended) analysis.clear()
    analysis.include(ctx.run.files.map(_.path))
    analysis.forget(sources.map(_.source.path).toSet)
    val paths = analysis.paths
    val edges = analysis.edges

    def valid(symbol: Symbol): Boolean =
      symbol != NoSymbol && Option(symbol.source).exists(source => source != NoSource && paths(source.path))

    sources.foreach(checks.remember)
    sources.foreach(unit => Edges(unit.source.path, valid(_), analysis).traverse(unit.tpdTree))

    val byPair  = edges.toVector.groupBy { case ((from, to, _), _) => (from, to) }
    val checked = byPair.filterNot { case (_, dependencies) =>
      // Removing compile-time edges exempts exactly the cycles that cross a macro boundary.
      dependencies.forall(_._1._3)
    }
    val graph = paths.map(path => path -> Set.empty[String]).toMap ++
      checked.keys.toVector.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
    // One shortest cycle shows a dependency to break; the other files of the component follow, to break theirs.
    for (group <- AcyclicPhase.components(graph).filter(_.size > 1)) {
      val cycle = AcyclicPhase.shortest(graph, group)
      val steps = (cycle :+ cycle.head).sliding(2).toVector.map { pair =>
        val step = (pair(0), pair(1))
        step -> checked(step).minBy(_._2.start)._2
      }
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

private object AcyclicPhase {
  // Iterative Kosaraju traversal avoids overflowing on long dependency chains.
  private def components(graph: Map[String, Set[String]]): Vector[Vector[String]] = {
    val visited  = mutable.Set.empty[String]
    val finished = mutable.ArrayBuffer.empty[String]
    for (start <- graph.keys.toVector.sorted if !visited(start)) {
      val stack = mutable.Stack((start, false))
      while (stack.nonEmpty) {
        val (node, returning) = stack.pop()
        if (returning) finished += node
        else if (visited.add(node)) {
          stack.push((node, true))
          graph(node).toVector.sorted.reverse.foreach(child => stack.push((child, false)))
        }
      }
    }
    val reverse = mutable.Map.from(graph.keys.map(_ -> mutable.Set.empty[String]))
    for {
      (from, targets) <- graph
      to              <- targets
    } reverse(to) += from
    visited.clear()
    val result = Vector.newBuilder[Vector[String]]
    for (start <- finished.reverse if !visited(start)) {
      val members = Vector.newBuilder[String]
      val stack   = mutable.Stack(start)
      while (stack.nonEmpty) {
        val node = stack.pop()
        if (visited.add(node)) {
          members += node
          reverse(node).foreach(stack.push)
        }
      }
      result += members.result().sorted
    }
    result.result().sortBy(_.head)
  }

  /** The shortest cycle through the members of a component, as its files in order, searching breadth-first from each
    * member back to itself.
    */
  private def shortest(graph: Map[String, Set[String]], members: Vector[String]): Vector[String] = {
    val inside                   = members.toSet
    def successors(node: String) = graph(node).filter(inside).toVector.sorted
    @annotation.tailrec
    def search(start: String, paths: Vector[Vector[String]], seen: Set[String]): Option[Vector[String]] =
      paths.find(path => graph(path.last)(start)) match {
        case Some(path) => Some(path)
        case None       =>
          val next = paths.flatMap(path => successors(path.last).filterNot(seen).map(path :+ _)).distinctBy(_.last)
          if (next.isEmpty) None else search(start, next, seen ++ next.map(_.last))
      }
    members.flatMap(start => search(start, Vector(Vector(start)), Set(start))).minBy(_.size)
  }
}
