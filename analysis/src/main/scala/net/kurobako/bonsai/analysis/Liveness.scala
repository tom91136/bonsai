package net.kurobako.bonsai.analysis

import scala.collection.mutable

object Liveness {

  /** Main definitions unreachable from main entry points, reported at their outermost unreachable enclosure. Generated
    * definitions mirror the table they come from, whose own consumers decide which entries live.
    */
  def findings(graph: Graph): Vector[Finding] = {
    val mainNodes   = graph.nodes.values.filter(node => !node.test && !node.generated).toVector
    val fromMain    = reachable(graph.edges, mainRoots(graph))
    val fromAnyRoot = live(graph)
    val findings    = mainNodes.collect {
      case node if !fromAnyRoot(node.name) =>
        node.name ->
          Finding(Finding.Rule.Unused, node, s"${Finding.simple(node.name)} is unreachable from every entry point")
      case node if !fromMain(node.name) =>
        node.name -> Finding(Finding.Rule.TestOnly, node, s"${Finding.simple(node.name)} is reachable only from tests")
    }.toMap
    findings.values.filterNot { finding =>
      finding.node.outer.flatMap(findings.get).exists(_.rule == finding.rule)
    }.toVector
  }

  /** Nodes reachable from main entry points or from tests. */
  def live(graph: Graph): Set[String] =
    reachable(graph.edges, mainRoots(graph) ++ graph.nodes.values.view.collect { case node if node.test => node.name })

  private def mainRoots(graph: Graph): Set[String] =
    graph.roots.filter(name => graph.nodes.get(name).exists(!_.test))

  private[analysis] def reachable(edges: Map[String, Set[String]], roots: Set[String]): Set[String] = {
    val visited = mutable.Set.from(roots)
    val pending = mutable.Queue.from(roots)
    while (pending.nonEmpty)
      edges.getOrElse(pending.dequeue(), Set.empty).foreach(target => if (visited.add(target)) pending.enqueue(target))
    visited.toSet
  }
}
