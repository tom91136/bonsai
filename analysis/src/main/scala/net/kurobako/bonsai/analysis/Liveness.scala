package net.kurobako.bonsai.analysis

import scala.collection.mutable

object Liveness {

  /** Main definitions no entry point reaches, reported at their outermost unreachable enclosure. A type is reached when
    * the program runs one of its methods or instantiates it. Generated definitions mirror the table they come from,
    * whose own consumers decide which entries live.
    */
  def findings(graph: Graph, reach: Reach): Vector[Finding] = {
    val mainNodes = graph.nodes.values.filter(node => !node.test && !node.generated).toVector
    val findings  = mainNodes.collect {
      case node if !reach.live(node.name) =>
        node.name ->
          Finding(Finding.Rule.Unused, node, s"${Finding.simple(node.name)} is unreachable from every entry point")
      case node if !reach.main(node.name) =>
        node.name -> Finding(Finding.Rule.TestOnly, node, s"${Finding.simple(node.name)} is reachable only from tests")
    }.toMap
    findings.values.filterNot { finding =>
      finding.node.outer.flatMap(findings.get).exists(_.rule == finding.rule)
    }.toVector
  }

  private[analysis] def reachable(edges: Map[String, Set[String]], roots: Set[String]): Set[String] = {
    val visited = mutable.Set.from(roots)
    val pending = mutable.Queue.from(roots)
    while (pending.nonEmpty)
      edges.getOrElse(pending.dequeue(), Set.empty).foreach(target => if (visited.add(target)) pending.enqueue(target))
    visited.toSet
  }
}
