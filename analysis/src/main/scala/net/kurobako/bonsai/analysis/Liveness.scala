package net.kurobako.bonsai.analysis

import scala.collection.mutable

object Liveness {

  /** Main definitions no entry point reaches, reported at their outermost unreachable enclosure. A type is reached when
    * the program runs one of its methods or instantiates it. Generated definitions mirror the table they come from,
    * whose own consumers decide which entries live.
    */
  def findings(graph: Graph, reach: Reach): Vector[Finding] = {
    val mainNodes = graph.nodes.values.filter(node => !node.test && !node.generated).toVector
    val support   = testSupport(graph, reach)
    val findings  = mainNodes.collect {
      case node if !reach.live(node.name) =>
        node.name ->
          Finding(Finding.Rule.Unused, node, s"${Finding.simple(node.name)} is unreachable from every entry point")
      case node if !reach.main(node.name) && !support(node.module) =>
        node.name -> Finding(Finding.Rule.TestOnly, node, s"${Finding.simple(node.name)} is reachable only from tests")
    }.toMap
    findings.values.filterNot { finding =>
      finding.node.outer.flatMap(findings.get).exists(_.rule == finding.rule)
    }.toVector
  }

  /** The modules whose main code exists to serve tests: no other module's main code uses them, and either other
    * modules' tests do or no root but a test reaches them.
    */
  def testSupport(graph: Graph, reach: Reach): Set[String] = {
    val crossing = for {
      (from, targets) <- graph.edges.toVector
      source          <- graph.nodes.get(from).toVector
      target          <- targets.flatMap(graph.nodes.get) if !target.test && target.module != source.module
    } yield target.module -> source.test
    val usedByMain  = crossing.collect { case (module, false) => module }.toSet
    val usedByTests = crossing.collect { case (module, true) => module }.toSet
    graph.nodes.values.filterNot(_.test).groupBy(_.module).collect {
      case (module, nodes)
          if !usedByMain(module) && (usedByTests(module) || !nodes.exists(node => reach.main(node.name))) =>
        module
    }.toSet
  }

  private[analysis] def reachable(edges: Map[String, Set[String]], roots: Set[String]): Set[String] = {
    val visited = mutable.Set.from(roots)
    val pending = mutable.Queue.from(roots)
    while (pending.nonEmpty)
      edges.getOrElse(pending.dequeue(), Set.empty).foreach(target => if (visited.add(target)) pending.enqueue(target))
    visited.toSet
  }
}
