package net.kurobako.baumkuchen.analysis

import scala.collection.mutable

object Liveness {

  /** The rules finding definitions nothing uses, or only tests use. */
  val Rules: List[String] =
    List(Finding.Rule.Unused, Finding.Rule.TestOnly, Finding.Rule.UnusedMember, Finding.Rule.TestOnlyMember)
      .map(_.id)

  /** Main definitions unreachable from main entry points, reported at their outermost unreachable enclosure. */
  def findings(graph: Graph): Vector[Finding] = {
    val mainNodes   = graph.nodes.values.filterNot(_.test).toVector
    val mainRoots   = graph.roots.filter(name => graph.nodes.get(name).exists(!_.test))
    val testRoots   = graph.nodes.values.filter(_.test).map(_.name).toSet
    val fromMain    = reachable(graph.edges, mainRoots)
    val fromAnyRoot = reachable(graph.edges, mainRoots ++ testRoots)
    val findings    = mainNodes.collect {
      case node if !fromAnyRoot(node.name) => node.name -> Finding(Finding.Rule.Unused, node)
      case node if !fromMain(node.name)    => node.name -> Finding(Finding.Rule.TestOnly, node)
    }.toMap
    findings.values.filterNot { finding =>
      finding.node.outer.flatMap(findings.get).exists(_.rule == finding.rule)
    }.toVector
  }

  /** Nodes reachable from main entry points or from tests. */
  def live(graph: Graph): Set[String] = {
    val roots = graph.roots.filter(name => graph.nodes.get(name).exists(!_.test)) ++
      graph.nodes.values.filter(_.test).map(_.name)
    reachable(graph.edges, roots)
  }

  private[analysis] def reachable(edges: Map[String, Set[String]], roots: Set[String]): Set[String] = {
    val visited = mutable.Set.from(roots)
    val pending = mutable.Queue.from(roots)
    while (pending.nonEmpty)
      edges.getOrElse(pending.dequeue(), Set.empty).foreach(target => if (visited.add(target)) pending.enqueue(target))
    visited.toSet
  }
}
