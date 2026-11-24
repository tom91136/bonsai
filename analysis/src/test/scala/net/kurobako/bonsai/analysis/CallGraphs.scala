package net.kurobako.bonsai.analysis

import net.kurobako.bonsai.analysis.ClassFiles.*

object CallGraphs {

  /** The graph one walk reaches from `roots`, and then from `tests` as well. */
  def build(
      world: ClassWorld,
      roots: Set[Reference],
      build: String => Boolean = _ => false,
      tests: Set[Reference] = Set.empty
  ): CallGraph = {
    val walk = CallGraph.Walk(world, build)
    walk.extend(roots, tests)
    walk.result()
  }
}
