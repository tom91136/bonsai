package net.kurobako.baumkuchen.plugin

import scala.collection.mutable

private object DependencyGraph {
  // Iterative Kosaraju traversal avoids overflowing on long dependency chains.
  def components(graph: Map[String, Set[String]]): Vector[Vector[String]] = {
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
}
