package net.kurobako.bonsai.java

import com.sun.source.tree.*
import net.kurobako.bonsai.collections.*

/** The sinks of the expressions of a method. */
private class CostFlow(sinks: collection.Map[Tree, Sink]) {
  def of(tree: Tree): Sink = sinks.getOrElse(tree, Sink.Returned)
}
