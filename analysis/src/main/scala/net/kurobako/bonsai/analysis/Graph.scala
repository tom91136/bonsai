package net.kurobako.bonsai.analysis

object Graph {

  /** The sets of `left` with those of `right` added under the same keys. */
  def union[K](left: Map[K, Set[String]], right: Iterable[(K, Set[String])]): Map[K, Set[String]] =
    right.foldLeft(left) { case (all, (key, values)) => all.updated(key, all.getOrElse(key, Set.empty) ++ values) }
}

/** The nodes of a build, the references between them, and the types the program is entered at from outside it.
  *
  * `declared` holds the references outside any method: the signatures, parents, imports and class bodies of a node.
  * `bodies` holds those inside a method the program runs as such, by the method, so that a reference from a method no
  * root reaches can be told apart; `edges` is every reference of either kind.
  */
case class Graph(
    nodes: Map[String, Node],
    declared: Map[String, Set[String]],
    roots: Set[Root],
    bodies: Map[Use, Set[String]] = Map.empty
) {

  lazy val edges: Map[String, Set[String]] = Graph.union(declared, bodies.groupMapReduce(_._1.node)(_._2)(_ ++ _))

  /** The names of the types that are roots, whatever their kind. */
  lazy val rooted: Set[String] = roots.map(_.name)
}
