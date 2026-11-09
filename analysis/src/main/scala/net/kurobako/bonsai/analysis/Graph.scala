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
  *
  * `inlines` holds the uses that expand each inline method, by its key, and `splices` the methods the splices of each
  * inline method call. An inline method is live when a live use expands it, and then its splices are roots. `arguments`
  * holds the types of the build that the type arguments of each such expansion name, with the use that expands it;
  * those types are read by reflection when the method expands a splice.
  */
case class Graph(
    nodes: Map[String, Node],
    declared: Map[String, Set[String]],
    roots: Set[Root],
    bodies: Map[Use, Set[String]] = Map.empty,
    inlines: Map[String, Set[Use]] = Map.empty,
    splices: Map[String, Set[Use]] = Map.empty,
    arguments: Map[String, Set[(Use, String)]] = Map.empty
) {

  /** The inline methods that expand a splice, directly or through the inline methods they expand. */
  lazy val macros: Set[String] = {
    @annotation.tailrec
    def settle(found: Set[String]): Set[String] = {
      val next = found ++ found.flatMap(inlines.getOrElse(_, Set.empty).flatMap(_.method).filter(inlines.contains))
      if (next == found) found else settle(next)
    }
    settle(splices.collect { case (key, targets) if targets.nonEmpty => key }.toSet)
  }

  lazy val edges: Map[String, Set[String]] = Graph.union(declared, bodies.groupMapReduce(_._1.node)(_._2)(_ ++ _))

  /** The names of the types that are roots, whatever their kind. */
  lazy val rooted: Set[String] = roots.map(_.name)
}
