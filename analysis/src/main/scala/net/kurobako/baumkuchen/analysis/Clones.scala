package net.kurobako.baumkuchen.analysis

/** Code fragments whose structure recurs elsewhere in the build, up to the renaming of local definitions. */
object Clones {
  case class Fragment(node: String, path: String, start: Int, end: Int, test: Boolean) {
    def lines: Int = end - start + 1
  }

  /** Copies of one fragment; `cost` counts the tree nodes of each copy. */
  case class Group(cost: Int, fragments: Vector[Fragment])

  private val ShownCopies = 4

  def findings(groups: Vector[Group], nodes: Map[String, Node]): Vector[Finding] = groups.flatMap { group =>
    val first     = group.fragments.head
    val locations = group.fragments.map(fragment => s"${fragment.path}:${fragment.start}-${fragment.end}")
    Finding.at(
      nodes,
      first.node,
      Finding.Rule.DuplicateCode,
      s"${group.fragments.size} copies of ${first.lines} lines (${group.cost} nodes): " +
        Finding.listed(locations, ShownCopies),
      onlyTests = group.fragments.forall(_.test)
    )
  }
}
