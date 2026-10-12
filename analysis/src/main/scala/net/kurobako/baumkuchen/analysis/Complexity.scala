package net.kurobako.baumkuchen.analysis

/** Methods whose work grows faster than linearly in the sizes of their collection parameters. */
object Complexity {

  /** A method's highest-degree cost terms, each a list of parameter names with their exponents. */
  case class Summary(name: String, node: String, terms: Vector[Vector[(String, Int)]])

  def findings(summaries: Vector[Summary], nodes: Map[String, Node]): Vector[Finding] = summaries.flatMap { summary =>
    val shown = summary.terms.map(_.sortBy(_._1).map((size, power) =>
      if (power == 1) s"|$size|" else s"|$size|^$power"
    ).mkString(" * ")).sorted.mkString(" + ")
    Finding.at(
      nodes,
      summary.node,
      Finding.Rule.Complexity,
      s"${Finding.short(summary.name)} is O($shown) in the sizes of its parameters",
      Finding.Level.Info
    )
  }
}
