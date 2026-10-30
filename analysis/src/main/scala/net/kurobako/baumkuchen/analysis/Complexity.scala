package net.kurobako.baumkuchen.analysis

/** Methods whose time or allocation grows faster than linearly in the sizes of collections they reach. */
object Complexity {

  /** A member, by qualified name glob as for `hot`, whose collections hold at most `max` elements, as the domain
    * declares; `reason` says why.
    */
  case class Bounded(member: String, max: Int, reason: String)

  /** A product of sizes with their powers, and of logarithms of sizes. */
  case class Term(sizes: Vector[(String, Int)], logs: Vector[String]) {
    def degree: Int = sizes.map(_._2).sum

    def show: String =
      (sizes.map((size, power) => if (power == 1) s"|$size|" else s"|$size|^$power") ++
        logs.map(size => s"log|$size|")).mkString(" * ")
  }

  /** The highest-degree terms of a bound, and the expression that contributes them, with the loop it runs in. */
  case class Hit(terms: Vector[Term], what: String, within: Option[String], line: Int) {
    def degree: Int = terms.map(_.degree).maxOption.getOrElse(0)

    def shown: String = terms.map(_.show).sorted.mkString(" + ")

    def cause: String = s"$what${within.fold("")(loop => s" inside $loop")}"
  }

  /** The bounds of one method: its time, its allocation, and linear work its callbacks do once per element. */
  case class Bounds(time: Option[Hit], space: Option[Hit], callback: Option[Hit]) {
    def nonEmpty: Boolean = time.isDefined || space.isDefined || callback.isDefined
  }

  case class Summary(name: String, node: String, bounds: Bounds, suppressed: Set[String])

  def findings(summaries: Vector[Summary], nodes: Map[String, Node], hot: Option[Set[String]]): Vector[Finding] =
    summaries.flatMap { summary =>
      val name   = Finding.short(summary.name)
      val bounds = summary.bounds
      val merged = bounds.time.zip(bounds.space).exists((time, space) => time.terms.toSet == space.terms.toSet)
      val timed  = bounds.time.flatMap(hit =>
        located(
          nodes,
          summary,
          hit,
          s"$name is O(${hit.shown}) in time${if (merged) " and space" else ""}: ${hit.cause}"
        )
      )
      val spaced = bounds.space.filterNot(_ => merged).flatMap(hit =>
        located(nodes, summary, hit, s"$name allocates O(${hit.shown}): ${hit.cause}", Finding.Rule.SpaceComplexity)
      )
      val called = bounds.callback.filter(_ => hot.exists(_.contains(summary.node))).flatMap(hit =>
        located(nodes, summary, hit, s"$name does linear work for each element of a loop in hot code: ${hit.cause}")
      )
      timed.toVector ++ spaced ++ called
    }

  private def located(
      nodes: Map[String, Node],
      summary: Summary,
      hit: Hit,
      detail: String,
      rule: Finding.Rule = Finding.Rule.Complexity
  ): Option[Finding] =
    Finding
      .at(
        nodes,
        summary.node,
        rule,
        detail,
        if (hit.degree >= 3) Finding.Level.Error else Finding.Level.Warning,
        suppressed = summary.suppressed
      )
      .map(found => found.copy(node = if (hit.line > 0) found.node.copy(line = hit.line) else found.node))
}
