package net.kurobako.baumkuchen.analysis

/** Selects the findings to report and decides which fail the build. */
object Report {

  /** The warnings of `failing` rules fail, except findings whose keys are in `baseline`. */
  case class Options(disabled: Set[String], failing: Set[String], baseline: Set[String], info: Boolean)

  /** A reported finding with its level: `error`, `warning` or `info`. */
  case class Line(level: String, finding: Finding) {
    def failing: Boolean = level == "error"

    def show: String = {
      val node     = finding.node
      val category = if (node.generated) "generated" else "handwritten"
      s"$level\t${finding.rule.id}\t$category\t${node.module}\t${node.name}\t${node.source}:${node.line}\t${finding.detail}"
    }
  }

  /** Every finding of every rule over an extracted build. */
  def findings(result: Extraction.Result): Vector[Finding] =
    Liveness.findings(result.graph) ++ Algebra.findings(result.algebra, result.graph.nodes) ++
      Invariants.findings(result.algebra, result.graph.nodes) ++
      Smells.findings(result.smells, result.algebra.products, result.graph.nodes) ++
      Clones.findings(result.clones, result.graph.nodes) ++
      Members.findings(result.members, result.graph.nodes, Liveness.live(result.graph)) ++
      Bytecode.findings(result.bytecode, result.graph.nodes) ++
      Complexity.findings(result.complexities, result.graph.nodes) ++
      Megamorphic.findings(result.megamorphic, result.graph.nodes) ++
      Purity.findings(result.purity, result.graph.nodes)

  def apply(findings: Vector[Finding], nodes: Map[String, Node], options: Options): Vector[Line] =
    findings.filter(finding =>
      !options.disabled(finding.rule.id) && !suppressed(finding, nodes, Some(finding.node.name)) &&
        (options.info || finding.level == Finding.Level.Warning)
    ).sorted.map { finding =>
      val escalated = finding.level == Finding.Level.Warning &&
        options.failing(finding.rule.id) && !options.baseline(finding.key)
      Line(if (escalated) "error" else finding.level.id, finding)
    }

  /** The baseline text recording `lines`, one finding key per line. */
  def write(lines: Vector[Line]): String = lines.map(_.finding.key).mkString("", "\n", "\n")

  def baseline(text: String): Set[String] = text.linesIterator.filter(_.nonEmpty).toSet

  // A suppression on a type covers everything declared inside it.
  @annotation.tailrec
  private def suppressed(finding: Finding, nodes: Map[String, Node], key: Option[String]): Boolean =
    key.flatMap(nodes.get) match {
      case Some(node) => finding.suppressedBy(node.suppressed) || suppressed(finding, nodes, node.outer)
      case None       => false
    }
}
