package net.kurobako.bonsai.analysis

/** Selects the findings to report and decides which fail the build. */
object Report {

  /** The warnings of `failing` rules fail, as do errors except those of `lenient` rules, and in either case except
    * findings whose keys are in `baseline`.
    */
  case class Options(
      disabled: Set[String],
      failing: Set[String],
      baseline: Set[String],
      info: Boolean,
      lenient: Set[String] = Set.empty
  )

  /** A reported finding with its level: `error`, `warning` or `info`; `repeat` counts the findings before it with the
    * same id, which it numbers apart.
    */
  case class Line(level: String, finding: Finding, repeat: Int = 0) {
    def failing: Boolean = level == "error"

    def id: String = if (repeat == 0) finding.id else s"${finding.id}-${repeat + 1}"

    def show: String = {
      val node     = finding.node
      val category = if (node.generated) "generated" else "handwritten"
      s"$level\t${finding.rule.id}\t$category\t${node.module}\t${node.name}\t${node.source}:${node.line}\t" +
        s"${Finding.escaped(finding.detail)}\t$id"
    }
  }

  /** Every finding of every rule over an extracted build. */
  def findings(
      result: Extraction.Result,
      intended: Intended = Intended(),
      nesting: Nesting.Config = Nesting.Config()
  ): Vector[Finding] =
    Liveness.findings(result.graph) ++ Algebra.findings(result.algebra, result.graph.nodes, intended) ++
      Invariants.findings(result.algebra, result.graph.nodes) ++
      Smells.findings(result.smells, result.algebra.products, result.graph.nodes, result.hot, result.graph.edges) ++
      Designs.findings(result.designs, result.graph.nodes) ++ Structures.findings(result.structures, result.graph) ++
      Clones.findings(result.clones, result.graph.nodes, intended) ++
      Members.findings(result.members, result.graph.nodes, Liveness.live(result.graph)) ++
      Bytecode.findings(result.bytecode, result.graph.nodes, result.hot) ++
      Complexity.findings(result.complexities, result.graph.nodes, result.hot) ++
      Pipelines.findings(result.pipelines, result.graph.nodes) ++
      Megamorphic.findings(result.megamorphic, result.graph.nodes) ++
      Purity.findings(result.purity, result.graph.nodes) ++
      JavaLints.findings(result.lints, result.graph.nodes) ++ Nesting.findings(result.nesting, nesting)

  def apply(findings: Vector[Finding], nodes: Map[String, Node], options: Options): Vector[Line] = {
    // numbered over every finding, so hiding some does not renumber the others
    val sorted    = findings.sorted
    val numbering = sorted.foldLeft((Map.empty[String, Int], Vector.empty[(Finding, Int)])) {
      case ((seen, numbered), finding) =>
        val repeat = seen.getOrElse(finding.id, 0)
        (seen.updated(finding.id, repeat + 1), numbered :+ (finding -> repeat))
    }._2
    numbering.view.collect {
      case (finding, repeat)
          if !options.disabled(finding.rule.id) && !finding.suppressedBy(finding.suppressed) &&
            !suppressed(finding, nodes, Some(finding.node.name)) &&
            (options.info || finding.level != Finding.Level.Info) =>
        val id        = finding.rule.id
        val escalated = !options.baseline(finding.key) &&
          (finding.level match {
            case Finding.Level.Error   => !options.lenient(id)
            case Finding.Level.Warning => options.failing(id)
            case Finding.Level.Info    => false
          })
        val level = if (finding.level == Finding.Level.Info) "info" else "warning"
        Line(if (escalated) "error" else level, finding, repeat)
    }.toVector
  }

  /** The baseline text recording `lines`, one finding key per line. */
  def write(lines: Vector[Line]): String = lines.map(_.finding.key).mkString("", "\n", "\n")

  /** The keys a file of one key per line records, such as a baseline or a purity lock. */
  def keys(text: String): Set[String] = text.linesIterator.filter(_.nonEmpty).toSet

  // A suppression on a type covers everything declared inside it.
  @annotation.tailrec
  private def suppressed(finding: Finding, nodes: Map[String, Node], key: Option[String]): Boolean =
    key.flatMap(nodes.get) match {
      case Some(node) => finding.suppressedBy(node.suppressed) || suppressed(finding, nodes, node.outer)
      case None       => false
    }
}
