package net.kurobako.bonsai.analysis

/** Selects the findings to report and decides which fail the build. */
object Report {

  /** The rules a disable entry covers: one by id, or every rule. */
  enum Selector {
    case Named(id: String)
    case All

    def covers(id: String): Boolean = this match {
      case Named(named) => named == id
      case All          => true
    }
  }

  /** Where a disable entry applies: everywhere, or over generated code only. */
  enum Scope {
    case Everywhere
    case Generated
  }

  case class Disable(selector: Selector, scope: Scope) {
    def covers(finding: Finding): Boolean =
      selector.covers(finding.rule.id) && (scope == Scope.Everywhere || finding.node.generated)
  }

  /** The warnings of `failing` rules fail, as do errors except those of `lenient` rules, and in either case except
    * findings whose keys are in `baseline`. `disabled` names the findings dropped, by rule and scope.
    */
  case class Options(
      disabled: Set[Disable],
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

  /** Every finding of every rule over an extracted build, the rules read on `parallel` and the findings joined in the
    * order of the rules.
    */
  def findings(
      result: Extraction.Result,
      intended: Intended = Intended(),
      nesting: Nesting.Config = Nesting.Config(),
      parallel: Parallel = Parallel.sequential
  ): Vector[Finding] = {
    val nodes = result.graph.nodes
    val rules = Vector[() => Vector[Finding]](
      () => Liveness.findings(result.graph, result.reach),
      () => Algebra.findings(result.algebra, nodes, intended),
      () => Invariants.findings(result.algebra, nodes),
      () => Smells.findings(result.smells, result.algebra.products, nodes, result.hot, result.graph.edges),
      () => Designs.findings(result.designs, nodes),
      () => Structures.findings(result.structures, result.graph),
      () => Equality.findings(result.equality, nodes),
      () => Clones.findings(result.clones, nodes, intended, result.graph.edges),
      () => Members.findings(result.members, nodes, result.reach, Liveness.testSupport(result.graph, result.reach)),
      () => Bytecode.findings(result.bytecode, nodes, result.hot),
      () => Complexity.findings(result.complexities, nodes, result.hot),
      () => Pipelines.findings(result.pipelines, nodes),
      () => Megamorphic.findings(result.megamorphic, nodes),
      () => Megamorphic.sharedFindings(result.shared, nodes),
      () => Dictionaries.findings(result.dictionaries, nodes),
      () => Purity.findings(result.purity, nodes),
      () => JavaLints.findings(result.lints, nodes),
      () => Nesting.findings(result.nesting, nesting)
    )
    parallel.map(rules)(_()).flatten
  }

  def apply(findings: Vector[Finding], nodes: Map[String, Node], options: Options): Vector[Line] = {
    // numbered over every finding, so hiding some does not renumber the others
    val sorted    = findings.sorted
    val baselined = options.baseline.map(Finding.positionless)
    val numbering = sorted.foldLeft((Map.empty[String, Int], Vector.empty[(Finding, Int)])) {
      case ((seen, numbered), finding) =>
        val repeat = seen.getOrElse(finding.id, 0)
        (seen.updated(finding.id, repeat + 1), numbered :+ (finding -> repeat))
    }._2
    numbering.view.collect {
      case (finding, repeat)
          if !options.disabled.exists(_.covers(finding)) && !finding.suppressedBy(finding.suppressed) &&
            !suppressed(finding, nodes, Some(finding.node.name)) &&
            (options.info || finding.level != Finding.Level.Info) =>
        val id        = finding.rule.id
        val escalated =
          if (finding.rule.recorded) finding.level == Finding.Level.Error
          else
            (finding.level match {
              case Finding.Level.Error   => !options.lenient(id)
              case Finding.Level.Warning => options.failing(id)
              case Finding.Level.Info    => false
            }) && !baselined(Finding.positionless(finding.key))
        val level = if (finding.level == Finding.Level.Info) "info" else "warning"
        Line(if (escalated) "error" else level, finding, repeat)
    }.toVector
  }

  /** The baseline text recording `lines`, one finding key per line, less the findings of the compiler plugin, which no
    * baseline holds back.
    */
  def write(lines: Vector[Line]): String =
    lines.filterNot(_.finding.rule.recorded).map(_.finding.key).mkString("", "\n", "\n")

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
