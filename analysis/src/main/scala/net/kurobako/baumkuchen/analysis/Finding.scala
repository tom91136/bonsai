package net.kurobako.baumkuchen.analysis

object Finding {

  /** Rules in reporting priority: evidence from references and constructions first, structural observations last.
    * Warnings of `strict` rules fail the ci mode.
    */
  enum Rule(val id: String, val strict: Boolean = false) {
    case Unused                  extends Rule("unused", strict = true)
    case TestOnly                extends Rule("test-only", strict = true)
    case UnusedMember            extends Rule("unused-member", strict = true)
    case TestOnlyMember          extends Rule("test-only-member", strict = true)
    case DuplicateCode           extends Rule("duplicate-code", strict = true)
    case InvariantViolation      extends Rule("invariant-violation", strict = true)
    case InvariantSum            extends Rule("invariant-sum")
    case InvariantRedundant      extends Rule("invariant-redundant")
    case InvariantConstant       extends Rule("invariant-constant")
    case BypassedFactory         extends Rule("bypassed-factory")
    case ParallelCollections     extends Rule("parallel-collections")
    case ExclusiveOptions        extends Rule("exclusive-options")
    case RedundantFlag           extends Rule("redundant-flag")
    case ConstantField           extends Rule("constant-field")
    case ConstantArgument        extends Rule("constant-argument")
    case FlagArgument            extends Rule("flag-argument")
    case IgnoredResult           extends Rule("ignored-result")
    case DataClump               extends Rule("data-clump")
    case SingleImplementation    extends Rule("single-implementation")
    case FixedTypeParameter      extends Rule("fixed-type-parameter")
    case RedundantDefault        extends Rule("redundant-default", strict = true)
    case WeakerAccess            extends Rule("weaker-access")
    case VarCouldBeVal           extends Rule("var-could-be-val", strict = true)
    case AnonymousGiven          extends Rule("anonymous-given")
    case CommonFactor            extends Rule("common-factor")
    case DuplicateShape          extends Rule("duplicate-shape")
    case IdenticalCases          extends Rule("identical-cases")
    case EnumCandidate           extends Rule("enum-candidate")
    case HugeMethod              extends Rule("huge-method", strict = true)
    case MethodLimit             extends Rule("method-limit", strict = true)
    case Complexity              extends Rule("complexity")
    case IntermediateCollections extends Rule("intermediate-collections")
    case MegamorphicCall         extends Rule("megamorphic-call")
    case PurityRegression        extends Rule("purity-regression", strict = true)
    case EffectPolicy            extends Rule("effect-policy", strict = true)
    case ImpureContract          extends Rule("impure-contract")
    case SharedWrite             extends Rule("shared-write")
    case PureDiscard             extends Rule("pure-discard")
    case EffectfulTransform      extends Rule("effectful-transform")
    case WarningsNotFatal        extends Rule("warnings-not-fatal")
    case PlatformDuplicate       extends Rule("platform-duplicate-source")
  }

  /** Warnings call for a change; information describes structure worth reviewing. */
  enum Level(val id: String) {
    case Warning extends Level("warning")
    case Info    extends Level("info")
  }

  given Ordering[Finding] =
    Ordering.by(finding =>
      (finding.level.ordinal, finding.rule.ordinal, finding.node.source, finding.node.line, finding.detail)
    )

  /** A finding at the node keyed `key`, lowered to information when it concerns test code or rests only on evidence
    * from tests.
    */
  def at(
      nodes: Map[String, Node],
      key: String,
      rule: Rule,
      detail: String,
      level: Level = Level.Warning,
      onlyTests: Boolean = false
  ): Option[Finding] = nodes.get(key).map(node =>
    Finding(
      rule,
      node,
      if (onlyTests) s"$detail (all in tests)" else detail,
      if (node.test || onlyTests) Level.Info else level
    )
  )

  /** A qualified name shortened to its owner and own name, keeping any parameter list after them. */
  def short(name: String): String = {
    val (path, parameters) = name.span(_ != '(')
    path.split('.').takeRight(2).mkString(".") + parameters
  }

  /** The first `limit` values, then a count of the rest. */
  def listed(values: Vector[String], limit: Int): String = {
    val shown = values.take(limit).mkString(", ")
    if (values.size > limit) s"$shown, ${values.size - limit} more" else shown
  }
}

case class Finding(rule: Finding.Rule, node: Node, detail: String = "", level: Finding.Level = Finding.Level.Warning) {

  /** Identifies the finding across runs, independent of line numbers. */
  def key: String = s"${rule.id}\t${node.name}\t$detail"

  /** Whether suppressed rule ids, with `*` for every rule, cover this finding. */
  def suppressedBy(ids: Set[String]): Boolean = ids("*") || ids(rule.id)
}
