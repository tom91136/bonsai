package net.kurobako.bonsai.analysis

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import net.kurobako.bonsai.java.JavaLint

object Finding {
  private[analysis] val Numbers = "[0-9]+".r

  /** `text` with every run of digits replaced by `#`. */
  private[analysis] def numberless(text: String): String = {
    val out = StringBuilder(text.length)
    var i   = 0
    while (i < text.length) {
      val c = text.charAt(i)
      if (c >= '0' && c <= '9') {
        while (i < text.length && text.charAt(i) >= '0' && text.charAt(i) <= '9') i += 1
        out.append('#')
      } else {
        out.append(c)
        i += 1
      }
    }
    out.toString
  }
  private val IdBytes = 5

  /** A rule of the analysis. Rules are in reporting priority: evidence from references and constructions first,
    * structural observations last. Warnings of `strict` rules fail the ci mode.
    */
  final class Rule private[Finding] (val id: String, val strict: Boolean, val ordinal: Int) {
    def suppressedBy(ids: Set[String]): Boolean =
      ids("*") || ids(id) || ids.exists(other => JavaLint.IntelliJ.get(other).exists(_(id)))

    override def toString: String = id
  }

  object Rule {
    private val declared = scala.collection.mutable.ArrayBuffer.empty[Rule]

    private def rule(id: String, strict: Boolean = false): Rule = {
      val found = new Rule(id, strict, declared.size)
      declared += found
      found
    }

    val Unused                  = rule("unused", strict = true)
    val TestOnly                = rule("test-only", strict = true)
    val UnusedMember            = rule("unused-member", strict = true)
    val UnreadField             = rule("unread-field", strict = true)
    val TestOnlyMember          = rule("test-only-member", strict = true)
    val DuplicateCode           = rule("duplicate-code", strict = true)
    val NearDuplicateCode       = rule("near-duplicate-code")
    val InlinedHelper           = rule("inlined-helper")
    val InvariantViolation      = rule("invariant-violation", strict = true)
    val InvariantSum            = rule("invariant-sum")
    val InvariantRedundant      = rule("invariant-redundant")
    val InvariantConstant       = rule("invariant-constant")
    val BypassedFactory         = rule("bypassed-factory")
    val ParallelCollections     = rule("parallel-collections")
    val ExclusiveOptions        = rule("exclusive-options")
    val RedundantFlag           = rule("redundant-flag")
    val ConstantField           = rule("constant-field")
    val ConstantArgument        = rule("constant-argument")
    val FlagArgument            = rule("flag-argument")
    val IgnoredResult           = rule("ignored-result")
    val InvertedBoolean         = rule("inverted-boolean")
    val UnusedReturnValue       = rule("unused-return-value")
    val ModuleRequirement       = rule("module-requirement")
    val DataClump               = rule("data-clump")
    val SingleImplementation    = rule("single-implementation")
    val FixedTypeParameter      = rule("fixed-type-parameter")
    val RedundantDefault        = rule("redundant-default", strict = true)
    val UnusedDefault           = rule("unused-default")
    val WeakerAccess            = rule("weaker-access")
    val VarCouldBeVal           = rule("var-could-be-val", strict = true)
    val RegistryDrift           = rule("registry-drift")
    val AnonymousGiven          = rule("anonymous-given")
    val CommonFactor            = rule("common-factor")
    val DuplicateShape          = rule("duplicate-shape")
    val IdenticalCases          = rule("identical-cases")
    val EnumCandidate           = rule("enum-candidate")
    val HugeMethod              = rule("huge-method", strict = true)
    val MethodLimit             = rule("method-limit", strict = true)
    val Complexity              = rule("complexity")
    val SpaceComplexity         = rule("space-complexity")
    val UnusedBound             = rule("unused-bound")
    val IntermediateCollections = rule("intermediate-collections", strict = true)
    val MegamorphicCall         = rule("megamorphic-call")
    val PurityRegression        = rule("purity-regression", strict = true)
    val EffectPolicy            = rule("effect-policy", strict = true)
    val ImpureContract          = rule("impure-contract")
    val SharedWrite             = rule("shared-write")
    val PureDiscard             = rule("pure-discard")
    val EffectfulTransform      = rule("effectful-transform")
    val WarningsNotFatal        = rule("warnings-not-fatal")
    val PlatformDuplicate       = rule("platform-duplicate-source")
    val DetachedCompanion       = rule("detached-companion")
    val AliasWithOperations     = rule("alias-with-operations")
    val ReceiverParameter       = rule("receiver-parameter")
    val NumberedFamily          = rule("numbered-family")
    val FieldStutter            = rule("field-stutter")
    val StaleNameString         = rule("stale-name-string")
    val ParameterAliasFamily    = rule("parameter-alias-family")
    val PackageSplitCandidate   = rule("package-split-candidate")
    val BaseClassSingleSubclass = rule("base-class-single-subclass")
    val DisjointObject          = rule("disjoint-object")
    val MisplacedMember         = rule("misplaced-member")
    val SatelliteType           = rule("satellite-type")
    val EnumDispatchFunction    = rule("enum-dispatch-function")
    val Nesting                 = rule("nesting")
    val ForbiddenTerm           = rule("forbidden-term")
    val ForbiddenPattern        = rule("forbidden-pattern")
    val BinaryFile              = rule("binary-file")
    val LicenceNotice           = rule("licence-notice")
    val ExcludedSource          = rule("excluded-source")
    val BlankLineBefore         = rule("blank-line-before")
    val BlankLineAfter          = rule("blank-line-after")

    // The lints the javac plugin finds in the Java code of a build, which the analysis reports as findings of their own,
    // and the laws of the chains of calls it finds there.
    JavaLint.values.foreach(lint => rule(lint.id))
    JavaLint.chains.foreach(rule(_, strict = true))

    /** Every rule, in reporting priority. */
    val values: Array[Rule] = declared.toArray
  }

  /** Errors fail in every mode, warnings call for a change, and information describes structure worth reviewing. */
  enum Level(val id: String) {
    case Error   extends Level("error")
    case Warning extends Level("warning")
    case Info    extends Level("info")
  }

  given Ordering[Finding] =
    Ordering.by(finding =>
      (finding.level.ordinal, finding.rule.ordinal, finding.node.source, finding.node.line, finding.detail)
    )

  /** A finding at the node keyed `key`, lowered to information when it concerns test code or rests only on evidence
    * from tests. `suppressed` holds the rules the declaration the finding concerns suppresses, when that declaration is
    * narrower than the node.
    */
  def at(
      nodes: Map[String, Node],
      key: String,
      rule: Rule,
      detail: String,
      level: Level = Level.Warning,
      onlyTests: Boolean = false,
      suppressed: Set[String] = Set.empty
  ): Option[Finding] = nodes.get(key).map(node =>
    Finding(
      rule,
      node,
      if (onlyTests) s"$detail (all in tests)" else detail,
      if (node.test || onlyTests) Level.Info else level,
      suppressed
    )
  )

  /** The last segment of a qualified name. */
  def simple(name: String): String = name.split('.').last

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

  /** A key with the numbers of its detail left out, so a moved line or a changed count still matches the baseline. */
  def unnumbered(key: String): String = key.split("\t", 3) match {
    case Array(rule, name, detail) => s"$rule\t$name\t${Numbers.replaceAllIn(detail, "#")}"
    case _                         => key
  }

  /** The characters of a text the lines of the report and of the baseline cannot hold as they are, which are written as
    * their code.
    */
  private val Codes: Map[Char, String] =
    Map('\t' -> "\\x09", '\n' -> "\\x0a", '\r' -> "\\x0d", '\\' -> "\\x5c")

  /** A text the lines of the report and of the baseline can hold, which the characters that would break a line of them
    * are written as the code of.
    */
  def escaped(text: String): String =
    text.flatMap(character => Codes.getOrElse(character, character.toString))
}

case class Finding(
    rule: Finding.Rule,
    node: Node,
    detail: String,
    level: Finding.Level = Finding.Level.Warning,
    suppressed: Set[String] = Set.empty
) {

  /** Names the finding across runs by its rule, symbol and file, and the words of its detail with the numbers left out,
    * so a moved line or a changed count keeps it. Findings that agree on these share it, which `Report.Line` tells
    * apart.
    */
  def id: String = {
    val words = Finding.Numbers.replaceAllIn(detail, "#")
    val text  = s"${rule.id}\t${node.name}\t${node.source}\t$words"
    MessageDigest.getInstance("SHA-256").digest(text.getBytes(UTF_8)).take(Finding.IdBytes)
      .map(byte => f"$byte%02x").mkString
  }

  /** Records the finding in a baseline, which matches it by `Finding.unnumbered`. */
  def key: String = s"${rule.id}\t${node.name}\t${Finding.escaped(detail)}"

  /** Whether suppressed rule ids, with `*` for every rule, cover this finding. */
  def suppressedBy(ids: Set[String]): Boolean = rule.suppressedBy(ids)
}
