package net.kurobako.bonsai.analysis

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import net.kurobako.bonsai.java.*
import scala.annotation.tailrec
import scala.collection.mutable

object Finding {
  private[analysis] val Numbers = "[0-9]+".r

  /** `text` with every run of digits replaced by `#`. */
  private[analysis] def numberless(text: String): String = {
    val out                          = mutable.StringBuilder(text.length)
    def digit(at: Int): Boolean      = text.charAt(at) >= '0' && text.charAt(at) <= '9'
    @tailrec def skip(at: Int): Int  = if (at < text.length && digit(at)) skip(at + 1) else at
    @tailrec def scan(at: Int): Unit = if (at < text.length) {
      if (digit(at)) {
        out.append('#')
        scan(skip(at))
      } else {
        out.append(text.charAt(at))
        scan(at + 1)
      }
    }
    scan(0)
    out.toString
  }
  private val IdBytes = 5

  object Rule {
    private val declared = mutable.ArrayBuffer.empty[Rule]

    private def rule(id: String, fatal: Boolean = false): Rule = {
      val found = Rule(id, fatal, declared.size)
      declared += found
      found
    }

    val Unused: Rule                  = rule("unused", fatal = true)
    val TestOnly: Rule                = rule("test-only", fatal = true)
    val UnusedMember: Rule            = rule("unused-member", fatal = true)
    val UnreadField: Rule             = rule("unread-field", fatal = true)
    val TestOnlyMember: Rule          = rule("test-only-member", fatal = true)
    val DuplicateCode: Rule           = rule("duplicate-code", fatal = true)
    val NearDuplicateCode: Rule       = rule("near-duplicate-code")
    val InlinedHelper: Rule           = rule("inlined-helper")
    val InvariantViolation: Rule      = rule("invariant-violation", fatal = true)
    val InvariantSum: Rule            = rule("invariant-sum")
    val InvariantRedundant: Rule      = rule("invariant-redundant")
    val InvariantConstant: Rule       = rule("invariant-constant")
    val BypassedFactory: Rule         = rule("bypassed-factory")
    val BypassedWrapper: Rule         = rule("bypassed-wrapper")
    val ParallelCollections: Rule     = rule("parallel-collections")
    val ExclusiveOptions: Rule        = rule("exclusive-options")
    val RedundantFlag: Rule           = rule("redundant-flag")
    val ConstantField: Rule           = rule("constant-field")
    val ConstantArgument: Rule        = rule("constant-argument")
    val FlagArgument: Rule            = rule("flag-argument")
    val IgnoredResult: Rule           = rule("ignored-result")
    val UnwrappedTry: Rule            = rule("unwrapped-try")
    val InvertedBoolean: Rule         = rule("inverted-boolean")
    val UnusedReturnValue: Rule       = rule("unused-return-value")
    val ModuleRequirement: Rule       = rule("module-requirement")
    val DataClump: Rule               = rule("data-clump")
    val SingleImplementation: Rule    = rule("single-implementation")
    val FixedTypeParameter: Rule      = rule("fixed-type-parameter")
    val FixedDictionary: Rule         = rule("fixed-dictionary")
    val RedundantDefault: Rule        = rule("redundant-default", fatal = true)
    val UnusedDefault: Rule           = rule("unused-default")
    val FixedDefault: Rule            = rule("fixed-default")
    val FoldedNone: Rule              = rule("folded-none")
    val RepeatedPredicate: Rule       = rule("repeated-predicate")
    val ComplementCount: Rule         = rule("complement-count")
    val WeakerAccess: Rule            = rule("weaker-access")
    val VarCouldBeVal: Rule           = rule("var-could-be-val", fatal = true)
    val UnstableEquality: Rule        = rule("unstable-equality", fatal = true)
    val RegistryDrift: Rule           = rule("registry-drift")
    val AnonymousGiven: Rule          = rule("anonymous-given")
    val CommonFactor: Rule            = rule("common-factor")
    val DuplicateShape: Rule          = rule("duplicate-shape")
    val IdenticalCases: Rule          = rule("identical-cases")
    val EnumCandidate: Rule           = rule("enum-candidate")
    val HugeMethod: Rule              = rule("huge-method", fatal = true)
    val MethodLimit: Rule             = rule("method-limit", fatal = true)
    val Complexity: Rule              = rule("complexity")
    val SpaceComplexity: Rule         = rule("space-complexity")
    val UnusedBound: Rule             = rule("unused-bound")
    val IntermediateCollections: Rule = rule("intermediate-collections", fatal = true)
    val MegamorphicCall: Rule         = rule("megamorphic-call")
    val PollutedCall: Rule            = rule("polluted-call")
    val PurityRegression: Rule        = rule("purity-regression", fatal = true)
    val EffectPolicy: Rule            = rule("effect-policy", fatal = true)
    val ImpureContract: Rule          = rule("impure-contract")
    val SharedWrite: Rule             = rule("shared-write")
    val PureDiscard: Rule             = rule("pure-discard")
    val EffectfulTransform: Rule      = rule("effectful-transform")
    val InputBeforePure: Rule         = rule("io-before-pure")
    val InvariantInput: Rule          = rule("invariant-input")
    val WarningsNotFatal: Rule        = rule("warnings-not-fatal")
    val PlatformDuplicate: Rule       = rule("platform-duplicate-source")
    val DetachedCompanion: Rule       = rule("detached-companion")
    val AliasWithOperations: Rule     = rule("alias-with-operations")
    val ReceiverParameter: Rule       = rule("receiver-parameter")
    val NumberedFamily: Rule          = rule("numbered-family")
    val FieldStutter: Rule            = rule("field-stutter")
    val StaleNameString: Rule         = rule("stale-name-string")
    val ParameterAliasFamily: Rule    = rule("parameter-alias-family")
    val PackageSplitCandidate: Rule   = rule("package-split-candidate")
    val BaseClassSingleSubclass: Rule = rule("base-class-single-subclass")
    val DisjointObject: Rule          = rule("disjoint-object")
    val MisplacedMember: Rule         = rule("misplaced-member")
    val SatelliteType: Rule           = rule("satellite-type")
    val EnumDispatchFunction: Rule    = rule("enum-dispatch-function")
    val ParallelFamily: Rule          = rule("parallel-family")
    val MirroredCases: Rule           = rule("mirrored-cases")
    val CaseDeterminedField: Rule     = rule("case-determined-field")
    val DerivedField: Rule            = rule("derived-field")
    val Nesting: Rule                 = rule("nesting")
    val ForbiddenTerm: Rule           = rule("forbidden-term")
    val ForbiddenPattern: Rule        = rule("forbidden-pattern")
    val BinaryFile: Rule              = rule("binary-file")
    val LicenceNotice: Rule           = rule("licence-notice")
    val ExcludedSource: Rule          = rule("excluded-source")
    val BlankLineBefore: Rule         = rule("blank-line-before")
    val BlankLineAfter: Rule          = rule("blank-line-after")

    // The lints the javac plugin finds in the Java code of a build, which the analysis reports as findings of their own,
    // and the laws of the chains of calls it finds there.
    LintId.values.foreach(lint => rule(lint.id))
    LintId.chains.foreach(rule(_, fatal = true))

    /** Every rule, in reporting priority. */
    val values: Vector[Rule] = declared.toVector

    /** The rule of a finding the compiler plugin recorded, which ranks after the rules of the analysis. */
    def compiled(id: String): Rule = Rule(id, fatal = false, values.size, recorded = true)
  }

  /** A rule of the analysis. Rules are in reporting priority: evidence from references and constructions first,
    * structural observations last. Warnings of `fatal` rules fail the strict mode. A `recorded` rule is a check of the
    * compiler plugin, whose findings arrive judged.
    */
  class Rule private (val id: String, val fatal: Boolean, val ordinal: Int, val recorded: Boolean = false) {
    def suppressedBy(ids: Set[String]): Boolean =
      ids("*") || ids(id)

    override def toString: String = id
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
    * narrower than the node. `line` places the finding on the method or call it concerns, when that is not the node's
    * line.
    */
  def at(
      nodes: Map[String, Node],
      key: String,
      rule: Rule,
      detail: String,
      level: Level = Level.Warning,
      onlyTests: Boolean = false,
      suppressed: Set[String] = Set.empty,
      line: Int = 0
  ): Option[Finding] = nodes.get(key).map(node =>
    Finding(
      rule,
      if (line > 0) node.copy(line = line) else node,
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

  private val Positions = """(\.[A-Za-z]\w*:|\blines? )(\{[\d ,{}-]*\}|\d+(?:[-:]\d+)*)""".r

  /** A key with the source positions of its detail replaced by `#`, so a moved line still matches the baseline. */
  def positionless(key: String): String = key.split("\t", 3) match {
    case Array(rule, name, detail) => s"$rule\t$name\t${Positions.replaceAllIn(detail, "$1#")}"
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
    val words = Finding.numberless(detail)
    val text  = s"${rule.id}\t${node.name}\t${node.source}\t$words"
    MessageDigest.getInstance("SHA-256").digest(text.getBytes(UTF_8)).take(Finding.IdBytes)
      .map(byte => f"$byte%02x").mkString
  }

  /** Records the finding in a baseline, which matches it by `Finding.positionless`. */
  def key: String = s"${rule.id}\t${node.name}\t${Finding.escaped(detail)}"

  /** Whether suppressed rule ids, with `*` for every rule, cover this finding. */
  def suppressedBy(ids: Set[String]): Boolean = rule.suppressedBy(ids)
}
