package net.kurobako.bonsai.analysis

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import net.kurobako.bonsai.java.JavaLint

object Finding {
  private val Numbers = "[0-9]+".r
  private val IdBytes = 5

  /** Rules in reporting priority: evidence from references and constructions first, structural observations last.
    * Warnings of `strict` rules fail the ci mode.
    */
  enum Rule(val id: String, val strict: Boolean = false) {
    case Unused                  extends Rule("unused", strict = true)
    case TestOnly                extends Rule("test-only", strict = true)
    case UnusedMember            extends Rule("unused-member", strict = true)
    case UnreadField             extends Rule("unread-field", strict = true)
    case TestOnlyMember          extends Rule("test-only-member", strict = true)
    case DuplicateCode           extends Rule("duplicate-code", strict = true)
    case NearDuplicateCode       extends Rule("near-duplicate-code")
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
    case InvertedBoolean         extends Rule("inverted-boolean")
    case UnusedReturnValue       extends Rule("unused-return-value")
    case ModuleRequirement       extends Rule("module-requirement")
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
    case SpaceComplexity         extends Rule("space-complexity")
    case UnusedBound             extends Rule("unused-bound")
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
    case DetachedCompanion       extends Rule("detached-companion")
    case AliasWithOperations     extends Rule("alias-with-operations")
    case ReceiverParameter       extends Rule("receiver-parameter")
    case NumberedFamily          extends Rule("numbered-family")
    case FieldStutter            extends Rule("field-stutter")
    case StaleNameString         extends Rule("stale-name-string")
    case ParameterAliasFamily    extends Rule("parameter-alias-family")
    case PackageSplitCandidate   extends Rule("package-split-candidate")
    case BaseClassSingleSubclass extends Rule("base-class-single-subclass")
    case DisjointObject          extends Rule("disjoint-object")
    case MisplacedMember         extends Rule("misplaced-member")
    case SatelliteType           extends Rule("satellite-type")
    case EnumDispatchFunction    extends Rule("enum-dispatch-function")
    case Nesting                 extends Rule("nesting")
    case ForbiddenTerm           extends Rule("forbidden-term")
    case ForbiddenPattern        extends Rule("forbidden-pattern")
    case BinaryFile              extends Rule("binary-file")
    case LicenceNotice           extends Rule("licence-notice")
    case ExcludedSource          extends Rule("excluded-source")
    case BlankLineBefore         extends Rule("blank-line-before")
    case BlankLineAfter          extends Rule("blank-line-after")

    // The lints the javac plugin finds in the Java code of a build, which the analysis reports as findings of their own.
    case JavaRedundantCast                extends Rule(JavaLint.RedundantCast.id)
    case JavaSwallowedException           extends Rule(JavaLint.SwallowedException.id)
    case JavaSelfAssignment               extends Rule(JavaLint.SelfAssignment.id)
    case JavaStringReferenceEquality      extends Rule(JavaLint.StringReferenceEquality.id)
    case JavaEqualsWithoutHashCode        extends Rule(JavaLint.EqualsWithoutHashCode.id)
    case JavaMissingFieldInitializer      extends Rule(JavaLint.MissingFieldInitializer.id)
    case JavaCloseInFinally               extends Rule(JavaLint.CloseInFinally.id)
    case JavaCatchThrowable               extends Rule(JavaLint.CatchThrowable.id)
    case JavaReturnInFinally              extends Rule(JavaLint.ReturnInFinally.id)
    case JavaConstantCondition            extends Rule(JavaLint.ConstantCondition.id)
    case JavaUnnecessaryBoxing            extends Rule(JavaLint.UnnecessaryBoxing.id)
    case JavaRedundantModifier            extends Rule(JavaLint.RedundantModifier.id)
    case JavaUnusedImport                 extends Rule(JavaLint.UnusedImport.id)
    case JavaMissingDefaultInSwitch       extends Rule(JavaLint.MissingDefaultInSwitch.id)
    case JavaFallthroughInSwitch          extends Rule(JavaLint.FallthroughInSwitch.id)
    case JavaPrintStackTrace              extends Rule(JavaLint.PrintStackTrace.id)
    case JavaSynchronizedOnNonFinal       extends Rule(JavaLint.SynchronizedOnNonFinal.id)
    case JavaWaitNotInLoop                extends Rule(JavaLint.WaitNotInLoop.id)
    case JavaStringConcatenationInLoop    extends Rule(JavaLint.StringConcatenationInLoop.id)
    case JavaCollectionSizeInLoop         extends Rule(JavaLint.CollectionSizeInLoop.id)
    case JavaBoxingInLoop                 extends Rule(JavaLint.BoxingInLoop.id)
    case JavaStringBufferInsteadOfBuilder extends Rule(JavaLint.StringBufferInsteadOfBuilder.id)
    case JavaMissingSerialVersionUid      extends Rule(JavaLint.MissingSerialVersionUid.id)
    case JavaRedundantThis                extends Rule(JavaLint.RedundantThis.id)
    case JavaMissingOverride              extends Rule(JavaLint.MissingOverride.id)
    case JavaIfCanBeSwitch                extends Rule(JavaLint.IfCanBeSwitch.id)
    case JavaIdenticalBranches            extends Rule(JavaLint.IdenticalBranches.id)
    case JavaUnusedAssignment             extends Rule(JavaLint.UnusedAssignment.id)
    case JavaTrivialIf                    extends Rule(JavaLint.TrivialIf.id)
    case JavaUnnecessaryReturn            extends Rule(JavaLint.UnnecessaryReturn.id)
    case JavaPointlessArithmetic          extends Rule(JavaLint.PointlessArithmetic.id)
    case JavaPointlessBoolean             extends Rule(JavaLint.PointlessBoolean.id)
    case JavaFieldCanBeLocal              extends Rule(JavaLint.FieldCanBeLocal.id)
    case JavaCatchMayIgnoreException      extends Rule(JavaLint.CatchMayIgnoreException.id)
    case JavaOptionalGet                  extends Rule(JavaLint.OptionalGet.id)
    case JavaOptionalIsPresent            extends Rule(JavaLint.OptionalIsPresent.id)
    case JavaOptionalSignature            extends Rule(JavaLint.OptionalSignature.id)
    case JavaAutoCloseableResource        extends Rule(JavaLint.AutoCloseableResource.id)
    case JavaNotNullField                 extends Rule(JavaLint.NotNullField.id)
    case JavaConstantValue                extends Rule(JavaLint.ConstantValue.id)
    case JavaDataFlowNull                 extends Rule(JavaLint.DataFlowNull.id)
    case JavaNullableProblem              extends Rule(JavaLint.NullableProblem.id)
    case JavaNullableReturn               extends Rule(JavaLint.NullableReturn.id)
    case JavaExtractMethod                extends Rule(JavaLint.ExtractMethod.id)
    case JavaReflectionMember             extends Rule(JavaLint.ReflectionMember.id)
    case JavaReflectionInvocation         extends Rule(JavaLint.ReflectionInvocation.id)
    case JavaJdkInternalApi               extends Rule(JavaLint.JdkInternalApi.id)
    case JavaLoggingSimilar               extends Rule(JavaLint.LoggingSimilar.id)
    case JavaMagicConstant                extends Rule(JavaLint.MagicConstant.id)
    case JavaStreamOf                     extends Rule(JavaLint.StreamOf.id)
    case JavaEmptyMethod                  extends Rule(JavaLint.EmptyMethod.id)
    case JavaEmptyStatementBody           extends Rule(JavaLint.EmptyStatementBody.id)
    case JavaSizeIsEmpty                  extends Rule(JavaLint.SizeIsEmpty.id)
    case JavaUnnecessaryUnboxing          extends Rule(JavaLint.UnnecessaryUnboxing.id)
    case JavaReadString                   extends Rule(JavaLint.ReadString.id)
    case JavaSimplifiableConditional      extends Rule(JavaLint.SimplifiableConditional.id)
    case JavaUnnecessaryUnicodeEscape     extends Rule(JavaLint.UnnecessaryUnicodeEscape.id)
    case JavaSuspiciousInvocationHandler  extends Rule(JavaLint.SuspiciousInvocationHandler.id)
    case JavaSameReturnValue              extends Rule(JavaLint.SameReturnValue.id)
    case JavaStreamJoin                   extends Rule(JavaLint.StreamJoin.id)
    case JavaMissingSerialAnnotation      extends Rule(JavaLint.MissingSerialAnnotation.id)
    case JavaBusyWait                     extends Rule(JavaLint.BusyWait.id)
    case JavaCanBeFinal                   extends Rule(JavaLint.CanBeFinal.id)
    case JavaCastNarrowingVariable        extends Rule(JavaLint.CastNarrowingVariable.id)
    case JavaStandardCharset              extends Rule(JavaLint.StandardCharset.id)
    case JavaConvert2Diamond              extends Rule(JavaLint.Convert2Diamond.id)
    case JavaSystemGetProperty            extends Rule(JavaLint.SystemGetProperty.id)
    case JavaThreadDefaultRun             extends Rule(JavaLint.ThreadDefaultRun.id)
    case JavaExplicitArrayFilling         extends Rule(JavaLint.ExplicitArrayFilling.id)
    case JavaCollectionFactory            extends Rule(JavaLint.CollectionFactory.id)
    case JavaPrimitiveArrayVarargs        extends Rule(JavaLint.PrimitiveArrayVarargs.id)
    case JavaOptionalChain                extends Rule(JavaLint.OptionalChain.id)
    case JavaIgnoredCallResult            extends Rule(JavaLint.IgnoredCallResult.id)
    case JavaNullableUse                  extends Rule(JavaLint.NullableUse.id)
    case JavaRedundantMethodOverride      extends Rule(JavaLint.RedundantMethodOverride.id)
    case JavaRedundantThrows              extends Rule(JavaLint.RedundantThrows.id)
    case JavaNestedLoopOverSameCollection extends Rule(JavaLint.NestedLoopOverSameCollection.id)
    case JavaListSearchInLoop             extends Rule(JavaLint.ListSearchInLoop.id)
    case JavaListFrontEditInLoop          extends Rule(JavaLint.ListFrontEditInLoop.id)
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

  /** The characters of a text the lines of the report and of the baseline cannot hold as they are, which are written as
    * their code.
    */
  private val Codes: Map[Char, String] =
    Map('\t' -> "\\x09", '\n' -> "\\x0a", '\r' -> "\\x0d", '\\' -> "\\x5c")

  /** A text the lines of the report and of the baseline can hold, which the characters that would break a line of them
    * are written as the code of.
    */
  def escaped(text: String): String =
    text.flatMap(character => Codes.get(character).fold(character.toString)(code => code))
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

  /** Identifies the finding across runs, independent of line numbers. */
  def key: String = s"${rule.id}\t${node.name}\t${Finding.escaped(detail)}"

  /** Whether suppressed rule ids, with `*` for every rule, cover this finding. */
  def suppressedBy(ids: Set[String]): Boolean =
    ids("*") || ids(rule.id) || ids.exists(id => JavaLint.IntelliJ.get(id).exists(_(rule.id)))
}
