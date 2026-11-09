package net.kurobako.bonsai.plugin

import net.kurobako.bonsai.collections.*
import scala.collection.mutable

private object Check {

  /** How a check reports by default: an error fails in every mode, and information never fails. */
  enum Severity {
    case Error, Warning, Info
  }

  private val declared = mutable.ListBuffer.empty[Check]

  private def check(id: String, fatal: Boolean = false, severity: Severity = Severity.Warning): Check = {
    val created = Check(id, fatal, severity)
    declared += created
    created
  }

  val Acyclic: Check                 = check("acyclic", fatal = true)
  val ArrayEquality: Check           = check("array-equality", fatal = true)
  val IteratorEquality: Check        = check("iterator-equality", fatal = true)
  val UnorderedComparison: Check     = check("unordered-comparison", fatal = true)
  val LongArguments: Check           = check("long-arguments", fatal = true)
  val SameTypeArguments: Check       = check("same-type-arguments", fatal = true)
  val SwappedArguments: Check        = check("swapped-arguments", fatal = true)
  val BooleanArgument: Check         = check("boolean-argument", fatal = true)
  val OpenTypeTest: Check            = check("open-type-test")
  val ExtractorTypeTest: Check       = check("extractor-type-test", fatal = true)
  val NamedPattern: Check            = check("named-pattern", fatal = true)
  val UnitMap: Check                 = check("unit-map", fatal = true)
  val RedundantConversion: Check     = check("redundant-conversion", fatal = true)
  val NestedFuture: Check            = check("nested-future", fatal = true)
  val LinearInLoop: Check            = check("linear-in-loop", fatal = true)
  val LoopBoxing: Check              = check("loop-boxing")
  val LocalThrow: Check              = check("local-throw", fatal = true)
  val ThrowInDefault: Check          = check("throw-in-default", fatal = true)
  val ThrowingHelper: Check          = check("throwing-helper", fatal = true)
  val PartialInTry: Check            = check("partial-in-try", fatal = true)
  val RedundantTry: Check            = check("redundant-try", fatal = true)
  val BroadCatch: Check              = check("broad-catch", fatal = true)
  val ConflatedFailure: Check        = check("conflated-failure", severity = Check.Severity.Info)
  val DiscardedCause: Check          = check("discarded-cause", fatal = true)
  val DiscardedEffectError: Check    = check("discarded-effect-error")
  val EagerEffect: Check             = check("eager-effect")
  val ImpliedCondition: Check        = check("implied-condition")
  val IdentityToken: Check           = check("identity-token")
  val BlockingCommonPool: Check      = check("blocking-common-pool")
  val UnguardedClose: Check          = check("unguarded-close")
  val OrphanThread: Check            = check("orphan-thread")
  val ReimplementedJoin: Check       = check("reimplemented-join")
  val UnboundedWait: Check           = check("unbounded-wait")
  val MessageClassification: Check   = check("message-classification")
  val WriteOnlyField: Check          = check("write-only-field")
  val RegexInDef: Check              = check("regex-in-def")
  val UnspecialisedFunction: Check   = check("unspecialised-function")
  val AccumulatingConcat: Check      = check("accumulating-concat")
  val TupleDestructure: Check        = check("tuple-destructure-in-loop")
  val InvariantInLoop: Check         = check("invariant-in-loop")
  val KeyedLookupInLoop: Check       = check("keyed-lookup-in-loop")
  val BoxingFold: Check              = check("boxing-fold")
  val InlineSetMembership: Check     = check("inline-set-membership")
  val LiteralTableInDef: Check       = check("literal-table-in-def")
  val InlineFunctionParameter: Check = check("inline-function-parameter")
  val InlineParameterReuse: Check    = check("inline-parameter-reuse")
  val InlineInstanceParameter: Check = check("inline-instance-parameter")
  val ShadowedExtension: Check       = check("shadowed-extension", fatal = true)
  val AccidentalSelfCall: Check      = check("accidental-self-call", severity = Check.Severity.Error)
  val CheckedOverloadBypass: Check   = check("checked-overload-bypass", severity = Check.Severity.Info)
  val BoxedOpaqueEquality: Check     = check("boxed-opaque-equality")
  val InferredRefinedType: Check     = check("inferred-refined-type", severity = Check.Severity.Info)
  val UnrelatedTypeTest: Check       = check("unrelated-type-test", severity = Check.Severity.Error)
  val NowarnSuppression: Check       = check("nowarn-suppression")
  val WrappedConversion: Check       = check("wrapped-conversion", fatal = true)
  val LambdaTrait: Check             = check("lambda-trait")
  val FunctionAppliedOnce: Check     = check("function-applied-once")
  val ForwardingAlias: Check         = check("forwarding-alias")
  val UnusedPrivateParam: Check      = check("unused-private-param")
  val ParameterObjectUnpacked: Check = check("parameter-object-unpacked")
  val JdkInternal: Check             = check("jdk-internal")
  val IdenticalBranches: Check       = check("identical-branches")
  val RepeatedBranchCall: Check      = check("repeated-branch-call")
  val BindingInOneBranch: Check      = check("binding-in-one-branch")
  val HandWrittenLazy: Check         = check("hand-written-lazy")
  val StringRoundTrip: Check         = check("string-round-trip")
  val RepeatedSubexpression: Check   = check("repeated-subexpression")

  private lazy val byLaw: Map[String, Check] =
    Laws.ids.map(id => id -> Check(id, true, Severity.Warning)).toMap

  /** The check named by the id of a law of collection chains, which reports what the law finds. */
  def law(id: String): Check = byLaw(id)

  /** The checks for the laws of collection chains. */
  def laws: List[Check] = byLaw.values.toList

  /** Every check, including one for each law of collection chains. */
  lazy val all: List[Check] = declared.toList ++ laws
}

/** A check of the compiler plugin, named by the id its options and messages use; `fatal` checks are errors in the
  * strict mode. Besides the checks declared here there is one for each law of collection chains.
  */
private class Check private (val id: String, val fatal: Boolean, val severity: Check.Severity)
