package net.kurobako.bonsai.plugin

import net.kurobako.bonsai.collections.*
import scala.collection.mutable

private object Check {

  /** How a check reports by default: an error fails in every mode, and information never fails. */
  enum Severity {
    case Error, Warning, Info
  }

  private val declared = mutable.ListBuffer.empty[Check]

  private def check(id: String, strict: Boolean = false, severity: Severity = Severity.Warning): Check = {
    val created = Check(id, strict, severity)
    declared += created
    created
  }

  val Acyclic: Check                 = check("acyclic", strict = true)
  val ArrayEquality: Check           = check("array-equality", strict = true)
  val IteratorEquality: Check        = check("iterator-equality", strict = true)
  val UnorderedComparison: Check     = check("unordered-comparison", strict = true)
  val LongArguments: Check           = check("long-arguments", strict = true)
  val SameTypeArguments: Check       = check("same-type-arguments", strict = true)
  val SwappedArguments: Check        = check("swapped-arguments", strict = true)
  val BooleanArgument: Check         = check("boolean-argument", strict = true)
  val OpenTypeTest: Check            = check("open-type-test")
  val ExtractorTypeTest: Check       = check("extractor-type-test", strict = true)
  val NamedPattern: Check            = check("named-pattern", strict = true)
  val UnitMap: Check                 = check("unit-map", strict = true)
  val RedundantConversion: Check     = check("redundant-conversion", strict = true)
  val NestedFuture: Check            = check("nested-future", strict = true)
  val LinearInLoop: Check            = check("linear-in-loop", strict = true)
  val LoopBoxing: Check              = check("loop-boxing")
  val LocalThrow: Check              = check("local-throw", strict = true)
  val ThrowInDefault: Check          = check("throw-in-default", strict = true)
  val ThrowingHelper: Check          = check("throwing-helper", strict = true)
  val PartialInTry: Check            = check("partial-in-try", strict = true)
  val RedundantTry: Check            = check("redundant-try", strict = true)
  val BroadCatch: Check              = check("broad-catch", strict = true)
  val DiscardedCause: Check          = check("discarded-cause", strict = true)
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
  val ShadowedExtension: Check       = check("shadowed-extension", strict = true)
  val AccidentalSelfCall: Check      = check("accidental-self-call", severity = Check.Severity.Error)
  val CheckedOverloadBypass: Check   = check("checked-overload-bypass", severity = Check.Severity.Info)
  val BoxedOpaqueEquality: Check     = check("boxed-opaque-equality")
  val InferredRefinedType: Check     = check("inferred-refined-type", severity = Check.Severity.Info)
  val UnrelatedTypeTest: Check       = check("unrelated-type-test", severity = Check.Severity.Error)
  val NowarnSuppression: Check       = check("nowarn-suppression")
  val WrappedConversion: Check       = check("wrapped-conversion", strict = true)
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

/** A check of the compiler plugin, named by the id its options and messages use; `strict` checks are errors in the ci
  * mode. Besides the checks declared here there is one for each law of collection chains.
  */
private class Check private (val id: String, val strict: Boolean, val severity: Check.Severity)
