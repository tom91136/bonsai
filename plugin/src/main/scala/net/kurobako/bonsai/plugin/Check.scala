package net.kurobako.bonsai.plugin

import net.kurobako.bonsai.collections.Laws
import scala.collection.mutable

/** A check of the compiler plugin, named by the id its options and messages use; `strict` checks are errors in the ci
  * mode. Besides the checks declared here there is one for each law of collection chains.
  */
private final class Check private (val id: String, val strict: Boolean, val severity: Check.Severity)

private object Check {

  /** How a check reports by default: an error fails in every mode, and information never fails. */
  enum Severity {
    case Error, Warning, Info
  }

  private val declared = mutable.ListBuffer.empty[Check]

  private def check(id: String, strict: Boolean = false, severity: Severity = Severity.Warning): Check = {
    val created = new Check(id, strict, severity)
    declared += created
    created
  }

  val Acyclic                 = check("acyclic", strict = true)
  val ArrayEquality           = check("array-equality", strict = true)
  val IteratorEquality        = check("iterator-equality", strict = true)
  val UnorderedComparison     = check("unordered-comparison", strict = true)
  val LongArguments           = check("long-arguments", strict = true)
  val SameTypeArguments       = check("same-type-arguments", strict = true)
  val SwappedArguments        = check("swapped-arguments", strict = true)
  val BooleanArgument         = check("boolean-argument", strict = true)
  val OpenTypeTest            = check("open-type-test")
  val UnstableEquality        = check("unstable-equality", strict = true)
  val ExtractorTypeTest       = check("extractor-type-test", strict = true)
  val NamedPattern            = check("named-pattern", strict = true)
  val UnitMap                 = check("unit-map", strict = true)
  val RedundantConversion     = check("redundant-conversion", strict = true)
  val NestedFuture            = check("nested-future", strict = true)
  val LinearInLoop            = check("linear-in-loop", strict = true)
  val LoopBoxing              = check("loop-boxing")
  val LocalThrow              = check("local-throw", strict = true)
  val ThrowInDefault          = check("throw-in-default", strict = true)
  val ThrowingHelper          = check("throwing-helper", strict = true)
  val PartialInTry            = check("partial-in-try", strict = true)
  val RedundantTry            = check("redundant-try", strict = true)
  val BroadCatch              = check("broad-catch", strict = true)
  val DiscardedCause          = check("discarded-cause", strict = true)
  val EagerEffect             = check("eager-effect")
  val ImpliedCondition        = check("implied-condition")
  val IdentityToken           = check("identity-token")
  val BlockingCommonPool      = check("blocking-common-pool")
  val UnguardedClose          = check("unguarded-close")
  val OrphanThread            = check("orphan-thread")
  val ReimplementedJoin       = check("reimplemented-join")
  val UnboundedWait           = check("unbounded-wait")
  val MessageClassification   = check("message-classification")
  val WriteOnlyField          = check("write-only-field")
  val RegexInDef              = check("regex-in-def")
  val UnspecialisedFunction   = check("unspecialised-function")
  val AccumulatingConcat      = check("accumulating-concat")
  val TupleDestructure        = check("tuple-destructure-in-loop")
  val InvariantInLoop         = check("invariant-in-loop")
  val KeyedLookupInLoop       = check("keyed-lookup-in-loop")
  val BoxingFold              = check("boxing-fold")
  val InlineSetMembership     = check("inline-set-membership")
  val LiteralTableInDef       = check("literal-table-in-def")
  val InlineFunctionParameter = check("inline-function-parameter")
  val InlineParameterReuse    = check("inline-parameter-reuse")
  val InlineInstanceParameter = check("inline-instance-parameter")
  val ShadowedExtension       = check("shadowed-extension", strict = true)
  val AccidentalSelfCall      = check("accidental-self-call", severity = Check.Severity.Error)
  val CheckedOverloadBypass   = check("checked-overload-bypass", severity = Check.Severity.Info)
  val BoxedOpaqueEquality     = check("boxed-opaque-equality")
  val InferredRefinedType     = check("inferred-refined-type", severity = Check.Severity.Info)
  val UnrelatedTypeTest       = check("unrelated-type-test", severity = Check.Severity.Error)
  val NowarnSuppression       = check("nowarn-suppression")
  val WrappedConversion       = check("wrapped-conversion", strict = true)
  val LambdaTrait             = check("lambda-trait")
  val FunctionAppliedOnce     = check("function-applied-once")
  val ForwardingAlias         = check("forwarding-alias")
  val UnusedPrivateParam      = check("unused-private-param")
  val ParameterObjectUnpacked = check("parameter-object-unpacked")
  val JdkInternal             = check("jdk-internal")
  val IdenticalBranches       = check("identical-branches")
  val RepeatedBranchCall      = check("repeated-branch-call")
  val BindingInOneBranch      = check("binding-in-one-branch")
  val HandWrittenLazy         = check("hand-written-lazy")
  val StringRoundTrip         = check("string-round-trip")
  val RepeatedSubexpression   = check("repeated-subexpression")

  private lazy val byLaw: Map[String, Check] =
    Laws.ids.map(id => id -> new Check(id, true, Severity.Warning)).toMap

  /** The check named by the id of a law of collection chains, which reports what the law finds. */
  def law(id: String): Check = byLaw(id)

  /** The checks for the laws of collection chains. */
  def laws: List[Check] = byLaw.values.toList

  /** Every check, including one for each law of collection chains. */
  lazy val all: List[Check] = declared.toList ++ byLaw.values
}
