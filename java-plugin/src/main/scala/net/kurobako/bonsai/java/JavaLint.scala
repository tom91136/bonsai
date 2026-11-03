package net.kurobako.bonsai.java

import net.kurobako.bonsai.collections.*

/** The lints the Java code is checked for, which the plugin finds in the code it compiles and the analysis reports the
  * findings of them under.
  *
  * The ids are named here, and not in the checks themselves, so that the analysis and the plugin name the lints alike:
  * the analysis holds the rule of every lint, and the plugin holds the check of it, and both read the id from here.
  */
enum JavaLint(val id: String) {

  /** A cast of a value to the type it already has. */
  case RedundantCast extends JavaLint("redundant-cast")

  /** An exception that is caught and never looked at. */
  case SwallowedException extends JavaLint("swallowed-exception")

  /** A value that is assigned to itself. */
  case SelfAssignment extends JavaLint("self-assignment")

  /** Two strings that are compared by reference and not by their text. */
  case StringReferenceEquality extends JavaLint("string-reference-equality")

  /** A type that compares its values and does not give them a hash code. */
  case EqualsWithoutHashCode extends JavaLint("equals-without-hashcode")

  /** A field of the state of a type that the code does not give the value it starts with. */
  case MissingFieldInitializer extends JavaLint("missing-field-initializer")

  /** An exception that is caught so generally that the failures of the program are hidden. */
  case CatchThrowable extends JavaLint("catch-throwable")

  /** A method that returns from its `finally` block, which hides the exception the method was to report. */
  case ReturnInFinally extends JavaLint("return-in-finally")

  /** A condition that is always the same. */
  case ConstantCondition extends JavaLint("constant-condition")

  /** A value that is boxed although the code does not need it. */
  case UnnecessaryBoxing extends JavaLint("unnecessary-boxing")

  /** A modifier that the code around it already implies. */
  case RedundantModifier extends JavaLint("redundant-modifier")

  /** An import that the file never uses. */
  case UnusedImport extends JavaLint("unused-import")

  /** A method that overrides a method of a supertype and does not say so. */
  case MissingOverride extends JavaLint("missing-override")

  /** A switch of an enumeration that does not handle every case of it. */
  case MissingDefaultInSwitch extends JavaLint("missing-default-in-switch")

  /** A case of a switch that falls through to the case after it. */
  case FallthroughInSwitch extends JavaLint("fallthrough-in-switch")

  /** A stack trace that is written to the output of the program instead of being reported. */
  case PrintStackTrace extends JavaLint("print-stack-trace")

  /** A lock of a field that may be replaced while the code waits for it. */
  case SynchronizedOnNonFinal extends JavaLint("synchronized-on-non-final")

  /** A wait that is not in a loop, which can miss the notification that wakes the thread. */
  case WaitNotInLoop extends JavaLint("wait-not-in-loop")

  /** A text that is built anew on every turn of a loop. */
  case StringConcatenationInLoop extends JavaLint("string-concatenation-in-loop")

  /** The size of a collection that is asked for on every turn of a loop. */
  case CollectionSizeInLoop extends JavaLint("collection-size-in-loop")

  /** A value that is boxed on every turn of a loop. */
  case BoxingInLoop extends JavaLint("boxing-in-loop")

  /** A text that is built in a way that another thread may see half of. */
  case StringBufferInsteadOfBuilder extends JavaLint("string-buffer-instead-of-builder")

  /** A type that is serialized and does not name the version of its form. */
  case MissingSerialVersionUid extends JavaLint("missing-serial-version-uid")

  /** A qualifier of a member that the code around it already holds. */
  case RedundantThis extends JavaLint("redundant-this")

  /** An if chain that compares one value with constants, which a switch states more directly. */
  case IfCanBeSwitch extends JavaLint("if-can-be-switch")

  /** The branches of an if that hold the same code, or end or start with it. */
  case IdenticalBranches extends JavaLint("if-identical-branches")

  /** An initializer of a field that gives it the value it has anyway, or an assignment no read sees. */
  case UnusedAssignment extends JavaLint("unused-assignment")

  /** An if that returns two boolean literals, which a return of the condition says in one statement. */
  case TrivialIf extends JavaLint("trivial-if")

  /** A return that is unnecessary as the last statement of a void method. */
  case UnnecessaryReturn extends JavaLint("unnecessary-return")

  /** An arithmetic expression with the identity of its operator, which the other operand alone says. */
  case PointlessArithmetic extends JavaLint("pointless-arithmetic")

  /** A boolean expression with the identity of its operator, which the other operand alone says. */
  case PointlessBoolean extends JavaLint("pointless-boolean")

  /** A private field whose every use is in one method, which a local variable would hold. */
  case FieldCanBeLocal extends JavaLint("field-can-be-local")

  /** A catch parameter named as ignored that the catch block reads. */
  case CatchMayIgnoreException extends JavaLint("catch-may-ignore-exception")

  /** An `Optional.get()` the code does not guard with a presence check. */
  case OptionalGet extends JavaLint("optional-get")

  /** An `isPresent` branch that calls `get`, which a functional expression states in one step. */
  case OptionalIsPresent extends JavaLint("optional-is-present")

  /** An Optional used as a field or parameter, which a project chooses to forbid or allow. */
  case OptionalSignature extends JavaLint("optional-signature")

  /** An AutoCloseable local used outside try-with-resources. */
  case AutoCloseableResource extends JavaLint("auto-closeable-resource")

  /** A non-null field that is not initialized where it is declared. */
  case NotNullField extends JavaLint("not-null-field")

  /** A condition comparing a non-null value with null. */
  case ConstantValue extends JavaLint("constant-value")

  /** A null value used where the scope says the value is not null. */
  case DataFlowNull extends JavaLint("data-flow-null")

  /** A null returned from a method whose scope says the result is not null. */
  case NullableProblem extends JavaLint("nullable-problem")

  /** A method whose result can be null, which an Optional result says instead. */
  case NullableReturn extends JavaLint("nullable-return")

  /** An anonymous implementation large enough to extract into a named type. */
  case ExtractMethod extends JavaLint("extract-method")

  /** A reflected member that the target type does not declare. */
  case ReflectionMember extends JavaLint("reflection-member")

  /** A reflective call whose arguments the compiler cannot check. */
  case ReflectionInvocation extends JavaLint("reflection-invocation")

  /** A use of a JDK type or member that the JDK does not support, by a package a module does not export or by the
    * unsupported module jdk.unsupported.
    */
  case JdkInternalApi extends JavaLint("jdk-internal-api")

  /** Two log messages whose words say nearly the same thing. */
  case LoggingSimilar extends JavaLint("logging-similar")

  /** A modifier argument written as a magic number or unchecked variable. */
  case MagicConstant extends JavaLint("magic-constant")

  /** A method whose body is empty. */
  case EmptyMethod extends JavaLint("empty-method")

  /** A loop or conditional whose body is the empty statement. */
  case EmptyStatementBody extends JavaLint("empty-statement-body")

  /** An explicit unboxing conversion that the context performs anyway. */
  case UnnecessaryUnboxing extends JavaLint("unnecessary-unboxing")

  /** A string built from the bytes of a file, which Files.readString reads directly. */
  case ReadString extends JavaLint("read-string")

  /** A conditional expression that a call or a boolean states more directly. */
  case SimplifiableConditional extends JavaLint("simplifiable-conditional")

  /** A Unicode escape that the source can hold as the character itself. */
  case UnnecessaryUnicodeEscape extends JavaLint("unnecessary-unicode-escape")

  /** An InvocationHandler whose invoke returns null for every method, including equals. */
  case SuspiciousInvocationHandler extends JavaLint("suspicious-invocation-handler")

  /** A method that always returns the same literal value. */
  case SameReturnValue extends JavaLint("same-return-value")

  /** A serial version field that can carry `@Serial`. */
  case MissingSerialAnnotation extends JavaLint("missing-serial-annotation")

  /** A `Thread.sleep` in a loop, which waits by polling rather than on the condition. */
  case BusyWait extends JavaLint("busy-wait")

  /** A field or local variable that is never assigned again, so it can be final. */
  case CanBeFinal extends JavaLint("can-be-final")

  /** A cast a narrower declaration of the variable would make unnecessary. */
  case CastNarrowingVariable extends JavaLint("cast-narrowing-variable")

  /** A charset name or `Charset.forName` that names one of the standard charsets. */
  case StandardCharset extends JavaLint("standard-charset")

  /** An explicit type argument a diamond would infer from the target type. */
  case Convert2Diamond extends JavaLint("convert-2-diamond")

  /** A system property that a standard constant or method already names. */
  case SystemGetProperty extends JavaLint("system-get-property")

  /** A thread built without a task, whose default run method does nothing. */
  case ThreadDefaultRun extends JavaLint("thread-default-run")

  /** A loop that fills an array with one value, which Arrays.fill says in one call. */
  case ExplicitArrayFilling extends JavaLint("explicit-array-filling")

  /** A primitive array passed to a varargs method without saying it is one argument. */
  case PrimitiveArrayVarargs extends JavaLint("primitive-array-varargs")

  /** A call whose result the code drops, where the result says whether the call worked. */
  case IgnoredCallResult extends JavaLint("ignored-call-result")

  /** A nullable value used where the JSpecify annotations say a non-null one is required. */
  case NullableUse extends JavaLint("nullable-use")

  /** An override that only calls the method it overrides. */
  case RedundantMethodOverride extends JavaLint("redundant-method-override")

  /** A checked exception a method declares but never throws. */
  case RedundantThrows extends JavaLint("redundant-throws")

  /** A resource that a finally block closes, which the try statement can close itself. */
  case CloseInFinally extends JavaLint("close-in-finally")

  /** A loop over a collection that a loop around it runs over as well, which takes quadratic time. */
  case NestedLoopOverSameCollection extends JavaLint("nested-loop-over-same-collection")

  /** A search of a list that is made on every turn of a loop. */
  case ListSearchInLoop extends JavaLint("list-search-in-loop")

  /** An edit at the front of an array list on every turn of a loop. */
  case ListFrontEditInLoop extends JavaLint("list-front-edit-in-loop")

  /** A doc comment that documents nothing, as another doc comment or the end of a body follows it. */
  case DanglingDocComment extends JavaLint("dangling-doc-comment")

  /** A loop that returns on the first element that matches, which a stream match states. */
  case LoopToMatch extends JavaLint("loop-to-match")

  /** A check for null that picks a default, which `Objects.requireNonNullElse` states. */
  case NullDefault extends JavaLint("null-default")

  /** Two loops in a row with the same body, which one loop over both sources runs. */
  case ConsecutiveIdenticalLoops extends JavaLint("consecutive-identical-loops")

  /** A list that one loop fills and one loop iterates, which the first loop can use directly. */
  case TemporaryListIteratedOnce extends JavaLint("temporary-list-iterated-once")

  /** A condition in a loop that the turns of the loop never change, which can be tested once outside it. */
  case LoopInvariantCondition extends JavaLint("loop-invariant-condition")
}

object JavaLint {

  /** The ids of the laws of collection chains that read as Java, which the lint of the chains reports under. */
  val chains: Vector[String] = Laws.javaIds

  /** The ids of every lint of the Java code. */
  val ids: Set[String] = values.map(_.id).toSet ++ chains

  /** The suppression id that matches every rule. */
  val Wildcard = "*"

  /** The IntelliJ inspection short names and their corresponding bonsai rule ids.
    *
    * The names are taken from the inspection export files in cef4j/lints and from IntelliJ documented ids.
    */
  val IntelliJ: Map[String, Set[String]] = Map(
    // The inspection export names from cef4j lints XML files
    "AutoCloseableResource"              -> Set("auto-closeable-resource"),
    "ConstantValue"                      -> Set("constant-value"),
    "DataFlowIssue"                      -> Set("data-flow-null", "nullable-problem", "nullable-use", "constant-value"),
    "EmptyMethod"                        -> Set("empty-method"),
    "EmptyStatementBody"                 -> Set("empty-statement-body"),
    "ExtractMethodRecommender"           -> Set("extract-method"),
    "Java9ReflectionClassVisibility"     -> Set("reflection-member", "jdk-internal-api"),
    "JavaReflectionInvocation"           -> Set("reflection-invocation"),
    "JavaReflectionMemberAccess"         -> Set("reflection-member"),
    "LoggingSimilarMessage"              -> Set("logging-similar"),
    "MagicConstant"                      -> Set("magic-constant"),
    "NotNullFieldNotInitialized"         -> Set("not-null-field", "missing-field-initializer"),
    "NullableProblems"                   -> Set("nullable-problem", "nullable-use"),
    "OptionalUsedAsFieldOrParameterType" -> Set("optional-signature"),
    "PointlessArithmeticExpression"      -> Set("pointless-arithmetic"),
    "PointlessBooleanExpression"         -> Set("pointless-boolean"),
    "ReadWriteStringCanBeUsed"           -> Set("read-string"),
    "RedundantBlock"                     -> Set.empty,
    "RedundantThrows"                    -> Set("redundant-throws"),
    "SameParameterValue"                 -> Set("constant-argument"),
    "SameReturnValue"                    -> Set("same-return-value"),
    "ScalaUnusedExpression"              -> Set("pure-discard"),
    "ScalaWeakerAccess"                  -> Set("weaker-access"),
    "SimplifiableConditionalExpression"  -> Set("simplifiable-conditional"),
    "SimplifyStreamApiCallChains"        -> Set("stream-of", "stream-join"),
    "SizeReplaceableByIsEmpty"           -> Set("size-to-is-empty", "size-to-non-empty"),
    "SuspiciousInvocationHandlerImplementation" -> Set("suspicious-invocation-handler"),
    "TryFinallyCanBeTryWithResources"           -> Set("close-in-finally"),
    "UnnecessaryModuleDependencyInspection"     -> Set("module-requirement"),
    "UnnecessaryUnboxing"                       -> Set("unnecessary-unboxing"),
    "UnnecessaryUnicodeEscape"                  -> Set("unnecessary-unicode-escape"),
    "UnusedReturnValue"                         -> Set("unused-return-value"),
    "unused"          -> Set("unused", "unused-member", "unread-field", "unused-return-value"),
    "ZeroIndexToHead" -> Set("list-front-edit-in-loop"),

    // Documented inspection short names and aliases
    "ALL"                                              -> Set(Wildcard),
    "all"                                              -> Set(Wildcard),
    "BusyWait"                                         -> Set("busy-wait"),
    "CallToPrintStackTrace"                            -> Set("print-stack-trace"),
    "CanBeFinal"                                       -> Set("can-be-final"),
    "CastNarrowingVariable"                            -> Set("cast-narrowing-variable"),
    "CatchMayIgnoreException"                          -> Set("catch-may-ignore-exception"),
    "CloseInFinally"                                   -> Set("close-in-finally"),
    "CollectionFactory"                                -> Set("java9-collection-factory"),
    "CollectionSizeInLoop"                             -> Set("collection-size-in-loop"),
    "DanglingJavadoc"                                  -> Set("dangling-doc-comment"),
    "ConstantConditions"                               -> Set("constant-condition", "constant-value", "data-flow-null"),
    "Convert2Diamond"                                  -> Set("convert-2-diamond"),
    "DuplicateBranchesInSwitch"                        -> Set("if-identical-branches"),
    "EmptyCatchBlock"                                  -> Set("swallowed-exception"),
    "EqualsAndHashcode"                                -> Set("equals-without-hashcode"),
    "ExplicitArrayFilling"                             -> Set("explicit-array-filling"),
    "fallthrough"                                      -> Set("fallthrough-in-switch"),
    "FieldCanBeLocal"                                  -> Set("field-can-be-local"),
    "FieldMayBeFinal"                                  -> Set("can-be-final"),
    "IdenticalBranchesInIf"                            -> Set("if-identical-branches"),
    "IfCanBeSwitch"                                    -> Set("if-can-be-switch"),
    "ListFrontEditInLoop"                              -> Set("list-front-edit-in-loop"),
    "ListSearchInLoop"                                 -> Set("list-search-in-loop"),
    "MissingDefaultInSwitch"                           -> Set("missing-default-in-switch"),
    "MissingOverrideAnnotation"                        -> Set("missing-override"),
    "MissingSerialAnnotation"                          -> Set("missing-serial-annotation"),
    "NestedLoopOverSameCollection"                     -> Set("nested-loop-over-same-collection"),
    "OptionalChain"                                    -> Set("none-check"),
    "OptionalGetWithoutIsPresent"                      -> Set("optional-get"),
    "OptionalIsPresent"                                -> Set("optional-is-present"),
    "OverlyBroadCatchBlock"                            -> Set("catch-throwable"),
    "PrimitiveArrayArgumentToVariableArgMethod"        -> Set("primitive-array-varargs"),
    "PrimitiveArrayVarargs"                            -> Set("primitive-array-varargs"),
    "RedundantCast"                                    -> Set("redundant-cast"),
    "RedundantMethodOverride"                          -> Set("redundant-method-override"),
    "RedundantModifier"                                -> Set("redundant-modifier"),
    "RedundantThis"                                    -> Set("redundant-this"),
    "ResultOfMethodCallIgnored"                        -> Set("ignored-call-result", "ignored-result"),
    "ReturnInsideFinallyBlock"                         -> Set("return-in-finally"),
    "SelfAssignment"                                   -> Set("self-assignment"),
    "serial"                                           -> Set("missing-serial-version-uid"),
    "StandardCharset"                                  -> Set("standard-charset"),
    "StringBufferReplaceableByStringBuilder"           -> Set("string-buffer-instead-of-builder"),
    "StringConcatenationInLoop"                        -> Set("string-concatenation-in-loop"),
    "StringEquality"                                   -> Set("string-reference-equality"),
    "SynchronizeOnNonFinalField"                       -> Set("synchronized-on-non-final"),
    "SystemGetProperty"                                -> Set("system-get-property"),
    "ThreadDefaultRun"                                 -> Set("thread-default-run"),
    "ThrowInsideCatchBlockWhichIgnoresCaughtException" -> Set("swallowed-exception"),
    "TooBroadCatch"                                    -> Set("catch-throwable"),
    "UnnecessaryBoxing"                                -> Set("unnecessary-boxing"),
    "UnnecessaryReturn"                                -> Set("unnecessary-return"),
    "UnusedAssignment"                                 -> Set("unused-assignment"),
    "TrivialIf"                                        -> Set("trivial-if"),
    "UNUSED_IMPORT"                                    -> Set("unused-import"),
    "UnusedImport"                                     -> Set("unused-import"),
    "WaitNotInLoop"                                    -> Set("wait-not-in-loop"),
    "WeakerAccess"                                     -> Set("weaker-access")
  )

  /** The Error Prone bug pattern names and their corresponding bonsai rule ids. */
  val ErrorProne: Map[String, Set[String]] = Map(
    "FallThrough"     -> Set("fallthrough-in-switch"),
    "MissingOverride" -> Set("missing-override")
  )

  /** The rule ids a suppression name names, with `*` for all rules. */
  def resolve(name: String): Set[String] =
    if (name == "bonsai") Set(Wildcard)
    else if (name.startsWith("bonsai:")) Set(name.drop("bonsai:".length))
    else IntelliJ.getOrElse(name, ErrorProne.getOrElse(name, Set(name)))
}
