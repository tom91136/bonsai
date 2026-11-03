# Closed-world analysis and bytecode optimisation

Bonsai reads the TASTy and the bytecode of a whole build. This plan extends it in two directions:

- A closed-world analysis: whether a program can be shaken and compiled ahead of time as a closed world, which dynamic behaviour prevents that, and which of it can be made static.
- A bytecode engine that performs the transformations the analysis justifies: dead code elimination, devirtualisation, closure elimination, specialisation, and lowering to older JDKs.

The analysis stays read-only and produces findings and generated configuration. The engine is a separate module that consumes the analysis model.

Steps 1 to 3 stand on their own and are committed to. Steps 4 and 5 turn Bonsai from a linter into an optimiser and backporter, and start only after the gate at the end of step 3.

## Success criteria

Bonsai on Bonsai is the acceptance test. Two of its artefacts run inside the user's tools, on JDKs older than the one Bonsai is built with:

- the compiler plugin of the Scala 3.3 line, `bonsai-plugin_3.3`, which bundles `collections`, `effects` and `lifting` and runs inside a Scala 3.3 compiler, which itself runs on JDK 8 and later;
- the javac plugin, `bonsai-java-plugin_3`, which runs inside the user's javac.

The analysis itself links the Scala 3.10 compiler, which needs JDK 17, and is out of scope as a target.

Criteria by step:

1. Step 1: `unused`, `test-only` and `unused-member` on Bonsai are decided per method, and a method called only from a dead method of a live type is reported.
2. Step 2: the target compatibility report for `bonsai-plugin_3.3` and `bonsai-java-plugin_3` against a JDK 8 target lists every post-8 construct and API member they reach, and nothing they do not reach. The dynamic sites of Bonsai are graded, and the generated keep rules let ProGuard shake the plugin jar without breaking its test suite.
3. Step 3: ProGuard's shaking numbers and a `PrintInlining` run agree with the step 2 findings on Bonsai, or every difference is explained.
4. Steps 4 and 5: Bonsai rewrites its own plugin jars so that the `bonsai-plugin_3.3` test suite passes inside a Scala 3.3 compiler running on JDK 8, and the javac plugin test suite passes inside javac 8. No output is written unless the linkage check of step 5 passes.

## Constraints

- Inputs are Scala 3 sources from the 3.3 line on and Java sources at release 8 and later. Dependencies arrive as jars: TASTy for Scala libraries, bytecode for everything.
- The analysis runs on JDK 17 and later, which its compiler (3.10) requires, and reads the TASTy of every Scala line from 3.3 on.
- The compiler plugin is built per Scala line and emits the class file version that line's compiler emits: 52 for the 3.3 line.
- The javac plugin is compiled to class file version 61 today, so it does not load in javac older than 17.
- No new dependencies.
- The class files Bonsai reads and writes must not depend on the JDK Bonsai runs on.

## Current state

- `Extraction` wires the phases: it reads the TASTy of every build output through `TASTYCompiler` and merges the model the javac plugin collects for Java code into one `Graph`. Its `Collector` composes `Symbols` (the node registry), `CallSites`, `Reads`, `Dispatch` and `StructureObserver` (the structure rules' state); the `References` traversal feeds those components directly and the rest of `Collector` through `Recorder`. `JavaConversion` turns the javac model into graph facts.
- The manifest is the analysis's only input. sbt writes it through `ManifestLines` (sbt-plugin), which the whole-build manifest in `build.sbt` shares; Maven writes it in `Analysis.manifest`. Both read `.bonsai.conf` through the `configuration` module, Maven by forking the analysis jar's `Configure` entry point. A new manifest line, such as `jdk <home>`, is added in those two writers and in `analysis/Manifest.scala`.
- Roots are main methods, natives, `META-INF/services`, `plugin.properties`, sbt autoplugins and the exported API of library modules.
- A string literal naming a build type makes it a reflective root (`qualifiedUses`), as do `--reflective` globs. The javac plugin records `Class.forName` with a constant argument (`Collector.forName`).
- `Dispatch.implementations` resolves a call to every override in the build's main classes by class hierarchy analysis.
- `Purity` infers the effects of each method: reads, writes, input, output, random, unknown and throws. Its axioms, the effect lattice and the tables of partial methods live in the `effects` module, which the compiler plugin's `eager-effect` and `Exceptions` checks share.
- `Keys` names a method by qualified owner, name and erased JVM descriptor, as the JVM does. The Scala cost model and the Java cost trees meet through it; both build descriptors with `Descriptors` in the javac plugin, so the two spellings cannot drift.
- The javac plugin's model carries, beside nodes, methods, sites and fragments: cost trees per method (`CostTrees`), field writes and reassignable fields, constructor guards and method preconditions (`Guards`, `Constructors`), construction sites, and the chains its lint lifts into the `collections` engine.
- `java/DataFlow.scala` tracks constants and nullness through Java code.
- `Bytecode.scala` parses class files by hand to read method sizes. It knows every constant pool tag up to 20 and skips unknown attributes, so it reads any class file version.
- `Megamorphic` reports virtual calls inside loops whose owner has more than two concrete implementations in the build.

### Checking a change

All of these pass on JDK 25 with sbt 2.0.10 and Maven 3.9; the Maven plugin build needs `mvn` on `PATH` or `MAVEN_COMMAND` set.

- `sbt scalafmtCheckAll scalafmtSbtCheck Test/compile testFull`, with no compiler warnings. Plain `test` is incremental in sbt 2 and can run no suites.
- `sbt ++3.3.9 lifting/testFull plugin/testFull`, and the same for each line in `scala3Versions`; 3.4 and 3.5 report discarded values that 3.3 and the newest do not.
- `sbt scriptedAll`.
- `scripts/publish-maven-local.sh`, then `mvn -B verify` in `maven-plugin`.
- `sbt analyse`: bonsai on itself, failing on any finding.

### Gaps

1. Every result assumes an open world. `implementations` returns `None` when an override lies outside the build. With the whole classpath known, that set is finite.
2. `Graph` nodes are types with their companions merged, so liveness is per type. `Members` records the users of a member but not whether those users are reachable, so a method called only from a dead method of a live type counts as used.
3. Dependencies are opaque. The classpaths of `manifest.classpath`, one per module, only resolve symbols.
4. Dynamic sites are recorded but not classified by whether their targets can be known.
5. `Megamorphic` cannot see effect code. A site counts only inside `while` or a collection traversal lambda, and only build classes count as implementations, so lambdas and library classes do not.
6. `Purity` reads TASTy only. A Java-only build gets `Purity.Model.empty`, and a call into Java library code is unknown unless an axiom covers it, which is never taken for pure.

## Design decisions

### Facts from TASTy, transformation on bytecode

TASTy holds what bytecode has lost: sealed types, effectively final members, module identity, given arguments as explicit trees, closures as `Closure` trees, and purity. Bytecode is the only form of the JDK and of Java dependencies, and it is what runs. The analysis reads facts from TASTy wherever TASTy exists, and the engine transforms bytecode.

Re-linking from TASTy, by pruning trees and running `-from-tasty` through the normal backend, keeps the most information. It cannot cover Java code or the JDK, and recompiling library TASTy is fragile, so it is not the transformation path.

### Class-file library

| Option | Runs on 17 | Newest class file read | New dependency | Writes |
|---|---|---|---|---|
| `java.lang.classfile` | no, final in 24 | the running JDK's | no | yes |
| Hand-rolled reader | yes | any | no | no, frame computation is the hard part |
| `org.ow2.asm` | yes | the compiler's pinned release | no, the 3.10 compiler brings it | yes |
| `scala.tools.asm` | yes | its pinned release | no longer, compilers from 3.10 depend on upstream ASM | yes |
| ProGuardCORE | yes | its release | yes | yes |

The choice is `org.ow2.asm`. `scala3-compiler` 3.10.0 depends on `asm-util` and `asm-commons` 9.10.1, which bring `asm`, `asm-tree` and `asm-analysis`. The analysis is built once, against that compiler, so the ASM release follows one compiler.

- Use the release the compiler resolves; do not pin ASM separately, or the two drift.
- The ceiling on readable class file versions is that ASM release's. A newer compiler release raises it.
- Rewriting the major version in memory to read a newer class file is a last resort.

### Tool JDK and target JDK

A closed world is the JDK the program runs on, not the JDK Bonsai runs on. The manifest gains a `jdk <home>` line naming the target.

- A target of 9 or later is read through its own `lib/jrt-fs.jar`: `FileSystems.newFileSystem(URI.create("jrt:/"), Map.of("java.home", target))`, which works from a JDK 17 host.
- A target of 8 is read from `jre/lib/rt.jar`.
- `ct.sym` holds signatures without bodies and cannot serve.

### One owner per fact

Each fact has exactly one producer, so no two models of the same code can drift. The split follows what each input still holds:

| Fact | Producer | Why |
|---|---|---|
| class hierarchy, call graph, reachability, dynamic sites | bytecode, for all code | the only form that covers the JDK and dependencies, and what runs |
| effects of code without TASTy or source (JDK, Java dependencies) | bytecode | nothing else exists for it |
| effects, sealedness, givens, closures of Scala code | TASTy | richer than bytecode |
| intra-method structure of the build's own Java: cost trees, guards and preconditions, field writes, chains, lints, `DataFlow` | javac plugin | bytecode has lost the loops, conditions and positions these need |
| type arguments at call sites inside Java method bodies | javac plugin | erased in bytecode |

Rules that follow:

- Methods are keyed by `Keys` (owner, name, erased descriptor) in every model, so a bytecode call edge, a javac cost tree and a TASTy summary of one method meet without translation.
- Bytecode effect inference produces the `effects` module's `Kind` and `Effect` and reads its `Axioms`. It adds no effect table of its own.
- Where the javac plugin already emits a fact the table gives to bytecode, the javac version is removed when the bytecode producer lands, not kept beside it. Today that is `Collector.forName`, which the dynamic site grading of step 2 replaces.

### Library sources

Sources are for display only. `SourceFile` and `LineNumberTable` map a finding in library bytecode to a line of the `-sources.jar`, which Maven Central requires and sbt (`updateClassifiers`), coursier (`--sources`) and Maven (`dependency:sources`) fetch. The JDK ships its sources as `lib/src.zip`, packaged separately on some distributions.

Library sources are never attributed with javac. That needs each library's exact classpath, flags and annotation processors, generated code is missing from source jars, and the result can disagree with the class files that run.

## Step 1: whole-program model

- An ASM reader over the build outputs, the classpath and the target JDK, loading classes on demand from the worklist. It is unnecessary to parse the whole JDK.
- One class hierarchy across Scala and Java code, since each extends the other.
- A method-level call graph built by rapid type analysis (RTA) from the existing roots, keyed by `Keys`. An override joins the graph once its class is instantiated.
- For each call site:
  - the receiver classes that reach it, counting each lambda as its own class;
  - the given instances flowing into each dictionary parameter;
  - the callers that share the body holding the site.
- Monadic recursion as a loop: a method returning an effect that refers to itself through `flatMap`, `>>`, `foreverM` or `tailRecM`.
- Effect inference over bytecode for code without TASTy, through the `effects` module: writes to fields of objects the method did not allocate, `native` methods, monitors, and calls into input and output roots. This closes gap 6 for Java-only builds and Java dependencies.
- The cost model's unknown callees (library and JDK methods with no summary) take their bounds from bytecode summaries where the call graph reaches them.

`Liveness` and `Members` move to method reachability, which makes the existing unused and test-only findings precise.

## Step 2: findings

### Dynamic sites

Each dynamic site is graded:

- static: the argument is constant, directly or after data flow and inlining. The site becomes a direct edge and a finding that it can be rewritten: `Class.forName("a.B")` to `classOf[B]`, a constant `getMethod` to a call, a `reflect.Selectable` structural call to a trait.
- enumerable: the targets form a known finite set. `ServiceLoader.load(classOf[S])` against every `META-INF/services` on the classpath, `forName(prefix + x)` with `x` from a sealed type or enum, Java serialisation of reachable `Serializable` types, enum `valueOf`.
- open: the target cannot be known. The site is reported and a keep rule is generated.

Grading covers library bytecode as well as the build: a constant argument to `forName` or `getMethod` is an `ldc` feeding the call.

Scala 3 shapes that defeat a naive shaker get dedicated handling:

- `$deserializeLambda$` refers to every lambda body of its class, and matters only when serialisation is reachable.
- `scala.Enumeration` looks its value names up reflectively.
- `reflect.Selectable` resolves structural calls with `getMethod` at run time.
- Object initialisers run all their effects on first access.

A static initialiser whose `Purity` verdict has no input, output, randomness or unknown effects can run at build time, and a load of such a module can be removed.

Outputs:

- findings for open sites, rewritable static sites and initialisers that block build-time initialisation;
- keep rules in ProGuard and R8 syntax, and `reachability-metadata.json`;
- shaking effectiveness: reachable methods and bytes as a share of the total.

### Dispatch

| Opportunity | Detected from | Fix |
|---|---|---|
| Tagless final `F[_]` that the closed world only instantiates to one effect | TASTy: given arguments are trees, so the instances reaching each dictionary parameter are known | bytecode: clone the method with the dictionary fixed, turning `invokeinterface` on the type class into `invokevirtual` on the instance class; source: make the method concrete |
| A shared generic body whose callers pass different receivers, polluting its profile | call graph with receiver sets per caller | bytecode: split the body per caller |
| A higher-order call passing a lambda, such as `traverse` or `map2` | TASTy: the argument is a `Closure` | bytecode: inline the callee and eliminate the closure; source: `inline def` with `inline` function parameters |
| A site few classes reach although more are loaded | RTA or type flow | bytecode: an `instanceof` cascade of direct calls |
| A hot `match` over a large sealed type, which compiles to a linear `instanceof` chain | TASTy and the `Algebra` sum model | source: switch on an ordinal or tag |
| Forwarder chains deeper than `MaxInlineLevel` (15), hot callees over `FreqInlineSize` (325 bytes) | `Bytecode` sizes | source or bytecode: collapse forwarders |

HotSpot already devirtualises a call when only one implementation is loaded, and inlines a dominant receiver with a fallback (`TypeProfileMajorReceiverPercent`). The closed world adds value where many classes are loaded and few reach a site.

`Megamorphic` moves onto the call graph's receiver sets, which counts lambdas and library classes and sees sites in effect code (gap 5).

The continuation call in the cats-effect `IOFiber` run loop is megamorphic by nature. Fixing it would mean cloning the run loop per program, which is out of scope.

These findings come first because they need no engine.

### Target compatibility

Given a target JDK older than the build or its dependencies, the analysis reports the reachable constructs and API members the target lacks, graded:

- lowerable: a bytecode construct with a mechanical lowering (step 5);
- stubbable: an API member a bundled static helper can replace;
- approximable: an API member an emulation can replace at some cost;
- blocked: no faithful replacement exists.

Reachability is what distinguishes this from animal-sniffer or `--release` checks, which flag dead code too.

The first report to produce is the success criterion's: `bonsai-plugin_3.3` and `bonsai-java-plugin_3` against JDK 8. The 3.3 plugin is compiled to class file 52 against a newer JDK's class library without `--release`, so it may reach post-8 API members that fail only at run time on JDK 8.

## Step 3: external oracles

- ProGuard run as an external tool checks the shaking effectiveness numbers.
- HotSpot's `-XX:+PrintInlining` output, with its megamorphic, too big and inlining too deep reasons, or JFR recordings, check the dispatch findings. Ingesting them replaces the `--hot` globs with measured hotness.

Neither becomes a dependency.

## Gate before steps 4 and 5

Decide whether to build the engine once step 3's criteria hold. Inputs to the decision:

- how many post-8 members the step 2 report lists for the two plugin jars, and how many are stubbable, approximable or blocked;
- whether raising the javac plugin's own target (compiling it with `--release 8` or `11`, and the parts of `collections` it needs) reaches the criterion more cheaply than lowering;
- the size of the dispatch opportunities on real builds, which decides whether steps 4.2 to 4.6 are worth their test burden.

## Step 4: optimisation engine

A new module beside `analysis`, run by the sbt and Maven plugins after packaging.

- Writing uses `COMPUTE_FRAMES` with `getCommonSuperClass` answered from the closed-world hierarchy. The default loads classes through Bonsai's own class loader, which is the wrong JDK.
- Every transformation consults one table of constructs by the class file version that permits them, and checks access. Inlining code from a version 61 class into a version 52 class can carry constant dynamic entries (55), private calls between nestmates (55) or indy string concatenation (53).
- The passes and inlining heuristics of the Scala 2 optimiser are ported, not its code: inlining with closure elimination, box and unbox elimination, copy propagation, nullness and unreachable code removal. That optimiser worked per compilation unit; this engine is whole-program.
- Partial evaluation is a constant propagating interpreter over `tree/analysis` frames, folding only calls `Purity` proves free of effects.

Pass order:

1. dead code elimination from RTA;
2. devirtualisation, and making final what the closed-world hierarchy proves final;
3. inlining of higher-order methods with closure elimination;
4. dictionary specialisation;
5. guarded devirtualisation;
6. splitting shared bodies, which needs a profile from step 3 to bound code growth.

## Step 5: lowering to older JDKs

Lowering is the version table of step 4 used to rewrite a construct where inlining would refuse it.

| Construct | From | Lowering |
|---|---|---|
| `module-info.class` | 53 (9) | drop |
| indy string concatenation | 53 | `StringBuilder` chain from the recipe |
| nestmate private access | 55 (11) | widen to package-private, which the closed world shows safe, or a synthetic accessor |
| constant dynamic entries | 55 | expand inline, or a lazy static holder |
| records | 60 (16) | superclass `Object`, generated `toString`, `equals` and `hashCode` in place of the `ObjectMethods` indy, drop the `Record` attribute |
| `PermittedSubclasses` | 61 (17) | drop |
| pattern `switch` through `SwitchBootstraps` | 65 (21) | `instanceof` or ordinal chain |

Lambdas, default and static interface methods, and private interface methods exist at version 52 and need nothing. The stack map frame format is unchanged since version 50.

The API delta is the larger part:

- stubbable: `List.of`, `Map.of`, `String.strip`, `isBlank`, `lines` and `repeat`, `Optional.orElseThrow()`, `Stream.toList` and `takeWhile`, `Objects.requireNonNullElse`, `HexFormat`, the `SequencedCollection` methods. Stubs reproduce behaviour as well as signatures: `List.of` rejects nulls and is immutable.
- approximable: `VarHandle` through field updaters or `Unsafe`, `StackWalker` through stack traces without `Class` objects, parts of `ProcessHandle`, `java.net.http` through a bundled backport.
- blocked: virtual threads (platform threads keep the semantics and lose the scalability), `java.lang.foreign`, the Vector API, hidden classes, `ModuleLayer`.

A program class overriding a new interface method, such as `reversed()`, needs a stub that dispatches to it. RTA lists every receiver, so the stub is a closed `instanceof` dispatch.

After rewriting, a linkage check resolves every member reference against the target JDK and the classpath, and ASM's `Analyzer` with frames from the target hierarchy stands in for the verifier. No output is written unless the check passes.

Order: the javac plugin from 61 to 52 first, since it is Bonsai's own and the success criterion names it, then the 3.3 compiler plugin's API delta, then 21 to 17 for user builds, then down to 8, where the work is mostly stubs.

The Scala library of the program's own line decides how far a Scala program can go: the 3.3 line's library targets 8, while newer lines' libraries target 17 and would need lowering too. The compatibility report of step 2 lists the post-8 APIs a given library reaches, and that list decides whether Scala to 8 is practical for a line or only Java to 8.

## Open questions

- Whether the Scala 3 backend emits the `ScalaInlineInfo` attribute. If it does, the bytecode side has inlining facts without TASTy.
- Which post-8 APIs the `bonsai-plugin_3.3` jar and the 3.3 line's library reach; the first step 2 report answers it.
- Which class file versions ASM 9.10.1 reads, against the newest target JDK users need.
- How much profile pollution cats' own combinators cause compared with the dictionary calls. A `PrintInlining` run of a cats-effect benchmark answers it.
- Folding `Bytecode.scala` into the ASM reader. ASM does not expose the length of the `Code` attribute directly.
