# Community build

Run bonsai over the Scala 3 open community build and the Scala 3 compiler, read the reports, and turn what they show into
rule fixes. The corpus answers three questions per rule: does it catch what it should, how noisy is it, and where does
bonsai crash or slow down.

## Corpus

- The project list and per-project fix-ups of VirtusLab community-build3 (`coordinator/configs/projects-config.conf`:
  source patches, Java version, excluded modules, sbt commands). Its runners are not used.
- `scala/scala3`, which builds the compiler and, from 3.8, the standard library from source.
- Each project builds at its own Scala 3 version, so the reports match what its users would see. Projects older than 3.3.4
  get the analysis without the compiler plugin.
- Scala 2 builds, `scala/scala` included, are out of scope: the rules need TASTy.

## sbt 1 support

Most of the corpus is on sbt 1, so `sbt-bonsai` cross-builds for it rather than the harness injecting a second path.

- `sbtBonsai` and `bonsaiConfig` cross-build to Scala 2.12 with `pluginCrossBuild / sbtVersion` picking sbt 1.x.
- Shared sources stay in the subset Scala 2.12 and Scala 3 both accept. Code that differs between sbt 1 and sbt 2, such
  as virtual file references against `Attributed[File]`, goes in `scala-2.12` and `scala-3` source directories.
- Scripted tests run on both sbt lines.
- Mill and scala-cli projects follow once sbt is done.

## Execution

One `Containerfile` serves the workstation and CI, so a run reproduces in either place.

- The image holds JDKs 8, 11, 17, 21 and 25, the sbt launcher, coursier and scala-cli. The bonsai artefacts of the
  revision under test are published into a repository mounted into each container, so the image changes only with the
  toolchain. Nothing reads or writes the host home directory, `~/.ivy2`, or `~/.m2`.
- A project builds on `/opt/jdk/<major>`, which the manifest names as its target, while the analysis runs on JDK 25
  through `bonsaiAnalysisJavaHome`, so Java 8 and 11 projects can be analysed.
- CI keeps the image in the GitHub Actions cache as a compressed archive keyed by the `Containerfile` digest. The
  archive stays well under the cache's 10 GB limit, since an entry past it is not saved at all.
- Dependency caches live in a named volume per run.
- Locally: `podman run` per project, 6 to 8 at a time, each under a memory cap.
- CI: every push to `main` runs the list in `community/projects.jsonl`, as a job matrix of the same image, at most 12 jobs
  at a time. A newer push cancels a run in progress. `workflow_dispatch` takes an ad-hoc list. Each job stays under the
  6 hour limit; a workflow run takes at most 256 jobs.
- The shards balance on the per-project timings of the previous run, falling back to list order. A project may set its own
  `memory`, `memory_swap` and `heap`, and runs in shards of the projects that set the same. `scala/scala3` needs more than
  a hosted runner's 16 GB, so its shard enables zram swap and the container may use it.
- Each project gets a wall-clock limit; a timeout is a finding about bonsai, not a skip.

## Outputs

Per project: build log, manifest, report, timings, peak heap, and any crash trace. Per run, an aggregate of:

- findings per rule per thousand lines;
- crash, timeout, and build-failure rates, with build failures split by whether bonsai caused them;
- the slowest projects by analysis time;
- a diff against the previous run by bonsai version, through `scripts/corpus.scala`.

CI turns the outputs into a verdict. The run is red when any project shows a bonsai health failure, or when a project that
was ok in the previous run on `main` is not ok now. Findings never fail it.

- A health failure is a crash, a `bonsai analysis failed` line, a `problem:` line or a `note:` line from the analysis, an
  unreadable, unanalysable or unpicklable class, a clash between a class and a package that fails the analysis, or a
  timeout, out-of-memory kill or kill while the analysis was running. A note that a class is missing from the
  project's own classpath is not one; the summary lists those per project. A project whose own build fails before the analysis runs is not one.
- The previous run is the newest unexpired `community-baseline` artifact of a run on `main`, which holds each project's
  status, timings and sorted findings. The first run has none, so only health failures apply.
- The job summary lists the counts per status, each failure and regression with a log excerpt, and the findings added and
  removed since the previous run. The `community-report` artifact has it whole.

## Reading the reports

- Precision: per rule, a sample of about 20 findings stratified across projects, judged with the evidence quoted. Contested
  findings go to a list for a human to check.
- Recall: a named list of expected catches, seeded from the archive-tools positives, checked on every run.
- Each fix is its own task with a red fixture cut from the corpus case.

## Phases

1. sbt 1 cross-build and the `Containerfile`.
2. Pilot of about 20 projects chosen for spread (effect systems, HTTP, small libraries, a Java-heavy build, a Scala.js
   cross-build), run with and without the whole-program model. It measures the time per project the later phases plan on.
3. The top 200 by stars, then `scala/scala3`.
4. The full list, then Mill and scala-cli builds.

Nothing goes upstream and no project is contacted without approval.
