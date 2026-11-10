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
- CI: a `workflow_dispatch` workflow shards the list into a job matrix that runs the same image. Each job stays under the
  6 hour limit; a workflow run takes at most 256 jobs.
- Each project gets a wall-clock limit; a timeout is a finding about bonsai, not a skip.

## Outputs

Per project: build log, manifest, report, timings, peak heap, and any crash trace. Per run, an aggregate of:

- findings per rule per thousand lines;
- crash, timeout, and build-failure rates, with build failures split by whether bonsai caused them;
- the slowest projects by analysis time;
- a diff against the previous run by bonsai version, through `scripts/corpus.scala`.

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
