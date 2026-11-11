#!/usr/bin/env -S scala-cli shebang --server=false
//> using scala 3.8
//> using dep com.lihaoyi::ujson:4.4.3
//> using file projects.scala
//> using file health.scala
//> using file summary.scala
// Judges a community run from the outputs of its projects and writes the job summary; exits 1 when the run is red.
// usage: report.scala --out DIR --projects FILE --summary FILE --report DIR [--previous DIR --previous-run ID] [--baseline DIR]
//   --out DIR           the merged <name> output directories of the shards
//   --projects FILE     the projects the run was planned over
//   --summary FILE      the job summary, appended to and cut to fit GitHub's limit
//   --report DIR        receives summary.md and findings.diff, whole
//   --previous DIR      the baseline of the previous run, with --previous-run its run id
//   --baseline DIR      receives the baseline the next run compares with
// A run is red when a project shows a bonsai health failure, or a project that was ok in the previous run is not ok now.

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardOpenOption

@main def report(args: String*): Unit = {
  val options                       = args.grouped(2).collect { case Seq(key, value) => key -> value }.toMap
  def required(key: String): String = options.getOrElse(
    key, {
      System.err.println(s"report.scala: $key is required")
      sys.exit(1)
    }
  )
  val out      = Path.of(required("--out"))
  val reports  = Path.of(required("--report"))
  val previous = options.get("--previous").map(Path.of(_))
  val before   = previous.map(readPrevious).getOrElse(Map.empty)

  val names   = parseProjects(Files.readString(Path.of(required("--projects")), UTF_8)).map(nameOf)
  val outputs = names.iterator.map { name =>
    val dir = out.resolve(name)
    if (Files.isDirectory(dir)) Output.read(dir)
    else Output(name, "missing", Vector.empty, Vector.empty, Vector.empty, Vector.empty, Set.empty, Vector.empty)
  }
  val all         = rows(outputs, before)
  val diffs       = findingChanges(all)
  val previousRun = Option.when(previous.isDefined)(options.getOrElse("--previous-run", "an earlier run"))

  Files.createDirectories(reports)
  Files.writeString(reports.resolve("summary.md"), render(all, diffs, previousRun, Int.MaxValue), UTF_8)
  Files.writeString(reports.resolve("findings.diff"), renderDiff(diffs), UTF_8)
  Files.writeString(
    Path.of(required("--summary")),
    render(all, diffs, previousRun, 900000),
    UTF_8,
    StandardOpenOption.CREATE,
    StandardOpenOption.APPEND
  )
  options.get("--baseline").foreach(dir => writeBaseline(all.map(_.output), out, Path.of(dir)))

  val failing     = all.filter(_.failures.nonEmpty)
  val regressions = all.filter(_.regressed)
  failing.foreach(row => println(s"health: ${row.name}: ${row.failures.map(_.kind).mkString(", ")}"))
  regressions.foreach(row => println(s"regression: ${row.name}: ok before, ${row.output.status} now"))
  println(s"${all.size} projects, ${failing.size} health failures, ${regressions.size} regressions")
  if (all.exists(_.red)) sys.exit(1)
}
