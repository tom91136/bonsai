// Compares a run with the previous one and renders the job summary.

import java.nio.charset.StandardCharsets.ISO_8859_1
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import scala.jdk.CollectionConverters.*

case class Before(status: String, findings: Vector[String])

case class Row(
    output: Output,
    failures: Vector[Failure],
    before: Option[Before],
    missing: Vector[String] = Vector.empty
) {
  def name: String       = output.name
  def regressed: Boolean = before.exists(_.status == "ok") && output.status != "ok"
  def red: Boolean       = failures.nonEmpty || regressed
}

case class Changes(name: String, added: Vector[String], removed: Vector[String]) {
  def size: Int = added.size + removed.size
}

val Statuses = Vector("ok", "failed", "timeout", "oom", "killed", "clone-failed", "runner-error", "not-run", "missing")

def changes(current: Vector[String], previous: Vector[String]): (Vector[String], Vector[String]) = {
  def counted(lines: Vector[String])                    = lines.groupMapReduce(identity)(_ => 1)(_ + _)
  val after                                             = counted(current)
  val before                                            = counted(previous)
  def surplus(a: Map[String, Int], b: Map[String, Int]) =
    a.toVector.sortBy(_._1).flatMap((line, n) => Vector.fill(n - b.getOrElse(line, 0))(line))
  (surplus(after, before), surplus(before, after))
}

def rows(outputs: Iterator[Output], previous: Map[String, Before]): Vector[Row] =
  outputs.map { output =>
    val kept = output.copy(log = output.log.takeRight(20), err = Vector.empty, crash = output.crash.takeRight(20))
    Row(kept, Health.assess(output), previous.get(output.name), Health.missing(output))
  }.toVector

def findingChanges(all: Vector[Row]): Vector[Changes] =
  all.flatMap { row =>
    row.before.map { before =>
      val (added, removed) = changes(row.output.findings, before.findings)
      Changes(row.name, added, removed)
    }
  }.filter(_.size > 0).sortBy(c => (-c.size, c.name))

def readPrevious(dir: Path): Map[String, Before] =
  if (!Files.isDirectory(dir)) Map.empty
  else {
    val stream = Files.list(dir)
    try
      stream.iterator.asScala.filter(Files.isDirectory(_)).flatMap { project =>
        val status   = project.resolve("status")
        val findings = project.resolve("findings")
        Option.when(Files.isRegularFile(status))(
          project.getFileName.toString -> Before(
            Files.readString(status, ISO_8859_1).trim,
            if (Files.isRegularFile(findings)) Files.readAllLines(findings, ISO_8859_1).asScala.toVector
            else Vector.empty
          )
        )
      }.toMap
    finally stream.close()
  }

def writeBaseline(outputs: Vector[Output], from: Path, dir: Path): Unit =
  outputs.foreach { output =>
    val target = dir.resolve(output.name)
    Files.createDirectories(target)
    Files.writeString(target.resolve("status"), s"${output.status}\n", ISO_8859_1)
    Files.write(target.resolve("findings"), output.findings.asJava, ISO_8859_1)
    val timings = from.resolve(output.name).resolve("timings.tsv")
    if (Files.isRegularFile(timings))
      Files.copy(timings, target.resolve("timings.tsv"), StandardCopyOption.REPLACE_EXISTING)
  }

def latestBaseline(artifacts: String, branch: String, exclude: String): Option[String] =
  ujson.read(artifacts)("artifacts").arr.toVector
    .filter { artifact =>
      val run = artifact("workflow_run")
      !artifact("expired").bool && run("head_branch").str == branch && text(run("id")) != exclude
    }
    .sortBy(_("created_at").str)
    .lastOption
    .map(artifact => text(artifact("workflow_run")("id")))

def fenced(lines: Vector[String]): String =
  lines.map(Health.truncated(_).replace("```", "'''")).mkString("```text\n", "\n", "\n```\n")

def regressionExcerpt(output: Output): Vector[String] = {
  val errors = output.log.filter(_.startsWith("[error]"))
  s"status ${output.status}" +: (if (errors.nonEmpty) errors else output.log).takeRight(15)
}

def render(all: Vector[Row], diffs: Vector[Changes], previousRun: Option[String], budget: Int): String = {
  val out                      = new StringBuilder
  var full                     = false
  def add(block: String): Unit =
    if (!full && out.length + block.length <= budget) out ++= block else full = true

  val failing     = all.filter(_.failures.nonEmpty).sortBy(_.name)
  val regressions = all.filter(_.regressed).sortBy(_.name)
  val red         = all.count(_.red)

  add("# Community build\n\n")
  add(
    if (red == 0) s"Green: no bonsai health failure and no regression across ${all.size} projects.\n\n"
    else
      s"Red: ${failing.size} projects with a bonsai health failure and ${regressions.size} regressions across ${all.size} projects.\n\n"
  )
  add(previousRun.fold("No previous run to compare with, so regressions were not checked.\n\n")(run =>
    s"Compared with the baseline of run $run.\n\n"
  ))

  val now    = all.groupMapReduce(_.output.status)(_ => 1)(_ + _)
  val before = all.flatMap(_.before).groupMapReduce(_.status)(_ => 1)(_ + _)
  val known  = Statuses ++ (now.keySet ++ before.keySet).toVector.sorted.filterNot(Statuses.contains)
  add("## Statuses\n\n")
  add(if (previousRun.isDefined) "| status | now | before |\n|---|---|---|\n" else "| status | now |\n|---|---|\n")
  known.filter(s => now.contains(s) || before.contains(s)).foreach { status =>
    add(
      if (previousRun.isDefined) s"| $status | ${now.getOrElse(status, 0)} | ${before.getOrElse(status, 0)} |\n"
      else s"| $status | ${now.getOrElse(status, 0)} |\n"
    )
  }
  val ownBuild = all.count(row => row.output.status != "ok" && row.failures.isEmpty)
  add(
    s"\nProjects not ok and free of a bonsai health failure (their own build, the clone or the runner): $ownBuild.\n\n"
  )

  add(s"## Bonsai health failures (${failing.size})\n\n")
  failing.foreach { row =>
    add(s"### ${row.name}\n\nStatus ${row.output.status}; ${row.failures.map(_.kind).mkString(", ")}.\n\n")
    row.failures.foreach(failure => add(s"${failure.kind}\n\n${fenced(failure.evidence)}\n"))
  }

  val absent = all.filter(_.missing.nonEmpty).sortBy(_.name)
  add(s"## Classes missing from the project's classpath (${absent.size} projects)\n\n")
  absent.foreach { row =>
    val shown = row.missing.take(10) ++ Option.when(row.missing.size > 10)(s"... and ${row.missing.size - 10} more")
    add(s"### ${row.name}\n\n${fenced(shown)}\n")
  }

  add(s"## Regressions (${regressions.size})\n\n")
  regressions.foreach { row =>
    add(s"### ${row.name}\n\nok before, ${row.output.status} now.\n\n${fenced(regressionExcerpt(row.output))}\n")
  }

  add(s"## Findings against the previous run (${diffs.size} projects changed)\n\n")
  if (previousRun.isEmpty) add("No previous run.\n\n")
  else {
    if (diffs.nonEmpty) {
      add("| project | added | removed |\n|---|---|---|\n")
      diffs.foreach(c => add(s"| ${c.name} | ${c.added.size} | ${c.removed.size} |\n"))
      add("\n")
    }
    diffs.take(25).foreach { c =>
      val lines = c.added.take(15).map("+ " + _) ++ c.removed.take(15).map("- " + _)
      add(s"<details><summary>${c.name}</summary>\n\n${fenced(lines)}\n</details>\n\n")
    }
  }
  if (full) out ++= s"\n(output cut at $budget bytes; the community-report artifact has it whole)\n"
  out.toString
}

def renderDiff(diffs: Vector[Changes]): String =
  diffs.map { c =>
    s"== ${c.name}\n" + (c.added.map("+ " + _) ++ c.removed.map("- " + _)).mkString("", "\n", "\n")
  }.mkString
