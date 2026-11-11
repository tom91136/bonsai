#!/usr/bin/env -S scala-cli shebang --server=false
//> using scala 3.8
//> using dep com.lihaoyi::ujson:4.4.3
// Runs the projects of one shard one after another within a time budget, through run.scala.
// usage: shard.scala <projects.jsonl> <shard> <of> <budget seconds> <per-project timeout seconds> [run.scala options]
// A project is a JSON object per line: {"url": ..., "ref": ..., "java": 17, "sbt": ["bonsaiAnalyse"], "name": ...};
// "sbt", "name" and "fetch_tag" (a tag, or ALL, to fetch) are optional. A project with a "shard" runs in that shard,
// and one without belongs to shard i mod <of>, i being its index in the list.
// A project whose status is clone-failed or runner-error runs once more if the budget allows.

import java.nio.file.Files
import java.nio.file.Path
import scala.jdk.CollectionConverters.*

def text(value: ujson.Value): String = value match {
  case ujson.Str(s) => s
  case ujson.Num(n) => if (n.isWhole) n.toLong.toString else n.toString
  case other        => ujson.write(other)
}

def optional(project: ujson.Value, key: String): String = project.obj.get(key) match {
  case None | Some(ujson.Null) | Some(ujson.False) => ""
  case Some(value)                                 => text(value)
}

def flaky(status: String): Boolean = Set("clone-failed", "runner-error", "").contains(status)

def derivedName(url: String): String =
  url
    .replaceFirst("/+$", "")
    .replaceFirst("\\.git$", "")
    .replaceFirst(".*[:/]([^/]+/[^/]+)$", "$1")
    .replace("/", "__")

@main def shard(args: String*): Unit = {
  if (args.length < 5) {
    System.err.println(
      "usage: shard.scala <projects.jsonl> <shard> <of> <budget seconds> <per-project timeout seconds> [run.scala options]"
    )
    sys.exit(1)
  }
  val here       = Path.of(sys.props("scala.sources")).toAbsolutePath.getParent
  val list       = args(0)
  val shard      = args(1).toInt
  val of         = args(2).toInt
  val budget     = args(3).toLong
  val perProject = args(4).toLong
  val passed     = args.drop(5)
  val out        = passed.indices.filter(passed(_) == "--out").foldLeft("community-out") { (_, i) =>
    passed.lift(i + 1).getOrElse {
      System.err.println("--out: missing value")
      sys.exit(1)
    }
  }

  val started       = System.nanoTime()
  def seconds: Long = (System.nanoTime() - started) / 1000000000L

  Files.readAllLines(Path.of(list)).asScala.filterNot(_.isBlank).map(ujson.read(_)).zipWithIndex
    .collect { case (project, i) if project.obj.get("shard").fold(i % of)(_.num.toInt) == shard => project }
    .foreach { project =>
      val url  = text(project.obj.getOrElse("url", ujson.Null))
      val ref  = text(project.obj.getOrElse("ref", ujson.Null))
      val java = text(project.obj.getOrElse("java", ujson.Null))
      val name = optional(project, "name")
      val tag  = optional(project, "fetch_tag")
      val sbt  = project.obj.get("sbt") match {
        case None | Some(ujson.Null) | Some(ujson.False) => Vector.empty
        case Some(commands)                              => commands.arr.map(text).toVector
      }
      val run = Vector("--url", url, "--ref", ref, "--java", java) ++
        (if (name.nonEmpty) Vector("--name", name) else Vector.empty) ++
        (if (tag.nonEmpty) Vector("--tag", tag) else Vector.empty) ++
        sbt.flatMap(Vector("--sbt", _))
      val derived                        = if (name.nonEmpty) name else derivedName(url)
      def limit                          = (budget - seconds - 600).min(perProject)
      def attempt(timeout: Long): String = {
        val command = Vector(here.resolve("run.scala").toString) ++ run ++ Vector("--timeout", timeout.toString) ++
          passed
        val code = new ProcessBuilder(command.asJava).inheritIO().start().waitFor()
        if (code != 0) System.err.println(s"$url: runner failed")
        val status = Path.of(out, derived, "status")
        if (Files.exists(status)) Files.readString(status).trim.takeWhile(_ != ':') else ""
      }
      if (limit < 300) {
        Files.createDirectories(Path.of(out, derived))
        Files.writeString(Path.of(out, derived, "status"), "not-run\n")
        println(s"$derived: not-run (shard budget spent)")
      } else if (flaky(attempt(limit)) && limit >= 300) {
        println(s"$derived: retrying once")
        attempt(limit)
      }
    }
}
