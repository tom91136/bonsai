#!/usr/bin/env -S scala-cli shebang --server=false
//> using scala 3.8
//> using dep com.lihaoyi::ujson:4.4.3
//> using file projects.scala
//> using file packing.scala
// Checks the projects, divides them over shards and prints the workflow outputs as key=value lines: projects, matrix, count.
// usage: plan.scala --projects FILE [--previous DIR] [--shards N] [--memory SIZE] [--heap SIZE]
//   --previous DIR  the baseline of the previous run; its <name>/timings.tsv balance the shards
//   --shards N      shards for the projects on the default memory, 0 for one each, none for about twenty per shard
//   --memory, --heap  the default container memory and sbt heap
// A project with its own "memory", "memory_swap" or "heap" runs in shards of the projects that share them.

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import scala.jdk.CollectionConverters.*
import scala.util.Try

case class Profile(memory: String, swap: String, heap: String)

def fail(message: String): Nothing = {
  System.err.println(s"plan.scala: $message")
  sys.exit(1)
}

def timings(previous: Option[Path], names: Vector[String]): Map[String, Double] =
  previous.filter(Files.isDirectory(_)).fold(Map.empty[String, Double]) { dir =>
    names.flatMap { name =>
      val file = dir.resolve(name).resolve("timings.tsv")
      Option.when(Files.isRegularFile(file))(name -> seconds(Files.readString(file, UTF_8))).filter(_._2 > 0)
    }.toMap
  }

@main def plan(args: String*): Unit = {
  val options = args.grouped(2).collect { case Seq(key, value) => key -> value }.toMap
  val file    = options.getOrElse("--projects", fail("--projects is required"))
  val default = Profile(
    options.get("--memory").filter(_.nonEmpty).getOrElse("12g"),
    "",
    options.get("--heap").filter(_.nonEmpty).getOrElse("6g")
  )
  val requested =
    options.get("--shards").filter(_.nonEmpty).map(s => Try(s.toInt).getOrElse(fail(s"--shards: not a number: $s")))

  val projects = Try(parseProjects(Files.readString(Path.of(file), UTF_8))).getOrElse(fail(s"$file: not JSON"))
  if (projects.isEmpty) fail(s"$file: no projects")
  val errors = projectErrors(projects)
  if (errors.nonEmpty) fail(errors.mkString("\n"))

  def profile(project: ujson.Value): Profile = {
    val memory = optional(project, "memory")
    Profile(
      if (memory.nonEmpty) memory else default.memory,
      optional(project, "memory_swap"),
      Some(optional(project, "heap")).filter(_.nonEmpty).getOrElse(default.heap)
    )
  }

  val names    = projects.map(nameOf)
  val known    = timings(options.get("--previous").map(Path.of(_)), names)
  val profiles = projects.map(profile)
  val order    = (default +: profiles).distinct

  var first  = 0
  val shards = Array.fill(projects.size)(0)
  val matrix = Vector.newBuilder[ujson.Value]
  order.foreach { each =>
    val members = projects.indices.filter(profiles(_) == each).toVector
    val wanted  = if (each == default) requested else None
    val others  = order.filter(_ != each).map(other => profiles.count(_ == other))
    val count   = wanted.fold(if (each == default) shardCount(members.size, others) else shardsOf(members.size))(n =>
      if (n < 1) members.size else n.min(members.size)
    )
    if (members.nonEmpty) {
      val placed = spread(members.map(names), count, known)
      members.zip(placed).foreach((i, shard) => shards(i) = first + shard)
      (0 until count).foreach { shard =>
        matrix +=
          ujson.Obj("shard" -> (first + shard), "memory" -> each.memory, "swap" -> each.swap, "heap" -> each.heap)
      }
      first += count
    }
  }
  if (first > MaxShards) fail(s"$first shards; a workflow run takes at most $MaxShards jobs")

  val stamped = ujson.Arr.from(projects.zipWithIndex.map { (project, i) =>
    val kept = Seq("name", "url", "ref", "java", "sbt", "fetch_tag").flatMap(key => project.obj.get(key).map(key -> _))
    ujson.Obj.from(("name" -> ujson.Str(names(i))) +: kept.filterNot(_._1 == "name") :+
      ("shard" -> ujson.Num(shards(i))))
  })
  println(s"projects=${ujson.write(stamped)}")
  println(s"matrix=${ujson.write(ujson.Obj("include" -> ujson.Arr.from(matrix.result())))}")
  println(s"count=$first")
  System.err.println(
    s"plan.scala: ${projects.size} projects in $first shards, ${known.size} with a previous timing" +
      (if (known.isEmpty) ", so by index" else "")
  )
}
