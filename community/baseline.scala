#!/usr/bin/env -S scala-cli shebang --server=false
//> using scala 3.8
//> using dep com.lihaoyi::ujson:4.4.3
//> using file projects.scala
//> using file health.scala
//> using file summary.scala
// Prints the id of the run that left the newest unexpired community-baseline artifact on a branch, or nothing.
// usage: gh api 'repos/<owner>/<repo>/actions/artifacts?name=community-baseline&per_page=100' | baseline.scala <branch> [<run id to skip>]

@main def baseline(branch: String, exclude: String*): Unit =
  latestBaseline(scala.io.Source.stdin.mkString, branch, exclude.headOption.getOrElse("")).foreach(println(_))
