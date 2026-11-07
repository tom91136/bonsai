package net.kurobako.bonsai.sbt

import sbt.*

/** The lines of the `manifest.tsv` the analysis reads, written alike by the sbt plugin and the whole-build analysis. */
object ManifestLines {

  def scope(fixtures: Set[String], project: String, test: Boolean): String =
    if (test || fixtures(project)) "test" else "main"

  def outputs(project: String, scope: String, library: Boolean, classes: String): Seq[String] =
    s"output\t$project\t$scope\t$classes" +: Option.when(library && scope == "main")(s"library\t$project").toSeq

  def sources(project: String, scope: String, files: Seq[String]): Seq[String] =
    files.map(file => s"source\t$project\t$scope\t$file")

  def classpath(project: String, entries: Seq[String]): Seq[String] =
    entries.map(entry => s"classpath\t$project\t$entry")

  def options(project: String, flags: Seq[String]): Seq[String] = flags.map(flag => s"option\t$project\t$flag")

  def defaultJdk: File = file(sys.props("java.home"))

  def jdk(home: File): Seq[String] = Seq(s"jdk\t$home")

  def render(lines: Seq[String], root: File): String = (lines :+ s"root\t$root").mkString("", "\n", "\n")
}
