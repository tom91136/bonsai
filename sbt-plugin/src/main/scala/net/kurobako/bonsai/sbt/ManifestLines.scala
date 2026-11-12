package net.kurobako.bonsai.sbt

import java.nio.file.Path
import java.nio.file.Paths
import sbt.*
import scala.annotation.tailrec

/** The lines of the `manifest.tsv` the analysis reads, written alike by the sbt plugin and the whole-build analysis. */
object ManifestLines {

  def scope(fixtures: Set[String], project: String, test: Boolean): String =
    if (test || fixtures(project)) "test" else "main"

  def outputs(project: String, scope: String, library: Boolean, classes: String): Seq[String] =
    s"output\t$project\t$scope\t$classes" +: (if (library && scope == "main") Seq(s"library\t$project") else Nil)

  def sources(project: String, scope: String, files: Seq[String]): Seq[String] =
    files.map(file => s"source\t$project\t$scope\t$file")

  /** The lines naming the files the compile of a scope recorded as its products. */
  def products(project: String, scope: String, files: Seq[String]): Seq[String] =
    files.map(file => s"product\t$project\t$scope\t$file")

  /** The lines naming the files among `files`, the resources of a build, that register entry points, each with its path
    * in the jar: below the nearest of `directories` that holds it, or by its name where none does.
    */
  def resources(files: Seq[String], directories: Seq[String]): Seq[String] = {
    val roots                                                          = directories.map(Paths.get(_)).toSet
    @tailrec def below(path: Path, tail: List[String]): Option[String] =
      Option(path.getParent) match {
        case Some(parent) =>
          val parts = path.getFileName.toString :: tail
          if (roots(parent)) Some(parts.mkString("/")) else below(parent, parts)
        case None => None
      }
    files.flatMap { file =>
      val path = Paths.get(file)
      val name = below(path, Nil).getOrElse(path.getFileName.toString)
      if (registers(name) && path.toFile.isFile) Seq(s"resource\t$name\t$file") else Nil
    }.distinct
  }

  /** The lines naming the modules whose resources the build lists, however few register entry points. */
  def resourced(projects: Seq[String]): Seq[String] = projects.distinct.map(project => s"resourced\t$project")

  private def registers(name: String): Boolean =
    name.startsWith("META-INF/services/") || name == "plugin.properties" || name == "sbt/sbt.autoplugins"

  /** The lines naming where the build generates sources: the directory of the managed sources that lie in one of the
    * `directories`, and the file of each that lies in none.
    */
  def generated(directories: Seq[String], files: Seq[String]): Seq[String] = {
    val known = directories.map(Paths.get(_)).toSet
    files.map(file => enclosing(Paths.get(file), known).getOrElse(Paths.get(file)).toString).distinct.sorted
      .map(path => s"generated\t$path")
  }

  private def enclosing(path: Path, known: Set[Path]): Option[Path] =
    if (known(path)) Some(path) else Option(path.getParent).flatMap(enclosing(_, known))

  def classpath(project: String, scope: String, entries: Seq[String]): Seq[String] =
    entries.map(entry => s"classpath\t$project\t$scope\t$entry")

  /** The lines naming the analysed projects each project depends on, as the `required` projects the build gives each of
    * its configurations, the analysed projects being those named.
    */
  def depends(required: Seq[(String, Seq[String])]): Seq[String] = {
    val analysed = required.map(_._1).toSet
    required.flatMap { case (project, others) =>
      others.filter(other => other != project && analysed(other)).map(project -> _)
    }.distinct.sorted.map { case (project, other) => s"depends\t$project\t$other" }
  }

  /** The line naming a module the compiler plugin compiled in the audit mode, which keeps its findings beside the
    * classes.
    */
  def recorded(project: String, compiled: Boolean): Seq[String] = if (compiled) Seq(s"recorded\t$project") else Nil

  def options(project: String, flags: Seq[String]): Seq[String] = flags.map(flag => s"option\t$project\t$flag")

  def defaultJdk: File = file(sys.props("java.home"))

  def jdk(home: File): Seq[String] = Seq(s"jdk\t$home")

  def render(lines: Seq[String], root: File): String = (lines :+ s"root\t$root").mkString("", "\n", "\n")
}
