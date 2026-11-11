package net.kurobako.bonsai.sbt

import java.nio.file.Path
import java.nio.file.Paths
import sbt.*

/** The lines of the `manifest.tsv` the analysis reads, written alike by the sbt plugin and the whole-build analysis. */
object ManifestLines {

  def scope(fixtures: Set[String], project: String, test: Boolean): String =
    if (test || fixtures(project)) "test" else "main"

  def outputs(project: String, scope: String, library: Boolean, classes: String): Seq[String] =
    s"output\t$project\t$scope\t$classes" +: (if (library && scope == "main") Seq(s"library\t$project") else Nil)

  def sources(project: String, scope: String, files: Seq[String]): Seq[String] =
    files.map(file => s"source\t$project\t$scope\t$file")

  /** The lines naming the resource directories that exist, which hold the registrations the classes may lack. */
  def resources(project: String, scope: String, directories: Seq[String]): Seq[String] =
    directories.filter(Paths.get(_).toFile.isDirectory).distinct.map(directory =>
      s"resource\t$project\t$scope\t$directory"
    )

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

  def classpath(project: String, entries: Seq[String]): Seq[String] =
    entries.map(entry => s"classpath\t$project\t$entry")

  /** The line naming a module the compiler plugin compiled in the audit mode, which keeps its findings beside the
    * classes.
    */
  def recorded(project: String, compiled: Boolean): Seq[String] = if (compiled) Seq(s"recorded\t$project") else Nil

  def options(project: String, flags: Seq[String]): Seq[String] = flags.map(flag => s"option\t$project\t$flag")

  def defaultJdk: File = file(sys.props("java.home"))

  def jdk(home: File): Seq[String] = Seq(s"jdk\t$home")

  def render(lines: Seq[String], root: File): String = (lines :+ s"root\t$root").mkString("", "\n", "\n")
}
