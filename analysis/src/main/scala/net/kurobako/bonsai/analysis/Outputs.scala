package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.PathMatcher
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** The files in the outputs of a build, and what they name. */
private[analysis] object Outputs {

  def isGenerated(generated: Vector[PathMatcher], path: String): Boolean =
    generated.exists(_.matches(Path.of(path)))

  /** The regular files under `directory` in path order, as the file system's own order depends on how it was written.
    */
  def files(directory: Path): Vector[Path] =
    if (!Files.isDirectory(directory)) Vector.empty
    else Using.resource(Files.walk(directory))(_.toList.asScala.toVector.filter(Files.isRegularFile(_)).sorted)

  // Classes instantiated reflectively by ServiceLoader, the compiler plugin loader or sbt plugin discovery.
  def entryPoints(directory: Path): Vector[Root] = registered(
    files(directory.resolve("META-INF/services")),
    Vector(directory.resolve("plugin.properties")),
    Vector(directory.resolve("sbt/sbt.autoplugins"))
  )

  def entryPoints(resources: Vector[Resource]): Vector[Root] = {
    def named(accepts: String => Boolean) = resources.filter(resource => accepts(resource.name)).map(_.path).sorted
    registered(
      named(_.startsWith("META-INF/services/")),
      named(_ == "plugin.properties"),
      named(_ == "sbt/sbt.autoplugins")
    )
  }

  private def registered(services: Vector[Path], plugins: Vector[Path], autoplugins: Vector[Path]): Vector[Root] = {
    def lines(files: Vector[Path]) = files.filter(Files.isRegularFile(_)).flatMap(Files.readAllLines(_).asScala)
    val names                      = lines(services).map(_.takeWhile(_ != '#').trim) ++
      lines(plugins).collect {
        case line if line.startsWith("pluginClass=") => line.stripPrefix("pluginClass=").trim
      } ++
      lines(autoplugins).map(_.trim)
    names.collect {
      case name if name.nonEmpty => Root(Keys.dotted(name), Root.Kind.Provided)
    }
  }
}
