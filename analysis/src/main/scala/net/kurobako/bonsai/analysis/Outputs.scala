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

  def files(directory: Path): Vector[Path] =
    if (!Files.isDirectory(directory)) Vector.empty
    else Using.resource(Files.walk(directory))(_.toList.asScala.toVector.filter(Files.isRegularFile(_)))

  // Classes instantiated reflectively by ServiceLoader, the compiler plugin loader or sbt plugin discovery.
  def entryPoints(directory: Path): Vector[String] = {
    val services = files(directory.resolve("META-INF/services")).view.flatMap(Files.readAllLines(_).asScala)
      .map(_.takeWhile(_ != '#').trim).toVector
    def lines(file: Path) = Some(file).filter(Files.isRegularFile(_)).toVector.flatMap(Files.readAllLines(_).asScala)
    val plugins           = lines(directory.resolve("plugin.properties")).collect {
      case line if line.startsWith("pluginClass=") => line.stripPrefix("pluginClass=").trim
    }
    val autoplugins = lines(directory.resolve("sbt/sbt.autoplugins")).map(_.trim)
    (services ++ plugins ++ autoplugins).view.collect {
      case name if name.nonEmpty => name.stripSuffix("$").replace('$', '.')
    }.toVector
  }
}
