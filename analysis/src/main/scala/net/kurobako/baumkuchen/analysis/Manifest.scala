package net.kurobako.baumkuchen.analysis

import java.nio.file.Files
import java.nio.file.Path
import scala.jdk.CollectionConverters.*

object Manifest {

  /** Reads tab-separated `output <module> <scope> <directory>`, `classpath <entry>`, `library <module>`,
    * `platform <module>`, `option <module> <compiler option>`, `source <module> <scope> <file>`, `root <directory>`,
    * `text-exclude <glob>` and `text <rule index> <field> <value>` lines.
    */
  def read(path: Path): Either[String, Manifest] =
    Files.readAllLines(path).asScala.toVector.zipWithIndex.filter(_._1.nonEmpty)
      .foldLeft[Either[String, Manifest]](Right(Manifest(Vector.empty, Vector.empty, Set.empty))) {
        case (manifest, (line, index)) =>
          manifest.flatMap { current =>
            line.split('\t').toList match {
              case List("output", module, scope, location) =>
                Output.Scope.values.find(_.id == scope).toRight(s"$path:${index + 1}: unknown scope: $scope").map {
                  parsed => current.copy(outputs = current.outputs :+ Output(module, parsed, Path.of(location)))
                }
              case List("classpath", location) =>
                Right(current.copy(classpath = current.classpath :+ Path.of(location)))
              case List("library", module)        => Right(current.copy(libraries = current.libraries + module))
              case List("platform", module)       => Right(current.copy(platforms = current.platforms + module))
              case List("option", module, option) =>
                Right(current.copy(options =
                  current.options.updated(module, current.options.getOrElse(module, Vector.empty) :+ option)
                ))
              case List("source", module, scope, location) =>
                Output.Scope.values.find(_.id == scope).toRight(s"$path:${index + 1}: unknown scope: $scope").map {
                  parsed => current.copy(sources = current.sources :+ Output(module, parsed, Path.of(location)))
                }
              case List("root", location)     => Right(current.copy(root = Some(Path.of(location))))
              case List("text-exclude", glob) => Right(current.copy(textExclusions = current.textExclusions :+ glob))
              case List("text", rule, field, value) if rule.toIntOption.isDefined =>
                val fields = current.texts.getOrElse(rule.toInt, Map.empty)
                Right(current.copy(texts =
                  current.texts.updated(
                    rule.toInt,
                    fields.updated(field, fields.getOrElse(field, Vector.empty) :+ value)
                  )
                ))
              case _ => Left(s"$path:${index + 1}: unrecognised manifest line: $line")
            }
          }
      }
}

/** Compiled outputs of a build and the external classpath needed to read them; the public API of `libraries`, modules
  * other builds depend on, is used by those builds. `options` holds the compiler options of each module, `sources` the
  * source files written by hand, and `root` the directory of the build, which reported paths are relative to.
  * `platforms` are the Scala.js and Scala Native twins of JVM modules, which compile the shared sources again. `texts`
  * holds the fields of each repository text rule by its index, and `textExclusions` the files no text rule reads.
  */
case class Manifest(
    outputs: Vector[Output],
    classpath: Vector[Path],
    libraries: Set[String],
    options: Map[String, Vector[String]] = Map.empty,
    sources: Vector[Output] = Vector.empty,
    root: Option[Path] = None,
    platforms: Set[String] = Set.empty,
    texts: Map[Int, Map[String, Vector[String]]] = Map.empty,
    textExclusions: Vector[String] = Vector.empty
)
