package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.PathMatcher
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters.*

object Manifest {

  private def scopeOf(path: Path, index: Int, scope: String): Either[String, Output.Scope] =
    Output.Scope.values.find(_.id == scope).toRight(s"$path:${index + 1}: unknown scope: $scope")

  /** Reads tab-separated `output <module> <scope> <directory>`, `classpath <module> <entry>`, `library <module>`,
    * `platform <module>`, `option <module> <compiler option>`, `source <module> <scope> <file>`, `model <module>
    * <scope> <file>`, `generated <path>`, `jdk <home>`, `root <directory>`, `text-exclude <glob>` and `text <rule
    * index> <field> <value>` lines.
    */
  def read(path: Path): Either[String, Manifest] =
    Files.readAllLines(path).asScala.toVector.zipWithIndex.filter(_._1.nonEmpty)
      .foldLeft[Either[String, Manifest]](Right(Manifest(Vector.empty, Map.empty, Set.empty))) {
        case (manifest, (line, index)) =>
          manifest.flatMap { current =>
            line.split('\t').toList match {
              case List("output", module, scope, location) =>
                scopeOf(path, index, scope).map {
                  parsed => current.copy(outputs = current.outputs :+ Output(module, parsed, Path.of(location)))
                }
              case List("classpath", module, location) =>
                Right(current.copy(classpath =
                  current.classpath.updated(
                    module,
                    current.classpath.getOrElse(module, Vector.empty) :+ Path.of(location)
                  )
                ))
              case List("library", module)        => Right(current.copy(libraries = current.libraries + module))
              case List("platform", module)       => Right(current.copy(platforms = current.platforms + module))
              case List("option", module, option) =>
                Right(current.copy(options =
                  current.options.updated(module, current.options.getOrElse(module, Vector.empty) :+ option)
                ))
              case List("source", module, scope, location) =>
                scopeOf(path, index, scope).map {
                  parsed => current.copy(sources = current.sources :+ Output(module, parsed, Path.of(location)))
                }
              case List("model", module, scope, location) =>
                scopeOf(path, index, scope).map {
                  parsed => current.copy(models = current.models :+ Output(module, parsed, Path.of(location)))
                }
              case List("generated", location) =>
                Right(current.copy(generated = current.generated :+ Path.of(location)))
              case List("jdk", location) =>
                val home = Path.of(location)
                if (current.jdk.exists(_ != home)) Left(s"$path:${index + 1}: conflicting jdk: $location")
                else Right(current.copy(jdk = Some(home)))
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

/** Compiled outputs of a build and the external classpath of each module needed to read them; the public API of
  * `libraries`, modules other builds depend on, is used by those builds. `options` holds the compiler options of each
  * module, `sources` the source files written by hand, and `root` the directory of the build, which reported paths are
  * relative to. `platforms` are the Scala.js and Scala Native twins of JVM modules, which compile the shared sources
  * again. `texts` holds the fields of each repository text rule by its index, and `textExclusions` the files no text
  * rule reads. `models` holds the files the javac plugin collected the model of the Java code of each module in,
  * `generated` the directories and files the build generates sources in, and `jdk` the home of the JDK the build
  * targets, whose classes are read before the outputs.
  */
case class Manifest(
    outputs: Vector[Output],
    classpath: Map[String, Vector[Path]],
    libraries: Set[String],
    options: Map[String, Vector[String]] = Map.empty,
    sources: Vector[Output] = Vector.empty,
    root: Option[Path] = None,
    platforms: Set[String] = Set.empty,
    texts: Map[Int, Map[String, Vector[String]]] = Map.empty,
    textExclusions: Vector[String] = Vector.empty,
    models: Vector[Output] = Vector.empty,
    jdk: Option[Path] = None,
    generated: Vector[Path] = Vector.empty
) {

  /** Whether a source lies in, or is, one of the directories and files the build generates, named by its path or
    * relative to `root`.
    */
  def isGenerated(source: Path): Boolean = {
    val absolute = root.fold(source)(_.resolve(source))
    generated.exists(absolute.startsWith(_))
  }

  def generatedMatcher: PathMatcher = {
    val marked = TrieMap.empty[Path, Boolean]
    source => {
      val absolute = root.fold(source)(_.resolve(source))
      isGenerated(source) || marked.getOrElseUpdate(absolute, Outputs.generated(absolute))
    }
  }

  /** The entries of the classpaths of `modules`, in their order, each once. */
  def classpathOf(modules: Seq[String]): Vector[Path] =
    modules.flatMap(classpath.getOrElse(_, Vector.empty)).distinct.toVector
}
