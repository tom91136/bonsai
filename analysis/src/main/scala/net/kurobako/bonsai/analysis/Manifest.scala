package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.PathMatcher
import net.kurobako.bonsai.collections.*
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters.*

object Manifest {

  private type Update = Manifest => Either[String, Manifest]

  private def scopeOf(path: Path, index: Int, scope: String): Either[String, Output.Scope] =
    Output.Scope.values.find(_.id == scope).toRight(s"$path:${index + 1}: unknown scope: $scope")

  private def parseOutput(path: Path, index: Int, module: String, scope: String, location: String): Update =
    current =>
      scopeOf(path, index, scope).map { parsed =>
        current.copy(outputs = current.outputs :+ Output(module, parsed, Path.of(location)))
      }

  private def parseClasspath(path: Path, index: Int, module: String, scope: String, location: String): Update =
    current =>
      scopeOf(path, index, scope).map { parsed =>
        val key = module -> parsed
        current.copy(classpath =
          current.classpath.updated(key, current.classpath.getOrElse(key, Vector.empty) :+ Path.of(location))
        )
      }

  private def parseDepends(module: String, other: String): Update =
    current =>
      Right(current.copy(dependencies =
        current.dependencies.updated(module, current.dependencies.getOrElse(module, Set.empty) + other)
      ))

  private def parseLibrary(module: String): Update =
    current => Right(current.copy(libraries = current.libraries + module))

  private def parsePlatform(module: String): Update =
    current => Right(current.copy(platforms = current.platforms + module))

  private def parseRecorded(module: String): Update =
    current => Right(current.copy(recorded = current.recorded + module))

  private def parseResourced(module: String): Update =
    current => Right(current.copy(resourced = current.resourced + module))

  private def parseOption(module: String, option: String): Update =
    current =>
      Right(current.copy(options =
        current.options.updated(module, current.options.getOrElse(module, Vector.empty) :+ option)
      ))

  private def parseSource(path: Path, index: Int, module: String, scope: String, location: String): Update =
    current =>
      scopeOf(path, index, scope).map { parsed =>
        current.copy(sources = current.sources :+ Output(module, parsed, Path.of(location)))
      }

  private def parseProduct(path: Path, index: Int, module: String, scope: String, location: String): Update =
    current =>
      scopeOf(path, index, scope).map { parsed =>
        current.copy(products = current.products :+ Output(module, parsed, Path.of(location)))
      }

  private def parseResource(name: String, location: String): Update =
    current => Right(current.copy(resources = current.resources :+ Resource(name, Path.of(location))))

  private def parseModel(path: Path, index: Int, module: String, scope: String, location: String): Update =
    current =>
      scopeOf(path, index, scope).map { parsed =>
        current.copy(models = current.models :+ Output(module, parsed, Path.of(location)))
      }

  private def parseGenerated(location: String): Update =
    current => Right(current.copy(generated = current.generated :+ Path.of(location)))

  private def parseJdk(path: Path, index: Int, location: String): Update =
    current => {
      val home = Path.of(location)
      if (current.jdk.exists(_ != home)) Left(s"$path:${index + 1}: conflicting jdk: $location")
      else Right(current.copy(jdk = Some(home)))
    }

  private def parseRoot(location: String): Update =
    current => Right(current.copy(root = Some(Path.of(location))))

  private def parseTextExclude(glob: String): Update =
    current => Right(current.copy(textExclusions = current.textExclusions :+ glob))

  private def parseText(rule: String, field: String, value: String): Update =
    current => {
      val fields = current.texts.getOrElse(rule.toInt, Map.empty)
      Right(current.copy(texts =
        current.texts.updated(rule.toInt, fields.updated(field, fields.getOrElse(field, Vector.empty) :+ value))
      ))
    }

  /** Reads tab-separated `output <module> <scope> <directory>`, `classpath <module> <scope> <entry>`,
    * `library <module>`, `depends <module> <module>`, `platform <module>`, `recorded <module>`,
    * `option <module> <compiler option>`, `source <module> <scope> <file>`, `product <module> <scope> <file>`,
    * `resource <name> <file>`, `resourced <module>`, `model <module> <scope> <file>`, `generated <path>`, `jdk <home>`,
    * `root <directory>`, `text-exclude <glob>` and `text <rule index> <field> <value>` lines.
    */
  def read(path: Path): Either[String, Manifest] =
    Files.readAllLines(path).asScala.toVector.zipWithIndex.filter(_._1.nonEmpty)
      .foldLeft[Either[String, Manifest]](Right(Manifest(Vector.empty, Map.empty, Set.empty))) {
        case (manifest, (line, index)) =>
          manifest.flatMap { current =>
            line.split("\t", -1).toList match {
              case List("output", module, scope, location) => parseOutput(path, index, module, scope, location)(current)
              case List("classpath", module, scope, location) =>
                parseClasspath(path, index, module, scope, location)(current)
              case List("depends", module, other)          => parseDepends(module, other)(current)
              case List("library", module)                 => parseLibrary(module)(current)
              case List("platform", module)                => parsePlatform(module)(current)
              case List("recorded", module)                => parseRecorded(module)(current)
              case List("resourced", module)               => parseResourced(module)(current)
              case List("option", module, option)          => parseOption(module, option)(current)
              case List("source", module, scope, location) => parseSource(path, index, module, scope, location)(current)
              case List("product", module, scope, location) =>
                parseProduct(path, index, module, scope, location)(current)
              case List("resource", name, location)       => parseResource(name, location)(current)
              case List("model", module, scope, location) => parseModel(path, index, module, scope, location)(current)
              case List("generated", location)            => parseGenerated(location)(current)
              case List("jdk", location)                  => parseJdk(path, index, location)(current)
              case List("root", location)                 => parseRoot(location)(current)
              case List("text-exclude", glob)             => parseTextExclude(glob)(current)
              case List("text", rule, field, value) if rule.toIntOption.isDefined =>
                parseText(rule, field, value)(current)
              case _ => Left(s"$path:${index + 1}: unrecognised manifest line: $line")
            }
          }
      }
}

/** Compiled outputs of a build and the external classpath each module's scope compiled with, needed to read them; the
  * public API of `libraries`, modules other builds depend on, is used by those builds. `options` holds the compiler
  * options of each module, `sources` the source files each compile read, `products` the class and TASTy files each
  * compile recorded, which must exist, and `root` the directory of the build, which reported paths are relative to.
  * `resources` are the files that register entry points, each with its path in the jar, and `resourced` the modules
  * whose resources the build lists, which their outputs are not read for. `platforms` are the Scala.js and Scala Native
  * twins of JVM modules, which compile the shared sources again. `texts` holds the fields of each repository text rule
  * by its index, and `textExclusions` the files no text rule reads. `models` holds the files the javac plugin collected
  * the model of the Java code of each module in, `generated` the directories and files the build generates sources in,
  * `jdk` the home of the JDK the build targets, whose classes are read before the outputs, and `recorded` the modules
  * the compiler plugin compiled in the audit mode, whose findings it kept beside their classes, and `dependencies` the
  * modules of the build each module depends on, beside those its classpath holds the outputs of.
  */
case class Manifest(
    outputs: Vector[Output],
    classpath: Map[(String, Output.Scope), Vector[Path]],
    libraries: Set[String],
    options: Map[String, Vector[String]] = Map.empty,
    sources: Vector[Output] = Vector.empty,
    root: Option[Path] = None,
    platforms: Set[String] = Set.empty,
    texts: Map[Int, Map[String, Vector[String]]] = Map.empty,
    textExclusions: Vector[String] = Vector.empty,
    models: Vector[Output] = Vector.empty,
    jdk: Option[Path] = None,
    generated: Vector[Path] = Vector.empty,
    recorded: Set[String] = Set.empty,
    resources: Vector[Resource] = Vector.empty,
    resourced: Set[String] = Set.empty,
    dependencies: Map[String, Set[String]] = Map.empty,
    products: Vector[Output] = Vector.empty
) {

  /** The modules each module depends on: those whose outputs its classpath holds, and those the build names. */
  def requires: Map[String, Set[String]] = {
    val owners = outputs.map(output => output.path -> output.module).toMap
    (classpath.keySet.map(_._1) ++ dependencies.keySet).map { module =>
      module ->
        (classpathOf(Seq(module)).flatMap(owners.get).toSet ++
          dependencies.getOrElse(module, Set.empty)).excl(module)
    }.toMap
  }

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
      isGenerated(source) || marked.getOrElseUpdate(absolute, Generated.file(absolute))
    }
  }

  def relative(path: Path): Path = root.filter(path.startsWith(_)).fold(path)(_.relativize(path))

  /** The compiles, each a scope of a module, that have outputs. */
  def parts: Vector[Output.Part] = outputs.map(_.part).distinct

  /** The compiles of the modules that have a classpath but no outputs, which are not read. */
  def unbuilt: Set[Output.Part] = {
    val built = outputs.map(_.module).toSet
    classpath.keySet.filterNot(part => built(part._1))
  }

  /** The entries of the classpaths of the scopes of `modules`, in their order, each once. */
  def classpathOf(modules: Seq[String]): Vector[Path] =
    modules.flatMap(module =>
      Output.Scope.values.toSeq.flatMap(scope => classpath.getOrElse(module -> scope, Vector.empty))
    )
      .distinct.toVector
}
