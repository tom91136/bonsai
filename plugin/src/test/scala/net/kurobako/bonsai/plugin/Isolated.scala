package net.kurobako.bonsai.plugin

import java.lang.ref.WeakReference
import java.net.URLClassLoader
import java.nio.file.Files
import java.nio.file.Path
import java.util.function.Function
import net.kurobako.bonsai.testing.*
import scala.jdk.CollectionConverters.*
import scala.util.Try

object Isolated {

  /** What one compilation reported: the loaders of the plugin's phases and the state of the compiler it ran in. */
  case class Compiled(diagnostics: String, loaders: List[ClassLoader], base: WeakReference[AnyRef])

  /** The jar of the plugin the tests run with. */
  def jar: Path = Path.of(sys.props("bonsai.plugin.jar"))

  /** The entries of the test classpath that are not the plugin or what the plugin bundles. */
  private def entries: Array[java.net.URL] =
    sys.props("bonsai.plugin.classpath")
      .split(java.io.File.pathSeparator)
      .map(Path.of(_))
      .filter { entry =>
        val name = entry.getFileName.toString
        !name.startsWith("bonsai-") || name.endsWith("-tests.jar") || name.startsWith("bonsai-test-support")
      }
      .map(_.toUri.toURL)

  def apply(): Isolated = {
    val loader = URLClassLoader(entries, ClassLoader.getPlatformClassLoader)
    require(
      Try(loader.loadClass("net.kurobako.bonsai.plugin.BonsaiPlugin")).isFailure,
      "the compiler's loader must not reach the plugin"
    )
    new Isolated(loader)
  }
}

/** A compiler of its own loader, which has the classpath of the tests without the plugin: the plugin is loaded for each
  * compilation from a jar, as a build tool loads it.
  */
class Isolated private (val loader: URLClassLoader) {
  private val probe = Class
    .forName(classOf[CompileProbe].getName, true, loader)
    .getDeclaredConstructor()
    .newInstance()
    .asInstanceOf[Function[Array[String], java.util.Map[String, AnyRef]]]

  /** Compiles `sources` with the plugin of `jar`. */
  def compile(jar: Path, sources: Seq[(String, String)], options: String*): Isolated.Compiled =
    Scratch.using("bonsai-isolated-") { directory =>
      val output = Files.createDirectory(directory.resolve("classes"))
      val files  = sources.map { (name, source) =>
        val file = directory.resolve(name)
        Files.createDirectories(file.getParent)
        Files.writeString(file, source).toString
      }
      val result = probe(
        Array(
          "-classpath",
          sys.props("bonsai.plugin.classpath"),
          "-d",
          output.toString,
          s"-Xplugin:$jar",
          "-Xplugin-require:bonsai",
          "-color:never"
        ) ++ options.map(option => if (option.startsWith("-")) option else s"-P:bonsai:$option") ++ files
      )
      Isolated.Compiled(
        result.get("diagnostics").asInstanceOf[String],
        result.get("loaders").asInstanceOf[java.util.List[ClassLoader]].asScala.toList,
        result.get("base").asInstanceOf[WeakReference[AnyRef]]
      )
    }
}
