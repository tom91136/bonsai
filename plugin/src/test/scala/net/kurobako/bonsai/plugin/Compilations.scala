package net.kurobako.bonsai.plugin

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.reporting.StoreReporter
import java.nio.file.Files
import java.nio.file.Path
import net.kurobako.bonsai.testing.Scratch

object Compilations {

  /** The minor version of the compiler the tests run in. */
  val minor: Int = dotty.tools.dotc.config.Properties.versionNumberString.split('.')(1).toInt

  /** Compiles the named sources in a scratch directory and passes the reporter and that directory to `body`. */
  def run[A](sources: Seq[(String, String)], options: Seq[String])(body: (Reporter, Path) => A): A =
    Scratch.using("bonsai-") { directory =>
      val output = Files.createDirectory(directory.resolve("classes"))
      sources.foreach { (name, source) =>
        val file = directory.resolve(name)
        Files.createDirectories(file.getParent)
        Files.writeString(file, source)
      }
      val files    = sources.map((name, _) => directory.resolve(name).toString)
      val reporter = Driver().process(
        Array(
          "-classpath",
          sys.props("bonsai.plugin.classpath"),
          "-d",
          output.toString,
          s"-Xplugin:${sys.props("bonsai.plugin.jar")}",
          "-Xplugin-require:bonsai",
          "-color:never"
        ) ++ options.map(option => if (option.startsWith("-")) option else s"-P:bonsai:$option") ++ files,
        StoreReporter()
      )
      body(reporter, directory)
    }
}
