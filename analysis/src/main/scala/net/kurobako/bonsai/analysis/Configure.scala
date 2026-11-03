package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import net.kurobako.bonsai.configuration.*
import net.kurobako.bonsai.configuration.BonsaiConfiguration.*

/** Reads `.bonsai.conf` for a module and writes what a build that does not read it itself needs to a file, one
  * tab-separated entry a line: the module's `disabled`, `errors`, `warnings`, `wildcards` and `library` settings, the
  * build's `hot`, `bounded`, and `checked` lists, the `option`s the analysis runs with, and the manifest lines of the
  * text rules as they are.
  */
object Configure {
  private val usage = "usage: bonsai-configure --config <file> --module <id> --out <file>"

  /** The lines of the module's configuration, whose paths are relative to the directory of the `file`. */
  def lines(file: Path, module: String): Either[String, Vector[String]] =
    try Right(render(BonsaiConfiguration.read(file.toFile), file.toAbsolutePath.getParent, module))
    catch { case error: ConfigurationError => Left(error.getMessage) }

  private def render(
      configured: BonsaiConfiguration.Configuration,
      root: Path,
      module: String
  ): Vector[String] = {
    val settings = configured.resolve(module)
    val analysis = configured.analysis(settings)
    val options  = analysis.options(
      Finding.Rule.values.map(_.id).toSet,
      root.resolve(configured.baseline.getOrElse(BonsaiConfiguration.DefaultBaseline)).toFile,
      root.resolve(configured.purity.getOrElse(BonsaiConfiguration.DefaultPurity)).toFile
    )
    def entries(key: String, values: Set[String]) = values.toVector.sorted.map(value => s"$key\t$value")
    entries("disabled", analysis.disabled) ++ entries("errors", analysis.errors) ++
      entries("warnings", analysis.warnings) ++ settings.wildcards.map(value => s"wildcards\t$value") ++
      settings.library.map(value => s"library\t$value") ++ entries("hot", analysis.hot) ++
      analysis.bounded.map((member, max, _) => s"bounded\t$member:$max") ++
      entries("checked", configured.checked.getOrElse(Set.empty)) ++ options.map(option => s"option\t$option") ++
      configured.text
  }

  def main(args: Array[String]): Unit = {
    val result = args.toList match {
      case List("--config", file, "--module", module, "--out", out) =>
        lines(Path.of(file), module).map(entries =>
          Files.writeString(Path.of(out), entries.map(_ + "\n").mkString): Unit
        )
      case _ => Left(usage)
    }
    result match {
      case Left(error) =>
        Console.err.println(error)
        sys.exit(1)
      case Right(_) =>
    }
  }
}
