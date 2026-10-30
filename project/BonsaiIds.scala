import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import scala.util.matching.Regex

/** The ids the tools accept, read from the tools' own sources, so that a plugin cannot drift from them. */
object BonsaiIds {

  /** The ids the compiler plugin knows, by the id its messages and options use. */
  val Checks: Regex = """extends Check\("([a-z-]+)"""".r

  /** The ids the analysis knows, by the id its findings and options use. */
  val Findings: Regex = """extends Rule\("([a-z-]+)"""".r

  /** The ids the lints of the Java code have, which the analysis reports as findings of their own. */
  val Lints: Regex = """extends JavaLint\("([a-z-]+)"""".r

  /** The names the scalafix rules have, by the class the service file names, one a line. */
  val Rules: Regex = """(?m)\.([A-Za-z]+)$""".r

  /** The ids the pattern finds in the source, one a line. */
  def of(source: Path, pattern: Regex): Seq[String] =
    pattern.findAllMatchIn(Files.readString(source)).map(_.group(1)).toSeq

  /** The ids the pattern finds in the source, one a line. */
  def of(source: File, pattern: Regex): Seq[String] = of(source.toPath, pattern)
}
