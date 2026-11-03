import java.io.File
import java.nio.file.Files
import scala.util.matching.Regex

/** The ids the tools accept, read from the tools' own sources, so that a plugin cannot drift from them. */
object BonsaiIds {

  /** The ids the compiler plugin knows, by the id its messages and options use. */
  val Checks: Regex = """= check\("([a-z-]+)"""".r

  /** The ids the analysis knows, by the id its findings and options use. */
  // the meta-build compiles before the analysis, so the ids are read from source, which FindingIdsSuite checks
  val Findings: Regex = """= rule\("([a-z0-9-]+)"""".r

  /** The ids the lints of the Java code have, which the analysis reports as findings of their own. */
  val Lints: Regex = """extends LintId\("([a-z0-9-]+)"""".r

  /** The names the scalafix rules have, by the class the service file names, one a line. */
  val Rules: Regex = """(?m)\.([A-Za-z]+)$""".r

  /** The ids of the laws of collection chains, which name the checks that report them, read from the compiled laws; the
    * `accessor` is `ids` for all of them and `javaIds` for those that read as Java.
    */
  def laws(classpath: Seq[File], accessor: String = "ids"): Seq[String] = {
    val loader = new java.net.URLClassLoader(classpath.map(_.toURI.toURL).toArray, null)
    try {
      val module = loader.loadClass("net.kurobako.bonsai.collections.Laws$").getField("MODULE$").get(null)
      val ids    = module.getClass.getMethod(accessor).invoke(module)
      ids.getClass.getMethod("mkString", classOf[String]).invoke(ids, "\n").asInstanceOf[String].split("\n").toSeq
    } finally loader.close()
  }

  /** The ids the pattern finds in the source, one a line. */
  def of(source: File, pattern: Regex): Seq[String] =
    pattern.findAllMatchIn(Files.readString(source.toPath)).map(_.group(1)).toSeq
}
