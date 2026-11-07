package net.kurobako.bonsai.analysis

/** Raw calls beside an inline or extension method of a library that does nothing but make that call. */
class WrappersSuite extends AnalysisSuite {

  private val Library = Seq(
    "Paths.scala" ->
      """package lib
        |import java.nio.file.Files
        |import java.nio.file.LinkOption
        |import java.nio.file.Path
        |object paths {
        |  extension (p: Path) {
        |    inline def exists(inline options: LinkOption*): Boolean = Files.exists(p, options*)
        |    inline def isSymLink: Boolean                           = Files.isSymbolicLink(p)
        |    def digest(algorithm: String, size: Int = 8): String     = algorithm + p + size
        |    inline def sha256(size: Int = 8): String                = digest("SHA-256", size)
        |    def name: String                                         = p.getFileName.toString
        |    inline def fileName: String                              = name
        |    def delete(): Boolean                                    = if (Files.isDirectory(p)) false else Files.deleteIfExists(p)
        |  }
        |}""".stripMargin
  )

  private def findings(sources: (String, String)*): Vector[Finding] =
    Report.findings(extract(sources, dependencies = Library))

  test("a raw call that a library method wraps exactly is bypassing the wrapper") {
    val found = findings(
      "Plan.scala" ->
        """import java.nio.file.Files
          |import java.nio.file.LinkOption
          |import java.nio.file.Path
          |import lib.paths.*
          |object Plan {
          |  def run(p: Path, q: Path): Boolean =
          |    Files.exists(p, LinkOption.NOFOLLOW_LINKS) && Files.exists(q) && Files.isSymbolicLink(p)
          |  def hash(p: Path): String = p.digest("SHA-256")
          |}""".stripMargin
    )
    assertFinding(found, "bypassed-wrapper", "Plan.run", "Files.exists 2 times", "paths.exists")(using
      Finding.Level.Info
    )
    assertFinding(found, "bypassed-wrapper", "Plan.run", "Files.isSymbolicLink", "paths.isSymLink")(using
      Finding.Level.Info
    )
    assertFinding(found, "bypassed-wrapper", "Plan.hash", "digest", "paths.sha256")(using Finding.Level.Info)
  }

  test("calls that differ from the wrapper, or reach it, or that cannot import it, bypass nothing") {
    val found = findings(
      "Plan.scala" ->
        """import java.nio.file.Files
          |import java.nio.file.LinkOption
          |import java.nio.file.Path
          |import lib.paths.*
          |object Plan {
          |  def kept(p: Path): Boolean = p.exists(LinkOption.NOFOLLOW_LINKS) && p.isSymLink
          |  def other(p: Path): String = p.digest("MD5") + p.sha256()
          |  def remove(p: Path): Boolean = Files.deleteIfExists(p)
          |  def label(p: Path): String = p.name
          |}""".stripMargin,
      "Elsewhere.scala" ->
        """import java.nio.file.Files
          |import java.nio.file.Path
          |object Elsewhere { def run(p: Path): Boolean = Files.isSymbolicLink(p) }""".stripMargin
    )
    assertNoFinding(found)(_.rule.id == "bypassed-wrapper")
  }
}
