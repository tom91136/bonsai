package net.kurobako.bonsai.analysis

/** Raw calls beside an inline or extension method of a library that does nothing but make that call. */
class WrappersSuite extends AnalysisSuite {

  private val Library = Seq(
    "Paths.scala" ->
      """package lib
        |import java.nio.file.Files
        |import java.nio.file.LinkOption
        |import java.nio.file.Path
        |import java.nio.charset.Charset
        |import java.nio.charset.StandardCharsets.UTF_8
        |object paths {
        |  extension (p: Path) {
        |    inline def exists(inline options: LinkOption*): Boolean = Files.exists(p, options*)
        |    inline def isSymLink: Boolean                           = Files.isSymbolicLink(p)
        |    inline def createDirIfNotExists(): Path                   = Files.createDirectories(p)
        |    inline def readString(inline charset: Charset = UTF_8): String = String(Files.readAllBytes(p), charset)
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

  test("a call with the varargs left empty is bypassing a wrapper that leaves them empty") {
    val found = findings(
      "Plan.scala" ->
        """import java.nio.file.Files
          |import java.nio.file.Path
          |import lib.paths.*
          |object Plan {
          |  def prepare(trash: Path): Path = Files.createDirectories(trash.resolve("files"))
          |}""".stripMargin
    )
    assertFinding(
      found,
      "bypassed-wrapper",
      "Plan.prepare",
      "Files.createDirectories",
      "paths.createDirIfNotExists"
    )(using
      Finding.Level.Info
    )
  }

  test("a constructor around another call is bypassing a wrapper that composes the two") {
    val found = findings(
      "Plan.scala" ->
        """import java.nio.charset.StandardCharsets
          |import java.nio.file.Files
          |import java.nio.file.Path
          |import lib.paths.*
          |object Plan {
          |  def read(p: Path): String = String(Files.readAllBytes(p), StandardCharsets.UTF_8)
          |}""".stripMargin
    )
    assertFinding(found, "bypassed-wrapper", "Plan.read", "paths.readString")(using Finding.Level.Info)
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
          |  def raw(p: Path): Array[Byte] = Files.readAllBytes(p)
          |  def built(size: Int): String = String(Array.fill(size)(0.toByte), java.nio.charset.StandardCharsets.UTF_8)
          |}""".stripMargin,
      "Elsewhere.scala" ->
        """import java.nio.file.Files
          |import java.nio.file.Path
          |object Elsewhere { def run(p: Path): Boolean = Files.isSymbolicLink(p) }""".stripMargin
    )
    assertNoFinding(found)(_.rule.id == "bypassed-wrapper")
  }
}
