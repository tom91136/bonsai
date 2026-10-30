package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.security.MessageDigest

/** Findings about the build rather than its code: modules whose warnings never fail the ci mode, and sources copied
  * byte for byte between the directories of a cross-built module.
  */
object Build {
  private val Fatal = Set("-Werror", "-Xfatal-warnings")

  /** Modules compiled without fatal warnings, which matter in the ci mode only. */
  def fatal(manifest: Manifest): Vector[Finding] = {
    val compiled = manifest.sources.filter(_.scope == Output.Scope.Main).map(_.module).toSet
    manifest.options.toVector.sortBy(_._1).collect {
      case (module, options) if compiled(module) && !options.exists(Fatal) =>
        Finding(
          Finding.Rule.WarningsNotFatal,
          node(module, module, Output.Scope.Main, "build.sbt"),
          s"$module compiles without -Werror, so its compiler warnings never fail the ci mode; add -Werror to its " +
            "scalacOptions"
        )
    }
  }

  /** Declared bounds that no collection the complexity analysis sizes reaches, which are stale. */
  def unused(bounds: Vector[Complexity.Bounded]): Vector[Finding] =
    bounds.map(bound =>
      Finding(
        Finding.Rule.UnusedBound,
        node(bound.member, "", Output.Scope.Main, ".bonsai.conf"),
        s"bounded ${bound.member} matches no collection the complexity analysis sizes; remove the entry or correct " +
          "the member"
      )
    )

  def duplicates(manifest: Manifest): Vector[Finding] =
    manifest.sources.distinctBy(_.path).filter(source => Files.isRegularFile(source.path))
      .groupBy(source => (source.path.getFileName.toString, digest(source))).values.toVector
      .filter(
        _.size > 1
      ).map(_.map(source => source.copy(path = relative(manifest, source.path))).sortBy(_.path.toString))
      .sortBy(_.head.path.toString).map { copies =>
        val first = copies.head
        Finding(
          Finding.Rule.PlatformDuplicate,
          node(first.path.toString, first.module, first.scope, first.path.toString),
          s"${first.path.getFileName} is identical in ${copies.map(_.path.getParent.toString).mkString(", ")}; move " +
            "it to a directory every platform compiles",
          if (copies.forall(_.scope == Output.Scope.Test)) Finding.Level.Info else Finding.Level.Warning
        )
      }

  private def relative(manifest: Manifest, path: java.nio.file.Path): java.nio.file.Path =
    manifest.root.filter(path.startsWith(_)).fold(path)(_.relativize(path))

  private def node(name: String, module: String, scope: Output.Scope, source: String): Node =
    Node(name, module, scope, source, line = 1, generated = false, exported = false, outer = None)

  private def digest(source: Output): String =
    MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(source.path)).map("%02x".format(_)).mkString
}
