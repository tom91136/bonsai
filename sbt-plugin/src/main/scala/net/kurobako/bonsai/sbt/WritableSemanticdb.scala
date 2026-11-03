package net.kurobako.bonsai.sbt

import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import sbt.*
import sbt.Keys.*

/** sbt restores cached outputs as links into its read-only cache, and the compiler rewrites the semanticdb of a changed
  * source in place, through such a link; each link becomes a file of its own before compiling, so the write lands. A
  * link whose entry the cache no longer holds is removed, as its source is gone or recompiles.
  */
object WritableSemanticdb extends AutoPlugin {
  override def requires: Plugins      = plugins.JvmPlugin
  override def trigger: PluginTrigger = allRequirements

  override def projectSettings: Seq[Setting[?]] = Seq(Compile, Test).flatMap(configuration =>
    inConfig(configuration)(
      compileIncremental := Def.uncached(
        compileIncremental.dependsOn(Def.task(copyLinks(semanticdbTargetRoot.value.toPath))).value
      )
    )
  )

  /** The links under the root, which the walk reads from the attributes it already holds of each entry. */
  private def links(root: Path): Vector[Path] = {
    val found = Vector.newBuilder[Path]
    val _     = Files.walkFileTree(
      root,
      new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attributes: BasicFileAttributes): FileVisitResult = {
          if (attributes.isSymbolicLink) found += file
          FileVisitResult.CONTINUE
        }
      }
    )
    found.result()
  }

  private def copyLinks(root: Path): Unit =
    if (Files.isDirectory(root))
      links(root).foreach { link =>
        val bytes = Option.when(Files.exists(link))(Files.readAllBytes(link))
        Files.delete(link)
        bytes.foreach(Files.write(link, _))
      }
}
