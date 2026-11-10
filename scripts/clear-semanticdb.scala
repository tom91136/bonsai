#!/usr/bin/env -S scala-cli shebang --server=false
//> using scala 3.8
// Removes the SemanticDB files that the build wrote through a link into the store of sbt, which it makes read-only.
//
// The compiler writes the SemanticDB of a source it compiles again, and sbt has replaced the file with a link into its
// content-addressed store, where the file cannot be written. The links are removed so that the compiler writes the
// files anew.

import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes

@main def clearSemanticdb(): Unit = {
  val root    = Path.of(sys.props("scala.sources")).toAbsolutePath.getParent.getParent
  var removed = 0
  Files.walkFileTree(
    root,
    new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attrs: BasicFileAttributes) = {
        if (attrs.isSymbolicLink && file.getFileName.toString.endsWith(".semanticdb")) {
          Files.deleteIfExists(file)
          removed += 1
        }
        FileVisitResult.CONTINUE
      }
      override def visitFileFailed(file: Path, error: java.io.IOException) = FileVisitResult.CONTINUE
    }
  )
  println(s"removed $removed semanticdb links")
}
