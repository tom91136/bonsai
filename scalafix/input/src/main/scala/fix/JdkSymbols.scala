/*
rule = ReversedIndexing
 */
package fix

object JdkSymbols {
  def sizes(paths: Vector[java.nio.file.Path]): Seq[Long] =
    paths.indices.map(index => java.nio.file.Files.size(paths(index)))
}
