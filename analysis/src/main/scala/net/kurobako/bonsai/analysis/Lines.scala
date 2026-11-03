package net.kurobako.bonsai.analysis

import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import java.util.Arrays
import scala.collection.mutable

/** Lines of offsets in sources, taken from the source's own text. The compiler's line table of a source read back from
  * TASTy is replaced by that of the last class read from it, so the lines it gives depend on the order classes were
  * read in.
  */
private[analysis] final class Lines {
  private val starts = mutable.Map.empty[SourceFile, Array[Int]]

  /** The one-based line `position` is on, or 0 when it does not exist. */
  def line(position: SourcePosition): Int = if (position.exists) line(position.source, position.point) else 0

  /** The one-based line `offset` is on. */
  def line(source: SourceFile, offset: Int): Int = at(source, offset) + 1

  /** The `path:line` `position` is on, or `unknown` when it does not exist. */
  def location(position: SourcePosition): String =
    if (position.exists) s"${position.source.path}:${line(position)}" else "unknown"

  /** The zero-based line `offset` is on. */
  def at(source: SourceFile, offset: Int): Int = {
    var table = starts.get(source).orNull
    if (table == null) {
      val text = source.content()
      if (text.isEmpty) return source.offsetToLine(offset)
      val builder = Array.newBuilder[Int]
      builder += 0
      var index = 0
      while (index < text.length) {
        if (text(index) == '\n') builder += index + 1
        index += 1
      }
      table = builder.result()
      starts(source) = table
    }
    val found = Arrays.binarySearch(table, offset)
    if (found >= 0) found else -found - 2
  }
}
