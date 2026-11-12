package net.kurobako.bonsai.analysis

import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import java.nio.file.Path
import scala.annotation.tailrec
import scala.collection.mutable

/** Lines of offsets in sources, taken from the source's own text. The compiler's line table of a source read back from
  * TASTy is replaced by that of the last class read from it, so the lines it gives depend on the order classes were
  * read in.
  */
private[analysis] class Lines(relative: Path => Path = identity) {
  private val starts = mutable.Map.empty[SourceFile, Array[Int]]

  /** The one-based line `position` is on, or 0 when it does not exist. */
  def line(position: SourcePosition): Int = if (position.exists) line(position.source, position.point) else 0

  /** The one-based line `offset` is on. */
  def line(source: SourceFile, offset: Int): Int = at(source, offset) + 1

  /** The `path:line` `position` is on, or `unknown` when it does not exist. */
  def location(position: SourcePosition): String =
    if (position.exists) s"${path(position.source.path)}:${line(position)}" else "unknown"

  def path(file: String): String = relative(Path.of(file)).toString

  /** The zero-based line `offset` is on. */
  def at(source: SourceFile, offset: Int): Int = starts.get(source) match {
    case Some(table) => within(table, offset)
    case None        =>
      val text = source.content()
      if (text.isEmpty) source.offsetToLine(offset)
      else {
        val table = scan(text)
        starts(source) = table
        within(table, offset)
      }
  }

  private def scan(text: Array[Char]): Array[Int] = {
    val builder = Array.newBuilder[Int]
    builder += 0
    @tailrec def go(index: Int): Unit = if (index < text.length) {
      if (text(index) == '\n') builder += index + 1
      go(index + 1)
    }
    go(0)
    builder.result()
  }

  private def within(table: Array[Int], offset: Int): Int = {
    val found = java.util.Arrays.binarySearch(table, offset)
    if (found >= 0) found else -found - 2
  }
}
