package net.kurobako.bonsai.analysis

import dotty.tools.dotc.util.SourceFile
import java.util.Arrays
import java.util.IdentityHashMap

/** Lines of offsets in sources, taken from the source's own text. The compiler's line table of a source read back from
  * TASTy is replaced by that of the last class read from it, so the lines it gives depend on the order classes were
  * read in.
  */
private[analysis] object Lines {
  private val starts = IdentityHashMap[SourceFile, Array[Int]]()

  /** The zero-based line `offset` is on. */
  def at(source: SourceFile, offset: Int): Int = {
    var table = starts.get(source)
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
      starts.put(source, table)
    }
    val found = Arrays.binarySearch(table, offset)
    if (found >= 0) found else -found - 2
  }
}
