package net.kurobako.bonsai.analysis

import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.PlainFile
import java.nio.file.Files
import net.kurobako.bonsai.testing.Scratch
import scala.io.Codec

class LinesSuite extends munit.FunSuite {
  private val text = "first\nsecond line\n\nfourth\nfifth"

  test("lines come from the text of the source, whatever line table the compiler holds for it") {
    val source = SourceFile.virtual("Lines.scala", text)
    source.setLineIndicesFromLineSizes(Array.fill(10)(3))
    val lines = Lines()
    assertNotEquals(source.offsetToLine(text.indexOf("fifth")), 4)
    assertEquals(lines.at(source, 0), 0)
    assertEquals(lines.at(source, text.indexOf("second")), 1)
    assertEquals(lines.at(source, text.indexOf("fourth")), 3)
    assertEquals(lines.at(source, text.indexOf("fifth")), 4)
    assertEquals(lines.at(source, text.length), 4)
  }

  test("the line of an offset is the same whichever order offsets are asked in") {
    val offsets = text.indices
    val source  = SourceFile.virtual("Lines.scala", text)
    val lines   = Lines()
    val forward = offsets.map(lines.at(source, _))
    val other   = SourceFile.virtual("Lines.scala", text)
    assertEquals(offsets.reverse.map(lines.at(other, _)).reverse, forward)
  }

  test("a later run over a file that changed at the same path reads the new lines") {
    Scratch.using("bonsai-lines-") { directory =>
      val path = directory.resolve("Lines.scala")
      Files.writeString(path, "a\nb\n"): Unit
      val before = SourceFile(PlainFile.toPlainFile(path), PlainFile.toPlainFile(directory), Codec.UTF8)
      assertEquals(Lines().line(before, 2), 2)
      Files.writeString(path, "x\ny\nz\nw\n"): Unit
      val after = SourceFile(PlainFile.toPlainFile(path), PlainFile.toPlainFile(directory), Codec.UTF8)
      assertEquals(before, after)
      assertEquals(Lines().line(after, 6), 4)
    }
  }
}
