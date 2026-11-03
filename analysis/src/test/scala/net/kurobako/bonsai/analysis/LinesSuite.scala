package net.kurobako.bonsai.analysis

import dotty.tools.dotc.util.SourceFile

class LinesSuite extends munit.FunSuite {
  private val text = "first\nsecond line\n\nfourth\nfifth"

  test("lines come from the text of the source, whatever line table the compiler holds for it") {
    val source = SourceFile.virtual("Lines.scala", text)
    source.setLineIndicesFromLineSizes(Array.fill(10)(3))
    assertNotEquals(source.offsetToLine(text.indexOf("fifth")), 4)
    assertEquals(Lines.at(source, 0), 0)
    assertEquals(Lines.at(source, text.indexOf("second")), 1)
    assertEquals(Lines.at(source, text.indexOf("fourth")), 3)
    assertEquals(Lines.at(source, text.indexOf("fifth")), 4)
    assertEquals(Lines.at(source, text.length), 4)
  }

  test("the line of an offset is the same whichever order offsets are asked in") {
    val offsets = text.indices
    val source  = SourceFile.virtual("Lines.scala", text)
    val forward = offsets.map(Lines.at(source, _))
    val other   = SourceFile.virtual("Lines.scala", text)
    assertEquals(offsets.reverse.map(Lines.at(other, _)).reverse, forward)
  }
}
