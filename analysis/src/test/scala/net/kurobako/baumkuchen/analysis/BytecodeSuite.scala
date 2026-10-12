package net.kurobako.baumkuchen.analysis

class BytecodeSuite extends AnalysisSuite {
  private def statements(count: Int): String = (1 to count).map(index =>
    s"    total += values($index % values.length) * $index"
  ).mkString("\n")

  private def method(name: String, count: Int): String =
    s"  def $name(values: Array[Int]): Int = {\n    var total = 0\n${statements(count)}\n    total\n  }"

  private lazy val found = {
    val result = extract(Seq(
      "Main.scala"   -> "object Main { def main(args: Array[String]): Unit = println(Kernel.small(Array(1))) }",
      "Kernel.scala" -> s"""object Kernel {
                           |${method("huge", 1000)}
                           |${method("limit", 3800)}
                           |${method("small", 10)}
                           |  def lambda(values: Array[Int]): Array[Int] = values.map { first =>
                           |    val values = Array(first)
                           |    var total = 0
                           |${statements(1000)}
                           |    total
                           |  }
                           |}
                           |object Table { val values: Vector[Int] = Vector(${(1 to 2000).mkString(", ")}) }
                           |enum Opcode { case ${(1 to 1500).map(index => s"Op$index").mkString(", ")} }
                           |enum Modifier(val spelling: String) {
                           |${(1 to 1500).map(index => s"  case Mod$index extends Modifier(\"$index\")").mkString("\n")}
                           |  case Vendor(value: String) extends Modifier(value)
                           |}
                           |class Lazy { lazy val values: Vector[Int] = Vector(${(1 to 2000).mkString(
                            ", "
                          )}) }""".stripMargin
    ))
    Bytecode.findings(result.bytecode, result.graph.nodes)
  }

  test("methods past HotSpot's huge method limit are reported, as are methods near the 64 KB limit") {
    assertFinding(found, "huge-method", "Kernel.huge has", "HotSpot interprets methods over 8000 bytes")
    assertFinding(found, "method-limit", "Kernel.limit has", "of the 64 KB method limit")
    assertNoFinding(found)(_.detail.startsWith("Kernel.small"))
  }

  test("lambda bodies are reported under the method that defines them") {
    assertFinding(found, "huge-method", "Kernel.lambda (lambda) has")
  }

  test("initialisers that run once are left alone") {
    assertNoFinding(found)(finding => finding.detail.contains("Table") || finding.detail.contains("Lazy"))
  }

  test("methods the compiler writes for a large enum are information, since only splitting the enum shrinks them") {
    assertFinding(found, "huge-method", "Opcode.valueOf has", "splitting the enum")(using Finding.Level.Info)
    assertNoFinding(found)(finding =>
      (finding.detail.contains("Opcode") || finding.detail.contains("Modifier")) &&
        finding.level == Finding.Level.Warning
    )
    assertFinding(found, "huge-method", "Modifier.fromOrdinal has", "splitting the enum")(using Finding.Level.Info)
  }
}
