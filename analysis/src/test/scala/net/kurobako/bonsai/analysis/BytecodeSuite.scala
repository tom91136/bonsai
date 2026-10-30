package net.kurobako.bonsai.analysis

import java.nio.file.Files
import scala.jdk.CollectionConverters.*
import scala.util.Using

class BytecodeSuite extends AnalysisSuite {
  // The fixture is a method of thousands of statements and a class of thousands of enum cases; on a loaded machine the
  // compiler needs more than the suite's default half minute to read it.
  override def munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(2, "min")

  private def statements(count: Int): String = (1 to count).map(index =>
    s"    total += values($index % values.length) * $index"
  ).mkString("\n")

  private def method(name: String, count: Int): String =
    s"  def $name(values: Array[Int]): Int = {\n    var total = 0\n${statements(count)}\n    total\n  }"

  private lazy val result = extract(Seq(
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
                        )}) }""".stripMargin,
    "Top.scala"      -> s"def topHuge(values: Array[Int]): Int = {\n  var total = 0\n${statements(1000)}\n  total\n}",
    "Packaged.scala" ->
      s"package demo\ndef packagedHuge(values: Array[Int]): Int = {\n  var total = 0\n${statements(1000)}\n  total\n}"
  ))

  private lazy val found = Bytecode.findings(result.bytecode, result.graph.nodes, None)

  test("where the build marks classes hot, huge methods elsewhere are information, and the 64 KB limit still fails") {
    val marked = Bytecode.findings(result.bytecode, result.graph.nodes, Some(Set("Kernel")))
    assertFinding(marked, "huge-method", "Kernel.huge has")
    assertFinding(marked, "method-limit", "Kernel.limit has")
    assertFinding(marked, "huge-method", "topHuge has", "outside the classes the build marks hot")(using
      Finding.Level.Info
    )
    assertNoFinding(marked)(finding => finding.detail.startsWith("topHuge") && finding.level == Finding.Level.Warning)
  }

  test("methods past HotSpot's huge method limit are reported, as are methods near the 64 KB limit") {
    assertFinding(found, "huge-method", "Kernel.huge has", "HotSpot interprets methods over 8000 bytes")
    assertFinding(found, "method-limit", "Kernel.limit has", "of the 64 KB method limit")
    assertNoFinding(found)(_.detail.startsWith("Kernel.small"))
  }

  test("lambda bodies are reported under the method that defines them") {
    assertFinding(found, "huge-method", "Kernel.lambda (lambda) has")
  }

  test("huge top-level methods are reported under their own name") {
    assertFinding(found, "huge-method", "topHuge has")
    assertFinding(found, "huge-method", "packagedHuge has")
  }

  test("a malformed class file fails the read rather than being skipped") {
    val directory = Files.createTempDirectory("bonsai-bytecode-")
    try {
      Files.write(
        directory.resolve("Junk.class"),
        Array[Byte](0xca.toByte, 0xfe.toByte, 0xba.toByte, 0xbe.toByte, 0, 0, 0, 61, 0, 2, 99)
      ): Unit
      val error = intercept[IllegalArgumentException](Bytecode.read(Vector(directory)))
      assert(error.getMessage.contains("unknown constant pool tag"), error.getMessage)
    } finally Using.resource(Files.walk(directory))(_.toList.asScala.reverse.foreach(Files.delete(_)))
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
