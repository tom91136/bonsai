package net.kurobako.bonsai.analysis

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import net.kurobako.bonsai.testing.*

class BuildRootSuite extends AnalysisSuite {
  override def munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(2, "min")

  private def source(owner: String): String =
    s"""object $owner {
       |  def summary(items: List[Int], label: String): String = {
       |    val kept = items.filter(_ > 0)
       |    val scaled = kept.map(value => value * 2 + 1)
       |    val total = scaled.foldLeft(0)(_ + _)
       |    val count = scaled.size
       |    if (count == 0) label else s"$$label: $${total / count}"
       |  }
       |}""".stripMargin

  private def analysed(): (String, String) = Scratch.using("bonsai-root-") { dir =>
    val classes = compile(
      dir.resolve("main"),
      Seq("First.scala" -> source("First"), "Second.scala" -> source("Second")),
      Vector.empty
    )
    val manifest = dir.resolve("target/bonsai/manifest.tsv")
    Files.createDirectories(manifest.getParent): Unit
    Files.writeString(
      manifest,
      (Seq(s"output\tm\tmain\t$classes", s"jdk\t${targetJdk.get}", s"root\t$dir") ++
        classpath.filter(Files.isRegularFile(_)).map(jar => s"classpath\tm\tmain\t$jar")).mkString("", "\n", "\n")
    ): Unit
    val baseline = dir.resolve("bonsai-baseline.tsv")
    val printed  = ByteArrayOutputStream()
    val status   = Console.withOut(printed)(
      CLI.run(List("--manifest", manifest.toString, "--mode", "relaxed", "--write-baseline", baseline.toString))
    )
    assertEquals(status, 0)
    val text = Files.readString(baseline)
    val out  = printed.toString(UTF_8)
    assert(!text.contains(dir.toString), text)
    assert(!out.contains(dir.toString), out)
    (text, out)
  }

  test("the baseline and the printed findings of a build are the same from any directory") {
    val (baseline, printed) = analysed()
    val (again, reprinted)  = analysed()
    assert(baseline.contains("duplicate-code"), baseline)
    assert(printed.contains("First.scala:"), printed)
    assertEquals(baseline, again)
    assertEquals(printed, reprinted)
  }
}
