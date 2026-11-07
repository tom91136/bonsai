package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import net.kurobako.bonsai.testing.*

class DeterminismSuite extends AnalysisSuite {
  override def munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(2, "min")

  private def record(kind: String, fields: String*): String = (kind +: fields).mkString("\t")

  private def statements(count: Int): String = (1 to count).map(index =>
    s"    total += values($index % values.length) * $index"
  ).mkString("\n")

  private def huge(name: String): String =
    s"object $name {\n  def run(values: Array[Int]): Int = {\n    var total = 0\n${statements(1000)}\n    total\n  }\n}"

  private def service(name: String): Vector[String] = {
    val source = s"demo/$name.java"
    Vector(
      record("node", s"demo.$name", source, "4", "true"),
      record("node", "demo.Shared", source, "9", "true"),
      record(
        "method",
        s"demo.$name",
        s"demo.$name.helper",
        s"demo.$name.helper()",
        "false",
        "",
        "",
        s"$source:8",
        "String",
        "false"
      ),
      record("site", s"demo.$name.one", s"demo.$name.helper()", "", s"$source:12", "true"),
      record("site", s"demo.$name.two", s"demo.$name.helper()", "", s"$source:16", "true")
    )
  }

  private def analyse(parallel: Parallel, classes: Path, models: Vector[Path]): Vector[String] = {
    val manifest = Manifest(
      Vector(Output("m", Output.Scope.Main, classes)),
      classpaths,
      Set.empty,
      models = models.map(Output("j", Output.Scope.Main, _)),
      jdk = targetJdk
    )
    val result = Extraction.read(manifest, parallel = parallel).fold(errors => fail(errors.mkString("\n")), identity)
    val found  = Report.findings(result, parallel = parallel)
    val shown  = Report(found, result.graph.nodes, Report.Options(Set.empty, Set.empty, Set.empty, info = true))
    found.map(finding => s"${finding.rule.id}\t${finding.node.name}\t${finding.detail}") ++ shown.map(_.show) ++
      result.bytecode.map(method => s"${method.owner}.${method.name}:${method.size}") ++
      result.graph.nodes.keys.toVector.sorted
  }

  test("the findings are the same, in the same order, whatever the parallelism") {
    Scratch.using("bonsai-") { directory =>
      val classes = compile(
        directory.resolve("scala"),
        Seq(
          "Main.scala" ->
            "object Main { def main(args: Array[String]): Unit = println(Tools.compute() + Tools.compute()) }",
          "Tools.scala"  -> "object Tools { def compute(): Int = 1\n  def unused(): Int = 2 }",
          "First.scala"  -> huge("First"),
          "Second.scala" -> huge("Second"),
          "Third.scala"  -> huge("Third")
        ),
        Vector.empty
      )
      val models = Vector("First", "Second", "Third").map { name =>
        val file = directory.resolve(s"$name.tsv")
        Files.writeString(file, service(name).mkString("", "\n", "\n")): Unit
        file
      }
      val sequential = analyse(Parallel.sequential, classes, models)
      assert(sequential.exists(_.startsWith("huge-method\t")), sequential.mkString("\n"))
      assert(sequential.exists(_.startsWith("ignored-result\t")), sequential.mkString("\n"))
      for (_ <- 1 to 3) {
        val parallel = Parallel(4)
        try assertEquals(analyse(parallel, classes, models), sequential)
        finally parallel.close()
      }
    }
  }
}
