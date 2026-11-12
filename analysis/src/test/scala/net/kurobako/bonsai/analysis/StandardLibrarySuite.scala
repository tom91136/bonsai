package net.kurobako.bonsai.analysis

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import net.kurobako.bonsai.testing.*

class StandardLibrarySuite extends AnalysisSuite {
  private val older: Vector[Path] =
    sys.props("bonsai.analysis.oldlibrary").split(File.pathSeparator).toVector.map(Path.of(_))

  test("a build on an older standard library is read against the library of the analysis") {
    Scratch.using("bonsai-") { directory =>
      val records = Files.createDirectories(directory.resolve("records"))
      val java    = Seq("rec/Point.java" -> "package rec; public record Point(int x, int y) {}")
      write(directory.resolve("java"), java)
      javac(java.map((name, _) => directory.resolve("java").resolve(name)), records, Vector.empty): Unit
      val main = compile(
        directory.resolve("main"),
        Seq(
          "Use.scala" ->
            """package use
              |import rec.Point
              |object Use {
              |  def length(text: String | Null): Int = if text == null then 0 else text.nn.length
              |  def order: Int = summon[Ordering[Int]].compare(1, 2)
              |  def across(point: Point): Int = point.x() + point.y()
              |}""".stripMargin,
          "Main.scala" -> "object Main { def main(args: Array[String]): Unit = println(use.Use.order) }"
        ),
        Vector(records)
      )
      val manifest = Manifest(
        Vector(Output("m", Output.Scope.Main, main)),
        scoped(Map("m" -> (records +: older))),
        Set.empty,
        sources = Vector(
          Output("m", Output.Scope.Main, directory.resolve("main/Use.scala")),
          Output("m", Output.Scope.Main, directory.resolve("main/Main.scala"))
        ),
        jdk = targetJdk
      )
      val result = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      assertEquals(result.problems, Vector.empty[String])
      assertEquals(result.notes, Vector.empty[String])
    }
  }

  test("only jars that are the standard library are replaced by the one the analysis runs on") {
    Scratch.using("bonsai-") { directory =>
      def jar(name: String, entry: String) = {
        val classes = Files.createDirectories(directory.resolve(s"classes-$name"))
        write(classes, Seq(entry -> ""))
        archive(classes, directory.resolve(name), "x", "1")
      }
      val current = Path.of(classOf[Predef.type].getProtectionDomain.getCodeSource.getLocation.toURI)
      val app     = jar("app.jar", "scala/Predef.class")
      val next    = jar("scala-library-next_3-0.1.0.jar", "scala/collection/Next.class")
      val renamed = jar("scala-library-fork.jar", "scala/Fork.class")
      val old     = jar("scala-library-2.13.16.jar", "scala/Predef.class")
      val three   = jar("scala3-library_3-3.3.6.jar", "scala/runtime/stdLibPatches/Predef.class")
      val sjs     = jar("scala3-library_sjs1_3-3.3.6.jar", "scala/runtime/stdLibPatches/Predef.class")
      assertEquals(
        Classpath.unified(Vector(app, next, old, renamed, three, sjs)),
        Vector(app, next, current, sjs, renamed)
      )
      assertEquals(Classpath.unified(Vector(app, next, renamed)), Vector(app, next, renamed))
      assertEquals(Classpath.unified(Vector(sjs, app, three)), Vector(current, sjs, app))
    }
  }
}
