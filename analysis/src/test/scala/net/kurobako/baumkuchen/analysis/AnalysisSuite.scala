package net.kurobako.baumkuchen.analysis

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import munit.FunSuite
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Compiles test sources into throwaway main and test outputs and analyses them as one build. */
trait AnalysisSuite extends FunSuite {
  protected val classpath: Vector[Path] =
    sys.props("baumkuchen.analysis.classpath").split(File.pathSeparator).toVector.map(Path.of(_))

  /** Sources and resources are named by relative path; test sources see the main output. A `library` publishes its main
    * output for other builds to use, `dependencies` are compiled onto the classpath outside the build, and `concurrent`
    * names methods that run the functions passed to them on other threads.
    */
  protected def extract(
      main: Seq[(String, String)],
      test: Seq[(String, String)] = Nil,
      resources: Seq[(String, String)] = Nil,
      library: Boolean = false,
      dependencies: Seq[(String, String)] = Nil,
      concurrent: Set[String] = Set.empty
  ): Extraction.Result = {
    val directory = Files.createTempDirectory("baumkuchen-")
    try {
      val external =
        Option.when(dependencies.nonEmpty)(compile(directory.resolve("dependencies"), dependencies, Vector.empty))
      val mainClasses = compile(directory.resolve("main"), main, external.toVector)
      resources.foreach { (name, content) =>
        val path = mainClasses.resolve(name)
        Files.createDirectories(path.getParent)
        Files.writeString(path, content): Unit
      }
      val testClasses = compile(directory.resolve("test"), test, mainClasses +: external.toVector)
      val outputs     = Vector(Output("m", Output.Scope.Main, mainClasses), Output("m", Output.Scope.Test, testClasses))
      val manifest    = Manifest(outputs, external.toVector ++ classpath, if (library) Set("m") else Set.empty)
      val result      =
        Extraction.read(manifest, concurrent = concurrent).fold(errors => fail(errors.mkString("\n")), identity)
      assert(result.problems.isEmpty, result.problems.mkString("\n"))
      result
    } finally Using.resource(Files.walk(directory))(_.toList.asScala.reverse.foreach(Files.delete(_)))
  }

  protected def assertFinding(found: Vector[Finding], rule: String, fragments: String*)(using
      level: Finding.Level = Finding.Level.Warning
  ): Unit =
    assert(
      found.exists(finding =>
        finding.rule.id == rule && finding.level == level && fragments.forall(finding.detail.contains(_))
      ),
      s"no $level $rule finding containing ${fragments.mkString(", ")} in\n${found.mkString("\n")}"
    )

  protected def assertNoFinding(found: Vector[Finding])(matches: Finding => Boolean): Unit =
    assert(!found.exists(matches), found.mkString("\n"))

  private def compile(directory: Path, sources: Seq[(String, String)], extra: Vector[Path]): Path = {
    val output = Files.createDirectories(directory.resolve("classes"))
    val files  = sources.map { (name, source) =>
      val path = directory.resolve(name)
      Files.createDirectories(path.getParent)
      Files.writeString(path, source).toString
    }
    val arguments =
      Array("-classpath", (extra ++ classpath).mkString(File.pathSeparator), "-d", output.toString, "-color:never") ++
        files
    val reporter = Driver().process(arguments, StoreReporter())
    assert(!reporter.hasErrors, reporter.allErrors.map(_.message).mkString("\n"))
    output
  }
}
