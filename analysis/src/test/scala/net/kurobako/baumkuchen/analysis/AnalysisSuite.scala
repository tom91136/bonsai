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
    * output for other builds to use, `dependencies` are compiled onto the classpath outside the build, `concurrent`
    * names methods that run the functions passed to them on other threads, and `twins` holds the sources each platform
    * twin, module `t0` and on, compiles beside the main ones, in place of those it names alike.
    */
  protected def extract(
      main: Seq[(String, String)],
      test: Seq[(String, String)] = Nil,
      resources: Seq[(String, String)] = Nil,
      library: Boolean = false,
      dependencies: Seq[(String, String)] = Nil,
      concurrent: Set[String] = Set.empty,
      generated: Vector[String] = Vector.empty,
      reflective: Vector[String] = Vector.empty,
      twins: Seq[Seq[(String, String)]] = Nil,
      hot: Vector[String] = Vector.empty,
      bounded: Vector[Complexity.Bounded] = Vector.empty
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
      def located(module: String, scope: Output.Scope, root: String, sources: Seq[(String, String)]) =
        sources.map((name, _) => Output(module, scope, directory.resolve(root).resolve(name))).toVector
      // A twin source named like a main one replaces it, as a source for one platform does.
      val platformClasspath = external.toVector
      val platforms         = twins.toVector.zipWithIndex.map { (own, index) =>
        val module    = s"t$index"
        val replacing = own.map(_._1).toSet
        val shared    = main.filterNot((name, _) => replacing(name))
        val classes   = compile(directory.resolve(module), shared ++ own, platformClasspath)
        (
          Output(module, Output.Scope.Main, classes),
          located(module, Output.Scope.Main, "main", shared) ++ located(module, Output.Scope.Main, module, own)
        )
      }
      // The twins come first, as a build may list them, so the analysis must not depend on the order.
      val outputs = platforms.map(_._1) ++
        Vector(Output("m", Output.Scope.Main, mainClasses), Output("m", Output.Scope.Test, testClasses))
      val manifest = Manifest(
        outputs,
        external.toVector ++ classpath,
        if (library) Set("m") else Set.empty,
        sources = located("m", Output.Scope.Main, "main", main) ++ located("m", Output.Scope.Test, "test", test) ++
          platforms.flatMap(_._2),
        platforms = platforms.map(_._1.module).toSet
      )
      val result =
        Extraction.read(manifest, generated, concurrent, reflective, hot, bounded).fold(
          errors => fail(errors.mkString("\n")),
          identity
        )
      assert(result.problems.isEmpty, result.problems.mkString("\n"))
      result
    } finally Using.resource(Files.walk(directory))(_.toList.asScala.reverse.foreach(Files.delete(_)))
  }

  /** The pattern a build passes to mark `src/generated` as generated. */
  protected val GeneratedSources: Vector[String] = Vector("glob:**/src/generated/**")

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
