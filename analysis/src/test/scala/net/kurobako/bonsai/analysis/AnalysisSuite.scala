package net.kurobako.bonsai.analysis

import com.sun.source.util.JavacTask
import com.sun.source.util.TaskEvent
import com.sun.source.util.TaskListener
import com.sun.source.util.Trees
import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.reporting.StoreReporter
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.util.regex.Pattern
import javax.tools.DiagnosticListener
import javax.tools.JavaFileObject
import javax.tools.StandardLocation
import javax.tools.ToolProvider
import munit.FunSuite
import net.kurobako.bonsai.java.*
import net.kurobako.bonsai.java.Model as JavaModel
import net.kurobako.bonsai.testing.*
import scala.concurrent.Promise
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** Compiles test sources into throwaway main and test outputs and analyses them as one build. */
trait AnalysisSuite extends FunSuite {
  protected val classpath: Vector[Path] =
    sys.props("bonsai.analysis.classpath").split(File.pathSeparator).toVector.map(Path.of(_))

  protected val classpaths: Map[String, Vector[Path]] = Map("m" -> classpath)

  protected val targetJdk: Option[Path] = Some(Path.of(sys.props("java.home")))

  /** Sources and resources are named by relative path; test sources see the main output. A module of `libraries`
    * publishes its main output for other builds to use, `dependencies` are compiled onto the classpath outside the
    * build, `absent` are compiled against but left off the analysed classpath, as a dependency a manifest misses is,
    * `concurrent` names methods that run the functions passed to them on other threads, and `twins` holds the sources
    * each platform twin, module `t0` and on, compiles beside the main ones, in place of those it names alike.
    */
  protected def extract(
      main: Seq[(String, String)],
      test: Seq[(String, String)] = Nil,
      resources: Seq[(String, String)] = Nil,
      libraries: Set[String] = Set.empty,
      dependencies: Seq[(String, String)] = Nil,
      absent: Seq[(String, String)] = Nil,
      concurrent: Set[String] = Set.empty,
      generated: Vector[String] = Vector.empty,
      reflective: Vector[String] = Vector.empty,
      twins: Seq[Seq[(String, String)]] = Nil,
      hot: Vector[String] = Vector.empty,
      bounded: Vector[Complexity.Bounded] = Vector.empty,
      removed: Seq[String] = Nil
  ): Extraction.Result =
    Scratch.using("bonsai-") { directory =>
      val external =
        Option.when(dependencies.nonEmpty)(compile(directory.resolve("dependencies"), dependencies, Vector.empty))
      val missing     = Option.when(absent.nonEmpty)(compile(directory.resolve("absent"), absent, Vector.empty))
      val mainClasses = compile(directory.resolve("main"), main, external.toVector ++ missing)
      write(mainClasses, resources)
      val testClasses = compile(directory.resolve("test"), test, mainClasses +: (external.toVector ++ missing))
      removed.foreach(name => Files.delete(directory.resolve("main").resolve(name)))
      def located(module: String, scope: Output.Scope, root: String, sources: Seq[(String, String)]) =
        sources.map((name, _) => Output(module, scope, directory.resolve(root).resolve(name))).toVector
      // A twin source named like a main one replaces it, as a source for one platform does.
      val platformClasspath = external.toVector ++ missing
      val mainByName        = main.toMap
      val platforms         = twins.toVector.zipWithIndex.map { (own, index) =>
        val module  = s"t$index"
        val shared  = (mainByName -- own.map(_._1)).toVector
        val classes = compile(directory.resolve(module), shared ++ own, platformClasspath)
        (
          Output(module, Output.Scope.Main, classes),
          located(module, Output.Scope.Main, "main", shared) ++ located(module, Output.Scope.Main, module, own)
        )
      }
      // The twins come first, as a build may list them, so the analysis must not depend on the order.
      val outputs = platforms.map(_._1) ++
        Vector(Output("m", Output.Scope.Main, mainClasses), Output("m", Output.Scope.Test, testClasses))
      val analysed = external.toVector ++ classpath
      val manifest = Manifest(
        outputs,
        outputs.map(_.module).distinct.map(_ -> analysed).toMap,
        libraries,
        sources = located("m", Output.Scope.Main, "main", main) ++ located("m", Output.Scope.Test, "test", test) ++
          platforms.flatMap(_._2),
        platforms = platforms.map(_._1.module).toSet,
        jdk = targetJdk
      )
      val result =
        Extraction.read(manifest, generated, concurrent, reflective, hot, bounded).fold(
          errors => fail(errors.mkString("\n")),
          identity
        )
      assert(result.problems.isEmpty, result.problems.mkString("\n"))
      result
    }

  /** Compiles Java sources under the package `demo`, and reads the model the javac plugin collects from them. */
  protected def javaFacts(libraries: Set[String], sources: (String, String)*): Extraction.Result =
    javaFactsReflective(libraries, Vector.empty, sources*)

  /** Compiles Java sources as `javaFacts` does, with the types the build names only in strings. */
  protected def javaFactsReflective(
      libraries: Set[String],
      reflective: Vector[String],
      sources: (String, String)*
  ): Extraction.Result =
    Scratch.using("bonsai-") { directory =>
      write(directory.resolve("src"), sources)
      val files   = sources.map((name, _) => directory.resolve("src").resolve(name))
      val classes = Files.createDirectories(directory.resolve("classes"))
      val model   = directory.resolve("java-model.tsv")
      JavaModel.write(javac(files, classes, classpath), model)
      val manifest = Manifest(
        Vector(Output("j", Output.Scope.Main, classes)),
        classpaths,
        libraries,
        models = Vector(Output("j", Output.Scope.Main, model)),
        jdk = targetJdk
      )
      Extraction.read(manifest, reflective = reflective).fold(errors => fail(errors.mkString("\n")), identity)
    }

  /** The model of the Java code of a build, read as the analysis reads it. */
  protected def readJavaModel(
      model: Vector[String],
      scope: Output.Scope = Output.Scope.Main,
      generated: Vector[String] = Vector.empty
  ): Extraction.Result =
    Scratch.using("bonsai-") { directory =>
      val file = directory.resolve("java-model.tsv")
      Files.writeString(file, model.mkString("", "\n", "\n")): Unit
      val manifest =
        Manifest(Vector.empty, classpaths, Set.empty, models = Vector(Output("j", scope, file)), jdk = targetJdk)
      Extraction.read(manifest, generated).fold(errors => fail(errors.mkString("\n")), identity)
    }

  /** An object `name` in a source of its own, which a main method prints. */
  protected def program(name: String, source: String): Seq[(String, String)] = Seq(
    "Main.scala"   -> s"object Main { def main(args: Array[String]): Unit = println($name) }",
    s"$name.scala" -> source
  )

  /** The pattern a build passes to mark `src/generated` as generated. */
  protected val GeneratedSources: Vector[String] = Vector("**/src/generated/**")

  protected def assertFinding(found: Vector[Finding], rule: String, fragments: String*)(using
      level: Finding.Level = Finding.Level.Warning
  ): Unit = {
    val containing = fragments.map(fragment => s"(?=.*${Pattern.quote(fragment)})").mkString
    val pattern    = Pattern.compile(containing, Pattern.DOTALL)
    assert(
      found.exists(finding =>
        finding.rule.id == rule && finding.level == level && pattern.matcher(finding.detail).find()
      ),
      s"no $level $rule finding containing ${fragments.mkString(", ")} in\n${found.mkString("\n")}"
    )
  }

  protected def assertNoFinding(found: Vector[Finding])(matches: Finding => Boolean): Unit =
    assert(!found.exists(matches), found.mkString("\n"))

  private class Analyzed(collector: Collector) extends TaskListener {
    override def finished(event: TaskEvent): Unit =
      if (event.getKind == TaskEvent.Kind.ANALYZE) collector.unit(event.getCompilationUnit, event.getTypeElement)
  }

  /** Compiles Java files into `classes` with the class path `path`, and returns the model the javac plugin collects. */
  protected def javac(files: Seq[Path], classes: Path, path: Seq[Path]): JavaModel.Model = {
    val compiler    = ToolProvider.getSystemJavaCompiler
    val diagnostics = java.util.ArrayList[String]()
    // scalafix:off DisableSyntax.null
    val manager = compiler.getStandardFileManager(null, null, null)
    manager.setLocation(StandardLocation.CLASS_OUTPUT, java.util.List.of(classes.toFile))
    manager.setLocation(StandardLocation.CLASS_PATH, path.map(_.toFile).asJava)
    val listener: DiagnosticListener[JavaFileObject] = diagnostic => diagnostics.add(diagnostic.getMessage(null)): Unit
    val task                                         = compiler.getTask(
      null,
      manager,
      listener,
      java.util.List.of("-proc:full"),
      null,
      manager.getJavaFileObjectsFromFiles(files.map(_.toFile).asJava)
    ).asInstanceOf[JavacTask]
    val collector = Collector(Trees.instance(task))
    task.addTaskListener(Analyzed(collector))
    // scalafix:on DisableSyntax.null
    assert(task.call(), s"the sources compile: ${diagnostics.asScala.mkString("\n")}")
    collector.model
  }

  protected def write(root: Path, files: Seq[(String, String)]): Unit = files.foreach { (name, content) =>
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, content): Unit
  }

  protected def compile(directory: Path, sources: Seq[(String, String)], extra: Vector[Path]): Path = {
    val output = Files.createDirectories(directory.resolve("classes"))
    write(directory, sources)
    val files     = sources.map((name, _) => directory.resolve(name).toString)
    val arguments =
      Array(
        "-classpath",
        (extra ++ classpath).mkString(File.pathSeparator),
        "-d",
        output.toString,
        "-color:never",
        "-Xmax-fuel",
        "100000"
      ) ++ files
    val reporting = Promise[Reporter]()
    val thread    = Thread(
      Thread.currentThread.getThreadGroup,
      () => reporting.complete(Try(Driver().process(arguments, StoreReporter()))): Unit,
      "bonsai-test-compile",
      1L << 30
    )
    thread.start()
    thread.join()
    val reporter = reporting.future.value.get.get
    assert(!reporter.hasErrors, reporter.allErrors.map(_.message).mkString("\n"))
    output
  }
}
