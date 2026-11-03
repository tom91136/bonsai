package net.kurobako.bonsai.java

import com.sun.source.util.JavacTask
import com.sun.source.util.TaskEvent
import com.sun.source.util.TaskListener
import com.sun.source.util.Trees
import java.nio.file.Files
import java.nio.file.Path
import javax.tools.Diagnostic
import javax.tools.DiagnosticListener
import javax.tools.JavaFileObject
import javax.tools.StandardJavaFileManager
import javax.tools.StandardLocation
import javax.tools.ToolProvider
import net.kurobako.bonsai.testing.*
import scala.jdk.CollectionConverters.*

object Fixtures {

  // scalafix:off DisableSyntax.null
  /** The argument for a javac parameter the code leaves out, which javac takes as null. */
  def omitted[A <: AnyRef]: A = null.asInstanceOf[A]
  // scalafix:on DisableSyntax.null

  /** Writes each named source as a Java file of the `demo` package under the root, and returns the files. */
  def write(root: Path, sources: Seq[(String, String)]): Seq[Path] = {
    val files = sources.map((name, _) => root.resolve(s"demo/$name.java"))
    files.zip(sources).foreach { case (file, (_, source)) =>
      Files.createDirectories(file.getParent)
      Files.writeString(file, source)
    }
    files
  }

  /** A file manager that writes the classes of a compilation under the directory. */
  def fileManager(classes: Path): StandardJavaFileManager = {
    val manager = ToolProvider.getSystemJavaCompiler.getStandardFileManager(omitted, omitted, omitted)
    manager.setLocation(StandardLocation.CLASS_OUTPUT, java.util.List.of(classes.toFile))
    manager
  }

  /** A listener that adds the message of each diagnostic to the messages. */
  def listener(messages: java.util.List[java.lang.String]): DiagnosticListener[JavaFileObject] =
    Messages(messages)

  private class Messages(messages: java.util.List[java.lang.String]) extends DiagnosticListener[JavaFileObject] {
    def report(diagnostic: Diagnostic[? <: JavaFileObject]): Unit =
      messages.add(diagnostic.getMessage(omitted)): Unit
  }

  private class Failures(collector: Collector, diagnostics: java.util.List[java.lang.String]) extends TaskListener {
    override def finished(event: TaskEvent): Unit =
      if (event.getKind == TaskEvent.Kind.ANALYZE)
        try collector.unit(event.getCompilationUnit, event.getTypeElement)
        catch {
          case error: Throwable =>
            println("COLLECTOR FAILED: " + error)
            diagnostics.add("the collector failed"): Unit
        }
  }

  /** Compiles the sources in a directory of its own, and returns the model the collector made of them. */
  def collect(sources: (String, String)*): Model.Model = {
    val root    = Scratch.directory("bonsai-java")
    val files   = Fixtures.write(root, sources)
    val classes = root.resolve("classes")
    Files.createDirectories(classes)

    val compiler    = ToolProvider.getSystemJavaCompiler
    val diagnostics = java.util.ArrayList[java.lang.String]()
    val fileManager = Fixtures.fileManager(classes)
    val units       = fileManager.getJavaFileObjectsFromFiles(files.map(_.toFile).asJava)
    val task        =
      compiler.getTask(
        omitted,
        fileManager,
        Fixtures.listener(diagnostics),
        java.util.List.of("-proc:full"),
        omitted,
        units
      ).asInstanceOf[JavacTask]

    val trees     = Trees.instance(task)
    val collector = Collector(trees)
    task.addTaskListener(Failures(collector, diagnostics))
    if (!task.call())
      throw AssertionError(
        s"the fixture compiles: ${diagnostics.asScala.mkString("\n")}\n${sources.map(_._2).mkString}"
      )
    collector.model
  }
}
