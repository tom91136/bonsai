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
import net.kurobako.bonsai.testing.Scratch
import scala.jdk.CollectionConverters.*

object Fixtures {

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
    val manager = ToolProvider.getSystemJavaCompiler.getStandardFileManager(null, null, null)
    manager.setLocation(StandardLocation.CLASS_OUTPUT, java.util.List.of(classes.toFile))
    manager
  }

  /** A listener that adds the message of each diagnostic to the messages. */
  def listener(messages: java.util.List[java.lang.String]): DiagnosticListener[JavaFileObject] =
    new DiagnosticListener[JavaFileObject] {
      def report(diagnostic: Diagnostic[? <: JavaFileObject]): Unit =
        messages.add(diagnostic.getMessage(null)): Unit
    }

  /** Compiles the sources in a directory of its own, and returns the model the collector made of them. */
  def collect(sources: (String, String)*): Model.Model = {
    val root    = Scratch.directory("bonsai-java")
    val files   = Fixtures.write(root, sources)
    val classes = root.resolve("classes")
    Files.createDirectories(classes)

    val compiler    = ToolProvider.getSystemJavaCompiler
    val diagnostics = new java.util.ArrayList[java.lang.String]()
    val fileManager = Fixtures.fileManager(classes)
    val units       = fileManager.getJavaFileObjectsFromFiles(files.map(_.toFile).asJava)
    val task        =
      compiler.getTask(
        null,
        fileManager,
        Fixtures.listener(diagnostics),
        java.util.List.of("-proc:full"),
        null,
        units
      ).asInstanceOf[JavacTask]

    val trees     = Trees.instance(task)
    val collector = Collector(trees)
    task.addTaskListener(new TaskListener {
      override def finished(event: TaskEvent): Unit =
        if event.getKind == TaskEvent.Kind.ANALYZE then
          try collector.unit(event.getCompilationUnit, event.getTypeElement)
          catch {
            case error: Throwable =>
              println("COLLECTOR FAILED: " + error)
              diagnostics.add("the collector failed"): Unit
          }
    })
    if !task.call() then
      throw AssertionError(
        s"the fixture compiles: ${diagnostics.asScala.mkString("\n")}\n${sources.map(_._2).mkString}"
      )
    collector.model
  }
}
