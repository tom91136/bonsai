package net.kurobako.bonsai.java

import com.sun.source.util.JavacTask
import com.sun.source.util.Plugin
import com.sun.source.util.TaskEvent
import com.sun.source.util.TaskListener
import com.sun.source.util.Trees
import java.nio.file.Path

/** The javac plugin that collects the model of the Java code the compiler compiles, which the analysis reads.
  *
  * The plugin runs inside the compiler, so the trees it walks are the ones the compiler attributed, with the types
  * resolved and the positions of the source. The model is written to the file the build names.
  */
class JavaPlugin extends Plugin {

  override def getName: String = "bonsai"

  // The plugin is named as an argument of the compiler, which names the file of its model beside it. A plugin that
  // starts on its own would read a property of the process, which the modules of a parallel build share, and the
  // compile of one of them would write the model of another.
  override def autoStart: Boolean = false

  override def init(task: JavacTask, args: String*): Unit = {
    val trees     = Trees.instance(task)
    val collector = Collector(trees)
    // The linter is of the whole compilation, as the lints of the file as a whole are asked once, and the compiler
    // reads the file one type at a time.
    val linter =
      Linter(trees, task.getElements, task.getTypes, args.drop(1).view.flatMap(_.split(",")).filter(_.nonEmpty).toSet)
    // The build names the file of the model as an argument of the compiler, which the compile of a module names
    // without the modules around it; the property of the process is the older way of naming it, which a build that
    // passes no argument keeps.
    val output = args.headOption
      .orElse(sys.props.get(JavaPlugin.Output))
      .map(Path.of(_))
    task.addTaskListener(new TaskListener {
      override def finished(event: TaskEvent): Unit =
        if (event.getKind == TaskEvent.Kind.ANALYZE) {
          val compilation = event.getCompilationUnit
          val typeElement = event.getTypeElement
          // The module and package descriptors are not a type, so the walk of a type has nothing to read.
          if (typeElement != null) {
            collector.unit(compilation, typeElement)
            collector.lint(linter.attributed(compilation, typeElement))
          }
          collector.lint(linter.file(compilation))
        } else if (event.getKind == TaskEvent.Kind.COMPILATION) {
          collector.lint(linter.finished())
          output.foreach(file => Model.write(collector.model, file))
        }
    })
  }
}

object JavaPlugin {

  /** The property the file the model is written to is named by, which the build sets. */
  val Output: String = "bonsai.model"
}
