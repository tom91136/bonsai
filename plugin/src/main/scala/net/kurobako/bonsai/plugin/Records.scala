package net.kurobako.bonsai.plugin

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.report
import dotty.tools.dotc.util.SourceFile
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.concurrent.atomic.AtomicReference
import net.kurobako.bonsai.collections.*
import scala.collection.mutable

/** The findings of the sources a compilation holds, which the audit mode writes beside the class files. A source
  * compiled again has its record replaced, and a source the compilation leaves alone keeps the one it has, as it keeps
  * its class files. Macro expansion resumes a compilation in further runs, which add to the findings of the first.
  */
private class Records {
  private val current = AtomicReference[Option[AnyRef]](None)
  private val found   = mutable.Map.empty[SourceFile, List[Recorded.Entry]]

  private def begin()(using Context): Unit = {
    val run = Option(ctx.run)
    if (current.getAndSet(run) != run) found.clear()
  }

  def add(source: SourceFile, entry: Recorded.Entry)(using Context): Unit = {
    begin()
    found.update(source, entry :: found.getOrElse(source, Nil))
  }

  def write(units: List[CompilationUnit])(using Context): Unit = {
    begin()
    output.fold(report.error(
      "bonsai: the audit mode records its findings in the class directory, which this compilation has none of"
    )) {
      directory =>
        val root = Paths.get(ctx.settings.sourceroot.value.toString)
        (units.map(_.source) ++ found.keys).distinct.filter(_.exists).foreach { source =>
          val path    = Paths.get(source.path)
          val content =
            if (Files.isRegularFile(path)) Files.readAllBytes(path) else String(source.content).getBytes(UTF_8)
          val entries = found.getOrElse(source, Nil).sortBy(entry => (entry.line, entry.rule, entry.message))
          Recorded.write(directory, Recorded.Record(Recorded.key(root, path), Recorded.digest(content), entries))
        }
    }
  }

  private def output(using Context): Option[Path] =
    Option(ctx.settings.outputDir.value.file).filter(_.isDirectory).map(_.toPath)
}
