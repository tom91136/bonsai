package net.kurobako.bonsai.analysis

import dotty.tools.dotc.core.tasty.AttributeUnpickler
import dotty.tools.dotc.core.tasty.Attributes
import dotty.tools.dotc.core.tasty.TastyUnpickler
import dotty.tools.tasty.TastyReader
import java.nio.file.Files
import java.nio.file.Path
import scala.collection.mutable
import scala.util.Try

/** The classes of platform twins that replace those of the JVM modules. */
private[analysis] object Twins {

  /** The TASTy files of each platform twin that replace a class of the JVM modules, which the twin compiled from a
    * source of its own: one no source of the JVM modules is. The TASTy records the source it came from.
    */
  def replacing(manifest: Manifest, copies: Vector[(Output, Path)]): Vector[(String, Vector[Path])] = {
    val primary =
      manifest.sources.view.collect { case source if !manifest.platforms(source.module) => source.path }.toVector
    val own                           = mutable.Map.empty[String, Boolean]
    def replaced(file: Path): Boolean = sourceFile(file).exists { source =>
      val relative = Path.of(source)
      own.getOrElseUpdate(source, !primary.exists(_.endsWith(relative)))
    }
    copies.filter((_, file) => replaced(file)).groupMap(_._1.module)(_._2).toVector
  }

  private def sourceFile(tasty: Path): Option[String] =
    Try(TastyUnpickler(Files.readAllBytes(tasty)).unpickle(Attribution)).toOption.flatten.flatMap(_.sourceFile)

  private object Attribution extends TastyUnpickler.SectionUnpickler[Attributes]("Attributes") {
    def unpickle(reader: TastyReader, names: TastyUnpickler.NameTable): Attributes =
      AttributeUnpickler(reader, names).attributes
  }
}
