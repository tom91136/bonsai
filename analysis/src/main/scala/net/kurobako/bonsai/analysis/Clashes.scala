package net.kurobako.bonsai.analysis

import java.nio.file.Path
import scala.jdk.CollectionConverters.*

/** Names the compiler cannot hold together: it enters every top-level class beside an object of its name, and refuses a
  * package of that name in the same package, so a build where one output has the class and another the package is
  * unreadable in a single run.
  */
private[analysis] object Clashes {

  private case class Holding(classes: Set[String], packages: Set[String])

  private def holding(root: Path, files: Vector[Path]): Holding = {
    val parts = files.map(root.relativize(_)).filter { file =>
      val name = file.getFileName.toString
      name.endsWith(".class") || name.endsWith(".tasty")
    }.map(_.asScala.map(_.toString).toVector)
    val packages = parts.flatMap(_.init.scanLeft(Vector.empty[String])(_ :+ _).tail).map(_.mkString(".")).toSet
    val classes  = parts.flatMap { path =>
      val name = path.last.takeWhile(_ != '.').stripSuffix("$")
      Option.when(name.nonEmpty)((path.init :+ name).mkString("."))
    }.toSet
    Holding(classes, packages)
  }

  def of(outputs: Vector[(Output, Vector[Path])]): Vector[String] = {
    val held       = outputs.map((output, files) => output.module -> holding(output.path, files))
    val packagedIn = held.flatMap((module, holds) => holds.packages.map(_ -> module)).groupMap(_._1)(_._2)
    (for {
      (classed, one) <- held
      name           <- one.classes.toVector.sorted
      packaged       <- packagedIn.getOrElse(name, Vector.empty)
    } yield (name, classed, packaged)).distinct.map { (name, classed, packaged) =>
      s"$name is a class in module $classed and a package in module $packaged, which the compiler cannot read " +
        "together: it refuses a package and an object of one name on a classpath, and the analysis reads the classes " +
        "of every module in one compiler run"
    }
  }
}
