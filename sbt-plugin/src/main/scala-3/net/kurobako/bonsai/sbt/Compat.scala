package net.kurobako.bonsai.sbt

import sbt.*
import sbt.Keys.*
import sbt.internal.inc.Analysis
import sbt.internal.util.MessageOnlyException

private[sbt] object Compat {
  inline def uncached[A](inline a: A): A = Def.uncached(a)

  def fail(message: String): Nothing = throw MessageOnlyException(message)

  def dependencyPaths: Def.Initialize[Task[Seq[String]]] = Def.task {
    val converter = fileConverter.value
    (externalDependencyClasspath.value ++ internalDependencyClasspath.value).map(entry =>
      converter.toPath(entry.data).toString
    )
  }

  def compiled: Def.Initialize[Task[(Seq[String], Seq[String])]] = Def.task {
    val converter = fileConverter.value
    val relations = compile.value.asInstanceOf[Analysis].relations
    (
      relations.allSources.toSeq.map(entry => converter.toPath(entry).toString).sorted,
      relations.allProducts.toSeq.map(entry => converter.toPath(entry).toString).sorted
    )
  }
}
