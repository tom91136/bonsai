package net.kurobako.bonsai.sbt

import sbt._
import sbt.Keys._
import sbt.internal.inc.Analysis
import sbt.internal.util.MessageOnlyException

private[sbt] object Compat {
  def uncached[A](a: A): A = a

  def fail(message: String): Nothing = throw new MessageOnlyException(message)

  def dependencyPaths: Def.Initialize[Task[Seq[String]]] = Def.task {
    dependencyClasspath.value.map(_.data.toString)
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
