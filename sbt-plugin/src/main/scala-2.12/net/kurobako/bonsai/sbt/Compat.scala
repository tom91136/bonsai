package net.kurobako.bonsai.sbt

import sbt._
import sbt.Keys._
import sbt.internal.util.MessageOnlyException

private[sbt] object Compat {
  def uncached[A](a: A): A = a

  def fail(message: String): Nothing = throw new MessageOnlyException(message)

  def dependencyPaths: Def.Initialize[Task[Seq[String]]] = Def.task {
    (externalDependencyClasspath.value ++ internalDependencyClasspath.value).map(_.data.toString)
  }
}
