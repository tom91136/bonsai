package net.kurobako.baumkuchen.plugin

import dotty.tools.dotc.util.SourcePosition
import scala.collection.mutable

/** The files of a compilation and their first references to each other, kept across the runs macro expansion resumes.
  */
private class Analysis {
  private val files = mutable.Set.empty[String]
  // (from file, to file, through macro expansion) -> first reference
  private val references = mutable.Map.empty[(String, String, Boolean), SourcePosition]

  def paths: collection.Set[String] = files

  def edges: collection.Map[(String, String, Boolean), SourcePosition] = references

  def clear(): Unit = {
    files.clear()
    references.clear()
  }

  def include(paths: Iterable[String]): Unit = files ++= paths: Unit

  /** Drops the references made from `paths`, which are about to be traversed again. */
  def forget(paths: Set[String]): Unit = references.filterInPlace((key, _) => !paths(key._1)): Unit

  def reference(from: String, to: String, macroCall: Boolean, position: => SourcePosition): Unit =
    references.getOrElseUpdate((from, to, macroCall), position): Unit
}
