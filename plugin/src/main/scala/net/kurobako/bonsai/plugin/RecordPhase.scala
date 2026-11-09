package net.kurobako.bonsai.plugin

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.plugins.PluginPhase

private class RecordPhase(records: Records) extends PluginPhase {
  val phaseName: String                 = "bonsai-record"
  override val runsAfter: Set[String]   = Set("bonsai-lint", "acyclic")
  override val runsBefore: Set[String]  = Set("pickler")
  override def run(using Context): Unit = ()

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
    records.write(units.filterNot(_.isJava))
    units
  }
}
