package net.kurobako.bonsai.plugin

import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ContextBase
import dotty.tools.dotc.transform.MegaPhase
import java.lang.ref.WeakReference
import java.util.concurrent.atomic.AtomicReference
import java.util.function.Function
import scala.jdk.CollectionConverters.*

object CompileProbe {

  /** A driver that hands out the base of the context it compiles in. */
  class Recording(created: AtomicReference[ContextBase]) extends Driver {
    override protected def initCtx: Context = {
      val base = ContextBase()
      created.set(base)
      base.initialCtx
    }
  }
}

/** Compiles in a loader that has the compiler but none of the plugin, so the plugin loads as it does for a build tool.
  * Its arguments and results are of the JDK alone, since the loader defines every other class it touches.
  */
class CompileProbe extends Function[Array[String], java.util.Map[String, AnyRef]] {
  def apply(args: Array[String]): java.util.Map[String, AnyRef] = {
    val created  = AtomicReference[ContextBase]()
    val reporter = CompileProbe.Recording(created).process(args)
    val base     = created.get
    val phases   = base.allPhases.toList.flatMap {
      case fused: MegaPhase => fused :: fused.miniPhases.toList
      case other            => List(other)
    }
    val loaders = phases
      .filter(_.getClass.getName.startsWith("net.kurobako.bonsai.plugin."))
      .map(_.getClass.getClassLoader)
      .foldLeft(List.empty[ClassLoader])((seen, loader) => if (seen.exists(_ eq loader)) seen else loader :: seen)
    java.util.Map.of(
      "diagnostics",
      (reporter.allErrors.map("error: " + _.message) ++ reporter.allWarnings.map("warning: " + _.message)).sorted
        .mkString("\n"),
      "loaders",
      java.util.ArrayList(loaders.asJava),
      "base",
      WeakReference[AnyRef](base)
    )
  }
}
