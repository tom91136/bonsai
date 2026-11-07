package net.kurobako.bonsai.plugin

import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.plugins.StandardPlugin

/** The plugin the compiler instantiates, which hands its options to the `BonsaiPlugin` of the loader shared by every
  * compilation. It refers to nothing else of the plugin, so that loading it for a compilation costs one class.
  */
class BonsaiEntry extends StandardPlugin {
  private val implementation = Loaders
    .of(classOf[BonsaiEntry], classOf[StandardPlugin])
    .loadClass("net.kurobako.bonsai.plugin.BonsaiPlugin")
    .getDeclaredConstructor()
    .newInstance()
    .asInstanceOf[StandardPlugin]

  val name        = implementation.name
  val description = implementation.description

  // The plan of an interactive compiler has none of the phases those of the plugin run between, and scheduling around a
  // phase the plan lacks throws. The compilers before 3.5 give no context to tell an interactive one by.
  override def init(options: List[String]): List[PluginPhase] =
    if (interactive) Nil else implementation.init(options)

  private def interactive: Boolean = StackWalker.getInstance().walk { frames =>
    frames.anyMatch(_.getClassName == classOf[InteractiveDriver].getName)
  }
}
