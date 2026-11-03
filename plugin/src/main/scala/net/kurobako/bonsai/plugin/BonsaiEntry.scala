package net.kurobako.bonsai.plugin

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

  override def init(options: List[String]): List[PluginPhase] = implementation.init(options)
}
