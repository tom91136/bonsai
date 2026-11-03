package dotty.tools.dotc.plugins

import java.util.concurrent.ConcurrentHashMap

/** The implementation loaders of the plugin in one compiler, a stamp and a loader for each plugin jar.
  *
  * The plugin defines this object in the loader of the compiler, which a compilation's own loader of the plugin cannot
  * outlive, and which only the objects of its own can reach. A loader links to the loader of its compiler, so a table
  * outside that loader would keep the compiler from being unloaded, and this one is unloaded with it. The table is of
  * the JDK alone, so that plugins of any version share it.
  */
object BonsaiLoaders extends ConcurrentHashMap[String, java.util.Map.Entry[String, ClassLoader]]
