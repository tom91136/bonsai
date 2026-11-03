package net.kurobako.bonsai.plugin

import java.lang.module.ModuleFinder
import scala.jdk.CollectionConverters.*

/** The packages of the JDK, with the module that holds each and whether that module exports it, read from the module
  * graph of the running JDK. The plugin jar does not carry the Java plugin, so the Java plugin's copy is not shared.
  */
private object JdkPackages {

  private lazy val packages: Map[String, (String, Boolean)] =
    ModuleFinder.ofSystem.findAll.asScala.toVector.flatMap { reference =>
      val descriptor = reference.descriptor
      descriptor.packages.asScala.toVector.map { pkg =>
        val visible = descriptor.exports.asScala.exists(exported => !exported.isQualified && exported.source == pkg)
        pkg -> (descriptor.name -> visible)
      }
    }.toMap

  def unsupported(pkg: String): Option[String] =
    packages.get(pkg).flatMap { case (module, exported) =>
      if (module == "jdk.unsupported") Some(s"in the unsupported module $module")
      else if (exported) None
      else Some(s"not exported by $module")
    }

  def unsupportedName(name: String): Option[String] = {
    val prefixes = name.indices.view.filter(name(_) == '.').map(name.take).toList.reverse
    (name :: prefixes).find(packages.contains).flatMap(unsupported(_))
  }
}
