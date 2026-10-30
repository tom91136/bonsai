package net.kurobako.baumkuchen.java

import java.lang.module.ModuleFinder
import scala.jdk.CollectionConverters.*

/** The packages of the JDK, with the module that holds each and whether that module exports it, read from the module
  * graph of the running JDK.
  */
object JdkPackages {

  /** The module that holds each package of the JDK, and whether the module exports the package to every module. */
  private lazy val packages: Map[String, (String, Boolean)] =
    ModuleFinder.ofSystem.findAll.asScala.toVector.flatMap { reference =>
      val descriptor = reference.descriptor
      descriptor.packages.asScala.toVector.map { pkg =>
        val visible = descriptor.exports.asScala.exists(exported => !exported.isQualified && exported.source == pkg)
        pkg -> (descriptor.name -> visible)
      }
    }.toMap

  /** Why code may not use a package of the JDK, or None when the JDK exports it or does not hold it. */
  def unsupported(pkg: String): Option[String] =
    packages.get(pkg).flatMap { case (module, exported) =>
      if module == "jdk.unsupported" then Some(s"in the unsupported module $module")
      else if exported then None
      else Some(s"not exported by $module")
    }

  /** Why code may not use the type or member a dotted name names, decided by the longest package the name starts with,
    * or None.
    */
  def unsupportedName(name: String): Option[String] = {
    val prefixes = name.indices.filter(name(_) == '.').map(name.take).reverse.toList
    (name :: prefixes).find(packages.contains).flatMap(unsupported)
  }
}
