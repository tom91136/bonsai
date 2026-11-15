package net.kurobako.bonsai.testing

import java.lang.management.ManagementFactory
import javax.management.ObjectName

object Heap {

  private val diagnostics = ObjectName("com.sun.management:type=DiagnosticCommand")

  /** The number of reachable instances of the class named `name`, counted after a full collection. */
  def live(name: String): Int = {
    val histogram = ManagementFactory.getPlatformMBeanServer.invoke(
      diagnostics,
      "gcClassHistogram",
      Array[AnyRef](Array.empty[String]),
      Array(classOf[Array[String]].getName)
    ).toString
    histogram.linesIterator.map(_.trim.split("\\s+")).collectFirst {
      case Array(_, count, _, `name`) => count.stripSuffix(":").toInt
    }.getOrElse(0)
  }
}
