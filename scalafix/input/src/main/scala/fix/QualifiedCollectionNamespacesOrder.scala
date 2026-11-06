/*
rule = QualifiedCollectionNamespaces
QualifiedCollectionNamespaces.namespaces = [
  { package = "java.time.temporal", prefix = "temporal", subpackages = false },
  { package = "org.w3c.dom", prefix = "dom", subpackages = false }
]
 */
package fix

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration

import org.xml.sax.InputSource

object QualifiedCollectionNamespacesOrder {
  val unit = TimeUnit.SECONDS
  val timeout = Duration.Zero
  val source: InputSource = null
  val day = java.time.temporal.ChronoUnit.DAYS// assert: QualifiedCollectionNamespaces.unqualified
  val node: org.w3c.dom.Node = null// assert: QualifiedCollectionNamespaces.unqualified
}
