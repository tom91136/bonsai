/*
rule = FullyQualifiedReference
 */
package fix

import java.util.concurrent.atomic.AtomicLong

import scala.concurrent.duration.Duration

import org.w3c.dom.Node

object FullyQualifiedReferenceOrder {
  val path = java.nio.file.Paths.get("a")// assert: FullyQualifiedReference.fully-qualified-reference
  val instant = java.time.Instant.EPOCH// assert: FullyQualifiedReference.fully-qualified-reference
  val counter: AtomicLong = new AtomicLong(0L)
  val timeout = Duration.Zero
  val attempt: scala.util.Try[Int] = null// assert: FullyQualifiedReference.fully-qualified-reference
  val element: org.w3c.dom.Element = null// assert: FullyQualifiedReference.fully-qualified-reference
  val node: Node = null
}
