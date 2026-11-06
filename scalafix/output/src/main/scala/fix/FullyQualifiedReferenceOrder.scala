package fix

import java.nio.file.Paths
import java.time.Instant
import java.util.concurrent.atomic.AtomicLong

import scala.concurrent.duration.Duration
import scala.util.Try

import org.w3c.dom.Element
import org.w3c.dom.Node

object FullyQualifiedReferenceOrder {
  val path = Paths.get("a")
  val instant = Instant.EPOCH
  val counter: AtomicLong = new AtomicLong(0L)
  val timeout = Duration.Zero
  val attempt: Try[Int] = null
  val element: Element = null
  val node: Node = null
}
