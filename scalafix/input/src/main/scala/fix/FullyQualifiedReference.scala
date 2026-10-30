/*
rule = FullyQualifiedReference
 */
package fix

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

object FullyQualifiedReference {
  val timeout: scala.concurrent.duration.FiniteDuration = Duration.Zero// assert: FullyQualifiedReference.fully-qualified-reference
  val unit = java.util.concurrent.TimeUnit.MINUTES// assert: FullyQualifiedReference.fully-qualified-reference
  val counter = new java.util.concurrent.atomic.AtomicLong(0L)// assert: FullyQualifiedReference.fully-qualified-reference
  val buffer = mutable.ArrayBuffer.empty[Int]
  val bits: java.util.BitSet = new java.util.BitSet(8)
  val full: scala.collection.mutable.ListBuffer[Int] = mutable.ListBuffer.empty[Int]
  val rooted: _root_.scala.concurrent.duration.FiniteDuration = Duration.Zero
  val range: scala.Range = 1 to 3
  val shared: FiniteDuration = Duration.Zero
  val long: AtomicLong = new AtomicLong(1L)
}
