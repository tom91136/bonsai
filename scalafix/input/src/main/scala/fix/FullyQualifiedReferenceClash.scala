/*
rule = FullyQualifiedReference
 */
package fix

import scala.concurrent.duration.Duration

object FullyQualifiedReferenceClash {
  final class FiniteDuration(val length: Long)
  val timeout: scala.concurrent.duration.FiniteDuration = Duration.Zero
  val local = new FiniteDuration(1L)
  val unit = java.util.concurrent.TimeUnit.SECONDS// assert: FullyQualifiedReference.fully-qualified-reference
}
