package fix

import java.util.concurrent.TimeUnit

object FullyQualifiedReferenceNoImport {
  val unit = TimeUnit.SECONDS
  val max = java.lang.Integer.MAX_VALUE
  val math = java.lang.Math.max(1, 2)
  val bits = java.lang.Long.highestOneBit(8L)
  val nan = java.lang.Double.isNaN(1.0)
  val builder = new java.lang.StringBuilder()
}
