/*
rule = FullyQualifiedReference
 */
package fix

object FullyQualifiedReferenceNoImport {
  val unit = java.util.concurrent.TimeUnit.SECONDS// assert: FullyQualifiedReference.fully-qualified-reference
  val max = java.lang.Integer.MAX_VALUE
  val math = java.lang.Math.max(1, 2)
  val bits = java.lang.Long.highestOneBit(8L)
  val nan = java.lang.Double.isNaN(1.0)
  val builder = new java.lang.StringBuilder()
}
