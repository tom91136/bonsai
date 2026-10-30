/*
rule = FullyQualifiedReference
QualifiedCollectionNamespaces.namespaces = [
  { package = "java.util", prefix = "java.util", subpackages = false }
  { package = "scala.collection", prefix = "collection", subpackages = "false" }
]
 */
package fix

object FullyQualifiedReferenceSubpackages {
  val bits: java.util.BitSet = new java.util.BitSet(8)
  val counter = new java.util.concurrent.atomic.AtomicLong(0L)// assert: FullyQualifiedReference.fully-qualified-reference
  val buffer = scala.collection.mutable.ListBuffer.empty[Int]// assert: FullyQualifiedReference.fully-qualified-reference
}
