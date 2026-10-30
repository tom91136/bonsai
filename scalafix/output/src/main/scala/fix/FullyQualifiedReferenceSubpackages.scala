package fix

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable.ListBuffer

object FullyQualifiedReferenceSubpackages {
  val bits: java.util.BitSet = new java.util.BitSet(8)
  val counter = new AtomicLong(0L)
  val buffer = ListBuffer.empty[Int]
}
