package fix

import scala.collection.mutable
import java.util.concurrent.atomic.AtomicInteger

object QualifiedCollectionNamespaces {
  val pages: mutable.LongMap[Int] = mutable.LongMap.empty[Int]
  val words: java.util.BitSet = java.util.BitSet(8)
  val queue: mutable.ArrayDeque[Int] = mutable.ArrayDeque.empty[Int]
  val counter = AtomicInteger(0)
  val buffer  = mutable.ArrayBuffer.empty[Int]
  val listed: java.util.ArrayList[String] = java.util.ArrayList[String]()
  val rooted: mutable.HashMap[Int, Int] = mutable.HashMap.empty
  val spelled = mutable.HashMap.empty[Int, Int]
  val inferred: mutable.Set[Int] = mutable.Set.empty[Int]
  def returned: mutable.Set[Int] = mutable.Set.empty[Int]
  val nested: List[mutable.Set[Int]] = Nil
}
