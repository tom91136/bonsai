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
  val rooted: _root_.scala.collection.mutable.HashMap[Int, Int] = scala.collection.mutable.HashMap.empty
}
