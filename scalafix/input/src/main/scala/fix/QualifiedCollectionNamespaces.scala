/*
rule = QualifiedCollectionNamespaces
 */
package fix

import scala.collection.mutable
import scala.collection.mutable.LongMap// assert: QualifiedCollectionNamespaces.member-import
import scala.collection.mutable.{ArrayDeque => Deque}// assert: QualifiedCollectionNamespaces.member-import
import java.util.BitSet// assert: QualifiedCollectionNamespaces.member-import
import java.util.concurrent.atomic.AtomicInteger

object QualifiedCollectionNamespaces {
  val pages: LongMap[Int] = mutable.LongMap.empty[Int]// assert: QualifiedCollectionNamespaces.unqualified
  val words: java.util.BitSet = BitSet(8)// assert: QualifiedCollectionNamespaces.unqualified
  val queue: Deque[Int] = mutable.ArrayDeque.empty[Int]// assert: QualifiedCollectionNamespaces.unqualified
  val counter = AtomicInteger(0)
  val buffer  = mutable.ArrayBuffer.empty[Int]
  val listed: java.util.ArrayList[String] = java.util.ArrayList[String]()
  val rooted: _root_.scala.collection.mutable.HashMap[Int, Int] = mutable.HashMap.empty// assert: QualifiedCollectionNamespaces.unqualified
  val spelled = scala.collection.mutable.HashMap.empty[Int, Int]// assert: QualifiedCollectionNamespaces.unqualified
  val inferred: scala.collection.mutable.Set[Int] = mutable.Set.empty[Int]// assert: QualifiedCollectionNamespaces.unqualified
  def returned: scala.collection.mutable.Set[Int] = mutable.Set.empty[Int]// assert: QualifiedCollectionNamespaces.unqualified
  val nested: List[scala.collection.mutable.Set[Int]] = Nil// assert: QualifiedCollectionNamespaces.unqualified
}
