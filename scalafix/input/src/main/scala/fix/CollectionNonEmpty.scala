/*
rule = CollectionIdioms
 */
// format: off
package fix

object CollectionNonEmpty {
  type Vec[A] = scala.collection.immutable.ArraySeq[A]
  def checks(list: List[Int], seq: Seq[Int], set: Set[Int], map: Map[Int, Int], vec: Vec[Int], array: Array[Int], text: String): Unit = {
    val a = list.size > 0// assert: CollectionIdioms.size-to-non-empty
    val b = seq.size > 0// assert: CollectionIdioms.size-to-non-empty
    val c = set.size > 0// assert: CollectionIdioms.size-to-non-empty
    val d = map.size > 0// assert: CollectionIdioms.size-to-non-empty
    val e = vec.size > 0// assert: CollectionIdioms.size-to-non-empty
    val f = array.size > 0// assert: CollectionIdioms.size-to-non-empty
    val custom = new Metric
    val g = custom.size > 0
    val h = text.size > 0
    val i = list.size > 1
    val j = list.size >= 0
    val k = list.nonEmpty
    val l = (if (a) list else Nil).size > 0// assert: CollectionIdioms.size-to-non-empty
  }
  class Metric { def size: Int = 3 }
}
