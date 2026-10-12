// format: off
package fix

object CollectionNonEmpty {
  type Vec[A] = scala.collection.immutable.ArraySeq[A]
  def checks(list: List[Int], seq: Seq[Int], set: Set[Int], map: Map[Int, Int], vec: Vec[Int], array: Array[Int], text: String): Unit = {
    val a = list.nonEmpty
    val b = seq.nonEmpty
    val c = set.nonEmpty
    val d = map.nonEmpty
    val e = vec.nonEmpty
    val f = array.nonEmpty
    val custom = new Metric
    val g = custom.size > 0
    val h = text.size > 0
    val i = list.size > 1
    val j = list.size >= 0
    val k = list.nonEmpty
    val l = (if (a) list else Nil).nonEmpty
  }
  class Metric { def size: Int = 3 }
}
