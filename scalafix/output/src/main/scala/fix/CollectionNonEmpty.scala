// format: off
package fix

object CollectionNonEmpty {
  type Vec[A] = scala.collection.immutable.ArraySeq[A]
  def checks(list: List[Int], seq: Seq[Int], set: Set[Int], map: Map[Int, Int], vec: Vec[Int], array: Array[Int], text: String): Unit = {
    val custom = new Metric
    val g = custom.size > 0
    val h = text.size > 0
    val i = list.size > 1
    val j = list.size >= 0
    val k = list.nonEmpty
    val q = text.length == 0
  }
  class Metric { def size: Int = 3 }
}
