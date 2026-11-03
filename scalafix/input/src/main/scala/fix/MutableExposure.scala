/*
rule = MutableExposure
 */
package fix

import scala.collection.mutable

object MutableExposure {
  var counter             = 0
  val registry            = mutable.Map.empty[String, Int] // assert: MutableExposure.global-state
  val names: List[String] = Nil
  private val factorials  = Array[Double](1, 1, 2, 6, 24)

  class Account {
    var balance: Long                      = 0
    val history: mutable.ArrayBuffer[Long] = mutable.ArrayBuffer.empty // assert: MutableExposure.exposed-mutable
    def pending: mutable.Queue[Long]       = mutable.Queue.empty
    val inferred                           = mutable.Map.empty[String, String] // assert: MutableExposure.exposed-mutable
    val legacy                             = new java.util.ArrayList[Int]()    // assert: MutableExposure.exposed-mutable
    private var cursor                     = 0
    private val buffer                     = mutable.ArrayBuffer.empty[Long]
    def snapshot: Vector[Long]             = buffer.toVector

    def historyView: mutable.ArrayBuffer[Long] = history // assert: MutableExposure.exposed-mutable
  }

  trait Cache {
    var hits: Int                       = 0
    val store: mutable.Map[String, Int] = mutable.Map.empty // assert: MutableExposure.exposed-mutable
  }

  class Table {
    private val cells   = Array.ofDim[Long](4)
    private val builder = mutable.ArrayBuilder.make[Long]

    def fresh: Array[Long]      = Array.ofDim[Long](4)
    def copy: Array[Long]       = cells.clone()
    def copied: Array[Long]     = java.util.Arrays.copyOf(cells, 4)
    def toArr: Array[Long]      = cells.toArray
    def viaBuilder: Array[Long] = builder.result()
    def cellsView: Array[Long]  = cells // assert: MutableExposure.exposed-mutable

    def built: Array[Long] = {
      val result = Array.ofDim[Long](4)
      result
    }
  }

  private class Hidden {
    val history: mutable.ArrayBuffer[Long] = mutable.ArrayBuffer.empty
    var count: Int = 0
  }

  object ArchiveCodec {
    private[ArchiveCodec] class Scoped { var state: Int = 0 }
  }

  private object Internal {
    class Nested { var state: Int = 0 }
  }

  def local(): Int = {
    class Counter { var count: Int = 0 }
    Counter().count
  }
}
