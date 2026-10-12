/*
rule = MutableExposure
 */
package fix

import scala.collection.mutable

object MutableExposure {
  var counter             = 0                              // assert: MutableExposure.global-state
  val registry            = mutable.Map.empty[String, Int] // assert: MutableExposure.global-state
  val names: List[String] = Nil
  private val factorials  = Array[Double](1, 1, 2, 6, 24)

  class Account {
    var balance: Long                      = 0                         // assert: MutableExposure.public-var
    val history: mutable.ArrayBuffer[Long] = mutable.ArrayBuffer.empty // assert: MutableExposure.exposed-mutable
    def pending: mutable.Queue[Long]       = mutable.Queue.empty       // assert: MutableExposure.exposed-mutable
    private var cursor                     = 0
    private val buffer                     = mutable.ArrayBuffer.empty[Long]
    def snapshot: Vector[Long]             = buffer.toVector
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
