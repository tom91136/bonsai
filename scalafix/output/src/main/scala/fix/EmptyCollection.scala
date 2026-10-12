// format: off
package fix

import scala.collection.mutable

object EmptyCollection {
  val vector = Vector.empty
  val typed = List.empty[Int]
  val map = Map.empty[String, Int]
  val set = Set.empty[Int]
  val buffer = mutable.ArrayBuffer.empty[Int]
  val array = Array.empty[Byte]
  val full = Vector(1, 2)

  def matched(values: Seq[Int]): Boolean = values match {
    case Seq() => true
    case _     => false
  }

  object Bag { def apply(): Int = 0 }
  val own = Bag()
}
