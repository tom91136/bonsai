/*
rule = CollectionIdioms
 */
// format: off
package fix

import scala.collection.mutable

object EmptyCollection {
  val vector = Vector()// assert: CollectionIdioms.empty-collection
  val typed = List[Int]()// assert: CollectionIdioms.empty-collection
  val map = Map[String, Int]()// assert: CollectionIdioms.empty-collection
  val set = Set.empty[Int]
  val buffer = mutable.ArrayBuffer[Int]()// assert: CollectionIdioms.empty-collection
  val array = Array[Byte]()// assert: CollectionIdioms.empty-collection
  val full = Vector(1, 2)

  def matched(values: Seq[Int]): Boolean = values match {
    case Seq() => true
    case _     => false
  }

  object Bag { def apply(): Int = 0 }
  val own = Bag()
}
