package fix

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

object EmptyCollectionQualified {
  val plain = ArrayBuffer.empty
  val typed = ListBuffer.empty[Int]
}
