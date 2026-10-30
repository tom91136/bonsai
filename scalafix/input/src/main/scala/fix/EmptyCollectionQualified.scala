/*
rule = CollectionIdioms
 */
package fix

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

object EmptyCollectionQualified {
  val plain = ArrayBuffer()// assert: CollectionIdioms.empty-collection
  val typed = ListBuffer[Int]()// assert: CollectionIdioms.empty-collection
}
