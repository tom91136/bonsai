// format: off
package fix

class RedundantCollectionConversion extends munit.FunSuite {
  val sequence: Seq[Int] = List(1, 2)
  val list = List(1, 2)
  val vector = Vector(1, 2)
  val identitySeq = sequence.toSeq// assert: redundant-collection-conversion
  val identityList = list.toList// assert: redundant-collection-conversion
  val identityVector = vector.toVector// assert: redundant-collection-conversion
  val indexed: IndexedSeq[Int] = Vector(1, 2)
  val identityIndexed = indexed.toIndexedSeq// assert: redundant-collection-conversion
  test("immutable sequence comparison") {
    assertEquals(list.toSeq, Seq(1, 2))// assert: redundant-collection-conversion
  }
  val buffer = scala.collection.mutable.ArrayBuffer(1, 2)
  val snapshot = buffer.toSeq
  val map: Map[String, Int] = Map("a" -> 1)
  val mapCopy = map.toMap// assert: redundant-collection-conversion
  val combined = (map ++ List("b" -> 2)).toMap// assert: redundant-collection-conversion
  val pairs = List("a" -> 1).toMap
  val set: Set[Int] = Set(1)
  val setCopy = set.toSet// assert: redundant-collection-conversion
  val fromList = List(1).toSet
  val arraySnapshot = Array(1, 2).toSeq
  val selectedArray = Vector(Array(1, 2))(0).toVector
  val filteredList = list.filter(_ > 0).toList// assert: redundant-collection-conversion
  val hashMap = scala.collection.immutable.HashMap("a" -> 1)
  val hashCopy = hashMap.toMap// assert: redundant-collection-conversion
  val filteredView = list.view.filter(_ > 0).toList
  val materialised = list.view.toSeq
  val conversion = list.toVector
  val widening = list.toSeq
  def choose(values: List[Int]): String = "list"
  def choose(values: Seq[Int]): String = "seq"
  val overload = choose(list.toSeq)
  class ApplicationMethods { def toSeq: List[Int] = List(1) }
  val custom = new ApplicationMethods
  val customConversion = custom.toSeq
}
