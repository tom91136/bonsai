/*
rule = CollectionIdioms
 */
// format: off
package fix

class RedundantCollectionConversion extends munit.FunSuite {
  val sequence: Seq[Int] = List(1, 2)
  val list = List(1, 2)
  val vector = Vector(1, 2)
  val identitySeq = sequence.toSeq// assert: CollectionIdioms.redundant-collection-conversion
  val identityList = list.toList// assert: CollectionIdioms.redundant-collection-conversion
  val identityVector = vector.toVector// assert: CollectionIdioms.redundant-collection-conversion
  test("immutable sequence comparison") {
    assertEquals(list.toSeq, Seq(1, 2))// assert: CollectionIdioms.redundant-collection-conversion
  }
  val buffer = scala.collection.mutable.ArrayBuffer(1, 2)
  val snapshot = buffer.toSeq
  val map: Map[String, Int] = Map("a" -> 1)
  val mapCopy = map.toMap// assert: CollectionIdioms.redundant-collection-conversion
  val combined = (map ++ List("b" -> 2)).toMap// assert: CollectionIdioms.redundant-collection-conversion
  val pairs = List("a" -> 1).toMap
  val set: Set[Int] = Set(1)
  val setCopy = set.toSet// assert: CollectionIdioms.redundant-collection-conversion
  val fromList = List(1).toSet
  val arraySnapshot = Array(1, 2).toSeq
  val selectedArray = Vector(Array(1, 2))(0).toVector
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
