// format: off
package fix

class RedundantCollectionConversion extends munit.FunSuite {
  val sequence: Seq[Int] = List(1, 2)
  val list = List(1, 2)
  val vector = Vector(1, 2)
  val identitySeq = sequence
  val identityList = list
  val identityVector = vector
  test("immutable sequence comparison") {
    assertEquals(list, Seq(1, 2))
  }
  val buffer = scala.collection.mutable.ArrayBuffer(1, 2)
  val snapshot = buffer.toSeq
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
