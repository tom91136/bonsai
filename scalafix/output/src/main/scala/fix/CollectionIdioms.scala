// format: off
package fix

object CollectionIdioms {
  val list = List(1, 2, 3)
  val map  = Map("a" -> 1)

  val array = Array(1, 2, 3)
  val text = "abc"
  val ap1 = println(array)

  val ok1 = list.contains(2)
  val ok2 = !list.contains(2)
  val ok3 = map.getOrElse("a", 0)
  val ok4 = list.slice(1, 3)
  val ok5 = list.drop(1)
  val ok6 = list.take(1)
  val ok7 = map.size
  val ok8 = array.length
  val ok9 = text.length
  val ok10 = println(text)

  val texts = List("a", "b")
  val literals = List(2L, 3L)
  val flags = List(true, false)
  val characters = List('a', 'b')
  val treeMap = scala.collection.immutable.TreeMap("a" -> 1)
  val hashMap = scala.collection.immutable.HashMap("b" -> 2)
  val range = 1 to 5
  val arraySeq = scala.collection.immutable.ArraySeq(1, 2)

  var evaluations = 0
  def next(): Int = { evaluations += 1; evaluations }
  val effects = list.exists(_ == next())
  val effectsEmpty = List.empty[Int].forall(_ != next())
  val capturesParameter = list.exists(x => x == x + 1)
  val reversed = list.exists(x => 2 == x)
  val variableTarget = list.exists(_ == evaluations)
  val nullTarget = List("x").exists(_ == null)
  val negativeStart = list.drop(-1).take(2)
  val negativeLength = list.drop(1).take(-1)
  val overflow = list.drop(1).take(2147483647)
  val dynamicStart = list.drop(next()).take(2)
  val dynamicLength = list.drop(1).take(next())
  val lazyValues = LazyList(1, 2, 3)
  val lazySlice = lazyValues.drop(1).take(2)
  val lazySearch = lazyValues.exists(_ == 2)
  val setSearch = Set(1, 2).exists(_ == 2)
  class ApplicationMethods {
    def exists(predicate: Int => Boolean): Boolean = predicate(1)
    def drop(n: Int): ApplicationMethods = this
    def take(n: Int): ApplicationMethods = this
    def get(key: String): Option[Int] = Some(1)
  }
  val custom = new ApplicationMethods
  val customSearch = custom.exists(_ == 2)
  val customSlice = custom.drop(1).take(2)
  val customLookup = custom.get("a").getOrElse(0)
}
