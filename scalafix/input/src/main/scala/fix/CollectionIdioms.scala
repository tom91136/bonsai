/*
rule = CollectionIdioms
 */
// format: off
package fix

object CollectionIdioms {
  val list = List(1, 2, 3)
  val map  = Map("a" -> 1)

  val ex1 = list.exists(_ == 2)// assert: CollectionIdioms.exists-to-contains
  val ex2 = list.exists(x => x == 2)// assert: CollectionIdioms.exists-to-contains
  val fa1 = list.forall(_ != 2)// assert: CollectionIdioms.forall-to-not-contains
  val fa2 = list.forall(x => x != 2)// assert: CollectionIdioms.forall-to-not-contains
  val gg1 = map.get("a").getOrElse(0)// assert: CollectionIdioms.get-get-or-else
  val dt1 = list.drop(1).take(2)// assert: CollectionIdioms.drop-take-to-slice

  val ok1 = list.contains(2)
  val ok2 = !list.contains(2)
  val ok3 = map.getOrElse("a", 0)
  val ok4 = list.slice(1, 3)

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
