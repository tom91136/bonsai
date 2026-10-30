/*
rule = CollectionIdioms
 */
// format: off
package fix

object FilterOtherContains {
  val other = Set("a", "b")

  val filtered = Set("a", "c").filter(other.contains)// assert: CollectionIdioms.filter-contains-to-intersect
  val named = Set("a").filter(value => other.contains(value))// assert: CollectionIdioms.filter-contains-to-intersect

  val list = List("a").filter(other.contains)
  val unrelated = Set("a").filter(_ == "a")
}
