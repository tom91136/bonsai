// format: off
package fix

object FilterOtherContains {
  val other = Set("a", "b")

  val filtered = Set("a", "c").filter(other.contains)// assert: filter-contains-to-intersect
  val named = Set("a").filter(value => other.contains(value))// assert: filter-contains-to-intersect

  val list = List("a").filter(other.contains)
  val unrelated = Set("a").filter(_ == "a")
}
