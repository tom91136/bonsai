// format: off
package fix

object FilterOtherContains {
  val other = Set("a", "b")

  val filtered = Set("a", "c").intersect(other)
  val named = Set("a").intersect(other)

  val list = List("a").filter(other.contains)
  val unrelated = Set("a").filter(_ == "a")
}
