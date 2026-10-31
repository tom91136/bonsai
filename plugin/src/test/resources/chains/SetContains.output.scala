// format: off
package fix

import scala.collection.immutable.TreeSet

object SetContains {
  sealed trait Kind
  object Kind {
    case object A extends Kind
    case object B extends Kind
  }

  val kinds: Set[Kind] = Set(Kind.A)
  val names: Set[String] = Set("a")
  val longs: Set[Long] = Set(1L)
  val counts: Map[String, Int] = Map("a" -> 1)
  val sorted: TreeSet[Int] = TreeSet(1)
  val words: List[String] = List("a")
  val needle: CharSequence = "a"

  val exists = kinds.contains(Kind.A)
  val forall = !(names.contains("a"))
  val found = Option.when(names.contains("a"))("a")
  val counted = names.contains("a")
  val missing = !names.contains("a")
  val inKeys = counts.contains("a")
  val notInKeys = !(counts.contains("a"))
  val widened = longs.contains(1)
  val listed = words.contains("a")
  val seen = words.contains(needle)

  val ordered = sorted.exists(_ == 1)
  val unrelated = names.exists(_ == needle)
  val absent = names.exists(_ != "a")
  val inverse = names.forall(_ == "a")
  val positive = names.exists(_.length > 1)
  val located = words.find(_ == "a")
  val sizes = counts.exists(_ == ("a", 1))
}
