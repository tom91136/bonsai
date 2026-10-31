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

  val exists = kinds.exists(_ == Kind.A)// assert: exists-to-contains
  val forall = names.forall(_ != "a")// assert: forall-to-not-contains
  val found = names.find(_ == "a")// assert: exists-to-contains
  val counted = names.count(_ == "a") > 0// assert: exists-to-contains
  val missing = names.count(_ == "a") == 0// assert: forall-to-not-contains
  val inKeys = counts.keys.exists(_ == "a")// assert: exists-to-contains
  val notInKeys = counts.keys.forall(_ != "a")// assert: forall-to-not-contains
  val widened = longs.exists(_ == 1)// assert: exists-to-contains
  val listed = words.count(_ == "a") > 0// assert: exists-to-contains
  val seen = words.exists(_ == needle)// assert: exists-to-contains

  val ordered = sorted.exists(_ == 1)
  val unrelated = names.exists(_ == needle)
  val absent = names.exists(_ != "a")
  val inverse = names.forall(_ == "a")
  val positive = names.exists(_.length > 1)
  val located = words.find(_ == "a")
  val sizes = counts.exists(_ == ("a", 1))
}
