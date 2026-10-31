/*
rule = CollectionIdioms
 */
// format: off
package fix

object TautologicalSizeComparison {
  val list = List(1, 2, 3)
  val other = List(4)
  val array = Array(1, 2)
  def next(): List[Int] = list

  val always = list.size >= 0// assert: CollectionIdioms.tautological-size-comparison
  val alwaysLength = array.length >= 0// assert: CollectionIdioms.tautological-size-comparison
  val never = list.size < 0// assert: CollectionIdioms.tautological-size-comparison
  val call = next().size >= 0// assert: CollectionIdioms.tautological-size-comparison

  val single = list.size >= 2 && (list.size == 2)// assert: CollectionIdioms.tautological-size-comparison
  val bare = list.size >= 2 && list.size == 2// assert: CollectionIdioms.tautological-size-comparison
  val several = list.size >= 1 && (list.size == 1 || list.size == 2 || list.size == 3)// assert: CollectionIdioms.tautological-size-comparison
  val nested = list.size >= 1 && ((list.size == 1) || (list.size == 2))// assert: CollectionIdioms.tautological-size-comparison
  val leading = list.size >= 1 && (list.size == 1 || list.size == 2) && other.nonEmpty// assert: CollectionIdioms.tautological-size-comparison
  val leadingSingle = list.size >= 1 && (list.size == 1) && other.nonEmpty// assert: CollectionIdioms.tautological-size-comparison
  val trailing = other.nonEmpty || list.size >= 1 && (list.size == 1 || list.size == 2)// assert: CollectionIdioms.tautological-size-comparison
  val wrapped = (list.size >= 1 && (list.size == 1 || list.size == 2)) && other.nonEmpty// assert: CollectionIdioms.tautological-size-comparison
  val zero = list.size >= 0 &&// assert: CollectionIdioms.tautological-size-comparison
    list.size == 0
  val arrays = array.length >= 2 && (array.length == 2)// assert: CollectionIdioms.tautological-size-comparison
  val reversed = 0 <= list.size// assert: CollectionIdioms.tautological-size-comparison
  val reversedNever = 0 > list.size// assert: CollectionIdioms.tautological-size-comparison
  val reversedBound = 2 <= list.size && (2 == list.size)// assert: CollectionIdioms.tautological-size-comparison
  val either = list.nonEmpty || list.isEmpty// assert: CollectionIdioms.tautological-size-comparison
  val neither = list.isEmpty && list.nonEmpty// assert: CollectionIdioms.tautological-size-comparison
  val counted = list.size >= 1 || list.isEmpty// assert: CollectionIdioms.tautological-size-comparison
  val chained = other.nonEmpty || list.nonEmpty || list.isEmpty// assert: CollectionIdioms.tautological-size-comparison
  val arrayEither = array.isEmpty || array.nonEmpty// assert: CollectionIdioms.tautological-size-comparison

  val positive = list.size >= 1
  val negative = list.size < 1
  val receivers = list.size >= 1 && (other.size == 1)
  val below = list.size >= 2 && (list.size == 1 || list.size == 2)
  val variable = list.size >= list.head && (list.size == 2)
  val variableDisjunct = list.size >= 1 && (list.size == list.head)
  val calls = next().size >= 1 && (next().size == 1)
  val upper = list.size >= 1 && (list.size <= 2)
  val text = "abc".length >= 0
  val distinct = list.nonEmpty || other.isEmpty
  val calledTwice = next().nonEmpty || next().isEmpty
  val grouped = list.nonEmpty && other.isEmpty || list.isEmpty
  val reversedPositive = 1 <= list.size
}
