// format: off
package fix

object TautologicalSizeComparison {
  val list = List(1, 2, 3)
  val other = List(4)
  val array = Array(1, 2)
  def next(): List[Int] = list

  val always = list.size >= 0
  val alwaysLength = array.length >= 0
  val never = list.size < 0
  val call = next().size >= 0

  val single = list.size == 2
  val bare = list.size == 2
  val several = list.size == 1 || list.size == 2 || list.size == 3
  val nested = (list.size == 1) || (list.size == 2)
  val leading = (list.size == 1 || list.size == 2) && other.nonEmpty
  val leadingSingle = list.size == 1 && other.nonEmpty
  val trailing = other.nonEmpty || list.size == 1 || list.size == 2
  val wrapped = (list.size == 1 || list.size == 2) && other.nonEmpty
  val zero = list.size == 0
  val arrays = array.length == 2
  val reversed = 0 <= list.size
  val reversedNever = 0 > list.size
  val reversedBound = 2 == list.size
  val either = list.nonEmpty || list.isEmpty
  val neither = list.isEmpty && list.nonEmpty
  val counted = list.size >= 1 || list.isEmpty
  val chained = other.nonEmpty || list.nonEmpty || list.isEmpty
  val arrayEither = array.isEmpty || array.nonEmpty

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
