/*
rule = ReversedIndexing
 */
package fix

object ReversedIndexing {
  def lambda(indices: Vector[Int], bits: Int): Seq[Int] =
    indices.indices.map { slot => indices(indices.size - 1 - slot) * bits + slot } // assert: ReversedIndexing.reversed-indexing

  def length(values: Vector[Int]): Seq[Int] =
    values.indices.map(index => values(values.length - 1 - index)) // assert: ReversedIndexing.reversed-indexing

  def array(values: Array[Int]): Seq[Int] =
    values.indices.map(index => values(values.length - 1 - index))

  def arrayLoop(values: Array[Int]): Unit =
    for (index <- values.indices) println(values(values.size - 1 - index))

  def arrayField: Seq[Int] = {
    val values = Array(1, 2, 3)
    (0 until values.length).map(index => values(values.length - 1 - index))
  }

  def subtracted(values: Vector[Int]): Seq[Int] =
    values.indices.map(index => values(values.size - index - 1)) // assert: ReversedIndexing.reversed-indexing

  def grouped(values: Vector[Int]): Seq[Int] =
    values.indices.map(index => values(values.size - (index + 1))) /* assert: ReversedIndexing.reversed-indexing
                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
values is read from its end by index while its indices are iterated; iterate values.reverseIterator, which copies nothing, and keep the index only when the position matters
*/

  def range(values: Vector[Int]): Seq[Int] =
    (0 until values.size).map(index => values(values.size - 1 - index)) // assert: ReversedIndexing.reversed-indexing

  def inclusive(values: Vector[Int]): Seq[Int] =
    (0 to values.size - 1).map(index => values(values.size - 1 - index)) // assert: ReversedIndexing.reversed-indexing

  def generated(values: Vector[Int]): Seq[Int] =
    for (index <- values.indices) yield values(values.size - 1 - index) // assert: ReversedIndexing.reversed-indexing

  def loop(values: Vector[Int]): Unit =
    for (index <- values.indices) println(values(values.size - 1 - index)) // assert: ReversedIndexing.reversed-indexing

  def palindrome(values: Vector[Int]): Boolean =
    values.indices.forall(index => values(index) == values(values.size - 1 - index))

  def last(values: Vector[Int]): Int = values(values.size - 1)

  def other(values: Vector[Int], others: Vector[Int]): Seq[Int] =
    others.indices.map(index => values(values.size - 1 - index))

  def reversed(values: Vector[Int]): Seq[Int] = values.reverse.zipWithIndex.map((value, slot) => value + slot)

  def outside(values: Vector[Int], index: Int): Int = values(values.size - 1 - index)

  @SuppressWarnings(Array("scalafix:ReversedIndexing"))
  def suppressed(values: Vector[Int]): Seq[Int] = values.indices.map(index => values(values.size - 1 - index))
}
