/*
rule = RequireTailRec
 */
// format: off
package fix

import java.util.Locale
import scala.collection.mutable

object RequireTailRecImport {
  val buffer = mutable.ListBuffer.empty[Int]
  val locale = Locale.ROOT

  def first(n: Int, acc: Int): Int =// assert: RequireTailRec.missing-tailrec
    if (n <= 0) acc else first(n - 1, acc + n)

  def second(n: Int, acc: Int): Int =// assert: RequireTailRec.missing-tailrec
    if (n <= 0) acc else second(n - 1, acc + n)
}
