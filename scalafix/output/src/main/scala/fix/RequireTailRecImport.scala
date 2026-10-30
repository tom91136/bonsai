// format: off
package fix

import java.util.Locale
import scala.annotation.tailrec
import scala.collection.mutable

object RequireTailRecImport {
  val buffer = mutable.ListBuffer.empty[Int]
  val locale = Locale.ROOT

  @tailrec
  def first(n: Int, acc: Int): Int =
    if (n <= 0) acc else first(n - 1, acc + n)

  @tailrec
  def second(n: Int, acc: Int): Int =
    if (n <= 0) acc else second(n - 1, acc + n)
}
