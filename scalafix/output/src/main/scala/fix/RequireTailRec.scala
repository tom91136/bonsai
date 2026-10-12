// format: off
package fix

import scala.annotation.tailrec

object RequireTailRec {
  @annotation.tailrec
  def unannotatedTailRec(n: Int, acc: Int): Int =
    if (n <= 0) acc else unannotatedTailRec(n - 1, acc + n)

  def nonTailRec(n: Int): Int =
    if (n <= 0) 1 else n * nonTailRec(n - 1)

  def notRecursive(n: Int): Int =
    n + 1

  @tailrec
  def annotatedTailRec(n: Int, acc: Int): Int =
    if (n <= 0) acc else annotatedTailRec(n - 1, acc + n)

  class Helper {
    @annotation.tailrec
    private def privateLoop(i: Int): Unit =
      if (i > 0) privateLoop(i - 1)

    def outer(): Unit = {
      @annotation.tailrec
      def localLoop(i: Int): Unit =
        if (i > 0) localLoop(i - 1)
      localLoop(10)
    }
  }
}
