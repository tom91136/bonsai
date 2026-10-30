// format: off
package fix

import scala.annotation.tailrec

object RequireTailRec {
  @tailrec
  def unannotatedTailRec(n: Int, acc: Int): Int =
    if (n <= 0) acc else unannotatedTailRec(n - 1, acc + n)

  def nonTailRec(n: Int): Int =
    if (n <= 0) 1 else n * nonTailRec(n - 1)

  def notRecursive(n: Int): Int =
    n + 1

  @tailrec
  def annotatedTailRec(n: Int, acc: Int): Int =
    if (n <= 0) acc else annotatedTailRec(n - 1, acc + n)

  @tailrec
  def typedTailRec[A](value: A, n: Int): A =
    if (n <= 0) value else typedTailRec[A](value, n - 1)

  @tailrec
  def matchedTailRec(n: Int): Int =
    n match {
      case 0 => 0
      case _ => matchedTailRec(n - 1)
    }

  class FinalHelper {
    @tailrec
    final def finalTailRec(i: Int): Unit =
      if (i > 0) finalTailRec(i - 1)
  }

  class Helper {
    @tailrec
    private def privateLoop(i: Int): Unit =
      if (i > 0) privateLoop(i - 1)

    def outer(): Unit = {
      @tailrec
      def localLoop(i: Int): Unit =
        if (i > 0) localLoop(i - 1)
      localLoop(10)
    }
  }
}
