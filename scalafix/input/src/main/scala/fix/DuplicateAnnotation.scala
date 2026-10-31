/*
rule = DuplicateAnnotation
 */
// format: off
package fix

import scala.annotation.tailrec

class Tag(name: String = "") extends scala.annotation.StaticAnnotation

object DuplicateAnnotation {
  @annotation.tailrec
  @annotation.tailrec// assert: DuplicateAnnotation.duplicate-annotation
  def Stacked(n: Int): Int = if (n <= 0) 0 else Stacked(n - 1)

  @tailrec @annotation.tailrec// assert: DuplicateAnnotation.duplicate-annotation
  def Spelt(n: Int): Int = if (n <= 0) 0 else Spelt(n - 1)

  @Tag("a") @Tag("a")// assert: DuplicateAnnotation.duplicate-annotation
  val Argued = 1

  @Tag
  @Tag// assert: DuplicateAnnotation.duplicate-annotation
  val Bare = 2

  @Tag("a")
  @Tag("a")// assert: DuplicateAnnotation.duplicate-annotation
  @Tag("a")// assert: DuplicateAnnotation.duplicate-annotation
  val Triple = 3

  @Tag("c") @Tag("c")// assert: DuplicateAnnotation.duplicate-annotation
  class Annotated(@Tag("p") @Tag("p")/* assert: DuplicateAnnotation.duplicate-annotation*/ val value: Int)

  @Tag("a") @Tag("b")
  val Differing = 4

  @Tag @Tag("b")
  val Bare_ = 5

  @tailrec @Tag("a")
  def Distinct(n: Int): Int = if (n <= 0) 0 else Distinct(n - 1)

  @Tag("a")
  val Once = 6

  @Tag("a")
  class Owner(@Tag("a") val value: Int) {
    @Tag("a")
    def member(@Tag("a") argument: Int): Int = argument
  }
}
