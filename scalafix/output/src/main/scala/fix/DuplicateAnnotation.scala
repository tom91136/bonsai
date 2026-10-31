// format: off
package fix

import scala.annotation.tailrec

class Tag(name: String = "") extends scala.annotation.StaticAnnotation

object DuplicateAnnotation {
  @annotation.tailrec
  
  def Stacked(n: Int): Int = if (n <= 0) 0 else Stacked(n - 1)

  @tailrec
  def Spelt(n: Int): Int = if (n <= 0) 0 else Spelt(n - 1)

  @Tag("a")
  val Argued = 1

  @Tag
  
  val Bare = 2

  @Tag("a")
  
  
  val Triple = 3

  @Tag("c")
  class Annotated(@Tag("p") val value: Int)

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
