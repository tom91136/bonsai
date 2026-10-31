/*
rule = TargetName
 */
package fix

import scala.annotation.targetName

object TargetName {
  case class Vec(x: Int, y: Int) {
    def +(other: Vec): Vec = Vec(x + other.x, y + other.y) // assert: TargetName.missing-target-name
    @targetName("minus") def -(other: Vec): Vec = Vec(x - other.x, y - other.y)
    def unary_- : Vec = Vec(-x, -y)
    def plus(other: Vec): Vec = this + other
    override def toString: String = s"($x, $y)"
  }

  trait Monoid[A] {
    def <>(left: A, right: A): A // assert: TargetName.missing-target-name
  }

  object Sums extends Monoid[Int] {
    override def <>(left: Int, right: Int): Int = left + right
  }

  object Implementation extends Monoid[String] {
    def <>(left: String, right: String): String = left + right
  }

  extension (value: Vec) def *(scale: Int): Vec = Vec(value.x * scale, value.y * scale) // assert: TargetName.missing-target-name

  type <+>[A, B] = Either[A, B]
  @targetName("either") type <|>[A, B] = Either[A, B] // assert: TargetName.redundant-target-name

  private def ~(value: Int): Int = -value
  @targetName("tilde") private def ~~(value: Int): Int = -value // assert: TargetName.redundant-target-name

  @targetName("timesInt") private def **(value: Int, by: Int): Int = value * by
  @targetName("timesLong") private def **(value: Long, by: Long): Long = value * by

  private object Internal {
    def ++(left: Int, right: Int): Int = left + right
    @targetName("concatenate") def +++(left: Int, right: Int): Int = left + right // assert: TargetName.redundant-target-name
  }

  extension (value: Int) {
    def <<>>(other: Int): Int = value + other // assert: TargetName.missing-target-name
  }

  private object Comparisons {
    extension (value: List[Int]) {
      @targetName("lessInts") def <(other: List[Int]): Boolean = value.sum < other.sum
    }
    extension (value: List[String]) {
      @targetName("lessStrings") def <(other: List[String]): Boolean = value.size < other.size
    }
  }

  def local(value: Int): Int = {
    def %%(left: Int, right: Int): Int = left % right
    %%(value, 2)
  }
}
