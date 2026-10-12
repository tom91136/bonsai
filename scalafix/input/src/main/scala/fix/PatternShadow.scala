/*
rule = PatternShadow
 */
package fix

val shared = 1

object PatternShadow {
  sealed trait Shape
  case class Applied(wrapper: String, arguments: List[Int]) extends Shape
  case class Single(value: Int) extends Shape

  class Collector(limit: Int) {
    private val arguments = List(1, 2)
    val threshold         = 3

    def lastArgument(shape: Shape): Int = shape match {
      case Applied(wrapper, arguments) => arguments.size // assert: PatternShadow.pattern-shadow
      case Single(threshold)           => threshold // assert: PatternShadow.pattern-shadow
    }

    def bounded(shape: Shape): Boolean = shape match {
      case Single(limit) => limit > 0 // assert: PatternShadow.pattern-shadow
      case _             => false
    }

    def matches(shape: Shape): Boolean = shape match {
      case Single(`threshold`) => true
      case Single(other)       => other == arguments.size
      case _                   => false
    }
  }

  def scale(shape: Shape, factor: Int): Int = {
    val offset = 1
    shape match {
      case Single(factor) if factor > 0 => factor + offset // assert: PatternShadow.pattern-shadow
      case Single(offset) => offset // assert: PatternShadow.pattern-shadow
      case _              => 0
    }
  }

  def sizes(shapes: List[Shape]): List[Int] = shapes.map(shape =>
    shape match {
      case Single(shape) if shape > 0 => shape // assert: PatternShadow.pattern-shadow
      case Single(size)  => size
      case _             => 0
    }
  )

  def narrowed(shape: Shape, value: Int): Int = shape match {
    case shape: Single if shape.value > 0 => shape.value
    case Single(value: Int) => value
    case _ => value
  }

  def rebound(tag: Int): Int = tag match {
    case tag if tag > 0 => tag
    case other => -other
  }

  def later(shape: Shape): Int = {
    val result = shape match {
      case Single(defined) => defined
      case _               => 0
    }
    val defined = 2
    result + defined
  }

  def generated(rows: List[List[(Int, Int)]]): List[Int] = for {
    fields <- rows
    total  <- fields.headOption.toList.map { case (fields, _) => fields } // assert: PatternShadow.pattern-shadow
  } yield total

  def unrelated(rows: List[(Int, Int)]): List[Int] = for {
    (key, _) <- rows
    doubled = key * 2
  } yield doubled

  def sourced(shape: Shape): Int = shape match {
    case Single(shared) => shared // assert: PatternShadow.pattern-shadow
    case _              => 0
  }

  def mutable(shape: Shape): Int = {
    var bound = 0
    shape match {
      case Single(bound) => bound // assert: PatternShadow.pattern-shadow
      case _             => 0
    }
  }
}
