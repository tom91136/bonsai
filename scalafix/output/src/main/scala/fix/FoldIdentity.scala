// format: off
package fix

import scala.util.Try

object FoldIdentity {
  val option: Option[Int] = Some(1)
  val either: Either[String, Int] = Right(1)
  def fallback: Int = 0

  class Box[A](value: A) {
    def fold[B](ifEmpty: => B)(f: A => B): B = f(value)
    def map[B](f: A => B): Box[B] = Box(f(value))
    def getOrElse[B >: A](default: => B): B = value
  }

  val FromLambda = option.getOrElse(0)
  val FromRenamed = option.getOrElse(fallback)
  val FromTyped = option.getOrElse(0)
  val FromIdentity = option.getOrElse(0)
  val FromPredef = option.getOrElse(0)
  val FromBlock = option.getOrElse(0)
  val FromSome = Some(1).getOrElse(0)
  val FromParenthesised = (option orElse Some(2)).getOrElse(0)
  val FromMap = option.getOrElse(0)
  val FromMapIdentity = option.getOrElse(fallback)
  val FromParenthesisedMap = (option orElse Some(2)).getOrElse(0)
  val Chained = option
    .getOrElse(0)
  val ChainedFold = option
    .getOrElse(0)
  val Nested = option.getOrElse(option.getOrElse(0))

  val Applied = option.fold(0)(x => x + 1)
  val Placeholder = option.fold(0)(_ + 1)
  val Constant = option.fold(0)(_ => 1)
  val Captured = option.fold(0)(x => fallback)
  val Mapped = option.map(x => x + 1).getOrElse(0)
  val MappedOther = option.map(x => x).orElse(None)
  val MappedFold = option.map(x => x).fold(0)(x => x + 1)
  val Flat = option.flatMap(x => Some(x)).getOrElse(0)
  val OfEither = either.fold(_.length, x => x)
  val OfTry = Try(1).fold(_ => 0, x => x)
  val OfBox = Box(1).fold(0)(x => x)
  val MappedBox = Box(1).map(x => x).getOrElse(0)
  val Plain = option.getOrElse(0)
  val Shadowed = {
    def identity(x: Int): Int = x + 1
    option.fold(0)(identity)
  }
  val TwoParameters = List(1).fold(0)((x, y) => x)
}
