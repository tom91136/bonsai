/*
rule = FoldIdentity
 */
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

  val FromLambda = option.fold(0)(x => x)// assert: FoldIdentity.fold-identity
  val FromRenamed = option.fold(fallback)(value => value)// assert: FoldIdentity.fold-identity
  val FromTyped = option.fold(0)((x: Int) => x)// assert: FoldIdentity.fold-identity
  val FromIdentity = option.fold(0)(identity)// assert: FoldIdentity.fold-identity
  val FromPredef = option.fold(0)(Predef.identity)// assert: FoldIdentity.fold-identity
  val FromBlock = option.fold(0) { x => x }// assert: FoldIdentity.fold-identity
  val FromSome = Some(1).fold(0)(x => x)// assert: FoldIdentity.fold-identity
  val FromParenthesised = (option orElse Some(2)).fold(0)(x => x)// assert: FoldIdentity.fold-identity
  val FromMap = option.map(x => x).getOrElse(0)// assert: FoldIdentity.fold-identity
  val FromMapIdentity = option.map(identity).getOrElse(fallback)// assert: FoldIdentity.fold-identity
  val FromParenthesisedMap = (option orElse Some(2)).map(x => x).getOrElse(0)// assert: FoldIdentity.fold-identity
  val Chained = option// assert: FoldIdentity.fold-identity
    .map(x => x)
    .getOrElse(0)
  val ChainedFold = option// assert: FoldIdentity.fold-identity
    .fold(0)(x => x)
  val Nested = option.fold(option.fold(0)(x => x))(x => x)// assert: FoldIdentity.fold-identity

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
