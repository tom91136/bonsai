/*
rules = [BannedSyntax, NoAsInstanceOf]
 */
package scala.scalajs.js

trait Any
trait Object extends Any
trait Facade extends Object

type Handle = Facade

object JavaScriptAllocations {
  val record = new Object {}
}

object Casts {
  val value: scala.Any = 1

  val number    = value.asInstanceOf[Int]       // assert: NoAsInstanceOf.as-instance-of
  val text      = value.asInstanceOf[String]    // assert: NoAsInstanceOf.as-instance-of
  val list      = value.asInstanceOf[List[Int]] // assert: NoAsInstanceOf.as-instance-of
  val matchable = value.asInstanceOf[Matchable] // assert: NoAsInstanceOf.as-instance-of-matchable
  val nested    = List(1).map(_.asInstanceOf[Option[Int]]) // assert: NoAsInstanceOf.as-instance-of

  val any       = value.asInstanceOf[Any]
  val obj       = value.asInstanceOf[Object]
  val facade    = value.asInstanceOf[Facade]
  val handle    = value.asInstanceOf[Handle]
  val qualified = value.asInstanceOf[scala.scalajs.js.Object]

  val suppressed = value.asInstanceOf[Int] // scalafix:ok NoAsInstanceOf.as-instance-of
}
