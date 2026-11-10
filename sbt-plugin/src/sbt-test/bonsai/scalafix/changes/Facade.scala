package scala.scalajs.js

trait Any
trait Object extends Any

object Boundary {
  def read(value: scala.Any): Object = value.asInstanceOf[Object]
}
