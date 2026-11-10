package demo

import scala.scalajs.js

object Boundary {
  def record(value: Any): js.Object      = value.asInstanceOf[js.Object]
  def numbers(value: Any): js.Array[Int] = value.asInstanceOf[js.Array[Int]]
  def dynamic(value: js.Any): js.Dynamic = value.asInstanceOf[js.Dynamic]
}
