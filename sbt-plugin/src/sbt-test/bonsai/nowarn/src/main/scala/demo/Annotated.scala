package demo

import scala.annotation.nowarn

@nowarn("msg=in package coding is deprecated")
object Annotated {
  def value: Int = 1
}
