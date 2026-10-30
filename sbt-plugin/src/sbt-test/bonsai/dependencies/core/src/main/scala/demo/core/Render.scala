package demo.core

class Render[A](val scope: A) {
  def shown: String = scope.toString
}
