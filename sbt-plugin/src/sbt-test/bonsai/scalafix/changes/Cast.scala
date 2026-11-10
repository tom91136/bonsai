package demo

object Cast {
  def cast(value: Any): Int = value.asInstanceOf[Int]
}
