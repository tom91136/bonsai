package demo

object Aaa {
  def positive(values: List[Int]): List[Int] = values.filter(_ > 0).map(_ + 1)
}
