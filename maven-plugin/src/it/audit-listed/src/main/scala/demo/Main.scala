package demo

object Main {
  def main(args: Array[String]): Unit = println(if (Compare.same(Array(1), Array(1))) Tools.used else 0)
}
