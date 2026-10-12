package app

object Main {
  def main(args: Array[String]): Unit = println(Sum.of(args.toList.map(_.length)))
}

object Sum {
  def of(values: List[Int]): Int = values.sum
}
