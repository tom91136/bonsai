package demo

object Main {
  def main(args: Array[String]): Unit = println(Report.summary(args.toList) + Digest.size)
}

object Report {
  def summary(words: List[String]): String = {
    val counted = words.groupBy(identity).view.mapValues(_.size).toList.sortBy(-_._2)
    val lines   = counted.map((word, count) => s"$word: $count")
    lines.mkString("\n")
  }

  def native(words: List[String]): String = words.mkString(" ")
}
