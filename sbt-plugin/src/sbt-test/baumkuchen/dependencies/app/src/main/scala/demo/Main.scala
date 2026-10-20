package demo

class Names(scope: String) extends demo.core.Render[String](scope)

object Main {
  def main(args: Array[String]): Unit = println(Names("main").shown)
}
