package demo

object Main {
  def main(args: Array[String]): Unit = println(Server(args.length, "server").label)
}

final case class Server(port: Int, name: String) {
  def label: String = name
}
