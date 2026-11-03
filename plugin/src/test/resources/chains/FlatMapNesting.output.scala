// format: off
package fix

object FlatMapNesting {
  case class Node(name: String, size: Int, children: List[Node])

  val nodes: List[Node] = Nil

  val mapped = nodes.flatMap(_.children).map(_.size)
  val worked = nodes.flatMap(x => x.children.map(_.size + 1))
  val filtered = nodes.flatMap(x => x.children.filter(_.size > 2))
  val dropped = nodes.flatMap(x => x.children.filterNot(_.size > 2))
  val flat = nodes.flatMap(_.children).flatMap(_.children)
  val grouped = nodes.flatMap(_.children).map(_.size).toSet
  val groupedWorked = nodes.flatMap(x => x.children.map(_.size * 2)).toSet

  val effectful = nodes.flatMap(_.children).map(node => println(node))
  val lazily = nodes.view.flatMap(_.children).map(_.size)
  val array = Array(1).flatMap(Array(_)).map(_ + 1)
  val optional = Option(1).flatMap(Some(_)).map(_ + 1)
  val separate = nodes.flatMap(_.children).sortBy(_.size).map(_.size)
}
