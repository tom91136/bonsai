// format: off
package fix

object FlatMapNesting {
  case class Node(name: String, size: Int, children: List[Node])

  val nodes: List[Node] = Nil

  val mapped = nodes.flatMap(_.children).map(_.size)// assert: flat-map-nesting
  val filtered = nodes.flatMap(_.children).filter(_.size > 2)// assert: flat-map-nesting
  val dropped = nodes.flatMap(_.children).filterNot(_.size > 2)// assert: flat-map-nesting
  val flat = nodes.flatMap(_.children).flatMap(_.children)// assert: flat-map-nesting
  val grouped = nodes.flatMap(_.children).map(_.size).toSet// assert: flat-map-nesting

  val effectful = nodes.flatMap(_.children).map(node => println(node))
  val lazily = nodes.view.flatMap(_.children).map(_.size)
  val array = Array(1).flatMap(Array(_)).map(_ + 1)
  val optional = Option(1).flatMap(Some(_)).map(_ + 1)
  val separate = nodes.flatMap(_.children).sortBy(_.size).map(_.size)
}
