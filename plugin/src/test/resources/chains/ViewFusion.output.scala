// format: off
package fix

object ViewFusion {
  case class Item(name: String, size: Int)

  val items: List[Item] = Nil
  val vector: Vector[Item] = Vector.empty
  val set: Set[Item] = Set.empty

  val chained = items.view.map(_.size + 1).takeWhile(_ > 3).toList
  val kept = vector.view.takeWhile(_.size > 1).map(_.name).toVector
  val unique = set.view.map(_.size).dropWhile(_ > 1).toSet
  val converted = items.view.map(item => (item.name, item.size)).takeWhile(_._2 > 1).toMap
  val counted = items.view.map(_.size).takeWhile(_ > 1).size
  val multiline = items.view
    .map(_.size)
    .takeWhile(_ > 1).toList

  val effectful = items.map(item => println(item)).takeWhile(_ => true)
  val lazily = items.view.map(_.size).takeWhile(_ > 1)
  val arrays = Array(1).map(_ + 1).takeWhile(_ > 1)
  val ordered = items.map(_.size).takeWhile(_ > 1).sorted
  val infix = items map (_.size) takeWhile (_ > 1)
  val single = items.map(_.size).sorted
}
