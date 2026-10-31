// format: off
package fix

object ViewFusion {
  case class Item(name: String, size: Int)

  val items: List[Item] = Nil
  val vector: Vector[Item] = Vector.empty
  val set: Set[Item] = Set.empty

  val chained = items.map(_.size + 1).takeWhile(_ > 3)// assert: intermediate-collections
  val kept = vector.takeWhile(_.size > 1).map(_.name)// assert: intermediate-collections
  val unique = set.map(_.size).dropWhile(_ > 1)// assert: intermediate-collections
  val converted = items.map(item => (item.name, item.size)).takeWhile(_._2 > 1).toMap// assert: intermediate-collections
  val counted = items.map(_.size).takeWhile(_ > 1).size// assert: intermediate-collections
  val multiline = items// assert: intermediate-collections
    .map(_.size)
    .takeWhile(_ > 1)

  val effectful = items.map(item => println(item)).takeWhile(_ => true)
  val lazily = items.view.map(_.size).takeWhile(_ > 1)
  val arrays = Array(1).map(_ + 1).takeWhile(_ > 1)
  val ordered = items.map(_.size).takeWhile(_ > 1).sorted
  val infix = items map (_.size) takeWhile (_ > 1)
  val single = items.map(_.size).sorted
}
