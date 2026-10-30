// format: off
package fix

object ExistsEquals {
  sealed trait CType
  object CType {
    case object OpaquePtr extends CType
    case object Integer extends CType
  }

  val types: List[CType] = List(CType.OpaquePtr)

  val anonymous = types.contains(CType.OpaquePtr)
  val named = types.contains(CType.OpaquePtr)
  val negated = !(types.contains(CType.OpaquePtr))

  val absent = types.exists(_ == null)
  val called = types.exists(_ == next())
  val self = types.exists(value => value == value)

  def next(): CType = CType.Integer
}
