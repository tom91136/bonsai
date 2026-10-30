/*
rule = CollectionIdioms
 */
// format: off
package fix

object ExistsEquals {
  sealed trait CType
  object CType {
    case object OpaquePtr extends CType
    case object Integer extends CType
  }

  val types: List[CType] = List(CType.OpaquePtr)

  val anonymous = types.exists(_ == CType.OpaquePtr)// assert: CollectionIdioms.exists-to-contains
  val named = types.exists(value => value == CType.OpaquePtr)// assert: CollectionIdioms.exists-to-contains
  val negated = types.forall(_ != CType.OpaquePtr)// assert: CollectionIdioms.forall-to-not-contains

  val absent = types.exists(_ == null)
  val called = types.exists(_ == next())
  val self = types.exists(value => value == value)

  def next(): CType = CType.Integer
}
