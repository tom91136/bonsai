/*
rule = CompanionOrder
 */
package fix

class CompanionOrder
object CompanionOrder { // assert: CompanionOrder.companion-order
  trait Newtype[A] {
    opaque type Type = A
  }

  class Record
  object Record // assert: CompanionOrder.companion-order

  trait Service
  object Service // assert: CompanionOrder.companion-order

  enum Mode { case First }
  object Mode // assert: CompanionOrder.companion-order

  type Items = List[Int]
  object Items // assert: CompanionOrder.companion-order

  type AliasFirst = AliasFirst.Type
  object AliasFirst extends Newtype[Long]

  type Refined = Refined.Type & Serializable
  object Refined extends Newtype[Long]

  object GoodRecord
  case class GoodRecord(value: Int)

  object GoodService
  trait GoodService

  object GoodMode
  enum GoodMode { case First }

  object GoodItems
  type GoodItems = List[Int]

  object GoodId extends Newtype[Long]
  type GoodId = GoodId.Type

  object ScopeOne { class Unrelated  }
  object ScopeTwo { object Unrelated }

  def local(): Unit = {
    class Local
    object Local // assert: CompanionOrder.companion-order
    ()
  }

  class Suppressed
  @SuppressWarnings(Array("scalafix:CompanionOrder"))
  object Suppressed
}
