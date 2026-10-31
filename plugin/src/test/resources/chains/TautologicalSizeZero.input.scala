// format: off
package fix

object TautologicalSizeZero {
  val list = List(1, 2, 3)

  val zero = list.size == 0// assert: size-to-is-empty
}
