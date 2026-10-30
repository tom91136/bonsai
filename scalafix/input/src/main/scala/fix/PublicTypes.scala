/*
rule = PublicTypes
 */
// format: off
package fix

class PublicTypes {
  val inferred = 1// assert: PublicTypes.missing-public-type
  def method = "x"// assert: PublicTypes.missing-public-type
  var mutable = 0// assert: PublicTypes.missing-public-type

  val typed: Int = 1
  def typedMethod: String = "x"

  private val hidden = 1
  private def hiddenMethod = "x"

  final val Max = 3
  final val Ratio = 0.5
  final val Scale = 1.5f
  final val Stride = 4L
  final val Initial = 'a'
  final val Name = "x"
  final val Enabled = true
  final val Floor = -3
  final val Offset = -0.25
  val plainName = "x"// assert: PublicTypes.missing-public-type
  final val Limit = Max + 1
  final val Called = math.max(1, 2)// assert: PublicTypes.missing-public-type
  final val Derived = Stride * 2L
  final val Stamp = math.abs(Floor)// assert: PublicTypes.missing-public-type
  inline val Cap = 10
  inline val Sized = Max * 2
  inline def Plain = 1// assert: PublicTypes.missing-public-type
  transparent inline def Picked = 1
}

trait PublicTypesParent {
  val timeout: Int = 1
}

object PublicTypesObject extends PublicTypesParent {
  override val timeout = 2// assert: PublicTypes.missing-public-type
  final val Depth = 8
}
