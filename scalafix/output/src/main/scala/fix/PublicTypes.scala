// format: off
package fix

class PublicTypes {
  val inferred = 1
  def method = "x"
  var mutable = 0

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
  val plainName = "x"
  final val Limit = Max + 1
  final val Called = math.max(1, 2)
  final val Derived = Stride * 2L
  final val Stamp = math.abs(Floor)
  inline val Cap = 10
  inline val Sized = Max * 2
  inline def Plain = 1
  transparent inline def Picked = 1
}

trait PublicTypesParent {
  val timeout: Int = 1
}

object PublicTypesObject extends PublicTypesParent {
  override val timeout = 2
  final val Depth = 8
}
