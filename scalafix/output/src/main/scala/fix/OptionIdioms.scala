// format: off
package fix

object OptionIdioms {
  val option: Option[Int] = Some(1)
  val empty: Option[Int] = None
  val map = Map(1 -> "a")
  val list = List(1, 2, 3)

  def positive(value: Int): Boolean = value > 0

  val applied = map(1)
  val missing = !map.contains(1)
  val present = map.contains(1)
  val defined = map.contains(1)
  val folded = option.fold(0)(_ + 1)
  val checked = option.fold(false)(_ > 0)
  val contained = option.contains(1)
  val excluded = !option.contains(1)
  val nothing = option.isEmpty
  val something = option.isDefined
  val itself = option
  val lastItself = option
  val fromNonEmpty = list.headOption
  val fromEmpty = list.headOption
  val lastFromNonEmpty = list.lastOption
  val first = list.headOption
  val last = list.lastOption
  def element(index: Int): Option[Int] = list.lift(index)
  val head = list.head
  val tail = list.last
  val lifted = map.get(1)
  val filtered = option.filter(positive)
  val value = 3
  val emulated = Some(value).filter(positive)

  class ApplicationMethods {
    def get(key: Int): ApplicationMethods              = this
    def isEmpty: Boolean                               = false
    def headOption: Option[Int]                        = None
    def lift(index: Int): ApplicationMethods           = this
    def map[A](function: Int => A): ApplicationMethods = this
    def getOrElse[A](default: A): A                    = default
    def apply(index: Int): ApplicationMethods          = this
  }
  val custom = new ApplicationMethods
  val customApplied = custom.get(1)
  val customEmpty = custom.get(1).isEmpty
  val customHead = custom.headOption
  val customLift = custom.lift(0)
  val customFold = custom.map(identity).getOrElse(0)
  val customIndex = custom(0)
  val mapHead = map.headOption
  val listLift = list.lift(1)
  val unrelated = if (positive(value)) None else Some(value)
}
