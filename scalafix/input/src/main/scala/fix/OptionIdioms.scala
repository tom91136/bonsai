/*
rule = OptionIdioms
 */
// format: off
package fix

object OptionIdioms {
  val option: Option[Int] = Some(1)
  val empty: Option[Int] = None
  val map = Map(1 -> "a")
  val list = List(1, 2, 3)

  def positive(value: Int): Boolean = value > 0

  val applied = map.get(1).get// assert: OptionIdioms.map-get-to-apply
  val missing = map.get(1).isEmpty// assert: OptionIdioms.map-get-emptiness
  val present = map.get(1).nonEmpty// assert: OptionIdioms.map-get-emptiness
  val defined = map.get(1).isDefined// assert: OptionIdioms.map-get-emptiness
  val folded = option.map(_ + 1).getOrElse(0)// assert: OptionIdioms.map-get-or-else-to-fold
  val checked = option.map(_ > 0).getOrElse(false)// assert: OptionIdioms.map-get-or-else-to-fold
  val contained = option == Some(1)// assert: OptionIdioms.some-to-contains
  val excluded = Some(1) != option// assert: OptionIdioms.some-to-contains
  val nothing = option == None// assert: OptionIdioms.none-check
  val something = None != option// assert: OptionIdioms.none-check
  val itself = option.headOption// assert: OptionIdioms.redundant-option-iteration
  val lastItself = option.lastOption// assert: OptionIdioms.redundant-option-iteration
  val fromNonEmpty = if (list.nonEmpty) Some(list.head) else None// assert: OptionIdioms.head-or-last-option
  val fromEmpty = if (list.isEmpty) None else Some(list.head)// assert: OptionIdioms.head-or-last-option
  val lastFromNonEmpty = if (list.nonEmpty) Some(list.last) else None// assert: OptionIdioms.head-or-last-option
  val first = list.lift(0)// assert: OptionIdioms.head-or-last-option
  val last = list.lift(list.size - 1)// assert: OptionIdioms.head-or-last-option
  def element(index: Int): Option[Int] = if (index < list.length) Some(list(index)) else None// assert: OptionIdioms.bounds-to-lift
  val head = list(0)// assert: OptionIdioms.zero-index-to-head
  val tail = list(list.size - 1)// assert: OptionIdioms.last-index-to-last
  val lifted = map.lift(1)// assert: OptionIdioms.map-lift-to-get
  val filtered = if (option.isDefined && positive(option.get)) option else None// assert: OptionIdioms.simulated-filter
  val value = 3
  val emulated = if (positive(value)) Some(value) else None// assert: OptionIdioms.if-else-to-filtered-option

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
