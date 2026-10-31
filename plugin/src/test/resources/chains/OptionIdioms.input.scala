// format: off
package fix

object OptionIdioms {
  val option: Option[Int] = Some(1)
  val empty: Option[Int] = None
  val map = Map(1 -> "a")
  val list = List(1, 2, 3)

  def positive(value: Int): Boolean = value > 0

  val applied = map.get(1).get// assert: map-get-to-apply
  val missing = map.get(1).isEmpty// assert: map-get-emptiness
  val present = map.get(1).nonEmpty// assert: map-get-emptiness
  val defined = map.get(1).isDefined// assert: map-get-emptiness
  val folded = option.map(_ + 1).getOrElse(0)// assert: map-get-or-else-to-fold
  val checked = option.map(_ > 0).getOrElse(false)// assert: map-get-or-else-to-fold
  val identityFolded = option.map(x => x).getOrElse(0)// assert: redundant-identity-map
  val identityMapped = option.map(identity)// assert: redundant-identity-map
  val transformed = list.map(identity)
  val viaFor = for (value <- option) yield value// assert: redundant-identity-map
  val viaTwo = for { first <- option; second <- empty } yield second// assert: redundant-identity-map
  val contained = option == Some(1)// assert: some-to-contains
  val excluded = Some(1) != option// assert: some-to-contains
  val nothing = option == None// assert: none-check
  val something = None != option// assert: none-check
  val headIs = list.headOption == Some(1)// assert: some-to-contains
  val headMissing = list.headOption == None// assert: none-check
  val plainEmpty = option.isEmpty
  val itself = option.headOption// assert: redundant-option-iteration
  val lastItself = option.lastOption// assert: redundant-option-iteration
  val first = list.lift(0)// assert: head-or-last-option
  val last = list.lift(list.size - 1)// assert: head-or-last-option
  val head = list(0)// assert: zero-index-to-head
  val tail = list(list.size - 1)// assert: last-index-to-last
  val updated = { val array = Array(1, 2); array(0) = 3; array }
  def pair(text: String): (String, String) = {
    val parts = text.split("=", 2)
    (parts(0), if (parts.length == 2) parts(1) else "")
  }
  def indexed(values: Vector[Int]): Int = values(0)
  def offset(values: Vector[Int], index: Int): Int = values(0) + values(index)
  def ends(values: List[Int]): Int = values(0) + values(values.size - 1)
  def channels(rows: List[List[Int]]): List[Int] = rows.map { row =>
    val red   = row(0)
    val green = row(1)
    red + green
  }
  locally {
    val results = List(1, 2)
    val first   = results(0)
    val second  = results(1)
  }
  def lone(values: List[Int]): Int = if (values.length > 1) values(0) else 0// assert: zero-index-to-head
  def twice(values: List[Int]): Int = {
    val first = values(0)// assert: zero-index-to-head
    first + values(0)// assert: zero-index-to-head
  }
  val lifted = map.lift(1)// assert: map-lift-to-get
  val value = 3
  val big: Int => Boolean = _ > 5

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
  val longMap = scala.collection.mutable.LongMap(1L -> "a")
  val longPresent = longMap.get(1L).isDefined// assert: map-get-emptiness
  val immutableLongMap = scala.collection.immutable.LongMap(1L -> "a")
  val immutableLongPresent = immutableLongMap.get(1L).isDefined
  val intMap = scala.collection.immutable.IntMap(1 -> "a")
  val intMissing = intMap.get(1).isEmpty
  val listLift = list.lift(1)
  val unrelated = if (positive(value)) None else Some(value)

  enum Instruction(val name: String) {
    case Add(pred: Option[Int]) extends Instruction("add")
    def pred: Option[Int]
  }
  def render(value: Int): String = value.toString
  def render(instruction: Instruction): String =
    instruction.pred.map(predication => s"${render(predication)} ").getOrElse("")// assert: map-get-or-else-to-fold
}
