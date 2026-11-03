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
  val folded = option.map(_ + 1).getOrElse(0)
  val checked = option.map(_ > 0).getOrElse(false)
  val failed = option.map(_ + 1).getOrElse(sys.error("none"))
  val thrown = option.map(_ + 1).getOrElse(throw new IllegalStateException("none"))
  val unfinished = option.map(_ + 1).getOrElse(???)
  val identityFolded = option.getOrElse(0)
  val identityMapped = option
  val upcast = Option("a").map[CharSequence](identity)
  val sameTyped = Option("a").map[String](identity)
  val transformed = list.map(identity)
  val viaFor = for (value <- option) yield value
  val viaTwo = for { first <- option; second <- empty } yield second
  val contained = option.contains(1)
  val excluded = !option.contains(1)
  val nothing = option.isEmpty
  val something = option.isDefined
  val headIs = list.headOption.contains(1)
  val headMissing = list.headOption.isEmpty
  val plainEmpty = option.isEmpty
  val itself = option
  val lastItself = option
  val first = list.headOption
  val last = list.lastOption
  val head = list.head
  val tail = list.last
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
  def lone(values: List[Int]): Int = if (values.length > 1) values.head else 0
  def twice(values: List[Int]): Int = {
    val first = values.head
    first + values.head
  }
  val lifted = map.get(1)
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
  val longPresent = longMap.contains(1L)
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
    instruction.pred.map(predication => s"${render(predication)} ").getOrElse("")
}
