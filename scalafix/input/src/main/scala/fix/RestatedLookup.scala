/*
rule = RestatedLookup
 */
package fix

import scala.language.implicitConversions

object RestatedLookup {
  enum Colour(val code: String) {
    case Red extends Colour("r")
    case Green extends Colour("g")
    case Blue extends Colour("b")

    def shortCode: String = this match { // assert: RestatedLookup.restated-reverse
      case Colour.Red   => "r"
      case Colour.Green => "g"
      case Colour.Blue  => "b"
    }
  }

  object Colour {
    val byCode = Map("r" -> Red, "g" -> Green, "b" -> Blue) // assert: RestatedLookup.restated-map

    def sorted: Set[Colour] = Set(Red, Green, Blue) // assert: RestatedLookup.restated-enumeration

    def fromCode(text: String): Colour = text match { // assert: RestatedLookup.restated-match
      case "r" => Red
      case "g" => Green
      case "b" => Blue
    }

    def codeOf(colour: Colour): String = colour match { // assert: RestatedLookup.restated-reverse
      case Red   => "r"
      case Green => "g"
      case Blue  => "b"
    }
  }

  val Lookup  = Map("r" -> Colour.Red, "g" -> Colour.Green, "b" -> Colour.Blue) // assert: RestatedLookup.restated-map
  val Applied = Map.apply( // assert: RestatedLookup.restated-map
    "r" -> Colour.Red,
    "g" -> Colour.Green,
    "b" -> Colour.Blue
  )
  val Pairs = Vector( // assert: RestatedLookup.restated-map
    "r" -> Colour.Red,
    "g" -> Colour.Green,
    "b" -> Colour.Blue
  ).toMap
  val Tuples = Seq( // assert: RestatedLookup.restated-map
    ("r", Colour.Red),
    ("g", Colour.Green),
    ("b", Colour.Blue)
  ).toMap
  val Colours = Set(Colour.Red, Colour.Green, Colour.Blue) // assert: RestatedLookup.restated-enumeration

  def codeOf(colour: Colour): String = colour match { // assert: RestatedLookup.restated-reverse
    case Colour.Red   => "r"
    case Colour.Green => "g"
    case Colour.Blue  => "b"
  }

  enum Level {
    case Low, Medium, High
  }

  val Names = Map( // assert: RestatedLookup.restated-map
    "Low" -> Level.Low,
    "Medium" -> Level.Medium,
    "High" -> Level.High
  )
  val Ordered = Seq(Level.Low, Level.Medium, Level.High) // assert: RestatedLookup.restated-enumeration
  val Sets    = Set(Level.High, Level.Low, Level.Medium) // assert: RestatedLookup.restated-enumeration
  val Lists   = List(Level.High, Level.Low, Level.Medium)

  enum Step(val delta: Int) {
    case Up extends Step(1)
    case Down extends Step(-1)
    case Stay extends Step(0)
  }

  val Steps = Map(1 -> Step.Up, -1 -> Step.Down, 0 -> Step.Stay) // assert: RestatedLookup.restated-map

  final case class Refined(raw: String)

  object Refined {
    given Conversion[String, Refined] = Refined(_)
  }

  enum Ftz(val name: Refined) {
    case Fast extends Ftz("fast")
    case Slow extends Ftz("slow")
    case Mid extends Ftz("mid")
  }

  val Speeds = Map("fast" -> Ftz.Fast, "slow" -> Ftz.Slow, "mid" -> Ftz.Mid) // assert: RestatedLookup.restated-map

  object Imported {
    import RestatedLookup.Level.*

    val all = Set(Low, Medium, High) // assert: RestatedLookup.restated-enumeration
  }

  enum Shade(val code: String) {
    case Light extends Shade("l")
    case Dark extends Shade("d")
    case Mid extends Shade("m")
    case Deep extends Shade("p")
  }

  val Subset  = Map("l" -> Shade.Light, "d" -> Shade.Dark, "m" -> Shade.Mid)
  val Shades  = Set(Shade.Light, Shade.Dark, Shade.Mid)
  val Partial = Map("r" -> Colour.Red, "x" -> Colour.Green, "b" -> Colour.Blue)
  val Few     = Map("r" -> Colour.Red, "g" -> Colour.Green)
  val Mixed   = Map("r" -> Colour.Red, "g" -> Level.Low, "b" -> Colour.Blue)
  val Literal = Map("r" -> "red", "g" -> "green", "b" -> "blue")
  val Repeats = Set(Colour.Red, Colour.Green, Colour.Green)

  enum Tag {
    case Alpha, Beta, Gamma

    override def toString: String = "tag"
  }

  val Tags = Map("Alpha" -> Tag.Alpha, "Beta" -> Tag.Beta, "Gamma" -> Tag.Gamma)

  enum Modifier(val name: String) {
    case Add extends Modifier("add")
    case Mul extends Modifier("mul")
    case Sub extends Modifier("sub")
    case Unrecognised(spelling: String) extends Modifier(spelling)
  }

  object Modifier {
    val known = Vector(Add, Mul, Sub)
  }

  val Modifiers = Vector(Modifier.Add, Modifier.Mul, Modifier.Sub)

  object Reversed {
    def partial(colour: Colour): String = colour match {
      case Colour.Red   => "r"
      case Colour.Green => "g"
      case _            => "other"
    }

    def mismatched(colour: Colour): String = colour match {
      case Colour.Red   => "r"
      case Colour.Green => "x"
      case Colour.Blue  => "b"
    }

    def named(colour: Colour): String = colour match {
      case Colour.Red   => colour.toString
      case Colour.Green => "g"
      case Colour.Blue  => "b"
    }

    def modifier(modifier: Modifier): String = modifier match {
      case Modifier.Add             => "add"
      case Modifier.Mul             => "mul"
      case Modifier.Sub             => "sub"
      case Modifier.Unrecognised(_) => "other"
    }
  }

  object Lookups {
    def fallback(text: String): Option[Colour] = text match { // assert: RestatedLookup.restated-match
      case "r" => Some(Colour.Red)
      case "g" => Some(Colour.Green)
      case "b" => Some(Colour.Blue)
      case _   => None
    }

    def guarded(text: String, flag: Boolean): Option[Colour] = text match {
      case "r" if flag => Some(Colour.Red)
      case "g"         => Some(Colour.Green)
      case "b"         => Some(Colour.Blue)
      case _           => None
    }

    def mixed(text: String): Any = text match {
      case "r" => Some(Colour.Red)
      case "g" => Colour.Green
      case "b" => Some(Colour.Blue)
    }

    def partial(text: String): Option[Colour] = text match {
      case "r" => Some(Colour.Red)
      case "q" => Some(Colour.Green)
      case "b" => Some(Colour.Blue)
    }

    def subset(text: String): Option[Shade] = text match {
      case "l" => Some(Shade.Light)
      case "d" => Some(Shade.Dark)
      case "m" => Some(Shade.Mid)
    }
  }
}
