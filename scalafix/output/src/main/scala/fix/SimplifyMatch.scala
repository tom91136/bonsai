// format: off
package fix

object SimplifyMatch {
  val xs = List(1, 2, 3)

  val rejected = xs.map {
case 1 => "one"
case _ => "other"
}

  val accepted = xs.map {
    case 1 => "one"
    case _ => "other"
  }
  val typed = xs.map((x: Int) => x match { case _ => "other" })
  val captures = xs.map(x => x match { case _ => x.toString })
}
