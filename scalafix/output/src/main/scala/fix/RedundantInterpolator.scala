// format: off
package fix

object RedundantInterpolator {
  val name = "x"

  val bare = "nothing"
  val dollar = "cost: $5"
  val triple = """user_home="$HOME""""
  val repeated = "$$"

  val interpolated = s"cost: $name"
  val escaped = s"""line\n$$HOME"""
  val tabbed = s"""a\tb"""
  val single = "line\n$HOME"
  val raw = raw"nothing"
}
