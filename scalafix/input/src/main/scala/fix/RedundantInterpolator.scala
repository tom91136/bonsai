/*
rule = InterpolationIdioms
 */
// format: off
package fix

object RedundantInterpolator {
  val name = "x"

  val bare = s"nothing"// assert: InterpolationIdioms.redundant-interpolator
  val dollar = s"cost: $$5"// assert: InterpolationIdioms.redundant-interpolator
  val triple = s"""user_home="$$HOME""""// assert: InterpolationIdioms.redundant-interpolator
  val repeated = s"$$$$"// assert: InterpolationIdioms.redundant-interpolator

  val interpolated = s"cost: $name"
  val escaped = s"""line\n$$HOME"""
  val tabbed = s"""a\tb"""
  val single = s"line\n$$HOME"// assert: InterpolationIdioms.redundant-interpolator
  val raw = raw"nothing"
}
