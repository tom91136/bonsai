/*
rules = [RedundantSyntax, InterpolationIdioms]
 */
package fix

object RedundantSyntaxEscape {
  val triple = s"""user_home="$$HOME""""// assert: InterpolationIdioms.redundant-interpolator
}
