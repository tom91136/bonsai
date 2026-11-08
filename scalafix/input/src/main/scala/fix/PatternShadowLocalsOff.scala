/*
rule = PatternShadow
PatternShadow.locals = false
 */
package fix

object PatternShadowLocalsOff {
  class Report(version: String) {
    def report(xs: Vector[Int]): String = {
      val version = xs.mkString
      version
    }
  }
}
