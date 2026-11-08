/*
rule = PatternShadow
PatternShadow.locals = true
 */
package fix

object PatternShadowLocals {
  class Report(retained: Int) {
    private def repaired(o: Int): String = o.toString
    private val limit                    = 2
    val label                            = "report"

    def report(xs: Vector[Int]): String = {
      val repaired = xs.count(_ > 0) // assert: PatternShadow.local-shadow
      s"$repaired"
    }

    def bounded(xs: Vector[Int]): Int = {
      def limit(x: Int): Int = x // assert: PatternShadow.local-shadow
      limit(xs.sum)
    }

    def counted(xs: Vector[Int]): Int = {
      var label = 0 // assert: PatternShadow.local-shadow
      xs.foreach(x => label += x)
      label
    }

    def paired(xs: Vector[Int]): Int = {
      val (retained, rest) = xs.partition(_ > 0) // assert: PatternShadow.local-shadow
      retained.size + rest.size
    }

    def fresh(xs: Vector[Int]): Int = {
      val kept  = xs.count(_ > limit)
      val other = kept + retained
      other
    }

    def nested(xs: Vector[Int]): Int = {
      def helper(ys: Vector[Int]): Int = {
        val helper = ys.size // assert: PatternShadow.local-shadow
        helper
      }
      helper(xs)
    }
  }

  object Sibling {
    def report(xs: Vector[Int]): Int = {
      val repaired = xs.size
      val label    = repaired
      label
    }
  }
}
