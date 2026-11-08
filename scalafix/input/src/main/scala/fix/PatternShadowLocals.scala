/*
rule = PatternShadow
PatternShadow.locals = true
 */
package fix

object PatternShadowLocals {
  type Count = Int

  val version = "1"

  final val ceiling = 9

  def describe(xs: Vector[Int]): String = {
    val version = xs.mkString // assert: PatternShadow.local-shadow
    version
  }

  class Report(retained: Int) {
    private def repaired(o: Int): String = o.toString
    private val limit                    = 2
    private val tally: Count             = 0
    private var hits                     = 0
    val label                            = "report"

    def total: Int = {
      val total = hits
      total
    }

    def weight: Int = {
      val weight = hits
      weight
    }

    def report(xs: Vector[Int]): String = {
      val repaired = xs.count(_ > 0)
      s"$repaired"
    }

    def bounded(xs: Vector[Int]): Int = {
      def limit(x: Int): Int = x
      limit(xs.sum)
    }

    def capped(xs: Vector[Int]): Int = {
      val ceiling = xs.size
      ceiling
    }

    def clamp(bound: Int): Int = {
      val limit = bound // assert: PatternShadow.local-shadow
      limit
    }

    def counted(xs: Vector[Int]): Int = {
      var hits = 0 // assert: PatternShadow.local-shadow
      xs.foreach(x => hits += x)
      hits
    }

    def paired(xs: Vector[Int]): String = {
      val (label, rest) = (xs.mkString, xs.size) // assert: PatternShadow.local-shadow
      label + rest
    }

    def sized(xs: Vector[Int]): Int = {
      val retained = xs.size // assert: PatternShadow.local-shadow
      retained
    }

    def nested(xs: Vector[Int]): Int = {
      if (xs.nonEmpty) {
        val tally = xs.size // assert: PatternShadow.local-shadow
        tally
      } else 0
    }

    def deep(xs: Vector[Int]): Vector[Int] = xs.map { x =>
      val hits = x // assert: PatternShadow.local-shadow
      hits + 1
    }

    def scalar(xs: Vector[Int]): Int = {
      def total: Int = xs.sum // assert: PatternShadow.local-shadow
      total
    }

    def fixed(xs: Vector[Int]): String = {
      def repaired(o: Int): String = o.toString // assert: PatternShadow.local-shadow
      repaired(xs.size)
    }

    def stacked(xs: Vector[Int]): Int = {
      val total = xs.size // assert: PatternShadow.local-shadow
      def inner: Int = {
        val total = 1
        total
      }
      total + inner
    }

    def mismatched(xs: Vector[Int]): Int = {
      val label = xs.size
      label
    }

    def rename(xs: Vector[Int]): String = {
      def repaired(o: Int): Int = o
      repaired(xs.size).toString
    }

    def matched(shape: Option[Int]): Int = shape match {
      case Some(count) =>
        val total = count // assert: PatternShadow.local-shadow
        total
      case None => 0
    }

    def bound(shape: Option[Int]): Int = shape match {
      case Some(total) =>
        val total = 2
        total
      case None => 0
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
