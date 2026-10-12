/*
rule = GivenPlacement
 */
package fix

object GivenPlacement {
  case class Celsius(value: Double)
  object Celsius {
    given Ordering[Celsius]           = Ordering.by(_.value)
    given Conversion[Double, Celsius] = Celsius(_)
  }

  object Instances {
    given Ordering[Celsius]           = Ordering.by(-_.value)          // assert: GivenPlacement.orphan-given
    given Int                         = 42                             // assert: GivenPlacement.ambient-given
    given Conversion[String, Celsius] = text => Celsius(text.toDouble) // assert: GivenPlacement.conversion-placement
  }

  object Internal {
    private given Ordering[Vector[BigInt]] = Ordering.Implicits.seqOrdering[Vector, BigInt]
  }

  def local(): Int = {
    given Int = 1
    summon[Int]
  }
}
