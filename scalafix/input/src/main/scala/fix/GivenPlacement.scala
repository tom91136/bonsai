/*
rule = GivenPlacement
 */
package fix

object GivenPlacement {
  case class Celsius(value: Double)
  case class Kelvin(value: Double)
  object Celsius {
    given Ordering[Celsius]           = Ordering.by(_.value)
    given Conversion[Double, Celsius] = Celsius(_)
  }

  object Instances {
    given Ordering[Celsius]           = Ordering.by(-_.value)          /* assert: GivenPlacement.orphan-given
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
an instance outside the companions of its types must be imported and may differ between call sites; for types the project does not own, suppress with `// scalafix:ok GivenPlacement`
*/
    given Int                         = 42                             // assert: GivenPlacement.ambient-given
    given Conversion[String, Celsius] = text => Celsius(text.toDouble) // assert: GivenPlacement.conversion-placement
  }

  object Bodies {
    given Ordering[Kelvin] with { // assert: GivenPlacement.orphan-given
      def compare(left: Kelvin, right: Kelvin): Int = left.value.compare(right.value)
    }
    given Conversion[String, Kelvin] with { // assert: GivenPlacement.conversion-placement
      def apply(text: String): Kelvin = Kelvin(text.toDouble)
    }
  }

  object Internal {
    private given Ordering[Vector[BigInt]] = Ordering.Implicits.seqOrdering[Vector, BigInt]
  }

  def local(): Int = {
    given Int = 1
    summon[Int]
  }
}
