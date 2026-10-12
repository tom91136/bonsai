/*
rule = TransparentOpaque
 */
package fix

object TransparentOpaque {
  opaque type Metres = Double // assert: TransparentOpaque.transparent-opaque
  object Metres {
    def apply(value: Double): Metres = value
  }
  extension (metres: Metres) def value: Double = metres

  opaque type Percent = Double
  object Percent {
    def apply(value: Double): Option[Percent] = Option.when(value >= 0 && value <= 100)(value)
  }
  extension (percent: Percent) def ratio: Double = percent / 100
}
