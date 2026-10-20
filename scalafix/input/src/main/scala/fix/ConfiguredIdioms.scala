/*
rule = ConfiguredIdioms
ConfiguredIdioms.idioms = [
  { pattern = "($v << (64 - $w)) >> (64 - $w)", helper = "fix.ConfiguredIdioms.Bits.signExtend", message = "use Bits.signExtend" }
  { pattern = "(floatToBits($x) >>> 23) & 0xff", helper = "fix.ConfiguredIdioms.Floats.exponent" }
]
 */
package fix

object ConfiguredIdioms {
  def floatToBits(value: Float): Int = java.lang.Float.floatToRawIntBits(value)

  object Bits {
    def signExtend(value: Long, width: Int): Long = (value << (64 - width)) >> (64 - width)
  }

  object Inlined {
    inline def extend(value: Long, width: Int): Long = (value << (64 - width)) >> (64 - width)
  }

  object Unrelated {
    def signExtend(value: Long, width: Int): Long = (value << (64 - width)) >> (64 - width) // assert: ConfiguredIdioms.configured-idiom
  }

  val value: Long = 0xffL
  val width: Int  = 8
  val other: Int  = 16

  val direct    = (value << (64 - width)) >> (64 - width)       // assert: ConfiguredIdioms.configured-idiom
  val bare      = (value << 64 - width) >> 64 - width           // assert: ConfiguredIdioms.configured-idiom
  val widened   = ((value << (64L - width)) >> (64L - width))   // assert: ConfiguredIdioms.configured-idiom
  val composite = (value + 1 << (64 - (width + 1))) >> (64 - (width + 1)) // assert: ConfiguredIdioms.configured-idiom
  val nested    = Bits.signExtend((value << (64 - width)) >> (64 - width), 4) // assert: ConfiguredIdioms.configured-idiom
  val exponent  = (floatToBits(1.5f) >>> 23) & 0xff /* assert: ConfiguredIdioms.configured-idiom
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
`(floatToBits($x) >>> 23) & 0xff` is `fix.ConfiguredIdioms.Floats.exponent`
*/
  val hex       = (floatToBits(1.5f) >>> 23) & 255              // assert: ConfiguredIdioms.configured-idiom

  val mismatched = (value << (64 - width)) >> (64 - other)
  val narrower   = (value << (32 - width)) >> (32 - width)
  val unsigned   = (value << (64 - width)) >>> (64 - width)
  val helper     = Bits.signExtend(value, width)
  val shifted    = (floatToBits(1.5f) >> 23) & 0xff
}
