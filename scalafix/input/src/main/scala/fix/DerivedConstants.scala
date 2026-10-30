/*
rule = DerivedConstants
 */
package fix

object DerivedConstants {
  val Window         = 0x100000000L
  val ParameterBase  = 0x700000000L
  val ParameterLimit = 0x800000000L // assert: DerivedConstants.derived-constant

  val RegionBase = 0x4800L
  val RegionSize = 0x1800L
  val RegionEnd  = 0x6000L // assert: DerivedConstants.derived-constant
  val Gap        = 0x3000L

  val Read      = 0x1000L
  val Write     = 0x2000L
  val ReadWrite = 0x3000L

  val Stride       = 0x1200
  val StrideDouble = 0x2400
  val Unrelated    = 0x2400

  val RequestMagic  = 0x474d4533
  val ResponseMagic = 0x474d4633

  val Written = RegionBase + RegionSize
  val Mask    = 0xffffffL
}

object PowersOfTwo {
  val SharedBase    = 0x10000000L
  val SharedLimit   = 0x40000000L
  val ClusterWindow = 0x30000000L
  val SharedWindow  = 0x30000000L
  val Page          = 0x1000L
  val Pages         = 0x2000L
  val Span          = 0x1000L
}

object SmallConstants {
  val One   = 1
  val Two   = 2
  val Three = 3
  val Mask  = 0xff
  val Low   = 0x10
  val High  = 0x20
  val Both  = 0x30
}

object Computed {
  val Base  = 0x700000000L
  val Limit = Base + 0x100000000L
  val Size  = 0x6000L
}

object Unrelated {
  val ColourRead   = 0x1400L
  val GroupMax     = 0x1600L
  val GroupUniform = 0x2a00L
}

object Mixed {
  val Heap      = 0x5800L
  val HeapTop   = 0x5900L // assert: DerivedConstants.derived-constant
  val HeapOther = 0x7123L

  @SuppressWarnings(Array("scalafix:DerivedConstants"))
  val HeapSuppressed = 0x5a00L
}

object IntegerConstants {
  val FrameBase = 0x4800
  val FrameNext = 0x4900 /* assert: DerivedConstants.derived-constant
  ^^^^^^^^^^^^^^^^^^^^^^
FrameNext is a literal equal to FrameBase + 0x100; write the expression, so it follows the constants it comes from
*/
  val ChunkHeader = 0x1200
  val ChunkBody   = 0x2400
  val ChunkTotal  = 0x3600 // assert: DerivedConstants.derived-constant
}

object Descending {
  val SlotLimit = 0x4900L
  val SlotTop   = 0x4800L /* assert: DerivedConstants.derived-constant
  ^^^^^^^^^^^^^^^^^^^^^^^
SlotTop is a literal equal to SlotLimit - 0x100L; write the expression, so it follows the constants it comes from
*/
}
