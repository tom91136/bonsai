/*
rule = NoBoxedPrimitives
 */
package fix

import java.lang.{Integer => JInt}
import scala.annotation.nowarn

object NoBoxedPrimitives {
  val boolean: java.lang.Boolean = ??? // assert: NoBoxedPrimitives.boxed-type
  val byte: java.lang.Byte       = ??? // assert: NoBoxedPrimitives.boxed-type
  val short: java.lang.Short     = ??? // assert: NoBoxedPrimitives.boxed-type
  val integer: Integer           = ??? // assert: NoBoxedPrimitives.boxed-type
  val long: java.lang.Long       = ??? // assert: NoBoxedPrimitives.boxed-type
  val float: java.lang.Float     = ??? // assert: NoBoxedPrimitives.boxed-type
  val double: java.lang.Double   = ??? // assert: NoBoxedPrimitives.boxed-type
  val character: Character       = ??? // assert: NoBoxedPrimitives.boxed-type
  val void: java.lang.Void       = ??? // assert: NoBoxedPrimitives.boxed-type
  type Alias = JInt // assert: NoBoxedPrimitives.boxed-type
  val nested: List[JInt]    = Nil                          // assert: NoBoxedPrimitives.boxed-type
  @nowarn("cat=deprecation") val constructed           = new JInt(1) // assert: NoBoxedPrimitives.boxed-type
  @nowarn("cat=deprecation") val constructedWithoutNew = JInt(1)     // assert: NoBoxedPrimitives.boxing
  val boxed                 = JInt.valueOf(1)              // assert: NoBoxedPrimitives.boxing
  val qualifiedBox          = java.lang.Integer.valueOf(2) // assert: NoBoxedPrimitives.boxing
  val decoded               = JInt.decode("0x10")          // assert: NoBoxedPrimitives.boxing
  val boxedTrue             = java.lang.Boolean.TRUE       // assert: NoBoxedPrimitives.boxing
  val boxedFalse            = java.lang.Boolean.FALSE      // assert: NoBoxedPrimitives.boxing
  val classToken            = classOf[JInt]                // assert: NoBoxedPrimitives.boxed-type

  val a                                                = boolean.booleanValue() // assert: NoBoxedPrimitives.unboxing
  val b                                                = byte.byteValue()       // assert: NoBoxedPrimitives.unboxing
  val c                                                = short.shortValue()     // assert: NoBoxedPrimitives.unboxing
  val d                                                = integer.intValue()     // assert: NoBoxedPrimitives.unboxing
  val e                                                = long.longValue()       // assert: NoBoxedPrimitives.unboxing
  val f                                                = float.floatValue()     // assert: NoBoxedPrimitives.unboxing
  val g                                                = double.doubleValue()   // assert: NoBoxedPrimitives.unboxing
  val h                                                = character.charValue()  // assert: NoBoxedPrimitives.unboxing
  def number(value: Number): Long                      = value.longValue()      // assert: NoBoxedPrimitives.unboxing
  val scalaBigInt                                      = BigInt(1).longValue    // assert: NoBoxedPrimitives.unboxing
  val scalaBigDecimal                                  = BigDecimal(1).intValue // assert: NoBoxedPrimitives.unboxing
  def javaBigInt(value: java.math.BigInteger): Double  = value.doubleValue()    // assert: NoBoxedPrimitives.unboxing
  def javaBigDecimal(value: java.math.BigDecimal): Int = value.intValue()       // assert: NoBoxedPrimitives.unboxing
  val eta: () => Int                                   = integer.intValue       // assert: NoBoxedPrimitives.unboxing

  val primitive: Int = 1
  val parsed         = JInt.parseInt("12")
  val rotated        = JInt.rotateLeft(primitive, 3)
  val bits           = java.lang.Double.doubleToRawLongBits(1.0)
  val limit          = JInt.MAX_VALUE
  val primitiveClass = JInt.TYPE
  val converted      = BigInt(1).toLong
  val atomic         = java.util.concurrent.atomic.AtomicInteger(0)

  object Shadowed {
    class Integer {
      def intValue(): Int = 1
    }
    object Integer {
      def valueOf(value: Int): Int = value
    }
    val local: Integer = new Integer
    val value          = local.intValue()
    val factory        = Integer.valueOf(1)
  }

  @SuppressWarnings(Array("scalafix:NoBoxedPrimitives"))
  def javaBoundary(value: JInt): Int = value.intValue()
}
