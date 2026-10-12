package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Keep primitive domains in Scala types and make explicit boxing an interop exception. */
class NoBoxedPrimitives extends SemanticRule("NoBoxedPrimitives") {
  private val boxedTypes = Map(
    "java/lang/Boolean#"   -> "Boolean",
    "java/lang/Byte#"      -> "Byte",
    "java/lang/Short#"     -> "Short",
    "java/lang/Integer#"   -> "Int",
    "java/lang/Long#"      -> "Long",
    "java/lang/Float#"     -> "Float",
    "java/lang/Double#"    -> "Double",
    "java/lang/Character#" -> "Char",
    "java/lang/Void#"      -> "Unit"
  )

  private val conversions = Map(
    "booleanValue" -> "toBoolean",
    "byteValue"    -> "toByte",
    "shortValue"   -> "toShort",
    "intValue"     -> "toInt",
    "longValue"    -> "toLong",
    "floatValue"   -> "toFloat",
    "doubleValue"  -> "toDouble",
    "charValue"    -> "toChar"
  )

  private val boxingMembers = Set("valueOf", "decode", "`<init>`", "TRUE", "FALSE")

  private val numericOwners = boxedTypes.keySet ++ Set(
    "java/lang/Number#",
    "java/math/BigInteger#",
    "java/math/BigDecimal#",
    "scala/math/ScalaNumber#",
    "scala/math/ScalaNumericConversions#",
    "scala/math/BigInt#",
    "scala/math/BigDecimal#"
  )

  override def fix(using document: SemanticDocument): Patch = document.tree.collect {
    case reference: Type.Ref if !references.isSelectionName(reference) =>
      boxedTypes.get(reference.symbol.value).map { replacement =>
        lint("boxed-type", s"use Scala $replacement instead of a Java boxed type", reference.pos)
      }
    case reference: Term.Ref if !references.isSelectionName(reference) =>
      val symbol = reference.symbol.value
      val owner  = symbol.take(symbol.lastIndexOf('#') + 1)
      val member = symbol.drop(owner.length).takeWhile(character => character != '(' && character != '.')
      conversions.get(member).filter(_ => numericOwners(owner)).map { replacement =>
        val message = if (member == "booleanValue" || member == "charValue")
          "keep a Scala primitive instead of explicit Java unboxing; isolate required Java interop"
        else s"use $replacement on a Scala numeric value; isolate required Java unboxing at the interop boundary"
        lint("unboxing", message, reference.pos)
      }.orElse {
        val constructor = boxedTypes.contains(symbol) && reference.parent.exists {
          case call: Term.Apply => call.fun == reference
          case _                => false
        }
        Option.when(constructor || (boxedTypes.contains(owner) && boxingMembers(member)))(
          lint("boxing", "use a Scala primitive; isolate required Java boxing at the interop boundary", reference.pos)
        )
      }
  }.flatten.asPatch
}
