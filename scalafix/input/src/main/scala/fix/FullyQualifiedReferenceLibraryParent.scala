/*
rule = FullyQualifiedReference
 */
package fix

import java.math.BigDecimal

class FullyQualifiedReferenceJavaParent extends BigDecimal("1") {
  val bound = scala.math.max(1, 2)
}

class FullyQualifiedReferenceReadableParent extends java.io.ByteArrayOutputStream {// assert: FullyQualifiedReference.fully-qualified-reference
  val bound = scala.math.max(1, 2)// assert: FullyQualifiedReference.fully-qualified-reference
}

class FullyQualifiedReferenceScalaParent extends scala.math.Ordering[Int] {
  def compare(x: Int, y: Int): Int = 0
  val bound = scala.math.max(1, 2)
}
