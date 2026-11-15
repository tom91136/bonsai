package fix

import java.io.ByteArrayOutputStream
import java.math.BigDecimal
import scala.math.max

class FullyQualifiedReferenceJavaParent extends BigDecimal("1") {
  val bound = scala.math.max(1, 2)
}

class FullyQualifiedReferenceReadableParent extends ByteArrayOutputStream {
  val bound = max(1, 2)
}

class FullyQualifiedReferenceScalaParent extends scala.math.Ordering[Int] {
  def compare(x: Int, y: Int): Int = 0
  val bound = scala.math.max(1, 2)
}
