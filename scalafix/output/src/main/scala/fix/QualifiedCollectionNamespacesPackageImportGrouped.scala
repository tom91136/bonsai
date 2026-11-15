package fix

import java.io

object QualifiedCollectionNamespacesPackageImportGrouped {
  val file: io.File = io.File("a")
  val copied: Array[Int] = java.util.Arrays.copyOf(Array(1), 1)
}
