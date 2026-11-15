/*
rule = QualifiedCollectionNamespaces
 */
package fix

import java.{io, util}

object QualifiedCollectionNamespacesPackageImportGrouped {
  val file: io.File = io.File("a")
  val copied: Array[Int] = util.Arrays.copyOf(Array(1), 1)// assert: QualifiedCollectionNamespaces.unqualified
}
