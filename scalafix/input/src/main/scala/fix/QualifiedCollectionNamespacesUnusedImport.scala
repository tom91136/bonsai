/*
rule = QualifiedCollectionNamespaces
 */
package fix

import java.util.Arrays// assert: QualifiedCollectionNamespaces.member-import

object QualifiedCollectionNamespacesUnusedImport {
  val copied: Array[Int] = Array(1)
}
