/*
rule = QualifiedCollectionNamespaces
 */
package fix

import java.util.Arrays// assert: QualifiedCollectionNamespaces.member-import

object QualifiedCollectionNamespacesLoneImport {
  val copied: Array[Int] = Arrays.copyOf(Array(1), 1)// assert: QualifiedCollectionNamespaces.unqualified
}
