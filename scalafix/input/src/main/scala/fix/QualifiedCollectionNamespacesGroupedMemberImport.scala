/*
rule = QualifiedCollectionNamespaces
 */
package fix

import java.util.{Arrays, Base64}// assert: QualifiedCollectionNamespaces.member-import

object QualifiedCollectionNamespacesGroupedMemberImport {
  val copied: Array[Int] = Arrays.copyOf(Array(1), 1)// assert: QualifiedCollectionNamespaces.unqualified
}
