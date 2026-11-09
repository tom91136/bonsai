/*
rule = QualifiedCollectionNamespaces
 */
package fix

import java.util.Base64// assert: QualifiedCollectionNamespaces.member-import
import java.util.UUID// assert: QualifiedCollectionNamespaces.member-import

object QualifiedCollectionNamespacesJavaOnly {
  val encoded = Base64.getEncoder// assert: QualifiedCollectionNamespaces.unqualified
  val id = UUID.randomUUID// assert: QualifiedCollectionNamespaces.unqualified
}
