/*
rule = QualifiedCollectionNamespaces
 */
package fix

import scala.collection.mutable.ArrayBuffer// assert: QualifiedCollectionNamespaces.member-import

object QualifiedCollectionNamespacesImport {
  val values = ArrayBuffer.empty[Int]// assert: QualifiedCollectionNamespaces.unqualified
}
