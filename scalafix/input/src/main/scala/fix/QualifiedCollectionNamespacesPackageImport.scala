/*
rule = QualifiedCollectionNamespaces
 */
package fix

import java.util

object QualifiedCollectionNamespacesPackageImport {
  val copied: Array[Int] = util.Arrays.copyOf(Array(1), 1)// assert: QualifiedCollectionNamespaces.unqualified
  val empty: util.List[Int] = util.Collections.emptyList[Int]()// assert: QualifiedCollectionNamespaces.unqualified
}
