/*
rule = QualifiedCollectionNamespaces
 */
package fix

import java.util.Arrays// assert: QualifiedCollectionNamespaces.member-import

import scala.annotation.tailrec

object QualifiedCollectionNamespacesImportBlock {
  @tailrec def copied(n: Int): Array[Int] = if (n > 0) copied(n - 1) else Arrays.copyOf(Array(1), 1)// assert: QualifiedCollectionNamespaces.unqualified
}
