package fix



import scala.annotation.tailrec

object QualifiedCollectionNamespacesImportBlock {
  @tailrec def copied(n: Int): Array[Int] = if (n > 0) copied(n - 1) else java.util.Arrays.copyOf(Array(1), 1)
}
