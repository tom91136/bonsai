package net.kurobako.bonsai.analysis

/** Frequent itemsets, as shared field groups among sum cases or parameter groups among methods. */
object Itemsets {

  /** Whether no larger candidate contains `items`. */
  def maximal(items: Set[String], candidates: Iterable[Set[String]]): Boolean =
    !candidates.exists(other => other.size > items.size && items.subsetOf(other))
}
