package net.kurobako.bonsai.analysis

private[analysis] object Maps {

  /** The entries of `left` with those of `right` added, combined where both hold a key. */
  def join[K, V](left: Map[K, V], right: Map[K, V])(combine: (V, V) => V): Map[K, V] =
    right.foldLeft(left)((joined, entry) =>
      joined.updated(entry._1, joined.get(entry._1).fold(entry._2)(combine(_, entry._2)))
    )
}
