package net.kurobako.bonsai.analysis

object Node {

  /** Whether `inner` is `outer` or a type declared, at any depth, inside it. */
  @annotation.tailrec
  def within(nodes: collection.Map[String, Node], inner: String, outer: String): Boolean =
    inner == outer ||
      (nodes.get(inner).flatMap(_.outer) match {
        case Some(enclosing) => within(nodes, enclosing, outer)
        case None            => false
      })
}

/** A type, with its companion merged in, or a top-level definition; `exported` when code outside the build uses it, as
  * the public API of a library or a type the build names reflectively, and `suppressed` holds the rule ids its
  * suppressing annotations name, with `*` for every rule.
  */
case class Node(
    name: String,
    module: String,
    scope: Output.Scope,
    source: String,
    line: Int,
    generated: Boolean,
    exported: Boolean,
    outer: Option[String],
    suppressed: Set[String] = Set.empty
) {
  def test: Boolean = scope == Output.Scope.Test
}
