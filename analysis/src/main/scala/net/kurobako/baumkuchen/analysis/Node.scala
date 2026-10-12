package net.kurobako.baumkuchen.analysis

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

/** A type, with its companion merged in, or a top-level definition; `exported` when it is part of the public API of a
  * library, and `suppressed` holds the rule ids its suppressing annotations name, with `*` for every rule.
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
