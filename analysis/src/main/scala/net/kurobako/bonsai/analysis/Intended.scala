package net.kurobako.bonsai.analysis

import net.kurobako.bonsai.lifting.Globs

/** Pairs of types, written `<glob>=<glob>` over qualified names as for the hot globs, whose copies or likeness are
  * meant, as the execute and render instances of one operation are. A group of copies or of types of one shape is
  * dropped when every two of its members are an intended pair, in either order.
  */
case class Intended(pairs: Vector[(String, String)] = Vector.empty) {
  private lazy val compiled =
    pairs.map((left, right) => Globs.qualifiedNames(left) -> Globs.qualifiedNames(right))

  private def covers(left: String, right: String): Boolean = compiled.exists((first, second) =>
    Globs.matches(first, left) && Globs.matches(second, right) ||
      Globs.matches(first, right) && Globs.matches(second, left)
  )

  /** Whether the group has several members and every two of them are an intended pair. */
  def settles(members: Seq[String]): Boolean =
    members.size > 1 && members.indices.combinations(2).forall(pair => covers(members(pair.head), members(pair.last)))
}
