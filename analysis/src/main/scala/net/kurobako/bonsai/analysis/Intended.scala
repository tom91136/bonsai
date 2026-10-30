package net.kurobako.bonsai.analysis

import scala.util.matching.Regex

object Intended {

  // The compiler plugin, which depends on nothing of the analysis, reads its hot globs the same way.
  /** A glob over qualified names, matched against a name with a dot appended: `*` stands for one name segment or part
    * of one, and `**` for any number of segments.
    */
  @SuppressWarnings(Array("bonsai:duplicate-code"))
  def qualifiedNames(glob: String): Regex = glob.split('.').map {
    case "**"    => "(?:[^.]+\\.)*"
    case segment => segment.split("\\*", -1).map(Regex.quote).mkString("[^.]*") + "\\."
  }.mkString.r

  /** A pair as written, `<glob>=<glob>`. */
  def parse(text: String): Either[String, (String, String)] = text.split("=", -1) match {
    case Array(left, right) if left.nonEmpty && right.nonEmpty => Right(left -> right)
    case _ => Left(s"an intended pair is written <glob>=<glob>, not $text")
  }
}

/** Pairs of types, written `<glob>=<glob>` over qualified names as for the hot globs, whose copies or likeness are
  * meant, as the execute and render instances of one operation are. A group of copies or of types of one shape is
  * dropped when every two of its members are an intended pair, in either order.
  */
case class Intended(pairs: Vector[(String, String)] = Vector.empty) {
  private lazy val compiled =
    pairs.map((left, right) => Intended.qualifiedNames(left) -> Intended.qualifiedNames(right))

  private def matches(glob: Regex, name: String): Boolean = glob.matches(s"$name.")

  def covers(left: String, right: String): Boolean = compiled.exists((first, second) =>
    matches(first, left) && matches(second, right) || matches(first, right) && matches(second, left)
  )

  /** Whether the group has several members and every two of them are an intended pair. */
  def settles(members: Seq[String]): Boolean =
    members.size > 1 && members.indices.combinations(2).forall(pair => covers(members(pair.head), members(pair.last)))
}
