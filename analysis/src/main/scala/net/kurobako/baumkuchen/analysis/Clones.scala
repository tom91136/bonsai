package net.kurobako.baumkuchen.analysis

/** Code fragments whose structure recurs elsewhere in the build, up to the renaming of local definitions and, for near
  * duplicates, the values of literals.
  */
object Clones {

  /** A copy; `family` names the overloads or numbered methods its method belongs to, and `specialised` holds when its
    * method is inline and expands inline methods on literals, which near duplicates of it are meant to differ in.
    */
  case class Fragment(
      node: String,
      path: String,
      start: Int,
      end: Int,
      test: Boolean,
      family: String = "",
      specialised: Boolean = false
  ) {
    def lines: Int = end - start + 1
  }

  /** Copies of one fragment; `cost` counts the tree nodes of each copy, and each of the `holes` of a near duplicate
    * holds the distinct values of one literal position across the copies.
    */
  case class Group(
      cost: Int,
      fragments: Vector[Fragment],
      holes: Vector[Vector[String]] = Vector.empty,
      mapping: Boolean = false
  )

  private val ShownCopies = 4

  // A file repeating itself this often follows one template or table, which a finding per copy would bury.
  private val MaximumPerFile = 5
  private val ShownGroups    = 3
  private val Width          = "[0-9]+".r

  /** A finding per group of copies, except that a file holding more than `MaximumPerFile` groups of one rule entirely
    * within itself gets one finding naming its largest. Copies split between main code and tests are information, as an
    * independent test oracle is meant to repeat what it checks. Groups whose copies are all intended pairs are left
    * out.
    */
  def findings(groups: Vector[Group], nodes: Map[String, Node], intended: Intended = Intended()): Vector[Finding] = {
    // Overloads one per width are what numbered-family asks for, and literals an inline method specialises on are its
    // point, so near duplicates among them are by design.
    val designed = groups.filterNot(group =>
      group.holes.nonEmpty &&
        (group.fragments.forall(_.specialised) ||
          group.fragments.map(_.family).distinct.forall(_.nonEmpty) &&
          group.fragments.map(_.family).distinct.size == 1 ||
          widthsOfCompanions(group))
    ).filterNot(group => intended.settles(group.fragments.map(_.node)))
    val described        = designed.map(describe(_))
    val (within, spread) = described.partition(_.group.fragments.map(_.path).distinct.size == 1)
    val crowded          = within.groupBy(entry => (entry.group.fragments.head.path, entry.rule))
      .filter((_, entries) => entries.size > MaximumPerFile)
    val single = (spread ++ within.filterNot(entry => crowded.contains((entry.group.fragments.head.path, entry.rule))))
      .flatMap { entry =>
        val fragments = entry.group.fragments
        val tests     = fragments.count(_.test)
        val level     = if (tests > 0) Finding.Level.Info else Finding.Level.Warning
        val detail    =
          if (tests > 0 && tests < fragments.size) s"${entry.detail} (copies in main code and tests)" else entry.detail
        Finding.at(nodes, fragments.head.node, entry.rule, detail, level, onlyTests = tests == fragments.size)
      }
    val summaries = crowded.toVector.sortBy(_._1._1).flatMap { case ((path, rule), entries) =>
      val largest = entries.map(_.group).sortBy(group => -group.cost).take(ShownGroups).map(group =>
        group.fragments.map(fragment => s"${fragment.start}-${fragment.end}").mkString("{", ", ", "}")
      )
      Finding.at(
        nodes,
        entries.head.group.fragments.head.node,
        rule,
        s"${entries.size} groups of copies repeat within $path, the largest at lines ${largest.mkString(", ")}; " +
          "they follow one template or table, so fix it at its source",
        onlyTests = entries.forall(_.group.fragments.forall(_.test))
      )
    }
    single ++ summaries
  }

  // Members of sibling companions of distinct types, each holding only the width of its own type.
  private def widthsOfCompanions(group: Group): Boolean = {
    val owners = group.fragments.map(_.node.split('.').last)
    val widths = owners.map(Width.findFirstIn(_))
    owners.distinct.size == owners.size && widths.forall(_.isDefined) && group.holes.nonEmpty &&
    group.holes.forall(_.toSet == widths.flatten.toSet)
  }

  private case class Described(group: Group, rule: Finding.Rule, detail: String)

  private def describe(group: Group): Described = {
    val first     = group.fragments.head
    val locations =
      group.fragments.map(fragment => s"${fragment.path}:${fragment.start}-${fragment.end}").mkString(", ")
    val lines  = if (first.lines == 1) "1 line" else s"${first.lines} lines"
    val copies = s"${group.fragments.size} copies of $lines (${group.cost} nodes)"
    if (group.holes.isEmpty) Described(group, Finding.Rule.DuplicateCode, s"$copies: $locations")
    else {
      val holes  = group.holes.map(values => s"{${Finding.listed(values, ShownCopies)}}")
      val advice =
        if (group.mapping) "share a common lookup table or keep them distinct"
        else "replace them with one method taking the literals as parameters"
      Described(
        group,
        Finding.Rule.NearDuplicateCode,
        s"$copies differ only in literals ${holes.mkString(", ")}: $locations; $advice"
      )
    }
  }
}
