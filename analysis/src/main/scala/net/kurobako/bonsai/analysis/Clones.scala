package net.kurobako.bonsai.analysis

import scala.collection.mutable

/** Code fragments whose structure recurs elsewhere in the build, up to the renaming of local definitions and, for near
  * duplicates, the values of literals and the references they make.
  */
object Clones {

  /** A copy; `member` names the member whose body it is or lies in, `family` the overloads or numbered methods its
    * method belongs to, and `specialised` holds when its method is inline and expands inline methods on literals, which
    * near duplicates of it are meant to differ in.
    */
  case class Fragment(
      node: String,
      path: String,
      start: Int,
      end: Int,
      test: Boolean,
      family: String = "",
      specialised: Boolean = false,
      member: String = ""
  ) {
    def lines: Int = end - start + 1
  }

  /** Copies of one fragment; `cost` counts the tree nodes of each copy, and each of the `holes` of a near duplicate
    * holds the distinct values of one literal or reference position across the copies. A group with a `helper` is one
    * copy that is an instance of the body of that method, the parameters of which are its holes.
    */
  case class Group(
      cost: Int,
      fragments: Vector[Fragment],
      holes: Vector[Vector[String]] = Vector.empty,
      mapping: Boolean = false,
      helper: Option[Helper] = None,
      references: Boolean = false
  )

  /** A method whose body copies repeat; `reach` names the type the code that may call it lies in, if it is not visible
    * to all code.
    */
  case class Helper(fragment: Fragment, reach: Option[String])

  /** The value of a candidate that a near duplicate differs in: a literal, or a reference to a value. `text` tells the
    * values apart, with locals numbered by first occurrence, and `shown`, rendered when a finding names it, is what the
    * finding says; `string` marks a string literal. A reference through a parameter of its method has a `parameter`,
    * which a call of the method passes. The `types` of a reference are those of the value and then of each selection
    * made from it, and those of a literal its own; they are empty where not known.
    */
  case class Literal(
      text: String,
      string: Boolean = false,
      reference: Boolean = false,
      parameter: Option[Binding] = None
  )(
      render: => String = text,
      types: => Vector[Typed] = Vector.empty
  ) {
    lazy val shown: String        = render
    lazy val typed: Vector[Typed] = types
  }

  /** The type of a value, which a parameter takes if it conforms to the type of the parameter. */
  trait Typed {
    def conforms(to: Typed): Boolean
  }

  /** A value computed when first used, for what only fragments found alike need. */
  final class Deferred[A](compute: => A) {
    lazy val value: A = compute
  }

  object Deferred {
    def apply[A](compute: => A): Deferred[A] = new Deferred(compute)
  }

  /** The `index` of the parameter that a reference starts at, and the `suffix` of selections that follow it, which
    * number `depth`. The parameter `accepts` values of a type, unless that type is not known or depends on the method.
    */
  case class Binding(index: Int, suffix: String)(val depth: Int = 0, val accepts: Option[Typed] = None)

  /** A fragment of code to group, as either language produces it. `fragment` says where it is and `span` the offsets
    * within its file that tell overlapping copies apart. `exact` and `loose` hash it with its locals numbered by first
    * occurrence, with the values of literals and without them; `literals` are those values in the order hashed; these,
    * the `variables` and the `callee` below are computed only for fragments that turn out alike.
    *
    * The remaining fields hold what bounds the groups it joins: the fewest `minimumLines` a copy must span; `solid`
    * when it is dense enough to be reported as a copy on its own, where lighter ones only form a family of near
    * duplicates; `crowded` when so many are alike that they are rows of a table, not copies; `application` when it is
    * one call of a function, and `callee` the call's computed arguments when it has one, either of which the copies
    * already share; `listing` the argument list it is an element of, if any; and `mapping` when it builds a map.
    *
    * The `broad` hash ignores the references too, which `variables` lists with the literals in the order hashed; it is
    * zero for a run of statements. `small` marks a chain of calls too small to be a copy of its own, `tiny` the whole
    * body of a member too small to be, `entire` the body of a member of any size, and `direct` a single call. A
    * `helper` is a body that calls of it may replace, and `reach` the type it is visible to, if not to all code. Of the
    * fragments that are `whole`, the copies of larger code and of bodies of methods, near duplicates are made, and a
    * `method` holds the code of a method, where a value holds the rows of a table.
    */
  case class Candidate(
      fragment: Fragment,
      span: (Int, Int),
      cost: Int,
      exact: Long,
      loose: Long,
      literals: Deferred[Vector[Literal]],
      minimumLines: Int,
      solid: Boolean = true,
      crowded: Boolean = false,
      application: Boolean = false,
      callee: Deferred[Option[Vector[String]]] = Deferred(None),
      listing: Option[(String, Int, Int)] = None,
      mapping: Boolean = false,
      broad: Long = 0L,
      variables: Deferred[Vector[Literal]] = Deferred(Vector.empty),
      small: Boolean = false,
      tiny: Boolean = false,
      entire: Boolean = false,
      direct: Boolean = false,
      helper: Boolean = false,
      reach: Option[String] = None,
      whole: Boolean = true,
      method: Boolean = true
  )

  /** The statements a run must span to be a copy of more than one expression. */
  val WindowLines = 4

  /** The prefixes of the text of a local that the fragment defines, and of one it takes from outside. */
  private[analysis] val Bound = "bound#"
  private[analysis] val Free  = "free#"

  // Each hole becomes a parameter of the shared method, which pays when enough code comes with it, while rows of a
  // table differ in most of their literals.
  private val MaximumHoles = 3
  private val HoleCost     = 16
  // Short bodies alike but for numbers are a family of methods, one per width or mask, once there are three; short
  // bodies alike but for strings are a table of names or messages.
  private val FamilyCopies = 3

  private val ShownCopies = 4

  // A chain of calls written this often across the build is an idiom worth a name, while one written a few times in a
  // method is a value to bind.
  private val RepeatedCopies = 4
  private val RepeatedWeight = 150
  private val LocalCopies    = 4
  private val LocalWeight    = 40
  // Small bodies alike that hold no parameter are constants, which are shared only once there are this many.
  private val ConstantCopies = 3
  // Copies that differ in a few references, within one file, are one function of them.
  private val ReferenceCopies = 4
  private val LargeCopies     = 3
  private val LargeCost       = 40
  private val ReferenceHoles  = 2
  private val ReferenceCost   = 20
  private val MaximumAlike    = 32

  // A file repeating itself this often follows one template or table, which a finding per copy would bury.
  private val MaximumPerFile = 5
  private val ShownGroups    = 3

  /** A finding per group of copies, except that a file holding more than `MaximumPerFile` groups of one rule entirely
    * within itself gets one finding naming its largest. Copies split between main code and tests are information, as an
    * independent test oracle is meant to repeat what it checks. Groups whose copies are all intended pairs are left
    * out.
    */
  def findings(groups: Vector[Group], nodes: Map[String, Node], intended: Intended = Intended()): Vector[Finding] = {
    // Overloads one per width are what numbered-family asks for, and literals an inline method specialises on are its
    // point, so near duplicates among them are by design.
    val designed = groups.filterNot { group =>
      lazy val families = group.fragments.map(_.family).distinct
      group.holes.nonEmpty &&
      (group.fragments.forall(_.specialised) || families.forall(_.nonEmpty) && families.size == 1 ||
        widthsOfCompanions(group)) || intended.settles(group.fragments.map(_.node))
    }
    val reachable = designed.filter(group =>
      group.helper.forall(helper =>
        group.fragments.forall(copy =>
          nodes.get(copy.node).map(_.module) == nodes.get(helper.fragment.node).map(_.module) &&
            helper.reach.forall(Node.within(nodes, copy.node, _))
        )
      )
    )
    val (helped, described) = reachable.map(describe(_)).partition(_.group.helper.isDefined)
    val (within, spread)    = described.partition(_.group.fragments.map(_.path).distinct.size == 1)
    val crowded             = within.groupBy(entry => (entry.group.fragments.head.path, entry.rule))
      .filter((_, entries) => entries.size > MaximumPerFile)
    val single =
      (helped ++ spread ++
        within.filterNot(entry => crowded.contains((entry.group.fragments.head.path, entry.rule))))
        .flatMap { entry =>
          val fragments = entry.group.fragments
          val tests     = fragments.count(_.test)
          val level     = if (tests > 0) Finding.Level.Info else Finding.Level.Warning
          val detail    =
            if (tests > 0 && tests < fragments.size) s"${entry.detail} (copies in main code and tests)"
            else entry.detail
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

  /** Confirmed groups of non-overlapping copies, without groups whose copies all lie inside a larger reported copy,
    * then near duplicates that no larger near duplicate overlaps. Besides the copies of larger code, these are chains
    * of calls repeated at many sites, bodies of members alike but too small to count as copies, and copies of a body of
    * a method that its parameters make an instance of.
    */
  def groups(candidates: Seq[Candidate]): Vector[Group] = {
    def exact(alike: Iterable[Seq[Candidate]]) =
      copied(alike).map(copies => Found(copies.head.cost, copies, Vector.empty))
    val small  = candidates.filter(copy => copy.small && !copy.fragment.test)
    val chains = copied(small.groupBy(_.exact).values)
      .filter(copies => copies.size >= RepeatedCopies && copies.size * copies.head.cost >= RepeatedWeight) ++
      copied(small.filter(_.method).groupBy(copy => (copy.exact, copy.fragment.node, copy.fragment.member)).values)
        .filter(copies => copies.size >= LocalCopies && copies.size * copies.head.cost >= LocalWeight)
    val tiny = copied(candidates.filter(copy => copy.tiny && !copy.fragment.test).groupBy(_.exact).values).filter {
      copies =>
        !overloads(copies) &&
        (copies.size >= ConstantCopies || copies.exists(_.variables.value.exists(_.parameter.isDefined)))
    }
    val near =
      copied(candidates.filter(copy => copy.whole && !copy.crowded).groupBy(_.loose).values).flatMap { copies =>
        val holes = differing(copies)
        Option.when(parameterised(copies, holes))(Found(copies.head.cost, copies, holes))
      }
    val referring =
      repeated(candidates.filter(_.broad != 0L).groupBy(_.broad).values.filter(_.size <= MaximumAlike))
        .flatMap(referred)
    val identical = exact(candidates.filter(_.solid).groupBy(_.exact).values) ++
      (chains ++ tiny).map(copies => Found(copies.head.cost, copies, Vector.empty))
    report(identical ++ inlined(candidates))((outer, inner) => outer._1 <= inner._1 && inner._2 <= outer._2) ++
      report(near ++ referring)((outer, inner) => outer._1 < inner._2 && inner._1 < outer._2)
  }

  private case class Found(
      cost: Int,
      copies: Vector[Candidate],
      holes: Vector[Vector[Literal]],
      helper: Option[Helper] = None,
      anchor: Option[Candidate] = None
  )

  /** Copies in one file that differ in few places, some of which are references, as one function of them would do. */
  private def referred(copies: Vector[Candidate]): Option[Found] = {
    val holes = differingVariables(copies)
    Option.when(
      (copies.size >= ReferenceCopies || copies.size >= LargeCopies && copies.head.cost >= LargeCost) &&
        copies.head.cost >= ReferenceCost &&
        copies.map(_.fragment.path).distinct.size == 1 && !copies.forall(_.direct) &&
        holes.nonEmpty && holes.size <= ReferenceHoles && holes.exists(_.exists(_.reference))
    )(Found(copies.head.cost, copies, holes))
  }

  /** Each copy that is an instance of the body of a method visible to it, named after the one that spans the most. */
  private def inlined(candidates: Seq[Candidate]): Vector[Found] = {
    val helpers = mutable.LongMap.from(
      candidates
        .filter(copy => copy.helper && !copy.fragment.test)
        .groupBy(_.broad)
        .map((broad, alike) => broad -> alike.toVector)
    )
    if (helpers.isEmpty) Vector.empty
    else
      candidates.view.filter(!_.entire).flatMap { site =>
        helpers.get(site.broad).flatMap(
          _.filter(helper => outside(site, helper) && instance(helper, site))
            .minByOption(helper => (-helper.cost, helper.fragment.path, helper.fragment.start))
            .map(helper =>
              Found(site.cost, Vector(site), Vector.empty, Some(Helper(helper.fragment, helper.reach)), Some(helper))
            )
        )
      }.toVector
  }

  private def outside(site: Candidate, helper: Candidate): Boolean =
    site.fragment.path != helper.fragment.path || site.span._1 < helper.span._1 || helper.span._2 < site.span._2

  /** Whether `site` is the body of `helper` with its parameters given values: the references apart from those through
    * parameters are the same, and each parameter has the one value wherever it is used.
    */
  private def instance(helper: Candidate, site: Candidate): Boolean = {
    val (parts, values) = (helper.variables.value, site.variables.value)
    parts.size == values.size && {
      val bound = parts.indices.map { index =>
        val part = parts(index)
        part.parameter match {
          case Some(binding) =>
            val Binding(parameter, suffix) = binding
            Option.when(
              values(index).text.endsWith(suffix) && (values(index).reference || suffix.isEmpty) &&
                accepted(binding, values(index))
            )(Some(parameter -> values(index).text.stripSuffix(suffix)))
          case None => Option.when(same(part, values(index)))(None)
        }
      }
      bound.forall(_.isDefined) && {
        val arguments = bound.flatten.flatten
        arguments.forall(!_._2.startsWith(Bound)) && arguments.groupMap(_._1)(_._2).values.forall(_.distinct.size == 1)
      }
    }
  }

  private def accepted(binding: Binding, value: Literal): Boolean = {
    val types = value.typed
    val index = types.size - 1 - binding.depth
    binding.accepts.forall(expected => types.isEmpty || index >= 0 && types(index).conforms(expected))
  }

  // Bodies alike in the overloads of one method differ in the types they take, which one method could not.
  private def overloads(copies: Vector[Candidate]): Boolean = {
    val families = copies.map(_.fragment.family).distinct
    families.size == 1 && families.head.nonEmpty
  }

  private def same(left: Literal, right: Literal): Boolean =
    left.text == right.text && left.string == right.string && left.reference == right.reference

  private def copied(alike: Iterable[Seq[Candidate]]): Vector[Vector[Candidate]] =
    repeated(alike).filter(_.forall(copy => copy.fragment.lines >= copy.minimumLines))

  private def repeated(alike: Iterable[Seq[Candidate]]): Vector[Vector[Candidate]] =
    alike.view.collect { case copies if copies.size > 1 => disjoint(copies.toVector) }.filter(_.size > 1).toVector

  /** Copies that do not overlap each other, as sliding windows over repeated statements would. */
  private def disjoint(copies: Vector[Candidate]): Vector[Candidate] =
    copies.sortBy(copy => (copy.fragment.path, copy.span._1)).foldLeft(Vector.empty[Candidate]) { (kept, copy) =>
      val overlaps =
        kept.lastOption.exists(last => last.fragment.path == copy.fragment.path && copy.span._1 < last.span._2)
      if (overlaps) kept else kept :+ copy
    }

  /** The literals of each position that differs between the copies, in copy order. */
  private def differing(copies: Vector[Candidate]): Vector[Vector[Literal]] = positions(copies.map(_.literals.value))

  /** The literals and references of each position that differs between the copies, in copy order. */
  private def differingVariables(copies: Vector[Candidate]): Vector[Vector[Literal]] =
    positions(copies.map(_.variables.value))

  private def positions(values: Vector[Vector[Literal]]): Vector[Vector[Literal]] =
    if (values.exists(_.size != values.head.size)) Vector.empty
    else values.transpose.filter(_.distinct.size > 1).distinct

  private def parameterised(copies: Vector[Candidate], holes: Vector[Vector[Literal]]): Boolean = {
    val cost = copies.head.cost
    holes.nonEmpty &&
    (holes.size <= MaximumHoles || holes.size * HoleCost <= cost) &&
    (copies.forall(_.solid) || copies.size >= FamilyCopies && holes.flatten.forall(!_.string)) &&
    !called(copies) &&
    !listed(copies)
  }

  // Calls of one function that differ only in the literals they pass to it, or in the strings they pass anywhere,
  // already share that function.
  private def unanimous[A](values: Vector[Option[A]]): Boolean =
    values.distinct match {
      case Vector(Some(_)) => true
      case _               => false
    }

  private def called(copies: Vector[Candidate]): Boolean =
    copies.forall(_.application) || unanimous(copies.map(_.callee.value))

  private def listed(copies: Vector[Candidate]): Boolean = unanimous(copies.map(_.listing))

  /** The largest groups first, without those whose every copy is covered by a copy reported before. Of equal cost,
    * calls of a helper come first, as the more specific advice, and the body of the helper counts as reported for the
    * groups after it, though not for the calls.
    */
  private def report(confirmed: Vector[Found])(covers: ((Int, Int), (Int, Int)) => Boolean): Vector[Group] = {
    def spans(seen: Map[String, Vector[(Int, Int)]], copies: Vector[Candidate]) =
      copies.foldLeft(seen)((seen, copy) =>
        seen.updated(copy.fragment.path, seen.getOrElse(copy.fragment.path, Vector.empty) :+ copy.span)
      )
    def within(seen: Map[String, Vector[(Int, Int)]], copy: Candidate) =
      seen.get(copy.fragment.path).exists(_.exists(covers(_, copy.span)))
    confirmed.sortBy(found => (-found.cost, found.helper.isEmpty, -found.copies.size))
      .foldLeft((Map.empty[String, Vector[(Int, Int)]], Map.empty[String, Vector[(Int, Int)]], Vector.empty[Group])) {
        case ((reported, anchored, groups), found) =>
          val copies = found.copies
          if (copies.forall(copy => within(reported, copy) || found.helper.isEmpty && within(anchored, copy)))
            (reported, anchored, groups)
          else
            (
              spans(reported, copies),
              spans(anchored, found.anchor.toVector),
              groups :+ Group(
                found.cost,
                copies.map(_.fragment),
                found.holes.map(_.distinct.map(_.shown)),
                copies.exists(_.mapping),
                found.helper,
                found.holes.exists(_.exists(_.reference))
              )
            )
      }._3
  }

  // Members of sibling companions of distinct types, each holding only the width of its own type.
  private def widthsOfCompanions(group: Group): Boolean = {
    val owners        = group.fragments.map(fragment => Finding.simple(fragment.node))
    val widths        = owners.map(Finding.Numbers.findFirstIn(_))
    lazy val expected = widths.flatten.toSet
    owners.distinct.size == owners.size && widths.forall(_.isDefined) && group.holes.nonEmpty &&
    group.holes.forall(_.toSet == expected)
  }

  private case class Described(group: Group, rule: Finding.Rule, detail: String)

  private def describe(group: Group): Described = {
    val first     = group.fragments.head
    val locations =
      group.fragments.map(fragment => s"${fragment.path}:${fragment.start}-${fragment.end}").mkString(", ")
    val lines  = if (first.lines == 1) "1 line" else s"${first.lines} lines"
    val copies = s"${group.fragments.size} copies of $lines (${group.cost} nodes)"
    group.helper match {
      case Some(found) =>
        val helper = found.fragment
        val name   = s"${Finding.simple(helper.node)}.${helper.member}"
        Described(
          group,
          Finding.Rule.InlinedHelper,
          s"$locations repeats the body of $name (${helper.path}:${helper.start}-${helper.end}); call it"
        )
      case None if group.holes.isEmpty => Described(group, Finding.Rule.DuplicateCode, s"$copies: $locations")
      case None                        =>
        val holes  = group.holes.map(values => s"{${Finding.listed(values, ShownCopies)}}")
        val advice =
          if (group.mapping) "share a common lookup table or keep them distinct"
          else if (group.references) "replace them with one method taking what differs as parameters"
          else "replace them with one method taking the literals as parameters"
        val differing = if (group.references) "literals and references" else "literals"
        Described(
          group,
          Finding.Rule.NearDuplicateCode,
          s"$copies differ only in $differing ${holes.mkString(", ")}: $locations; $advice"
        )
    }
  }
}
