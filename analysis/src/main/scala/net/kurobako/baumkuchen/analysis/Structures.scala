package net.kurobako.baumkuchen.analysis

/** How the build's code is arranged: names that repeat their owner or point at what no longer exists, families of
  * aliases, packages and objects that hold two designs, classes and types that stand apart from the one they serve, and
  * dispatch on an enum kept away from it.
  */
object Structures {

  /** A field of `owner` named like it, with the number of places code reads it off a value named the same. */
  case class Stutter(owner: String, node: String, field: String, accesses: Int)

  /** A string literal naming `member` of build types called `owner`, none of which has it. */
  case class Mention(text: String, owner: String, member: String, node: String, test: Boolean)

  /** An alias that fills `constructor` with literal arguments. */
  case class Applied(name: String, node: String, scope: String, constructor: String)

  /** A class or object of the build with the build classes it extends; `unconstructed` when it is a concrete class
    * nothing constructs but through a subclass.
    */
  case class Concrete(name: String, node: String, unconstructed: Boolean, parents: Vector[String])

  /** A member of an object, with the other members of that object its body names and the build types it uses. */
  case class Member(name: String, owner: String, node: String, siblings: Set[String], types: Set[String])

  /** A method whose result is a match on its enum `parameter`, naming `covered` of the enum's `cases`; `nearby` when it
    * is in the enum's companion or file.
    */
  case class Dispatch(
      method: String,
      owner: String,
      node: String,
      enumeration: String,
      parameter: String,
      covered: Int,
      cases: Int,
      nearby: Boolean
  )

  object Model {
    val empty: Model =
      Model(Vector.empty, Vector.empty, Vector.empty, Vector.empty, Vector.empty, Vector.empty, Map.empty)
  }

  /** `extents` holds the lines of each top-level type. */
  case class Model(
      stutters: Vector[Stutter],
      mentions: Vector[Mention],
      applied: Vector[Applied],
      concretes: Vector[Concrete],
      members: Vector[Member],
      dispatches: Vector[Dispatch],
      extents: Map[String, Int]
  )

  private val MinimumFamily = 3
  private val MinimumSide   = 3
  private val MinimumGroup  = 2
  private val Satellite     = 10
  private val Shown         = 4

  def findings(model: Model, graph: Graph): Vector[Finding] = {
    val nodes = graph.nodes
    stutters(model.stutters, nodes) ++ stale(model.mentions, nodes) ++ families(model.applied, nodes) ++
      packages(graph) ++ bases(model.concretes, nodes) ++ disjoint(model.members, nodes) ++
      satellites(model.extents, graph) ++ dispatches(model.dispatches, nodes)
  }

  private def stutters(stutters: Vector[Stutter], nodes: Map[String, Node]): Vector[Finding] = stutters.flatMap {
    stutter =>
      val read = if (stutter.accesses > 0) s", as ${counted(stutter.accesses, "place")} do" else ""
      Finding.at(
        nodes,
        stutter.node,
        Finding.Rule.FieldStutter,
        s"${short(stutter.owner)}.${stutter.field} is named like ${short(stutter.owner)}, so code reads " +
          s"${stutter.field}.${stutter.field}$read; name it for what it holds, or give ${short(stutter.owner)} the " +
          "operations on it",
        // Until code reads the field off a value named for its type, the name only risks the stutter.
        if (stutter.accesses > 0) Finding.Level.Warning else Finding.Level.Info
      )
  }

  /** Strings naming a member of a build type that has no such member, as a rename leaves behind. */
  private def stale(mentions: Vector[Mention], nodes: Map[String, Node]): Vector[Finding] =
    mentions.distinct.flatMap(mention =>
      Finding.at(
        nodes,
        mention.node,
        Finding.Rule.StaleNameString,
        s"\"${mention.text}\" names ${mention.owner}.${mention.member}, which no ${mention.owner} of the build has; " +
          "update the string to the name it stands for",
        if (mention.test) Finding.Level.Info else Finding.Level.Warning
      )
    )

  private def families(applied: Vector[Applied], nodes: Map[String, Node]): Vector[Finding] =
    applied.groupBy(alias => (alias.scope, alias.constructor)).toVector.sortBy(_._1).flatMap {
      case ((_, constructor), aliases) =>
        Option.when(aliases.size >= MinimumFamily)(aliases).flatMap(family =>
          Finding.at(
            nodes,
            family.head.node,
            Finding.Rule.ParameterAliasFamily,
            s"${family.size} aliases fill ${short(constructor)} with a literal " +
              s"(${Finding.listed(family.map(alias => short(alias.name)).sorted, Shown)}), giving each type a second " +
              s"spelling; write ${short(constructor)}[...] where they are used",
            Finding.Level.Info
          )
        )
    }

  /** Packages whose files fall into two groups of which only one depends on the other, at the split crossed least. */
  private def packages(graph: Graph): Vector[Finding] = {
    val nodes = graph.nodes
    val files = nodes.values.filter(node => !node.test && !node.generated).groupBy(node => pkg(node.name, nodes))
    val edges = graph.edges.toVector
    files.toVector.sortBy(_._1).flatMap { (pkg, members) =>
      val file     = members.map(node => node.name -> node.source).toMap
      val sources  = file.values.toSet
      val weighted = (for {
        (from, targets) <- edges
        source          <- file.get(from).toVector
        to              <- targets
        target          <- file.get(to) if source != target
      } yield (source, target)).groupMapReduce(identity)(_ => 1)(_ + _)
      val dependents = weighted.keys.groupMap(_._2)(_._1)
      // The files that depend on a file, directly or not, form a group nothing outside it depends on.
      val uppers = sources.toVector.sorted.map(start => reach(List(start), dependents.getOrElse(_, Nil), Set(start)))
      val splits = uppers.distinct.flatMap { group =>
        val rest     = sources -- group
        val crossing = weighted.collect { case ((from, to), count) if group(from) && rest(to) => count }.sum
        val internal = weighted.collect {
          case ((from, to), count) if group(from) == group(to) => count
        }.sum
        Option.when(group.size >= MinimumSide && rest.size >= MinimumSide && crossing > 0 && crossing * 4 <= internal)(
          (group, rest, crossing)
        )
      }
      splits.minByOption((_, _, crossing) => crossing).flatMap { (group, rest, crossing) =>
        Finding.at(
          nodes,
          members.collect { case node if group(node.source) => node.name }.min,
          Finding.Rule.PackageSplitCandidate,
          s"in $pkg, ${group.size} files (${Finding.listed(group.toVector.map(base(_)).sorted, Shown)}) depend on " +
            s"${rest.size} others through ${counted(crossing, "reference")}, which never depend back; give them a subpackage",
          Finding.Level.Info
        )
      }
    }
  }

  /** `seen` with everything `next` leads to from `todo`, directly or not. */
  @annotation.tailrec
  private def reach(todo: List[String], next: String => Iterable[String], seen: Set[String]): Set[String] = todo match {
    case Nil          => seen
    case head :: rest =>
      val found = next(head).filterNot(seen).toSet
      reach(found.toList ++ rest, next, seen ++ found)
  }

  private def pkg(name: String, nodes: Map[String, Node]): String = {
    @annotation.tailrec
    def top(key: String): String = nodes.get(key).flatMap(_.outer) match {
      case Some(outer) => top(outer)
      case None        => key
    }
    top(name).split('.').init.mkString(".")
  }

  private def bases(concretes: Vector[Concrete], nodes: Map[String, Node]): Vector[Finding] = {
    val subclasses        = concretes.flatMap(concrete => concrete.parents.map(_ -> concrete)).groupMap(_._1)(_._2)
    def test(key: String) = nodes.get(key).exists(_.test)
    concretes.filter(_.unconstructed).flatMap { concrete =>
      subclasses.getOrElse(concrete.name, Vector.empty) match {
        // A test double extends what main code constructs elsewhere.
        case Vector(only) if test(only.node) == test(concrete.node) && !nodes.get(concrete.node).exists(_.exported) =>
          Finding.at(
            nodes,
            concrete.node,
            Finding.Rule.BaseClassSingleSubclass,
            s"${short(concrete.name)} is never constructed itself and only ${short(only.name)} extends it; merge them, " +
              s"or give ${short(concrete.name)} a parameter for what ${short(only.name)} adds"
          )
        case _ => None
      }
    }
  }

  /** Objects whose members fall into groups that never name each other and use different types. */
  private def disjoint(members: Vector[Member], nodes: Map[String, Node]): Vector[Finding] =
    members.groupBy(_.owner).toVector.sortBy(_._1).flatMap { (owner, declared) =>
      val names = declared.map(_.name).toSet
      val links =
        declared.flatMap(member => member.siblings.collect { case sibling if names(sibling) => member.name -> sibling })
      val adjacent  = (links ++ links.map(_.swap)).groupMap(_._1)(_._2)
      val component = names.toVector.sorted.foldLeft(Map.empty[String, Set[String]]) { (found, name) =>
        if (found.contains(name)) found
        else {
          val connected = reach(List(name), adjacent.getOrElse(_, Vector.empty), Set(name))
          found ++ connected.map(_ -> connected)
        }
      }
      val groups = declared.groupBy(member => component(member.name)).values.toVector.collect {
        case group if group.size >= MinimumGroup => (group.map(_.name).distinct.sorted, group.flatMap(_.types).toSet)
      }.sortBy(_._1.head)
      val separate = groups.size >= 2 && groups.combinations(2).forall {
        case Vector((_, left), (_, right)) => (left & right).isEmpty
        case _                             => true
      }
      Option.when(separate && !nodes.get(declared.head.node).exists(_.generated))(()).flatMap(_ =>
        Finding.at(
          nodes,
          declared.head.node,
          Finding.Rule.DisjointObject,
          s"object ${short(owner)} holds ${groups.size} groups of members that never use each other and use " +
            s"different types (${groups.map((names, _) => names.take(Shown).mkString("{", ", ", "}")).mkString(", ")}); " +
            "split it, or move each group next to what it uses",
          Finding.Level.Info
        )
      )
    }

  /** Small top-level types alone in their file that only one or two other types use. */
  private def satellites(extents: Map[String, Int], graph: Graph): Vector[Finding] = {
    val nodes                    = graph.nodes
    def top(key: String): String = nodes.get(key).flatMap(_.outer).fold(key)(top(_))
    val alone = nodes.values.filter(_.outer.isEmpty).groupBy(_.source).values.map(_.toVector).collect {
      case Vector(node) if !node.test && !node.generated && !node.exported => node
    }
    val users = graph.edges.toVector.flatMap((from, targets) => targets.map(_ -> top(from))).groupMap(_._1)(_._2)
    alone.toVector.sortBy(_.name).flatMap { node =>
      val by =
        users.getOrElse(node.name, Vector.empty).filter(user => user != node.name && nodes.get(user).exists(!_.test))
          .distinct
      extents.get(node.name).filter(lines => lines < Satellite && by.nonEmpty && by.size <= 2).flatMap(lines =>
        Finding.at(
          nodes,
          node.name,
          Finding.Rule.SatelliteType,
          s"${short(node.name)} is ${counted(lines, "line")} in a file of its own, used only by " +
            s"${by.map(short(_)).sorted.mkString(" and ")}; nest it in the companion of the type it belongs with",
          Finding.Level.Info
        )
      )
    }
  }

  // A match naming fewer than half of the cases leaves the rest to a default, which is not a dispatch.
  private def dispatches(dispatches: Vector[Dispatch], nodes: Map[String, Node]): Vector[Finding] =
    dispatches.filter(dispatch => dispatch.nearby && dispatch.covered * 2 >= dispatch.cases).groupBy(dispatch =>
      (dispatch.owner, dispatch.enumeration)
    ).toVector.sortBy(_._1).flatMap { case ((_, enumeration), found) =>
      val level = if (found.size >= 2) Finding.Level.Warning else Finding.Level.Info
      found.flatMap(dispatch =>
        Finding.at(
          nodes,
          dispatch.node,
          Finding.Rule.EnumDispatchFunction,
          s"${short(dispatch.method)} matches its ${short(enumeration)} parameter ${dispatch.parameter} over its cases; " +
            s"make it a method of ${short(enumeration)}, beside the cases it dispatches on",
          level
        )
      )
    }

  private def short(name: String): String = name.split('.').last

  private def counted(count: Int, noun: String): String = if (count == 1) s"1 $noun" else s"$count ${noun}s"

  private def base(path: String): String = path.split('/').last
}
