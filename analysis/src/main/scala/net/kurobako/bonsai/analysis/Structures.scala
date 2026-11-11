package net.kurobako.bonsai.analysis

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

  /** A build type named by the visible signature of a member of `owner`, or by the parents of `owner`. */
  case class Signature(owner: String, named: String, result: Boolean)

  object Model {
    val empty: Model =
      Model(
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Map.empty,
        Counterparts.Model.empty,
        Vector.empty,
        Vector.empty
      )
  }

  /** `extents` holds the lines of each top-level type. */
  case class Model(
      stutters: Vector[Stutter],
      mentions: Vector[Mention],
      applied: Vector[Applied],
      concretes: Vector[Concrete],
      members: Vector[Member],
      dispatches: Vector[Dispatch],
      extents: Map[String, Int],
      counterparts: Counterparts.Model,
      bypasses: Vector[Wrappers.Bypass],
      signatures: Vector[Signature]
  ) {
    def joined(other: Model): Model = Model(
      stutters ++ other.stutters,
      mentions ++ other.mentions,
      applied ++ other.applied,
      concretes ++ other.concretes,
      members ++ other.members,
      dispatches ++ other.dispatches,
      extents ++ other.extents,
      counterparts.joined(other.counterparts),
      bypasses ++ other.bypasses,
      signatures ++ other.signatures
    )
  }

  private val MinimumFamily = 3
  private val MinimumSide   = 3
  private val MinimumGroup  = 2
  private val Tiny          = 10
  private val Small         = 60
  private val Tied          = 120
  private val Callers       = 3
  private val Takes         = 2
  private val Launchers     = Set("main", "run")
  private val Shown         = 4

  def findings(model: Model, built: Graph): Vector[Finding] = {
    val graph = written(built)
    val nodes = graph.nodes
    stutters(model.stutters, nodes) ++ stale(model.mentions, nodes) ++ families(model.applied, nodes) ++
      packages(graph) ++ bases(model.concretes, nodes) ++ disjoint(model.members, nodes) ++
      satellites(model, graph) ++ dispatches(model.dispatches, nodes) ++
      Counterparts.findings(model.counterparts, nodes) ++ Wrappers.findings(model.bypasses, nodes)
  }

  /** The graph without generated code, which no one maintains, so no rule reports or counts it. */
  private def written(graph: Graph): Graph = {
    val nodes = graph.nodes.filterNot(_._2.generated)
    Graph(
      nodes,
      graph.edges.collect { case (from, targets) if nodes.contains(from) => from -> targets.filter(nodes.contains) },
      graph.roots.filter(root => nodes.contains(root.name))
    )
  }

  private def stutters(stutters: Vector[Stutter], nodes: Map[String, Node]): Vector[Finding] = stutters.flatMap {
    stutter =>
      val read = if (stutter.accesses > 0) s", as ${counted(stutter.accesses, "place")} do" else ""
      Finding.at(
        nodes,
        stutter.node,
        Finding.Rule.FieldStutter,
        s"${Finding.simple(stutter.owner)}.${stutter.field} is named like ${Finding.simple(stutter.owner)}, so code reads " +
          s"${stutter.field}.${stutter.field}$read; name it for what it holds, or give ${Finding.simple(stutter.owner)} the " +
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
            s"${family.size} aliases fill ${Finding.simple(constructor)} with a literal " +
              s"(${Finding.listed(family.map(alias => Finding.simple(alias.name)).sorted, Shown)}), giving each type a second " +
              s"spelling; write ${Finding.simple(constructor)}[...] where they are used",
            Finding.Level.Info
          )
        )
    }

  /** Packages whose files fall into two groups of which only one depends on the other, at the split crossed least. A
    * group holding an entry point or a type others name, which a package change would break, is not offered a move.
    */
  private def packages(graph: Graph): Vector[Finding] = {
    val nodes          = graph.nodes
    val files          = nodes.values.filterNot(_.test).groupBy(node => pkg(node.name, nodes))
    val edgesByPackage = graph.edges.toVector.flatMap { (from, targets) =>
      nodes.get(from).collect { case node if !node.test => pkg(node.name, nodes) -> (from -> targets) }
    }.groupMap(_._1)(_._2)
    files.toVector.sortBy(_._1).flatMap { (pkg, members) =>
      val file     = members.map(node => node.name -> node.source).toMap
      val sources  = file.values.toSet
      val named    = members.collect { case node if graph.rooted(node.name) => node.source }.toSet
      val weighted = edgesByPackage.getOrElse(pkg, Vector.empty).flatMap { (from, targets) =>
        file.get(from).toVector.flatMap(source =>
          targets.flatMap(to => file.get(to).collect { case target if source != target => (source, target) })
        )
      }.groupMapReduce(identity)(_ => 1)(_ + _)
      val dependents = weighted.keys.groupMap(_._2)(_._1)
      // The files that depend on a file, directly or not, form a group nothing outside it depends on.
      val uppers = sources.toVector.sorted.map(start => reach(List(start), dependents.getOrElse(_, Nil), Set(start)))
      val splits = uppers.distinct.flatMap { group =>
        val rest     = sources -- group
        val crossing = weighted.collect { case ((from, to), count) if group(from) && rest(to) => count }.sum
        val internal = weighted.collect {
          case ((from, to), count) if group(from) == group(to) => count
        }.sum
        Option.when(
          group.size >= MinimumSide && rest.size >= MinimumSide && crossing > 0 && crossing * 4 <= internal &&
            !group.exists(named)
        )((group, rest, crossing))
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

  @annotation.tailrec
  private def outermost(nodes: Map[String, Node], key: String): String = nodes.get(key).flatMap(_.outer) match {
    case Some(outer) => outermost(nodes, outer)
    case None        => key
  }

  private def pkg(name: String, nodes: Map[String, Node]): String =
    outermost(nodes, name).split('.').init.mkString(".")

  private def bases(concretes: Vector[Concrete], nodes: Map[String, Node]): Vector[Finding] = {
    val subclasses        = concretes.flatMap(concrete => concrete.parents.map(_ -> concrete)).groupMap(_._1)(_._2)
    def test(key: String) = nodes.get(key).exists(_.test)
    concretes.view.filter(_.unconstructed).flatMap { concrete =>
      subclasses.getOrElse(concrete.name, Vector.empty) match {
        // A test double extends what main code constructs elsewhere.
        case Vector(only) if test(only.node) == test(concrete.node) && !nodes.get(concrete.node).exists(_.exported) =>
          Finding.at(
            nodes,
            concrete.node,
            Finding.Rule.BaseClassSingleSubclass,
            s"${Finding.simple(concrete.name)} is never constructed itself and only ${Finding.simple(only.name)} extends it; merge them, " +
              s"or give ${Finding.simple(concrete.name)} a parameter for what ${Finding.simple(only.name)} adds"
          )
        case _ => None
      }
    }.toVector
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
      val groups = declared.groupBy(member => component(member.name)).values.toVector.map { group =>
        (group.map(_.name).distinct.sorted, group.flatMap(_.types).toSet)
      }.filter((names, _) => names.size >= MinimumGroup).sortBy(_._1.head)
      val separate = groups.size >= 2 && groups.combinations(2).forall {
        case Vector((_, left), (_, right)) => (left & right).isEmpty
        case _                             => true
      }
      Option.when(separate)(()).flatMap(_ =>
        Finding.at(
          nodes,
          declared.head.node,
          Finding.Rule.DisjointObject,
          s"object ${Finding.simple(owner)} holds ${groups.size} groups of members that never use each other and use " +
            s"different types (${groups.map((names, _) => names.take(Shown).mkString("{", ", ", "}")).mkString(", ")}); " +
            "split it, or move each group next to what it uses",
          Finding.Level.Info
        )
      )
    }

  private case class Move(node: String, why: String, host: Option[String], entry: Boolean)

  /** Top-level types alone in their file that few other types use, and entry points that only run one type, with the
    * type each belongs in. A type whose results and parents are nested types of one other goes there, whoever calls it;
    * so does one that only that other names in a signature, up to twice the size a lone user is given. Failing both, a
    * small type one type uses goes to it, and a tiny one two types use has no host to name. A type is no larger than
    * half as much again as its host, and a host takes at most two.
    */
  private def satellites(model: Model, graph: Graph): Vector[Finding] = {
    val nodes                   = graph.nodes
    def production(key: String) = nodes.get(key).exists(!_.test)
    val links                   = graph.edges.toVector
      .flatMap((from, targets) => targets.map(target => outermost(nodes, from) -> outermost(nodes, target)))
      .distinct
      .filter((from, to) => from != to && production(from) && production(to))
    val linked = links.toSet
    val users  = links.groupMap(_._2)(_._1)
    val uses   = links.groupMap(_._1)(_._2)
    val named  = model.signatures.flatMap(signature =>
      nodes.get(signature.named).map(node =>
        (outermost(nodes, signature.owner), outermost(nodes, signature.named), node.outer.nonEmpty && signature.result)
      )
    ).filter((owner, to, _) => linked(owner -> to))
    // A type that builds or extends the nested types of another is a part of it; one that takes them as parameters
    // only configures itself with them.
    val building  = named.collect { case (owner, to, true) => owner -> to }.groupMap(_._1)(_._2)
    val naming    = named.groupMap(_._2)(_._1)
    val launchers = model.members.groupMap(_.owner)(_.name).collect {
      case (owner, names) if names.contains("main") && names.forall(Launchers) => owner
    }.toSet
    def sole(candidates: Iterable[String]): Option[String] = candidates.toVector.distinct match {
      case Vector(only) => Some(only)
      case _            => None
    }
    def bound(node: Node): Option[(String, String)] = {
      val hosts = building.getOrElse(node.name, Vector.empty)
      sole(hosts).map(host => host -> s"tied to ${Finding.simple(host)} by its signatures").orElse(
        Option.when(hosts.isEmpty)(sole(naming.getOrElse(node.name, Vector.empty))).flatten.map(host =>
          host -> s"named in a signature only by ${Finding.simple(host)}"
        )
      )
    }
    def hostable(node: Node, lines: Int)(host: String) =
      nodes(host).module == node.module && model.extents.get(host).exists(size => lines * 2 <= size * 3)
    val alone = nodes.values.filter(_.outer.isEmpty).groupBy(_.source).values.view.map(_.toVector).collect {
      case Vector(node) if !node.test && !node.exported => node
    }
    val moves = alone.toVector.sortBy(_.name).flatMap { node =>
      val by    = users.getOrElse(node.name, Vector.empty)
      val roots = graph.rooted(node.name)
      val entry = roots && launchers(node.name)
      model.extents.get(node.name).flatMap { lines =>
        val size    = s"${Finding.simple(node.name)} is ${counted(lines, "line")}"
        val hosting = hostable(node, lines)
        val moved   =
          if (entry)
            sole(uses.getOrElse(node.name, Vector.empty)).filter(hosting).map(host =>
              s"${Finding.simple(node.name)} is an entry point of ${counted(lines, "line")} that only runs ${Finding.simple(host)}" ->
                Some(host)
            )
          else if (roots || by.isEmpty || by.size > Callers) None
          else
            bound(node).filter((host, _) => lines <= Tied && hosting(host) && by != Vector(host)).map((host, why) =>
              s"$size in a file of its own, $why" -> Some(host)
            ).orElse(by match {
              case Vector(only) if lines <= Small && hosting(only) =>
                Some(s"$size in a file of its own, used only by ${Finding.simple(only)}" -> Some(only))
              case Vector(_, _) if lines < Tiny =>
                Some(
                  s"$size in a file of its own, used only by ${by.map(Finding.simple(_)).sorted.mkString(" and ")}" ->
                    None
                )
              case _ => None
            })
        moved.map((why, host) => Move(node.name, why, host, entry))
      }
    }
    val hostOf  = moves.map(move => move.node -> move.host).toMap
    val offered = moves.filterNot(move =>
      move.host.exists(host =>
        hostOf.get(host).contains(Some(move.node)) &&
          Ordering[(Int, String)].gt((model.extents(move.node), move.node), (model.extents(host), host))
      )
    )
    val taken   = offered.collect { case Move(_, _, Some(host), false) => host }.toSet
    val staying = offered.filter(move => move.entry || !taken(move.node))
    val load    = staying.collect { case Move(_, _, Some(host), false) => host }.groupMapReduce(identity)(_ => 1)(_ + _)
    staying.flatMap(move =>
      Option.when(move.entry || move.host.forall(load(_) <= Takes))(()).flatMap(_ =>
        Finding.at(
          nodes,
          move.node,
          Finding.Rule.SatelliteType,
          s"${move.why}; nest it in the companion of ${move.host.fold("the type it belongs with")(Finding.simple)}",
          Finding.Level.Info
        )
      )
    )
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
          s"${Finding.simple(dispatch.method)} matches its ${Finding.simple(enumeration)} parameter ${dispatch.parameter} over its cases; " +
            s"make it a method of ${Finding.simple(enumeration)}, beside the cases it dispatches on",
          level
        )
      )
    }

  private def counted(count: Int, noun: String): String = if (count == 1) s"1 $noun" else s"$count ${noun}s"

  private def base(path: String): String = path.split('/').last
}
