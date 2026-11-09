package net.kurobako.bonsai.analysis

/** Liveness and visibility of the methods and values of build classes, from every reference in the build. */
object Members {

  /** A member with the uses that reference it from outside its own body.
    *
    * `inherited` holds when a member it overrides is referenced or declared outside the build, so calls can reach it
    * through dynamic dispatch; `rooted` when it overrides a member of the build, which is reported instead; `constant`
    * when it is a `var` nothing assigns after initialisation; `detachable` when it is a member of an object that names
    * nothing else of the object; `field` when it is a parameter of a case class; `unlisted` when it is of the kind a
    * collection of its siblings registers and no such collection lists it; `alias` when it is a type alias, `key` is
    * the key `Keys` gives the method that stands for it, and `line` is where it is declared, or 0 when unknown.
    */
  case class Member(
      name: String,
      node: String,
      users: Set[Use],
      inherited: Boolean,
      rooted: Boolean,
      overriding: Boolean,
      deferred: Boolean,
      public: Boolean,
      constant: Boolean,
      detachable: Boolean,
      test: Boolean,
      suppressed: Set[String],
      field: Boolean = false,
      unlisted: Boolean = false,
      alias: Boolean = false,
      key: Option[String] = None,
      line: Int = 0
  ) {
    def short: String = Finding.short(name)
  }

  /** A named given; `named` holds when some code refers to it by name rather than through given resolution. */
  case class Given(
      name: String,
      node: String,
      named: Boolean,
      suppressed: Set[String]
  )

  /** A visible type declared inside another, with the nodes that reference it; `exposed` when a visible signature
    * outside the type names it, which the compiler forbids for private types.
    */
  case class Nested(node: String, outer: String, users: Set[String], exposed: Boolean)

  object Model {
    val empty: Model = Model(Vector.empty, Vector.empty, Vector.empty)
  }

  /** The uses of methods by the code of other languages, which the sources of the members do not hold, by the key of
    * the method called.
    */
  case class Model(
      members: Vector[Member],
      givens: Vector[Given],
      nested: Vector[Nested],
      calls: Map[String, Set[Use]] = Map.empty
  )

  /** Members of live nodes; members of unreachable types are covered by the type-level liveness findings. A member is
    * used when a use that `reach` runs references it. Generated members are where their generator puts them, whatever
    * uses them.
    */
  def findings(
      model: Model,
      nodes: Map[String, Node],
      reach: Reach,
      support: Set[String] = Set.empty
  ): Vector[Finding] = {
    val written = model.members.filterNot(member => nodes.get(member.node).exists(_.generated)).map(member =>
      member.copy(users = member.users ++ member.key.flatMap(model.calls.get).getOrElse(Set.empty))
    )
    val live = reach.live(_)
    anonymous(model.givens, nodes, live) ++ hidden(model.nested, nodes, live) ++
      grouped(visibility(written, nodes, reach, support)) ++ misplaced(written, nodes, live) ++
      drifted(written, nodes, live)
  }

  private val Shown = 6

  private def at(
      nodes: Map[String, Node],
      member: Member,
      rule: Finding.Rule,
      detail: String,
      level: Finding.Level = Finding.Level.Warning
  ): Option[Finding] =
    Finding.at(nodes, member.node, rule, detail, level).map(finding =>
      if (member.line > 0) finding.copy(node = finding.node.copy(line = member.line)) else finding
    )

  // A type alias beside a value of its name, as a top-level alias pair is, shares one node with it.
  private def unreferenced(member: Member, values: Map[(String, String), Member]): String =
    values.get(member.node -> member.name).filter(_ => member.alias) match {
      case Some(value) =>
        val used = if (value.users.nonEmpty || value.inherited) "; the value of that name is" else ""
        s"the type alias ${member.short} is never referenced$used"
      case None => s"${member.short} is never referenced"
    }

  // Members of one type that only tests use say the same of the type, as of a builder only tests drive, so they are
  // one finding.
  private def grouped(findings: Vector[Finding]): Vector[Finding] = {
    val (tested, others) = findings.partition(_.rule == Finding.Rule.TestOnlyMember)
    others ++ tested.groupBy(_.node.name).toVector.sortBy(_._1).flatMap {
      case (_, Vector(single)) => Vector(single)
      case (owner, several)    =>
        val members = several.map(_.detail.stripSuffix(" is referenced only from tests")).sorted
        Vector(
          several.minBy(finding => (finding.node.line, finding.detail)).copy(detail =
            s"${members.size} members of ${Finding.simple(owner)} are referenced only from tests: " +
              Finding.listed(members, Shown)
          )
        )
    }
  }

  private def visibility(
      members: Vector[Member],
      nodes: Map[String, Node],
      reach: Reach,
      support: Set[String]
  ): Vector[Finding] = {
    val values          = members.collect { case member if !member.alias => (member.node, member.name) -> member }.toMap
    def tests(use: Use) = nodes.get(use.node).exists(_.test) || !reach.mainUse(use)
    members.view.filter(member => !member.test && reach.live(member.node)).flatMap {
      member =>
        val running = member.users.filter(reach.liveUse)
        val used    = running.nonEmpty || member.inherited
        val access  =
          if (!used)
            Option.unless(member.rooted || drifting(member))(
              if (member.field) s"${member.short} is a field nothing reads; remove it, or read it"
              else unreferenced(member, values)
            ).flatMap(
              at(nodes, member, if (member.field) Finding.Rule.UnreadField else Finding.Rule.UnusedMember, _)
            )
          else if (!member.inherited && running.forall(tests(_)))
            at(nodes, member, Finding.Rule.TestOnlyMember, s"${member.short} is referenced only from tests")
              .filterNot(found => support(found.node.module))
          else
            Option.when(
              member.public && !member.overriding && !member.deferred && !member.inherited &&
                member.users.forall(user => Node.within(nodes, user.node, member.node))
            )(s"${member.short} is referenced only by its own type and companion; it can be private")
              .flatMap(at(nodes, member, Finding.Rule.WeakerAccess, _, Finding.Level.Info))
        val mutability = Option.when(used && member.constant)(s"${member.short} is never reassigned; it can be a val")
          .flatMap(at(nodes, member, Finding.Rule.VarCouldBeVal, _))
        (access ++ mutability).filterNot(_.suppressedBy(member.suppressed))
    }.toVector
  }

  /** Members of objects that need nothing else of their object and that one other type of the build uses, and nothing
    * else, not even their own object.
    */
  private def misplaced(members: Vector[Member], nodes: Map[String, Node], live: String => Boolean): Vector[Finding] =
    members.view.filter(member =>
      member.detachable && member.public && !member.test && !member.overriding &&
        !member.inherited && live(member.node)
    ).flatMap { member =>
      member.users.map(_.node).toVector match {
        case Vector(user)
            if !Node.within(nodes, user, member.node) && !Node.within(nodes, member.node, user) &&
              nodes.get(user).exists(node => !node.test) =>
          at(
            nodes,
            member,
            Finding.Rule.MisplacedMember,
            s"${member.short} is used only by ${Finding.simple(user)}; move it there",
            Finding.Level.Info
          ).filterNot(_.suppressedBy(member.suppressed))
        case _ => None
      }
    }.toVector

  private def drifting(member: Member): Boolean =
    member.unlisted && member.public && !member.test && !member.inherited && member.users.isEmpty

  private def drifted(members: Vector[Member], nodes: Map[String, Node], live: String => Boolean): Vector[Finding] =
    members.view.filter(member => drifting(member) && live(member.node)).flatMap(member =>
      at(
        nodes,
        member,
        Finding.Rule.RegistryDrift,
        s"${member.short} is of the kind the collections of its siblings register, but none lists it; add it to one, " +
          "or derive the registry from the definitions"
      ).filterNot(_.suppressedBy(member.suppressed))
    ).toVector

  // A name is part of the API: renaming the type renames an anonymous given, so this is information, not a warning.
  private def anonymous(givens: Vector[Given], nodes: Map[String, Node], live: String => Boolean): Vector[Finding] =
    givens.view.filter(instance => !instance.named && live(instance.node)).flatMap(instance =>
      Finding.at(
        nodes,
        instance.node,
        Finding.Rule.AnonymousGiven,
        s"${Finding.short(instance.name)} is never referenced by name; it can be anonymous",
        Finding.Level.Info
      ).filterNot(_.suppressedBy(instance.suppressed))
    ).toVector

  // The innermost type, from `scope` outwards, that holds every user.
  @annotation.tailrec
  private def enclosure(nodes: Map[String, Node], scope: Option[String], users: Set[String]): Option[String] =
    scope match {
      case Some(candidate) if !users.forall(Node.within(nodes, _, candidate)) =>
        enclosure(nodes, nodes.get(candidate).flatMap(_.outer), users)
      case other => other
    }

  // Unreferenced nested types are covered by the type-level liveness findings.
  private def hidden(nested: Vector[Nested], nodes: Map[String, Node], live: String => Boolean): Vector[Finding] =
    nested.view.filter(inner => !inner.exposed && inner.users.nonEmpty && live(inner.node)).flatMap { inner =>
      nodes.get(inner.node).filterNot(node => node.test || node.exported).flatMap { _ =>
        enclosure(nodes, Some(inner.outer), inner.users).flatMap { scope =>
          val name   = Finding.simple(scope)
          val access = if (scope == inner.outer) "private" else s"private[$name]"
          Finding.at(
            nodes,
            inner.node,
            Finding.Rule.WeakerAccess,
            s"${Finding.simple(inner.outer)}.${Finding.simple(inner.node)} is referenced only inside $name; " +
              s"it can be $access",
            Finding.Level.Info
          )
        }
      }
    }.toVector
}
