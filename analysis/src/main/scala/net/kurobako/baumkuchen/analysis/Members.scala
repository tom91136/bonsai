package net.kurobako.baumkuchen.analysis

/** Liveness and visibility of the methods and values of build classes, from every reference in the build. */
object Members {

  /** A member with the nodes that reference it from outside its own body.
    *
    * `inherited` holds when a member it overrides is referenced or declared outside the build, so calls can reach it
    * through dynamic dispatch; `rooted` when it overrides a member of the build, which is reported instead; `constant`
    * when it is a `var` nothing assigns after initialisation.
    */
  case class Member(
      name: String,
      node: String,
      users: Set[String],
      inherited: Boolean,
      rooted: Boolean,
      overriding: Boolean,
      deferred: Boolean,
      public: Boolean,
      constant: Boolean,
      test: Boolean,
      generated: Boolean,
      suppressed: Set[String]
  ) {
    def short: String = Finding.short(name)
  }

  /** A named given; `named` holds when some code refers to it by name rather than through given resolution. */
  case class Given(
      name: String,
      node: String,
      named: Boolean,
      test: Boolean,
      generated: Boolean,
      suppressed: Set[String]
  )

  /** A visible type declared inside another, with the nodes that reference it; `exposed` when a visible signature
    * outside the type names it, which the compiler forbids for private types.
    */
  case class Nested(node: String, outer: String, users: Set[String], exposed: Boolean)

  object Model {
    val empty: Model = Model(Vector.empty, Vector.empty, Vector.empty)
  }

  case class Model(members: Vector[Member], givens: Vector[Given], nested: Vector[Nested])

  /** Members of `live` nodes; members of unreachable types are covered by the type-level liveness findings. */
  def findings(model: Model, nodes: Map[String, Node], live: String => Boolean): Vector[Finding] =
    anonymous(model.givens, nodes, live) ++ hidden(model.nested, nodes, live) ++ visibility(model.members, nodes, live)

  private def visibility(members: Vector[Member], nodes: Map[String, Node], live: String => Boolean): Vector[Finding] =
    members.filter(member => !member.test && !member.generated && live(member.node)).flatMap {
      member =>
        val used   = member.users.nonEmpty || member.inherited
        val access =
          if (!used)
            Option.unless(member.rooted)(s"${member.short} is never referenced")
              .flatMap(Finding.at(nodes, member.node, Finding.Rule.UnusedMember, _))
          else if (!member.inherited && member.users.forall(user => nodes.get(user).exists(_.test)))
            Finding.at(
              nodes,
              member.node,
              Finding.Rule.TestOnlyMember,
              s"${member.short} is referenced only from tests"
            )
          else
            Option.when(
              member.public && !member.overriding && !member.deferred && !member.inherited &&
                member.users.forall(Node.within(nodes, _, member.node))
            )(s"${member.short} is referenced only by its own type and companion; it can be private")
              .flatMap(Finding.at(nodes, member.node, Finding.Rule.WeakerAccess, _, Finding.Level.Info))
        val mutability = Option.when(used && member.constant)(s"${member.short} is never reassigned; it can be a val")
          .flatMap(Finding.at(nodes, member.node, Finding.Rule.VarCouldBeVal, _))
        (access ++ mutability).filterNot(_.suppressedBy(member.suppressed))
    }

  // A name is part of the API: renaming the type renames an anonymous given, so this is information, not a warning.
  private def anonymous(givens: Vector[Given], nodes: Map[String, Node], live: String => Boolean): Vector[Finding] =
    givens.filter(instance => !instance.named && !instance.generated && live(instance.node)).flatMap(instance =>
      Finding.at(
        nodes,
        instance.node,
        Finding.Rule.AnonymousGiven,
        s"${Finding.short(instance.name)} is never referenced by name; it can be anonymous",
        Finding.Level.Info
      ).filterNot(_.suppressedBy(instance.suppressed))
    )

  // Unreferenced nested types are covered by the type-level liveness findings.
  private def hidden(nested: Vector[Nested], nodes: Map[String, Node], live: String => Boolean): Vector[Finding] =
    nested.filter(inner =>
      !inner.exposed && inner.users.nonEmpty && live(inner.node) &&
        inner.users.forall(Node.within(nodes, _, inner.outer))
    ).flatMap { inner =>
      def short(key: String) = key.split('.').last
      nodes.get(inner.node).filterNot(node => node.test || node.generated || node.exported).flatMap(_ =>
        Finding.at(
          nodes,
          inner.node,
          Finding.Rule.WeakerAccess,
          s"${short(inner.outer)}.${short(inner.node)} is referenced only inside ${short(inner.outer)}; it can be private",
          Finding.Level.Info
        )
      )
    }
}
