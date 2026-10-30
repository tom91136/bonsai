package net.kurobako.baumkuchen.analysis

import scala.annotation.tailrec
import scala.collection.mutable

/** Where the types and the helpers of a build belong, as one target placement worked out from who uses what.
  *
  * The graph holds declarations only: a type with its companion as one unit, and the members of objects that can move.
  * An edge is a reference written inside one declaration to another. It never counts as a reference to the
  * declarations around its target, whatever the code writes before the name, so the graph is the same wherever the
  * declarations stand. A unit used from another sbt module, or entered from outside the build, is a root; tests are
  * left out.
  *
  * The immediate dominator of a unit in the graph of the units, where a member stands for the unit it is placed in, is
  * the unit that owns it. Guards then veto moves on the target tree, and a vetoed declaration stays where it is, which
  * changes the tree the next round reads; the rounds only add vetoes, so they end. What the rule reports is the
  * difference between where declarations are and the target. The target depends on the current placement only through
  * the declarations that stay, so applying any of the moves leaves the others as they were.
  */
object Nesting {

  /** `depth` is the deepest nesting a move may create, one level less when the move crosses files; `damping` the
    * number of declarations that must use a moved one from its new owner; `size` the most lines a moved unit, or a
    * group of helpers that move together, may have; the rule reports only when `enabled`.
    */
  case class Config(depth: Int = 2, damping: Int = 1, size: Int = 200, enabled: Boolean = false)

  /** Where a declaration stands: inside the unit `owner`, or at the top level, and for a type in the body of a class
    * rather than in its companion `inside`.
    */
  case class Place(owner: Option[String], inside: Boolean)

  /** A type with its companion, or a member of an object; `node` names the unit it stands in as its `outer`.
    *
    * `lines` is its extent without the declarations inside it, `movable` holds when nothing keeps it where it stands,
    * `owner` when it can hold other declarations, `bare` when it holds nothing but such declarations, and `entry` when
    * code outside the build enters at it.
    */
  case class Declaration(
      node: Node,
      unit: Boolean,
      inside: Boolean,
      lines: Int,
      movable: Boolean,
      owner: Boolean,
      bare: Boolean,
      entry: Boolean
  ) {
    def name: String = node.name

    def place: Place = Place(node.outer, inside)
  }

  case class Model(declarations: Vector[Declaration], edges: Map[String, Set[String]])

  object Model {
    val empty: Model = Model(Vector.empty, Map.empty)
  }

  case class Move(declaration: Declaration, to: Place) {
    def from: Place = declaration.place
  }

  /** The place of every declaration in the target, and the declarations that are not there. `reported` is `moves` with
    * the contents of a unit that holds nothing else, and that all move to one owner, joined into the move of the unit.
    */
  case class Plan(places: Map[String, Place], moves: Vector[Move], reported: Vector[Move])

  def plan(model: Model, config: Config): Plan = Planner(model, config).plan()

  def findings(model: Model, config: Config): Vector[Finding] =
    if (!config.enabled) Vector.empty
    else plan(model, config).reported.map { move =>
      val declaration = move.declaration
      Finding(
        Finding.Rule.Nesting,
        declaration.node,
        describe(declaration, move.from, move.to),
        Finding.Level.Info,
        declaration.node.suppressed
      )
    }

  private def last(name: String): String = Finding.short(name).split('.').last

  private def describe(declaration: Declaration, from: Place, to: Place): String = {
    val name = last(declaration.name)
    (from.owner, to.owner) match {
      case (_, Some(owner)) if from.owner.contains(owner) =>
        s"$name needs no instance of ${last(owner)}; move it to the companion object of ${last(owner)}"
      case (_, Some(owner)) if declaration.unit => s"$name is reached only through ${last(owner)}; nest it in ${last(owner)}"
      case (_, Some(owner)) => s"$name is used only by ${last(owner)}; move it into ${last(owner)}"
      case (Some(owner), None) => s"$name uses nothing of ${last(owner)}; move it out to the top level"
      case (None, None) => s"$name stays at the top level"
    }
  }

  /** The immediate dominator of every node reachable from `root`, with -1 for the others. */
  private def dominators(successors: Array[Array[Int]], root: Int): Array[Int] = {
    @tailrec
    def depthFirst(stack: List[(Int, Int)], seen: Set[Int], finished: List[Int]): List[Int] = stack match {
      case Nil                                                    => finished
      case (node, next) :: rest if next < successors(node).length =>
        val child = successors(node)(next)
        if (seen(child)) depthFirst((node, next + 1) :: rest, seen, finished)
        else depthFirst((child, 0) :: (node, next + 1) :: rest, seen + child, finished)
      case (node, _) :: rest => depthFirst(rest, seen, node :: finished)
    }
    // reverse postorder: every node comes after the node it was first reached from
    val order        = depthFirst(List((root, 0)), Set(root), Nil)
    val position     = Array.fill(successors.length)(-1)
    val predecessors = order.flatMap(node => successors(node).map(_ -> node)).groupMap(_._1)(_._2)
    order.zipWithIndex.foreach((node, index) => position(node) = index)
    val dominator = Array.fill(successors.length)(-1)
    dominator(root) = root
    @tailrec
    def meet(left: Int, right: Int): Int =
      if (left == right) left
      else if (position(left) > position(right)) meet(dominator(left), right)
      else meet(left, dominator(right))
    @tailrec
    def settle(): Unit = {
      val changed = order.tail.map { node =>
        val candidate = predecessors(node).filter(dominator(_) >= 0).reduce(meet)
        val moved     = dominator(node) != candidate
        dominator(node) = candidate
        moved
      }.contains(true)
      if (changed) settle()
    }
    settle()
    dominator
  }

  private enum Candidate {
    case Unreachable
    case Top
    case Under(owner: String)
  }

  private final class Planner(model: Model, config: Config) {
    private val declarations: Map[String, Declaration] = model.declarations.map(d => d.name -> d).toMap
    private val names: Vector[String]                  = declarations.keys.toVector.sorted
    private val units: Vector[String]                  = names.filter(declarations(_).unit)
    private val members: Vector[String]                = names.filterNot(declarations(_).unit)

    private val edges: Map[String, Vector[String]] = model.edges.collect {
      case (from, targets) if declarations.contains(from) =>
        from -> targets.filter(target => target != from && declarations.contains(target)).toVector.sorted
    }

    private val users: Map[String, Vector[String]] =
      edges.toVector.flatMap((from, targets) => targets.map(_ -> from)).groupMap(_._1)(_._2)
        .view.mapValues(_.sorted).toMap

    private def uses(name: String): Vector[String]    = edges.getOrElse(name, Vector.empty)
    private def usersOf(name: String): Vector[String] = users.getOrElse(name, Vector.empty)
    private def outer(name: String): Option[String]   = declarations(name).node.outer

    private val crossed: Set[String] = (for {
      (from, targets) <- edges.toVector
      target          <- targets if declarations(from).node.module != declarations(target).node.module
    } yield target).toSet

    private def rooted(name: String): Boolean = crossed(name) || declarations(name).entry

    // Users come before what they use; a member behind a cycle of members is never reached, so it stays.
    private val order: Vector[String] = {
      val pending = mutable.Map.from(members.map(member => member -> usersOf(member).count(!declarations(_).unit)))
      @tailrec
      def drain(ready: Vector[String], done: Vector[String]): Vector[String] = ready match {
        case next +: rest =>
          val freed = mutable.ArrayBuffer.empty[String]
          uses(next).filterNot(declarations(_).unit).foreach { target =>
            pending(target) -= 1
            if (pending(target) == 0) freed += target
          }
          drain(rest ++ freed, done :+ next)
        case _ => done
      }
      drain(members.filter(pending(_) == 0), Vector.empty)
    }

    private def canOwn(owner: String, subject: Declaration): Boolean =
      declarations.get(owner).exists(held =>
        held.owner && held.node.module == subject.node.module && held.name != subject.name
      )

    // A member moves to the one unit all its users are in.
    private val owners: Map[String, Option[String]] = {
      val placed = mutable.Map.empty[String, Option[String]]
      def home(user: String): Option[String] =
        if (declarations(user).unit) Some(user) else placed.getOrElse(user, outer(user))
      order.foreach { member =>
        val declaration = declarations(member)
        placed(member) = usersOf(member).map(home).distinct match {
          case Vector(Some(owner))
              if declaration.movable && !rooted(member) && !outer(member).contains(owner) &&
                canOwn(owner, declaration) =>
            Some(owner)
          case _ => outer(member)
        }
      }
      members.map(member => member -> placed.getOrElse(member, outer(member))).toMap
    }

    private val candidate: Map[String, Candidate] = {
      def host(name: String): Option[String] = if (declarations(name).unit) Some(name) else owners(name).filter(declarations.contains)
      val index                              = units.zipWithIndex.toMap
      val root                               = units.size
      val links = edges.toVector.flatMap { (from, targets) =>
        val hosted = targets.flatMap(host(_))
        host(from).toVector.flatMap(source => hosted.collect { case target if target != source => index(source) -> index(target) })
      }.distinct
      val following = links.groupMap(_._1)(_._2)
      val reached   = links.map(_._2).toSet
      // a top-level unit nothing uses is the API of its build
      val standing = units.filter(unit => outer(unit).isEmpty && !reached(index(unit))).map(index)
      val entered  =
        (crossed.toVector ++ names.filter(declarations(_).entry)).flatMap(host(_)).map(index) ++ standing
      val successors = Array.tabulate(units.size + 1) { node =>
        (if (node == root) entered.distinct else following.getOrElse(node, Vector.empty)).sorted.toArray
      }
      val dominator = dominators(successors, root)
      units.zipWithIndex.map { (unit, position) =>
        val declaration = declarations(unit)
        @tailrec
        def lift(node: Int): Candidate =
          if (node == root) Candidate.Top
          else if (canOwn(units(node), declaration)) Candidate.Under(units(node))
          else lift(dominator(node))
        unit -> (if (dominator(position) < 0) Candidate.Unreachable else lift(dominator(position)))
      }.toMap
    }

    private def target(unit: String): Place = {
      val declaration = declarations(unit)
      candidate(unit) match {
        case _ if !declaration.movable                       => declaration.place
        case Candidate.Unreachable                           => declaration.place
        case Candidate.Top if declaration.node.outer.isEmpty => declaration.place
        case Candidate.Top                                   => Place(None, inside = false)
        case Candidate.Under(owner)                          => Place(Some(owner), inside = false)
      }
    }

    // Where every declaration would stand if nothing vetoed a move; the target is what is left of that.
    private val wanted: Map[String, Place] =
      units.map(unit => unit -> target(unit)).toMap ++
        members.map(member => member -> Place(owners(member), inside = false))

    private final class Tree(placed: Map[String, Place]) {
      private val kids: Map[String, Vector[String]] =
        names.flatMap(name => placed(name).owner.map(_ -> name)).groupMap(_._1)(_._2)

      private def owner(name: String): Option[String] = placed(name).owner

      def chain(name: String): Vector[String] = {
        @tailrec
        def climb(current: Option[String], found: Vector[String]): Vector[String] = current match {
          case Some(next) if found.size <= names.size => climb(owner(next), found :+ next)
          case _                                      => found
        }
        climb(owner(name), Vector.empty)
      }

      lazy val cyclic: Set[String] = names.filter(chain(_).size > names.size).toSet

      def within(name: String, parent: String): Boolean = name == parent || chain(name).contains(parent)

      def depth(name: String): Int = chain(name).size

      def bundle(name: String): Vector[String] = {
        @tailrec
        def gather(pending: Vector[String], found: Vector[String]): Vector[String] = pending match {
          case next +: rest => gather(rest ++ kids.getOrElse(next, Vector.empty), found :+ next)
          case _            => found
        }
        gather(Vector(name), Vector.empty)
      }

      // the levels of units inside `name` that stay where they are; those that move answer for their own depth
      def height(name: String, moved: Set[String]): Int = {
        @tailrec
        def descend(level: Vector[String], count: Int): Int = {
          val below = level.flatMap(kids.getOrElse(_, Vector.empty)).filter(kid => declarations(kid).unit && !moved(kid))
          if (below.isEmpty) count else descend(below, count + 1)
        }
        descend(Vector(name), 0)
      }

      // a top-level type is taken to be a file of its own
      def top(name: String): String = chain(name).lastOption.getOrElse(name)
    }

    private def moving(placed: Map[String, Place]): Vector[String] =
      names.filter(name => placed(name).owner != outer(name))

    private def violations(placed: Map[String, Place]): Set[String] = {
      val tree    = Tree(placed)
      val moved   = moving(placed)
      val tangled = moved.filter(tree.cyclic)
      if (tangled.nonEmpty) tangled.toSet
      else {
        val going = moved.toSet
        moved.filter(violates(tree, placed, going, _)).toSet ++ crowded(placed, moved)
      }
    }

    // Helpers that move together from one owner to another are one move, however large each is.
    private def crowded(placed: Map[String, Place], moved: Vector[String]): Set[String] = {
      val helpers  = moved.filterNot(declarations(_).unit)
      val together = helpers.toSet
      def route(name: String) = (outer(name), placed(name).owner)
      val joined = helpers.map { helper =>
        helper -> (uses(helper) ++ usersOf(helper)).filter(other => together(other) && route(other) == route(helper))
      }.toMap
      @tailrec
      def component(pending: Vector[String], found: Set[String]): Set[String] = pending match {
        case next +: rest =>
          val fresh = joined(next).filterNot(found)
          component(rest ++ fresh, found ++ fresh)
        case _ => found
      }
      helpers.map(helper => component(Vector(helper), Set(helper))).distinct.filter(group =>
        group.toVector.map(declarations(_).lines).sum > config.size
      ).flatten.toSet
    }

    private def violates(tree: Tree, placed: Map[String, Place], moved: Set[String], name: String): Boolean = {
      val declaration = declarations(name)
      val from        = outer(name)
      val to          = placed(name).owner
      val bundle      = tree.bundle(name).toSet
      val outside     = bundle.toVector.flatMap(usersOf(_)).filterNot(bundle).distinct
      val reached     = bundle.toVector.flatMap(uses(_)).filterNot(bundle).distinct
      val landing     = to.fold(0)(parent => tree.depth(parent) + 1)
      val oldFile     = from.fold(name)(tree.top)
      val crossing    = to.exists(parent => tree.top(parent) != oldFile)
      val tooDeep     = landing + tree.height(name, moved) > config.depth || (crossing && landing > (config.depth - 1).max(1))
      // moving up out of a unit that is already inside the owner would only flatten it
      val lifting     = (from, to) match {
        case (Some(parent), Some(owner)) => parent != owner && tree.within(parent, owner)
        case _                           => false
      }
      val leaving     = from.exists(parent => !to.exists(tree.within(_, parent)))
      val dependent   = leaving && from.exists(parent => reached.exists(tree.within(_, parent)))
      val scattered   = to.exists(parent => outside.exists(user => tree.top(user) != tree.top(parent)))
      val welcome     = (to, from) match {
        case (Some(parent), _) => outside.count(tree.within(_, parent)) >= config.damping
        case (None, Some(parent)) =>
          outside.count { user =>
            !tree.within(user, parent) && uses(user).forall(used => bundle(used) || !tree.within(used, parent))
          } >= config.damping
        case (None, None) => true
      }
      val extent = bundle.toVector.map(declarations(_).lines).sum
      (declaration.unit && extent > config.size) || tooDeep || lifting || dependent || scattered || !welcome
    }

    def plan(): Plan = {
      @tailrec
      def settle(accepted: Set[String]): Map[String, Place] = {
        val placed = names.map(name => name -> (if (accepted(name)) wanted(name) else declarations(name).place)).toMap
        val vetoed = violations(placed)
        if (vetoed.isEmpty) placed else settle(accepted -- vetoed)
      }
      val placed = settle(names.filter(name => wanted(name) != declarations(name).place).toSet)
      val all    = names.filter(name => placed(name) != declarations(name).place).map(name =>
        Move(declarations(name), placed(name))
      )
      Plan(placed, all, joined(Tree(placed), all))
    }

    // A unit that holds only what leaves it for one owner is better said to move to that owner as it is.
    @tailrec
    private def joined(tree: Tree, moves: Vector[Move]): Vector[Move] = {
      val moving   = moves.map(move => move.declaration.name -> move).toMap
      val going    = moving.keySet
      val children = names.groupBy(outer(_).getOrElse(""))
      val joinable = units.flatMap { unit =>
        val declaration = declarations(unit)
        val kids        = children.getOrElse(unit, Vector.empty).flatMap(moving.get)
        val owners      = kids.map(_.to.owner).distinct
        owners match {
          case Vector(Some(owner))
              if declaration.bare && declaration.movable && !going(unit) && kids.size == children(unit).size &&
                !outer(unit).contains(owner) && kids.forall(_.from.owner.contains(unit)) &&
                fits(tree, owner, kids.map(_.declaration.name), going, declaration) =>
            Some(unit -> (owner, kids))
          case _ => None
        }
      }
      joinable.headOption match {
        case Some((unit, (owner, kids))) =>
          val gone = kids.map(_.declaration.name).toSet
          joined(tree, moves.filterNot(move => gone(move.declaration.name)) :+ Move(declarations(unit), Place(Some(owner), inside = false)))
        case None => moves
      }
    }

    private def fits(tree: Tree, owner: String, kids: Vector[String], going: Set[String], shell: Declaration): Boolean = {
      val landing  = tree.depth(owner) + 1
      val below    = kids.map(kid => 1 + tree.height(kid, going)).max
      val crossing = tree.top(owner) != tree.top(shell.name)
      landing + below <= config.depth && !(crossing && landing > (config.depth - 1).max(1))
    }
  }
}
