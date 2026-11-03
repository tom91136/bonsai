package net.kurobako.bonsai.analysis

import net.kurobako.bonsai.collections.{Kind as _, Step as _, *}
import net.kurobako.bonsai.java.Model.Cost.*
import net.kurobako.bonsai.java.Model as JavaModel
import scala.collection.mutable

private object JavaCosts {

  /** Finds the bound of a method of the Scala code by its key. */
  type Resolver = String => Option[Charges.Callee]

  val none: Resolver = _ => None
}

/** The bounds of the methods of the Java code, evaluated from the trees of loops, library work and calls that the javac
  * plugin wrote, with the same charges and scopes as the Scala code. A call is bound by key to a Java method here or to
  * a Scala method through the resolver, so each language sees the other's loops. The sink the plugin recorded for an
  * operation or call says how long what it builds stays live, as the sinks of the Scala code do.
  *
  * The extents of the trees are positions, `$0` for the first parameter; they are named after the parameters here, so
  * that the findings read as the Scala ones do.
  */
private class JavaCosts(costs: Iterable[JavaModel.Cost], declared: Complexity.Declared) {
  import Charges.*
  import JavaCosts.*

  private val facts = costs.map(cost => cost.key -> cost).toMap
  private val memo  = Memo[String]()

  def nonEmpty: Boolean = facts.nonEmpty

  private def rooted(cost: JavaModel.Cost): Vector[String] =
    cost.parameters.zipWithIndex.map((name, index) => s"$name#$index")

  private def declaredSize(cost: JavaModel.Cost, extent: String): Option[Polynomial] =
    extent match {
      case _ if extent.startsWith("$") =>
        extent.drop(1).toIntOption.flatMap(cost.parameters.lift).flatMap(name =>
          declared.size(s"${cost.name}.$name", cost.name)
        )
      case _ if extent.startsWith("this.") && !extent.drop(5).contains('.') =>
        declared.size(s"${cost.owner}.${extent.drop(5)}", cost.owner)
      case _ if extent.contains('.') && !extent.startsWith("this.") =>
        declared.size(extent, extent.take(extent.lastIndexOf('.')))
      case _ => None
    }

  private def bound(key: String, resolve: Resolver): Bound = memo(key) {
    if (!facts.contains(key)) Bound.Zero
    else {
      val evaluation = evaluate(facts(key), resolve, identity)
      total(evaluation.recursion, evaluation.charges)
    }
  }

  def callee(key: String, resolve: Resolver): Option[Callee] =
    facts.get(key).map(cost => Callee(bound(key, resolve), rooted(cost)))

  /** The hits of the methods that cost more than linear, for the findings. */
  def summaries(resolve: Resolver): Vector[Complexity.Summary] = facts.values.toVector.flatMap { cost =>
    val evaluation = evaluate(cost, resolve, _.linear)
    val bounds     = Charges.bounds(evaluation.recursion, evaluation.charges)
    Option.when(bounds.nonEmpty)(Complexity.Summary(cost.name, cost.owner, bounds, cost.suppressed))
  }

  private case class Evaluation(charges: Vector[Charge], recursion: Recursion)

  /** A call a method makes to itself: the arguments, and whether it is in a loop. */
  private case class SelfCall(arguments: Vector[String], looped: Boolean)

  private def evaluate(cost: JavaModel.Cost, resolve: Resolver, inherit: Bound => Bound): Evaluation = {
    val charges = mutable.ArrayBuffer.empty[Charge]
    val selves  = mutable.ArrayBuffer.empty[SelfCall]

    def named(extent: String): String =
      if (extent.startsWith("$")) {
        val (index, rest) = extent.drop(1).span(_.isDigit)
        cost.parameters.lift(index.toInt).fold(extent)(name => s"$name#$index$rest")
      } else extent

    def sizeOf(extent: String): Option[Polynomial] =
      if (extent == Fixed) Some(Polynomial.One)
      else
        Option.unless(extent == Unknown || extent == Static)(
          declaredSize(cost, extent).getOrElse(Atoms.size(named(extent)))
        )

    def binding(extent: String): Atoms.Binding =
      if (extent == Fixed) Atoms.Binding(None, Some(Polynomial.One), static = true)
      else sizeOf(extent).fold(Atoms.Binding(None, None))(size => Atoms.Binding(Some(named(extent)), Some(size)))

    def charge(site: Site, work: Work, counted: Scope): Unit =
      charges += Charge.at(site, counted, work)

    def operation(site: Site, kind: Kind, extent: String): Unit = {
      val count  = sizeOf(extent)
      val size   = count.getOrElse(Atoms.Unknown)
      val linear = growsWith(count)
      val what   = s"${kind.render.takeWhile(_ != ':')} over ${count.fold("?")(Atoms.show)} (line ${site.line})"
      val work   = kind match {
        case Kind.Search        => Operation.Search.work(what, size, linear)
        case Kind.Shift         => Operation.Shift.work(what, size, linear)
        case Kind.Copy          => Operation.Copy.work(what, size, linear)
        case Kind.Sort          => Operation.Sort.work(what, size, linear)
        case Kind.Concat(depth) =>
          val length = Polynomial.One + Polynomial.product(site.scope.loops.takeRight(depth).map(_.trips))
          Work(s"text growth over ${Atoms.show(length)} (line ${site.line})", length, length, linear = true)
      }
      charge(site, work, site.scope)
    }

    def call(site: Site, key: String, receiver: String, arguments: Vector[String]): Unit =
      if (key == cost.key) selves += SelfCall(arguments, site.scope.loops.nonEmpty)
      else
        callee(key, resolve).orElse(resolve(key)).foreach { found =>
          val inherited = inherit(found.bound)
          val bindings  = found.roots.zip(arguments.map(binding(_))).toMap + ("this" -> binding(receiver))
          val time      = Atoms.rebind(inherited.time, bindings)
          val space     = Atoms.rebind(inherited.space, bindings)
          val what      = s"call of ${key.takeWhile(_ != '(').split('.').last} (line ${site.line})"
          charge(site, Work(what, time, space, linearIn(time)), site.scope)
        }

    def recursion: Recursion = selves.toVector match {
      case Vector()                           => Recursion.Absent
      case Vector(SelfCall(arguments, false)) =>
        arguments.zipWithIndex.collectFirst {
          case (argument, index)
              if argument == JavaModel.Cost.smaller(index) || argument == JavaModel.Cost.stepped(index) =>
            Recursion.Bounded(Atoms.size(named(s"$$$index")))
        }.getOrElse(Recursion.Unbounded)
      case _ => Recursion.Unbounded
    }

    def walk(node: Node, scope: Scope): Unit = node.step match {
      case Step.Loop(extent, callback, absorbs, body) =>
        val count = sizeOf(extent)
        val what  = s"${if (callback) "each" else "loop"} over ${count.fold("?")(Atoms.show)} (line ${node.line})"
        val inner =
          scope.pushed(Scope.Loop(count.getOrElse(Atoms.Unknown), what, absorbs)).copy(callback =
            scope.callback || callback
          )
        charge(
          Site(node.line, scope, Sink.Returned),
          Work(what, Polynomial.One, Polynomial.Zero, linear = false),
          inner
        )
        body.foreach(walk(_, inner))
      case Step.Exit(body)                           => body.foreach(walk(_, scope.copy(terminal = true)))
      case Step.Op(kind, extent, sink)               => operation(Site(node.line, scope, sink), kind, extent)
      case Step.Call(key, receiver, arguments, sink) => call(Site(node.line, scope, sink), key, receiver, arguments)
    }

    cost.body.foreach(walk(_, Scope()))
    Evaluation(charges.toVector, recursion)
  }
}
