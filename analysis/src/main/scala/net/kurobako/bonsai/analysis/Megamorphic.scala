package net.kurobako.bonsai.analysis

import net.kurobako.bonsai.analysis.CallGraph.*
import net.kurobako.bonsai.analysis.ClassFiles.*

/** Virtual calls inside loops whose targets have more receiver classes than HotSpot inlines at one call site. */
object Megamorphic {

  /** A method calling `callee` inside a loop. `key` names the bytecode method the call runs, as `Keys` does with the
    * class of the receiver as the owner, `holder` the key of the calling method, `settled` whether every receiver is
    * the same on each iteration of the loop, `unreaching` the build classes whose type arguments rule them out as
    * receivers, and `stable` holds when every implementation in the build reads a field, and `generic` when the class
    * of the callee has type parameters, which decide the receivers.
    */
  case class Site(
      caller: String,
      node: String,
      callee: String,
      key: String,
      holder: Option[String],
      settled: Boolean,
      unreaching: Set[String],
      stable: Boolean,
      suppressed: Set[String],
      generic: Boolean = false
  )

  /** A site whose receivers are `names`, in all `count` classes and lambdas. */
  case class Found(site: Site, count: Int, names: Vector[String])

  /** A site whose `callers` pass `count` receiver classes in all, `names`, and no more than `perCaller` each. */
  case class Shared(site: Site, callers: Int, count: Int, perCaller: Int, names: Vector[String])

  private val Shown = 4

  // HotSpot's inline caches hold two receiver types; a third makes a call site megamorphic.
  private val Inlined = 2

  /** The calls of `sites` whose receivers in `graph` are too many to inline. Only a call of a type of the build counts:
    * its receivers are the classes and lambdas the build creates, and any library class or lambda that implements it.
    * The graph holds every class a library instantiates, so the receivers of a library type, such as `Function1`, are
    * thousands that have nothing to do with the loop.
    */
  def resolve(sites: Vector[Site], nodes: Map[String, Node], graph: CallGraph, world: ClassWorld): Vector[Found] = {
    val callees = calleesOf(sites, nodes, graph)
    val known   = callees.values.flatten.toSet.map(callee => callee -> graph.receivers(callee, world)).toMap
    sites.flatMap { site =>
      val receivers =
        callees.getOrElse(site.key, Vector.empty).flatMap(known(_).filter(counts(_, site, nodes))).distinct
      Option.when(receivers.size > Inlined && !exempt(site, receivers, nodes))(
        Found(site, receivers.size, names(receivers).sorted)
      )
    }
  }

  /** The sites of `found` whose body is shared by callers that each pass a closed set of receivers too small to make
    * the call megamorphic, which are too many together. The profile of a call is kept per method, so it sees them all.
    */
  def shared(found: Vector[Found], nodes: Map[String, Node], graph: CallGraph, world: ClassWorld): Vector[Shared] =
    if (found.isEmpty) Vector.empty
    else {
      val flow    = ReceiverFlow(world, graph)
      val callees = calleesOf(found.map(_.site), nodes, graph)
      found.flatMap { case Found(site, _, _) =>
        callees.getOrElse(site.key, Vector.empty).flatMap { callee =>
          flow.holders(callee).filter(holder =>
            site.holder.contains(world.key(holder)) && flow.callers(holder).size > 1
          ).flatMap(holder => pollution(site, flow.supplied(holder, callee), nodes))
        }.headOption
      }
    }

  private def pollution(site: Site, supplied: ReceiverFlow.Supplied, nodes: Map[String, Node]): Option[Shared] = {
    val callers = supplied.byCaller.values.toVector
    val sets    = callers.map(_.receivers.filter(counts(_, site, nodes))).filter(_.nonEmpty)
    val all     = sets.flatten.distinct
    Option.when(
      sets.size > 1 && !supplied.own.open && callers.forall(!_.open) && sets.forall(_.size <= Inlined) &&
        all.size > Inlined
    )(
      Shared(site, sets.size, all.size, sets.map(_.size).max, names(all).sorted)
    )
  }

  def sharedFindings(shared: Vector[Shared], nodes: Map[String, Node]): Vector[Finding] = shared.flatMap {
    case Shared(site, callers, count, perCaller, names) =>
      Finding.at(
        nodes,
        site.node,
        Finding.Rule.PollutedCall,
        s"${Finding.short(site.caller)} calls ${site.callee} in a loop that its $callers callers share: they pass " +
          s"$count receiver classes (${Finding.listed(names, Shown)}), at most $perCaller each, which HotSpot profiles " +
          "together; split the body per caller",
        level = Finding.Level.Info,
        suppressed = site.suppressed
      )
  }

  def findings(found: Vector[Found], nodes: Map[String, Node]): Vector[Finding] = found.flatMap {
    case Found(site, count, names) =>
      Finding.at(
        nodes,
        site.node,
        Finding.Rule.MegamorphicCall,
        s"${Finding.short(site.caller)} calls ${site.callee} in a loop, which has $count receiver classes " +
          s"(${Finding.listed(names, Shown)}); HotSpot inlines at most two receiver types at a call site",
        suppressed = site.suppressed
      )
  }

  private def calleesOf(
      sites: Vector[Site],
      nodes: Map[String, Node],
      graph: CallGraph
  ): Map[String, Vector[Reference]] = {
    val wanted = sites.map(_.key).toSet
    graph.dispatch.keys.view.map(callee => ClassFiles.key(callee, None) -> callee)
      .filter((key, callee) => wanted(key) && located(callee.owner, nodes).isDefined).toVector.groupMap(_._1)(_._2)
  }

  private def counts(receiver: Receiver, site: Site, nodes: Map[String, Node]): Boolean =
    !site.unreaching(Keys.dotted(receiver.owner)) &&
      !(located(receiver.owner, nodes).exists(_.test) && !nodes.get(site.node).exists(_.test))

  // A member every build class implements by reading a field costs a load, not a dispatch.
  private def exempt(site: Site, receivers: Vector[Receiver], nodes: Map[String, Node]): Boolean =
    site.stable && receivers.forall {
      case Receiver.Instance(name) => located(name, nodes).isDefined
      case Receiver.Closure(_)     => false
    }

  private def located(internal: String, nodes: Map[String, Node]): Option[Node] = {
    @annotation.tailrec
    def climb(name: String): Option[Node] = nodes.get(name) match {
      case found @ Some(_) => found
      case None            =>
        val at = name.lastIndexOf('.')
        if (at < 0) None else climb(name.take(at))
    }
    climb(Keys.dotted(internal))
  }

  private def names(receivers: Vector[Receiver]): Vector[String] = {
    val classes = receivers.collect { case Receiver.Instance(name) => Keys.dotted(name) }.distinct
    val simple  = classes.map(Finding.simple(_))
    val shown   = if (simple.distinct.size == simple.size) simple else classes.map(Finding.short(_))
    val lambdas = receivers.collect { case Receiver.Closure(site) =>
      s"lambda in ${Finding.simple(Keys.dotted(site.holder.owner))}"
    }
    shown ++ lambdas.distinct
  }
}
