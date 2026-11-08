package net.kurobako.bonsai.analysis

import net.kurobako.bonsai.analysis.ClassFiles.*
import net.kurobako.bonsai.effects.{Effect as Fact, Kind as EffectKind, *}
import org.objectweb.asm.Opcodes
import scala.collection.mutable
import scala.reflect.NameTransformer

/** Effects and cost bounds of the methods of classes that have no TASTy, read from their bytecode: the JDK, Java
  * dependencies and the build's own Java code.
  *
  * A method's effects are those of its body and of the methods it can call, reached by passing each effect to the
  * callers until none is new, so that recursion ends. An effect on an object acts on the receiver when that is `this`,
  * on nothing a caller can see when the object was allocated by the method, and otherwise on whatever the caller
  * passed.
  *
  *   - A write to a field or array element is an effect unless the holder was allocated by the method, found by
  *     following values through locals and the stack. A constructor's writes to `this` are not effects on others.
  *   - Entering a monitor writes the monitor's object, as the axioms treat `lock` and `acquire` calls.
  *   - A `native` method, a call to a class the world lacks, a call to Scala code with TASTy, a virtual call no class
  *     implements and an `invokedynamic` besides lambdas and string concatenation are unknown.
  *   - A call takes its callee's axiom when one covers it, and otherwise the union of the callee's possible bodies.
  *     Constructors are never covered, as their effects are in their bodies. An effect that only some of several
  *     possible bodies perform is `assumed`, and `Behaviour.certain` reports it as unknown.
  *   - A class initialiser runs on the first use of its class, which no caller can tell from another, so each method
  *     that creates an object of the class, calls one of its static methods or accesses one of its static fields has
  *     the effects of the initialiser, apart from writes to the class's own statics.
  *   - A field written lazily, such as a cached hash code, is a write like any other.
  *   - A summary keeps one effect of each kind on the receiver and of each kind elsewhere, which is all a verdict
  *     reads.
  *
  * A method is bounded when it runs in constant time: no loop, no array allocation, no recursion, no dynamic call and
  * only bounded callees, leaving out the paths that only lead to a throw and the initialisers.
  */
private[analysis] object BytecodeEffects {

  /** The effects of one method, as its callers see them, and whether its running time is constant. */
  class Behaviour(found: () => Set[Fact[Nothing]], constant: () => Boolean) {
    lazy val effects: Set[Fact[Nothing]] = found()

    lazy val bounded: Boolean = constant()

    /** The effects that hold of every run: those that only some of the classes a call can reach perform are unknown. */
    lazy val certain: Set[Fact[Nothing]] = effects.map(effect =>
      if (effect.assumed)
        effect.copy(kind = EffectKind.Unknown, cause = s"${effect.cause} in some implementation", assumed = false)
      else effect
    )
  }

  /** A summarised method, with the contract it fulfils for collections and sorting, if any. */
  case class Entry(method: Reference, key: String, summary: Behaviour, role: Option[String])

  /** The summaries by method key, which `ClassWorld.key` and `Keys` agree on for a method. Only the methods whose
    * summaries are read are worked out, with those they can reach. The effects of the methods `require`d settle
    * together when the first is read, which saves working out the methods they share more than once; a method asked for
    * after that settles with its own.
    */
  trait Summaries {

    /** Whether the method has a summary. */
    def require(key: String): Boolean

    def get(key: String): Option[Behaviour]

    def isEmpty: Boolean

    /** The summaries of the methods `wanted` accepts, given a method and whether the compiler made it. */
    def owned(wanted: (Reference, Boolean) => Boolean): Vector[Entry]

    /** The cost of a call to a method that runs in constant time, which depends on no argument. */
    def callee(key: String): Option[Charges.Callee] =
      get(key).filter(_.bounded).map(_ => Charges.Callee(Charges.Bound(Polynomial.One, Polynomial.Zero), Seq.empty))
  }

  private object Empty extends Summaries {
    def require(key: String): Boolean = false

    def get(key: String): Option[Behaviour] = None

    def isEmpty: Boolean = true

    def owned(wanted: (Reference, Boolean) => Boolean): Vector[Entry] = Vector.empty
  }

  val none: Summaries = Empty

  /** The summaries of the methods of `graph` that have bytecode and no TASTy. */
  def summarise(world: ClassWorld, graph: CallGraph): Summaries = Inference(world, graph).discovered()

  private val concatenation = "java/lang/invoke/StringConcatFactory"

  private val Comparisons = Set("java.util.Comparator", "java.lang.Comparable")
  private val Comparing   = Set("compare", "compareTo")
  private val Contracts   = Map("equals" -> "(Ljava/lang/Object;)Z", "hashCode" -> "()I")

  private def qualified(internal: String): String = internal.replace('/', '.').replace('$', '.').stripSuffix(".")

  private def receiverOf(origin: Origin): Receiver[Nothing] = origin match {
    case Origin.This                  => Receiver.This
    case Origin.Fresh                 => Receiver.Fresh
    case Origin.Other | Origin.Caught => Receiver.Other(None)
  }

  private def lasting(
      effect: Fact[Nothing],
      origin: Origin,
      wide: Boolean,
      via: Option[String]
  ): Option[Fact[Nothing]] = {
    val seen  = via.fold(effect)(name => effect.copy(via = Some(name)))
    val shown = if (wide && effect.kind != EffectKind.Throws) seen.copy(assumed = true) else seen
    if (!effect.onThis) Some(shown)
    else
      origin match {
        case Origin.This                  => Some(shown)
        case Origin.Fresh                 => None
        case Origin.Other | Origin.Caught => Some(shown.copy(onThis = false))
      }
  }

  private class Visit(val id: Int, val callees: Vector[Int]) {
    val next: Array[Int] = Array(0)
  }

  private class Reached(val nodes: Vector[Int], val absent: Boolean, owner: () => String) {
    lazy val declared: String = owner()
  }

  /** A call: the nodes it can run, why some target has no summary, what the receiver is, and the name effects show. */
  private case class Step(
      targets: Vector[Int],
      absent: Boolean,
      origin: Origin,
      name: String,
      axiom: Option[Set[Fact[Nothing]]],
      exceptional: Boolean,
      initialiser: Boolean
  ) {
    def wide: Boolean = targets.size > 1
  }

  /** A call, or the initialiser of a class, before the trace of its method says what the receiver is. */
  private case class Shape(
      targets: Vector[Int],
      absent: Boolean,
      name: String,
      axiom: Option[Set[Fact[Nothing]]],
      position: Int
  ) {
    def initialiser: Boolean = position < 0

    def step(trace: Trace): Step =
      if (initialiser) Step(targets, absent, Origin.Other, name, axiom = None, exceptional = false, initialiser = true)
      else
        Step(targets, absent, trace.receivers(position), name, axiom, trace.exceptional(position), initialiser = false)
  }

  private class Inference(world: ClassWorld, graph: CallGraph) extends Summaries {
    private val references = mutable.ArrayBuffer.empty[Reference]
    private val methods    = mutable.ArrayBuffer.empty[(ClassFile, Method)]
    private val ids        = mutable.HashMap.empty[Reference, Option[Int]]
    private val outers     = mutable.HashMap.empty[String, Boolean]
    private val lineages   = mutable.HashMap.empty[String, Set[String]]
    private val shorts     = mutable.HashMap.empty[(String, String), String]
    private val coverings  = mutable.HashMap.empty[(String, String, Boolean, String), Option[Set[Fact[Nothing]]]]
    private val chains     = mutable.HashMap.empty[String, Vector[String]]
    private val reaches    = mutable.HashMap.empty[(Kind, Reference), Reached]
    private val shaped     = mutable.HashMap.empty[Int, Vector[Shape]]
    private val traced     = mutable.HashMap.empty[Int, Trace]
    private val stepped    = mutable.HashMap.empty[Int, Vector[Step]]
    private val localised  = mutable.HashMap.empty[Int, Set[Fact[Nothing]]]
    private val bounds     = mutable.HashMap.empty[Int, Boolean]
    private val owned      = mutable.HashMap.empty[String, mutable.ArrayBuffer[Int]]
    private val required   = mutable.BitSet.empty
    private val settled    = mutable.HashMap.empty[Int, Array[Option[Fact[Nothing]]]]

    private lazy val closures: Map[String, Vector[Lambda]] = graph.lambdas.values.filter(lambda =>
      lambda.tag == Opcodes.H_INVOKEVIRTUAL || lambda.tag == Opcodes.H_INVOKEINTERFACE
    ).toVector.groupBy(_.sam)

    private lazy val byKey: Map[String, Vector[Int]] =
      references.indices.toVector.groupBy(id => world.key(references(id)))

    /** Numbers every method that has a body, in the order a full pass over them reaches them. */
    def discovered(): this.type = {
      graph.reachable.foreach(nodeFor(_))
      Iterator.from(0).takeWhile(_ < references.size).foreach { id =>
        val (file, method) = methods(id)
        method.invocations.foreach(reached(_))
        starting(file, method).foreach(name => nodeFor(Reference(name, "<clinit>", "()V")))
      }
      this
    }

    def isEmpty: Boolean = references.isEmpty

    def require(key: String): Boolean = byKey.get(key).exists { found =>
      required ++= found
      true
    }

    def get(key: String): Option[Behaviour] = byKey.get(key).map { found =>
      Behaviour(() => found.map(effectsOf(_)).reduce(_ ++ _), () => found.forall(bounded(_)))
    }

    def owned(wanted: (Reference, Boolean) => Boolean): Vector[Entry] = {
      val chosen = references.indices.filter(id => wanted(references(id), synthetic(methods(id)._2))).toVector
      required ++= chosen
      chosen.map { id =>
        val (file, method) = methods(id)
        Entry(
          references(id),
          world.key(references(id)),
          Behaviour(() => effectsOf(id), () => bounded(id)),
          role(file, method)
        )
      }
    }

    private def synthetic(method: Method): Boolean = (method.access & (Opcodes.ACC_SYNTHETIC | Opcodes.ACC_BRIDGE)) != 0

    private def role(file: ClassFile, method: Method): Option[String] = {
      val comparing = lineage(file.name).exists(Comparisons)
      Option.when(Contracts.get(method.name).contains(method.descriptor) || comparing && Comparing(method.name))(
        method.name
      )
    }

    private def nodeFor(reference: Reference): Option[Int] =
      ids.getOrElseUpdate(
        reference,
        if (tasty(reference.owner)) None
        else
          world.get(reference.owner).flatMap { file =>
            world.method(reference).filterNot(_.isAbstract).map { method =>
              references += reference
              methods += file -> method
              owned.getOrElseUpdate(file.name, mutable.ArrayBuffer.empty) += references.size - 1
              references.size - 1
            }
          }
      )

    private def tracing(wanted: Iterable[Int]): Unit =
      wanted.view.filterNot(traced.contains).toVector.groupBy(id => methods(id)._1.name).foreach {
        (owner, group) =>
          val found = world.trace(owner, group.map(methods(_)._2))
          group.foreach { id =>
            val method = methods(id)._2
            traced(id) = found((method.name, method.descriptor))
          }
      }

    private def traceOf(id: Int): Trace = {
      if (!traced.contains(id)) tracing(owned(methods(id)._1.name))
      traced(id)
    }

    private def tasty(owner: String): Boolean =
      outers.getOrElseUpdate(
        owner,
        world.get(owner).exists { file =>
          file.tasty || file.outer.exists(tasty(_)) ||
          file.sourceFile.exists(_.endsWith(".scala")) && owner.endsWith("$") && tasty(owner.stripSuffix("$"))
        }
      )

    private def lineage(owner: String): Set[String] =
      lineages.getOrElseUpdate(owner, (owner +: world.ancestors(owner)).map(qualified(_)).toSet)

    private def short(owner: String, name: String): String =
      shorts.getOrElseUpdate((owner, name), Finding.short(s"${qualified(owner)}.${NameTransformer.decode(name)}"))

    private def covered(owner: String, name: String, instance: Boolean, receiver: String): Option[Set[Fact[Nothing]]] =
      coverings.getOrElseUpdate(
        (owner, name, instance, receiver),
        Option.when(name != "<init>" && name != "<clinit>") {
          val operand = Option.when(instance)(Axioms.Operand(lineage(receiver)))
          Axioms.effects(Axioms.Call(qualified(owner), NameTransformer.decode(name), receiver = operand))
        }.filterNot(_.exists(_.kind == EffectKind.Unknown))
      )

    private def ownAxiom(id: Int): Option[Set[Fact[Nothing]]] = {
      val (file, method) = methods(id)
      covered(file.name, method.name, !method.isStatic, file.name)
    }

    private def localOf(id: Int): Set[Fact[Nothing]] =
      localised.getOrElseUpdate(id, local(methods(id)._1, methods(id)._2, traceOf(id)))

    private def local(file: ClassFile, method: Method, trace: Trace): Set[Fact[Nothing]] = {
      val found                                                     = mutable.Set.empty[Fact[Nothing]]
      val initialising                                              = method.name == "<clinit>"
      def on(origin: Origin, kind: EffectKind, cause: String): Unit =
        Fact.on(receiverOf(origin), kind, cause).foreach(found += _)
      def lock(origin: Origin): Unit = on(origin, EffectKind.Writes, "locks a monitor")
      if (method.isNative) found += Fact(EffectKind.Unknown, "is native")
      if ((method.access & Opcodes.ACC_SYNCHRONIZED) != 0) lock(if (method.isStatic) Origin.Other else Origin.This)
      trace.locked.foreach(lock(_))
      if (trace.throws) found += Fact(EffectKind.Throws, "throws")
      trace.stores.foreach(on(_, EffectKind.Writes, "writes an array element"))
      trace.loads.foreach(on(_, EffectKind.Reads, "reads an array element"))
      method.fields.zip(trace.holders).foreach { (access, holder) =>
        val declared = world.fieldHolder(access)
        val own      = initialising && access.static && declared.contains(file.name)
        if (!own) {
          val cause = s"${if (access.write) "writes" else "reads"} ${access.name}"
          if (access.write) {
            if (access.static) found += Fact(EffectKind.Writes, cause) else on(holder, EffectKind.Writes, cause)
          } else if (!isFinal(declared, access)) {
            if (access.static) found += Fact(EffectKind.Reads, cause) else on(holder, EffectKind.Reads, cause)
          } else if (declared.exists(owner => s"${qualified(owner)}.${access.name}" == Axioms.Console))
            found += Fact(EffectKind.Input, cause)
        }
      }
      method.dynamics.filterNot(_.owner == concatenation).foreach { dynamic =>
        found += Fact(EffectKind.Unknown, s"runs dynamic code of ${short(dynamic.owner, dynamic.name)}")
      }
      found.toSet
    }

    private def isFinal(declared: Option[String], access: FieldAccess): Boolean =
      declared.flatMap(world.get(_)).flatMap(_.fields.find(field =>
        field.name == access.name && field.descriptor == access.descriptor
      )).exists(field => (field.access & Opcodes.ACC_FINAL) != 0)

    private def reference(target: ClassWorld.Target): Reference =
      Reference(target.owner, target.method.name, target.method.descriptor)

    private def closureTargets(callee: Reference): Vector[Reference] =
      closures.getOrElse(callee.name, Vector.empty).filter { lambda =>
        (callee.descriptor == lambda.samDescriptor || lambda.bridges.contains(callee.descriptor)) &&
        (lambda.interface +: lambda.markers).exists(name =>
          name == callee.owner || world.ancestors(name).contains(callee.owner)
        )
      }.flatMap { lambda =>
        graph.dispatch.getOrElse(Reference(lambda.owner, lambda.name, lambda.descriptor), Set.empty[Reference])
      }

    private def targetsOf(invocation: Invocation, callee: Reference): Vector[Reference] =
      invocation.kind match {
        case Kind.Static | Kind.Special        => world.resolve(callee).map(reference(_)).toVector
        case _ if callee.owner.startsWith("[") => world.resolve(callee).map(reference(_)).toVector
        case _ => (graph.dispatch.getOrElse(callee, Set.empty[Reference]).toVector ++ closureTargets(callee)).distinct
      }

    private def reached(invocation: Invocation): Reached = {
      val callee = Reference(invocation.owner, invocation.name, invocation.descriptor)
      reaches.getOrElseUpdate((invocation.kind, callee), reach(invocation, callee))
    }

    private def reach(invocation: Invocation, callee: Reference): Reached = {
      val targets = targetsOf(invocation, callee)
      val nodes   = targets.map(nodeFor(_))
      Reached(
        nodes.flatten,
        targets.isEmpty || nodes.exists(_.isEmpty),
        () => world.resolve(callee).fold(callee.owner)(_.owner)
      )
    }

    private def shapesOf(id: Int): Vector[Shape] = shaped.getOrElseUpdate(
      id, {
        val (file, method) = methods(id)
        val calls          = method.invocations.zipWithIndex.map { (invocation, position) =>
          val found = reached(invocation)
          Shape(
            found.nodes,
            found.absent,
            short(found.declared, invocation.name),
            covered(found.declared, invocation.name, invocation.kind != Kind.Static, invocation.owner),
            position
          )
        }
        calls ++ starting(file, method).map { name =>
          val node = nodeFor(Reference(name, "<clinit>", "()V"))
          Shape(node.toVector, node.isEmpty, short(name, "<clinit>"), axiom = None, position = -1)
        }
      }
    )

    private def stepsOf(id: Int): Vector[Step] =
      stepped.getOrElseUpdate(id, shapesOf(id).map(_.step(traceOf(id))))

    private def starting(file: ClassFile, method: Method): Vector[String] = {
      val used = method.instantiated ++
        method.invocations.filter(_.kind == Kind.Static).flatMap(invocation =>
          world.resolve(Reference(invocation.owner, invocation.name, invocation.descriptor)).map(_.owner)
        ) ++ method.fields.filter(_.static).flatMap(world.fieldHolder(_))
      val ready = initialised(file.name).toSet
      used.toVector.flatMap(initialised(_)).distinct.filterNot(ready)
    }

    // the classes with an initialiser that using `name` runs, its own and those of its ancestors
    private def initialised(name: String): Vector[String] =
      chains.getOrElseUpdate(
        name,
        (name +: world.ancestors(name)).filter(owner =>
          world.get(owner).exists(_.methods.exists(_.name == "<clinit>"))
        )
      )

    private def slot(effect: Fact[Nothing]): Int = effect.kind.ordinal * 2 + (if (effect.onThis) 1 else 0)

    private def effectsOf(id: Int): Set[Fact[Nothing]] = ownAxiom(id).getOrElse {
      if (!settled.contains(id)) settle(if (settled.isEmpty) required += id else Seq(id))
      settled(id).flatten.toSet
    }

    /** `ids` and the methods whose effects they take, which are the ones their calls without an axiom reach. A method
      * takes effects from those alone, so they settle as they would among all the methods.
      */
    private def reaching(ids: Iterable[Int]): Array[Int] = {
      val seen    = mutable.BitSet.empty ++= ids
      val pending = mutable.Stack.from(ids)
      while (pending.nonEmpty)
        shapesOf(pending.pop()).foreach(shape =>
          if (shape.axiom.isEmpty) shape.targets.foreach(target => if (seen.add(target)) pending.push(target))
        )
      seen.toArray
    }

    private def settle(seeds: Iterable[Int]): Unit = {
      val order = reaching(seeds.filter(ownAxiom(_).isEmpty))
      tracing(order)
      propagate(order).zip(order).foreach((held, id) => settled.getOrElseUpdate(id, held))
    }

    /** The effects of each of the methods `order` holds, reached by passing each new effect of a callee to its callers
      * until none is new. A node keeps one effect of each kind on its receiver and of each kind elsewhere, the first to
      * reach it that is not assumed. That is all a verdict and a report of the kinds a method has read, so a summary
      * stays as small as the kinds are few, however many fields and callees a cycle of the JDK's methods reaches.
      */
    private def propagate(order: Array[Int]): Array[Array[Option[Fact[Nothing]]]] = {
      val slots    = EffectKind.values.length * 2
      val position = Array.fill(references.size)(-1)
      order.indices.foreach(at => position(order(at)) = at)
      val held    = Array.fill(order.length)(Array.fill(slots)(Option.empty[Fact[Nothing]]))
      val callers = Array.fill(order.length)(mutable.ArrayBuffer.empty[(Int, Step)])
      val queue   = mutable.Queue.empty[(Int, Fact[Nothing])]
      def offer(at: Int, effect: Fact[Nothing]): Unit =
        if (!(effect.onThis && methods(order(at))._2.name == "<init>")) {
          val place = slot(effect)
          if (held(at)(place).forall(current => current.assumed && !effect.assumed)) {
            held(at)(place) = Some(effect)
            queue.enqueue(at -> effect)
          }
        }
      order.indices.foreach { at =>
        localOf(order(at)).foreach(offer(at, _))
        stepsOf(order(at)).foreach { step =>
          step.axiom match {
            case Some(axiom) =>
              axiom.view.flatMap(lasting(_, step.origin, wide = false, via = None)).foreach(offer(at, _))
            case None =>
              if (step.absent) offer(at, Fact(EffectKind.Unknown, s"calls ${step.name}"))
              step.targets.foreach(target => callers(position(target)) += at -> step)
          }
        }
      }
      while (queue.nonEmpty) {
        val (at, effect) = queue.dequeue()
        if (held(at)(slot(effect)).contains(effect))
          callers(at).foreach((caller, step) =>
            lasting(effect, step.origin, step.wide, Some(step.name)).foreach(offer(caller, _))
          )
      }
      held
    }

    private def bounded(id: Int): Boolean = bounds.getOrElse(id, descend(id))

    /** Whether the method and every method it calls, bar the paths that only throw and the initialisers, runs in
      * constant time. A method that reaches itself does not, so a call back to a method being walked ends the walk.
      */
    private def descend(root: Int): Boolean = {
      val path                    = mutable.ArrayBuffer.empty[Visit]
      val onPath                  = mutable.BitSet.empty
      def enter(id: Int): Boolean = {
        val constant = constantBody(id)
        if (constant) {
          path += Visit(
            id,
            stepsOf(id).view.filterNot(step => step.exceptional || step.initialiser).flatMap(_.targets).toVector
          )
          onPath += id
        } else bounds(id) = false
        constant
      }
      def refuse(): Boolean = {
        path.foreach(visit => bounds(visit.id) = false)
        false
      }
      @annotation.tailrec
      def walk(): Boolean = path.lastOption match {
        case None        => true
        case Some(visit) =>
          if (visit.next(0) < visit.callees.size) {
            val next = visit.callees(visit.next(0))
            visit.next(0) += 1
            bounds.get(next) match {
              case Some(true)  => walk()
              case Some(false) => refuse()
              case None        => if (onPath(next) || !enter(next)) refuse() else walk()
            }
          } else {
            path.remove(path.size - 1): Unit
            onPath -= visit.id
            bounds(visit.id) = true
            walk()
          }
      }
      if (enter(root)) walk() else false
    }

    private def constantBody(id: Int): Boolean = {
      val (_, method) = methods(id)
      !method.isNative && method.dynamics.isEmpty && !traceOf(id).loops && !traceOf(id).allocates &&
      stepsOf(id).forall(step => step.exceptional || step.initialiser || !step.absent)
    }
  }
}
