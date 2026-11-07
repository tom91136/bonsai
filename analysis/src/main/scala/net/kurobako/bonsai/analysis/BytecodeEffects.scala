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
  class Behaviour(found: () => Set[Fact[Nothing]], val bounded: Boolean) {
    lazy val effects: Set[Fact[Nothing]] = found()

    /** The effects that hold of every run: those that only some of the classes a call can reach perform are unknown. */
    lazy val certain: Set[Fact[Nothing]] = effects.map(effect =>
      if (effect.assumed)
        effect.copy(kind = EffectKind.Unknown, cause = s"${effect.cause} in some implementation", assumed = false)
      else effect
    )
  }

  /** A summarised method, with the contract it fulfils for collections and sorting, if any. */
  case class Entry(method: Reference, key: String, summary: Behaviour, role: Option[String], synthetic: Boolean)

  /** The summaries by method key, which `ClassWorld.key` and `Keys` agree on for a method. */
  class Summaries(val entries: Vector[Entry]) {
    private val byKey: Map[String, Behaviour] = entries.groupMapReduce(_.key)(_.summary)((left, right) =>
      Behaviour(() => left.effects ++ right.effects, left.bounded && right.bounded)
    )

    def get(key: String): Option[Behaviour] = byKey.get(key)

    def isEmpty: Boolean = entries.isEmpty

    /** The cost of a call to a method that runs in constant time, which depends on no argument. */
    def callee(key: String): Option[Charges.Callee] =
      get(key).filter(_.bounded).map(_ =>
        Charges.Callee(Charges.Bound(Polynomial.One, Polynomial.Zero), Seq.empty)
      )
  }

  val none: Summaries = Summaries(Vector.empty)

  /** The summaries of every method of `graph` that has bytecode and no TASTy. */
  def summarise(world: ClassWorld, graph: CallGraph): Summaries =
    Inference(world, graph).run()

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
      effects: Iterable[Fact[Nothing]],
      origin: Origin,
      wide: Boolean,
      via: Option[String]
  ): Iterable[Fact[Nothing]] =
    effects.flatMap { effect =>
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

  private case class Reached(nodes: Vector[Int], absent: Boolean, declared: String)

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

  private class Inference(world: ClassWorld, graph: CallGraph) {
    private val references = mutable.ArrayBuffer.empty[Reference]
    private val methods    = mutable.ArrayBuffer.empty[(ClassFile, Method)]
    private val steps      = mutable.ArrayBuffer.empty[Vector[Step]]
    private val locals     = mutable.ArrayBuffer.empty[Set[Fact[Nothing]]]
    private val traces     = mutable.ArrayBuffer.empty[Trace]
    private val ids        = mutable.HashMap.empty[Reference, Option[Int]]
    private val outers     = mutable.HashMap.empty[String, Boolean]
    private val lineages   = mutable.HashMap.empty[String, Set[String]]
    private val shorts     = mutable.HashMap.empty[(String, String), String]
    private val coverings  = mutable.HashMap.empty[(String, String, Boolean, String), Option[Set[Fact[Nothing]]]]
    private val chains     = mutable.HashMap.empty[String, Vector[String]]
    private val reaches    = mutable.HashMap.empty[(Kind, Reference), Reached]
    private val owned      = mutable.HashMap.empty[String, mutable.ArrayBuffer[Method]]
    private val traced     = mutable.HashMap.empty[(String, String, String), Trace]

    private lazy val closures: Map[String, Vector[Lambda]] = graph.lambdas.values.filter(lambda =>
      lambda.tag == Opcodes.H_INVOKEVIRTUAL || lambda.tag == Opcodes.H_INVOKEINTERFACE
    ).toVector.groupBy(_.sam)

    def run(): Summaries = {
      graph.reachable.foreach(nodeFor(_))
      Iterator.from(0).takeWhile(_ < references.size).foreach { id =>
        val (file, method) = methods(id)
        val trace          = traceOf(file, method)
        traces += trace
        locals += local(file, method, trace)
        steps += stepsOf(file, method, trace)
      }
      val count      = references.size
      val successors = Array.tabulate(count)(id =>
        steps(id).filterNot(step => step.exceptional || step.initialiser).flatMap(_.targets).distinct.toArray
      )
      val effects = propagate(count)
      val bounded = Array.fill(count)(false)
      Tarjan.components(count, successors).foreach { component =>
        val cyclic = component.size > 1 || successors(component.head).contains(component.head)
        component.foreach(id => bounded(id) = !cyclic && constant(id, bounded))
      }
      Summaries(Vector.tabulate(count) { id =>
        val reference      = references(id)
        val (file, method) = methods(id)
        val axiom          = ownAxiom(id)
        val inferred       = effects(id)
        Entry(
          reference,
          world.key(reference),
          Behaviour(() => axiom.getOrElse(inferred()), bounded(id)),
          role(file, method),
          (method.access & (Opcodes.ACC_SYNTHETIC | Opcodes.ACC_BRIDGE)) != 0
        )
      })
    }

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
              owned.getOrElseUpdate(file.name, mutable.ArrayBuffer.empty) += method
              references.size - 1
            }
          }
      )

    // a class is read once for every method of it that is summarised by the time its first is
    private def traceOf(file: ClassFile, method: Method): Trace = {
      def key(of: Method) = (file.name, of.name, of.descriptor)
      if (!traced.contains(key(method))) {
        val batch = owned(file.name).view.filterNot(found => traced.contains(key(found))).toVector
        world.trace(file.name, batch).foreach((signature, trace) =>
          traced((file.name, signature._1, signature._2)) = trace
        )
      }
      traced(key(method))
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
          }
        }
      }
      method.dynamics.filterNot(_.owner == concatenation).foreach { dynamic =>
        found += Fact(EffectKind.Unknown, s"runs dynamic code of ${short(dynamic.owner, dynamic.name)}")
      }
      if (method.name == "<init>") found.filterNot(_.onThis).toSet else found.toSet
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
        world.resolve(callee).fold(callee.owner)(_.owner)
      )
    }

    private def stepsOf(file: ClassFile, method: Method, trace: Trace): Vector[Step] = {
      val calls = Vector.newBuilder[Step]
      method.invocations.zipWithIndex.foreach { (invocation, position) =>
        val found = reached(invocation)
        calls += Step(
          found.nodes,
          found.absent,
          trace.receivers(position),
          short(found.declared, invocation.name),
          covered(found.declared, invocation.name, invocation.kind != Kind.Static, invocation.owner),
          trace.exceptional(position),
          initialiser = false
        )
      }
      calls.result() ++ initialisers(file, method)
    }

    private def initialisers(file: ClassFile, method: Method): Vector[Step] = {
      val used = method.instantiated ++
        method.invocations.filter(_.kind == Kind.Static).flatMap(invocation =>
          world.resolve(Reference(invocation.owner, invocation.name, invocation.descriptor)).map(_.owner)
        ) ++ method.fields.filter(_.static).flatMap(world.fieldHolder(_))
      val ready = initialised(file.name).toSet
      used.toVector.flatMap(initialised(_)).distinct.filterNot(ready).map { name =>
        val node = nodeFor(Reference(name, "<clinit>", "()V"))
        Step(
          node.toVector,
          node.isEmpty,
          Origin.Other,
          short(name, "<clinit>"),
          axiom = None,
          exceptional = false,
          initialiser = true
        )
      }
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

    /** The effects of each node, reached by passing each new effect of a callee to its callers until none is new. A
      * node keeps one effect of each kind on its receiver and of each kind elsewhere, the first to reach it that is not
      * assumed. That is all a verdict and a report of the kinds a method has read, so a summary stays as small as the
      * kinds are few, however many fields and callees a cycle of the JDK's methods reaches.
      */
    private def propagate(count: Int): Array[() => Set[Fact[Nothing]]] = {
      val slots   = EffectKind.values.length * 2
      val held    = Array.fill(count)(Array.fill(slots)(Option.empty[Fact[Nothing]]))
      val callers = Array.fill(count)(mutable.ArrayBuffer.empty[(Int, Step)])
      val queue   = mutable.Queue.empty[(Int, Fact[Nothing])]
      def offer(id: Int, effect: Fact[Nothing]): Unit =
        if (!(effect.onThis && methods(id)._2.name == "<init>")) {
          val place = slot(effect)
          if (held(id)(place).forall(current => current.assumed && !effect.assumed)) {
            held(id)(place) = Some(effect)
            queue.enqueue(id -> effect)
          }
        }
      (0 until count).foreach { id =>
        locals(id).foreach(offer(id, _))
        steps(id).foreach { step =>
          step.axiom match {
            case Some(axiom) => lasting(axiom, step.origin, wide = false, via = None).foreach(offer(id, _))
            case None        =>
              if (step.absent) offer(id, Fact(EffectKind.Unknown, s"calls ${step.name}"))
              step.targets.foreach(target => callers(target) += id -> step)
          }
        }
      }
      while (queue.nonEmpty) {
        val (id, effect) = queue.dequeue()
        if (held(id)(slot(effect)).contains(effect))
          callers(id).foreach((caller, step) =>
            lasting(Seq(effect), step.origin, step.wide, Some(step.name)).foreach(offer(caller, _))
          )
      }
      Array.tabulate(count)(id => () => held(id).flatten.toSet)
    }

    private def constant(id: Int, bounded: Array[Boolean]): Boolean = {
      val (_, method) = methods(id)
      !method.isNative && !traces(id).loops && !traces(id).allocates && method.dynamics.isEmpty &&
      steps(id).forall(step =>
        step.exceptional || step.initialiser || !step.absent && step.targets.forall(bounded(_))
      )
    }
  }

  private object Tarjan {

    /** The strongly connected components, each before any component that reaches it. */
    def components(count: Int, successors: Array[Array[Int]]): Vector[Vector[Int]] = {
      val index                  = Array.fill(count)(-1)
      val low                    = Array.fill(count)(0)
      val onStack                = Array.fill(count)(false)
      val stack                  = mutable.Stack.empty[Int]
      val result                 = Vector.newBuilder[Vector[Int]]
      val counter                = Array(0)
      val frames                 = mutable.Stack.empty[(Int, Int)]
      def enter(node: Int): Unit = {
        index(node) = counter(0)
        low(node) = counter(0)
        counter(0) += 1
        stack.push(node)
        onStack(node) = true
        frames.push((node, 0))
      }
      (0 until count).foreach { root =>
        if (index(root) == -1) {
          enter(root)
          while (frames.nonEmpty) {
            val (node, position) = frames.pop()
            if (position < successors(node).length) {
              frames.push((node, position + 1))
              val next = successors(node)(position)
              if (index(next) == -1) enter(next)
              else if (onStack(next)) low(node) = math.min(low(node), index(next))
            } else {
              if (low(node) == index(node)) {
                @annotation.tailrec
                def popped(members: List[Int]): List[Int] = {
                  val member = stack.pop()
                  onStack(member) = false
                  if (member == node) member :: members else popped(member :: members)
                }
                result += popped(Nil).toVector
              }
              frames.headOption.foreach((parent, _) => low(parent) = math.min(low(parent), low(node)))
            }
          }
        }
      }
      result.result()
    }
  }
}
