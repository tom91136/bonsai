package net.kurobako.bonsai.analysis

import net.kurobako.bonsai.analysis.CallGraph.*
import net.kurobako.bonsai.analysis.ClassFiles.*
import org.objectweb.asm.Opcodes
import scala.collection.mutable

private[analysis] object CallGraph {

  /** The `position`th lambda of the method `holder`; each such site is one class of its own. */
  case class LambdaSite(holder: Reference, position: Int)

  private val objectClass = "java/lang/Object"

  // The JVM creates strings for literals and classes for class literals without a `new`.
  private val created = Vector("java/lang/String", "java/lang/Class")

  // The JVM runs `Thread.run` on the thread it starts, with no call in any method. The `Runnable` of a thread is
  // reached through the body of that method.
  private val upcalls = Vector(Reference("java/lang/Thread", "run", "()V"))

  private val concatenation = "java/lang/invoke/StringConcatFactory"
  private val stringified   = Reference(objectClass, "toString", "()Ljava/lang/String;")

  private val mainDescriptor = "([Ljava/lang/String;)V"

  /** The methods reachable from `roots`. A body is read only for the classes `scan` accepts, and the others are leaves
    * as native methods and missing classes are.
    */
  def build(world: ClassWorld, roots: Set[Reference], scan: String => Boolean = _ => true): CallGraph =
    Walk(world, scan).run(roots)

  /** The methods the roots of `graph` enter the program at. A type with a main method roots it, and a type the build
    * exports roots its public and protected members. Any other root, which the build names to a loader, roots its
    * public constructors and the methods it overrides, and the initialiser of an object.
    */
  def roots(graph: Graph, world: ClassWorld): Set[Reference] =
    graph.roots.flatMap { name =>
      val exported = graph.nodes.get(name).exists(_.exported)
      val files    = classes(name, world)
      val mains    = files.flatMap(mainOf(_, world)).distinct
      if (mains.nonEmpty && !exported) mains else mains ++ files.flatMap(entered(_, exported, world))
    }

  private def classes(name: String, world: ClassWorld): Vector[ClassFile] = {
    val parts = name.split('.').toVector
    parts.indices.view.map { nested =>
      val (packages, types) = parts.splitAt(parts.size - nested - 1)
      val binary            = (packages :+ types.mkString("$")).mkString("/")
      Vector(binary, s"$binary$$").flatMap(world.get(_))
    }.find(_.nonEmpty).getOrElse(Vector.empty)
  }

  private def mainOf(file: ClassFile, world: ClassWorld): Option[Reference] =
    world.resolve(Reference(file.name, "main", mainDescriptor)).filter(_.method.isStatic)
      .map(target => Reference(target.owner, "main", mainDescriptor))

  private def isPublic(method: Method): Boolean = (method.access & (Opcodes.ACC_PUBLIC | Opcodes.ACC_PROTECTED)) != 0

  private def entered(file: ClassFile, exported: Boolean, world: ClassWorld): Vector[Reference] = {
    def reference(method: Method): Reference = Reference(file.name, method.name, method.descriptor)
    def overrides(method: Method): Boolean   =
      method.overridable && method.name != "<init>" &&
        world.ancestors(file.name).exists(
          world.get(_).exists(
            _.methods.exists(other =>
              other.name == method.name && other.descriptor == method.descriptor && other.overridable
            )
          )
        )
    def provider(method: Method): Boolean =
      method.name == "provider" && method.isStatic && isPublic(method) && method.descriptor.startsWith("()")
    if (exported)
      file.methods.filter(method => isPublic(method)).filterNot(_.name == "<clinit>").map(method => reference(method))
    else {
      val loaded = file.methods.filter(method => (method.name == "<init>" && isPublic(method)) || overrides(method))
      val module = file.fields.exists(field => field.name == "MODULE$" && field.isStatic)
      (loaded ++ file.methods.filter(method => provider(method))).map(method => reference(method)) ++
        Option.when(module)(Reference(file.name, "<clinit>", "()V"))
    }
  }

  private enum Receiver {
    case Instance(name: String)
    case Closure(site: LambdaSite)
  }

  private class Walk(world: ClassWorld, scan: String => Boolean) {
    private val reachable    = mutable.Set.empty[Reference]
    private val pending      = mutable.Queue.empty[Reference]
    private val direct       = mutable.Map.empty[Reference, mutable.Set[Reference]]
    private val sites        = mutable.Map.empty[Reference, mutable.Set[Reference]]
    private val instances    = mutable.Set.empty[String]
    private val closures     = mutable.Map.empty[LambdaSite, Lambda]
    private val initialised  = mutable.Set.empty[String]
    private val unresolved   = mutable.Set.empty[Reference]
    private val targets      = mutable.Map.empty[Reference, mutable.Set[Reference]]
    private val sitesByOwner = mutable.Map.empty[String, mutable.Set[Reference]]
    private val receivers    = mutable.Map.empty[String, mutable.Set[Receiver]]

    def run(roots: Set[Reference]): CallGraph = {
      created.foreach(name => instantiate(None, name))
      upcalls.foreach(callee => site(None, callee))
      roots.foreach(root => enter(root))
      while (pending.nonEmpty) visit(pending.dequeue())
      CallGraph(
        reachable.toSet,
        direct.view.mapValues(_.toSet).toMap,
        sites.view.mapValues(_.toSet).toMap,
        targets.view.mapValues(_.toSet).toMap,
        instances.toSet,
        closures.toMap,
        unresolved.toSet
      )
    }

    private def enter(root: Reference): Unit =
      world.resolve(root) match {
        case Some(target) =>
          reach(reference(target))
          if (target.method.isStatic) initialise(None, target.owner)
          else if (world.get(target.owner).exists(file => !file.isAbstract && !file.isInterface))
            instantiate(None, target.owner)
        case None => unresolved += root: Unit
      }

    private def reference(target: ClassWorld.Target): Reference =
      Reference(target.owner, target.method.name, target.method.descriptor)

    private def reach(method: Reference): Unit =
      if (reachable.add(method)) pending.enqueue(method)

    private def edge(from: Reference, to: Reference): Unit =
      direct.getOrElseUpdate(from, mutable.Set.empty) += to: Unit

    private def visit(method: Reference): Unit =
      if (scan(method.owner))
        world.get(method.owner)
          .flatMap(_.methods.find(found => found.name == method.name && found.descriptor == method.descriptor))
          .foreach(body(method, _))

    private def body(holder: Reference, method: Method): Unit = {
      method.invocations.foreach { invocation =>
        val callee = Reference(invocation.owner, invocation.name, invocation.descriptor)
        invocation.kind match {
          case Kind.Static                   => direct(holder, callee, initialising = true)
          case Kind.Special                  => direct(holder, callee, initialising = false)
          case Kind.Virtual | Kind.Interface =>
            if (callee.owner.startsWith("[")) direct(holder, callee, initialising = false)
            else site(Some(holder), callee)
        }
      }
      method.instantiated.foreach(instantiate(Some(holder), _))
      method.lambdas.zipWithIndex.foreach((lambda, position) => closure(LambdaSite(holder, position), lambda))
      method.fields.filter(_.static).foreach { access =>
        world.fieldHolder(access).foreach(initialise(Some(holder), _))
      }
      if (method.dynamics.exists(_.owner == concatenation)) site(Some(holder), stringified)
    }

    private def direct(holder: Reference, callee: Reference, initialising: Boolean): Unit =
      world.resolve(callee) match {
        case Some(target) =>
          val resolved = reference(target)
          edge(holder, resolved)
          reach(resolved)
          if (initialising) initialise(Some(holder), target.owner)
        case None => unresolved += callee: Unit
      }

    private def site(holder: Option[Reference], callee: Reference): Unit = {
      holder.foreach(sites.getOrElseUpdate(_, mutable.Set.empty) += callee: Unit)
      if (sitesByOwner.getOrElseUpdate(callee.owner, mutable.Set.empty).add(callee)) {
        if (world.resolve(callee).isEmpty) unresolved += callee: Unit
        receivers.getOrElse(callee.owner, mutable.Set.empty).toVector.foreach(link(callee, _))
      }
    }

    private def reached(callee: Reference, target: Reference): Unit =
      if (targets.getOrElseUpdate(callee, mutable.Set.empty).add(target)) reach(target)

    private def link(callee: Reference, receiver: Receiver): Unit =
      receiver match {
        case Receiver.Instance(name) =>
          world.select(name, callee) match {
            case Some(target) => reached(callee, reference(target))
            case None         => unresolved += callee: Unit
          }
        case Receiver.Closure(at) => invoke(callee, at, closures(at))
      }

    private def invoke(callee: Reference, at: LambdaSite, lambda: Lambda): Unit =
      if (
        callee.name == lambda.sam &&
        (callee.descriptor == lambda.samDescriptor || lambda.bridges.contains(callee.descriptor))
      ) implementation(callee, at, lambda)
      else
        (lambda.interface +: lambda.markers).view.flatMap(world.select(_, callee)).find(!_.method.isAbstract)
          .foreach(target => reached(callee, reference(target)))

    private def implementation(callee: Reference, at: LambdaSite, lambda: Lambda): Unit = {
      val handle = Reference(lambda.owner, lambda.name, lambda.descriptor)
      lambda.tag match {
        case Opcodes.H_INVOKEVIRTUAL | Opcodes.H_INVOKEINTERFACE => site(Some(at.holder), handle)
        case tag                                                 =>
          world.resolve(handle) match {
            case Some(target) =>
              reached(callee, reference(target))
              if (tag == Opcodes.H_INVOKESTATIC) initialise(Some(at.holder), target.owner)
            case None => unresolved += handle: Unit
          }
      }
    }

    private def closure(at: LambdaSite, lambda: Lambda): Unit =
      if (!closures.contains(at)) {
        closures(at) = lambda
        if (lambda.tag == Opcodes.H_NEWINVOKESPECIAL) instantiate(Some(at.holder), lambda.owner)
        val types = (lambda.interface +: lambda.markers).flatMap(name => name +: world.ancestors(name))
        receive(Receiver.Closure(at), (types :+ objectClass).distinct)
      }

    private def instantiate(holder: Option[Reference], name: String): Unit =
      if (instances.add(name)) {
        initialise(holder, name)
        receive(Receiver.Instance(name), name +: world.ancestors(name))
      }

    private def receive(receiver: Receiver, types: Vector[String]): Unit = {
      types.foreach(receivers.getOrElseUpdate(_, mutable.Set.empty) += receiver)
      types.foreach(tpe =>
        sitesByOwner.getOrElse(tpe, mutable.Set.empty).toVector.foreach(link(_, receiver))
      )
    }

    private def initialise(holder: Option[Reference], name: String): Unit =
      if (initialised.add(name))
        world.get(name).foreach { file =>
          if (!file.isInterface) {
            file.superName.foreach(initialise(holder, _))
            world.ancestors(name).filter(ancestor =>
              world.get(ancestor).exists(parent => declaresDefault(parent))
            ).foreach(initialise(holder, _))
          }
          if (file.methods.exists(_.name == "<clinit>")) {
            val clinit = Reference(name, "<clinit>", "()V")
            holder.foreach(edge(_, clinit))
            reach(clinit)
          }
        }

    private def declaresDefault(file: ClassFile): Boolean =
      file.isInterface && file.methods.exists(method => !method.isAbstract && !method.isStatic)
  }
}

/** The methods the program can run, by rapid type analysis over bytecode. A virtual or interface call reaches the
  * override of each class the program instantiates and of each lambda it creates, and nothing else. Calls are
  * `Reference`s as the class files name them and are never keyed, so a class and its object cannot merge in the walk.
  *
  * A method reaches the methods it calls directly, the initialisers it triggers, and the virtual or interface
  * references it holds; `dispatch` gives the methods each such reference reaches. `lambdas` holds the lambda sites
  * whose holder is reachable, and `unresolved` the references no class declares.
  */
private[analysis] case class CallGraph(
    reachable: Set[Reference],
    direct: Map[Reference, Set[Reference]],
    sites: Map[Reference, Set[Reference]],
    dispatch: Map[Reference, Set[Reference]],
    instantiated: Set[String],
    lambdas: Map[LambdaSite, Lambda],
    unresolved: Set[Reference]
) {

  /** The methods each method reaches, which is as large as the product of the holders and targets of a reference that
    * many classes implement.
    */
  lazy val edges: Map[Reference, Set[Reference]] = (direct.keySet ++ sites.keySet).view.map { holder =>
    holder ->
      (direct.getOrElse(holder, Set.empty) ++ sites.getOrElse(holder, Set.empty).flatMap(
        dispatch.getOrElse(_, Set.empty)
      ))
  }.toMap
}
