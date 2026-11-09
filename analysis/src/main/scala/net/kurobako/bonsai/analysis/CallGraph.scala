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

  /** The methods reachable from `roots`, and then from `tests` as well; a method reached only after `tests` enter is
    * `testOnly`. Native methods and missing classes are leaves. A class `build` accepts that the program instantiates
    * has its overrides of methods declared outside the build reached, since code outside the build may dispatch to
    * them.
    */
  def build(
      world: ClassWorld,
      roots: Set[Reference],
      build: String => Boolean = _ => false,
      tests: Set[Reference] = Set.empty
  ): CallGraph = Walk(world, build).run(roots, tests)

  /** The methods the roots of `graph` enter the program at, by the kind of each root. */
  def roots(graph: Graph, world: ClassWorld): Set[Reference] =
    graph.roots.flatMap(root => classes(root.name, world).flatMap(entered(_, root.kind, world)))

  /** The methods the uses name: those of the class each use names whose key is the method the use names. */
  def named(uses: Set[Use], world: ClassWorld): Set[Reference] = for {
    use    <- uses
    file   <- classes(use.node, world)
    method <- file.methods
    reference = Reference(file.name, method.name, method.descriptor)
    if use.method.contains(world.key(reference))
  } yield reference

  private def classes(name: String, world: ClassWorld): Vector[ClassFile] = {
    val parts = name.split('.').toVector
    parts.indices.view.map { nested =>
      val (packages, types) = parts.splitAt(parts.size - nested - 1)
      val binary            = (packages :+ types.mkString("$")).mkString("/")
      Vector(binary, s"$binary$$").flatMap(world.get(_))
    }.find(_.nonEmpty).getOrElse(Vector.empty)
  }

  private def isPublic(method: Method): Boolean = (method.access & (Opcodes.ACC_PUBLIC | Opcodes.ACC_PROTECTED)) != 0

  private def mainOf(file: ClassFile, world: ClassWorld): Option[Reference] =
    world.resolve(Reference(file.name, "main", mainDescriptor)).filter(_.method.isStatic)
      .map(target => Reference(target.owner, "main", mainDescriptor))

  private def entered(file: ClassFile, kind: Root.Kind, world: ClassWorld): Vector[Reference] = {
    def reference(method: Method): Reference = Reference(file.name, method.name, method.descriptor)
    def overrides(method: Method): Boolean   =
      method.overridable && method.name != "<init>" &&
        world.ancestors(file.name).exists(world.declares(_, method))
    def provided(method: Method): Boolean =
      (method.name == "<init>" && isPublic(method) && method.descriptor == "()V") || overrides(method) ||
        (method.name == "provider" && method.isStatic && isPublic(method) && method.descriptor.startsWith("()")) ||
        (method.name == "<clinit>" && file.fields.exists(field => field.name == "MODULE$" && field.isStatic))
    kind match {
      case Root.Kind.Main       => mainOf(file, world).toVector
      case Root.Kind.Exported   => file.methods.filter(isPublic(_)).map(reference(_))
      case Root.Kind.Reflective => file.methods.map(reference(_))
      case Root.Kind.Provided   => file.methods.filter(provided(_)).map(reference(_))
    }
  }

  /** A class the program instantiates, or a lambda, which is a class of its own. */
  enum Receiver {
    case Instance(name: String)
    case Closure(site: LambdaSite)

    /** The class the receiver is of, or that holds the lambda. */
    def owner: String = this match {
      case Instance(name) => name
      case Closure(site)  => site.holder.owner
    }
  }

  private def runs(callee: Reference, lambda: Lambda): Boolean =
    callee.name == lambda.sam &&
      (callee.descriptor == lambda.samDescriptor || lambda.bridges.contains(callee.descriptor))

  private def conforms(name: String, owner: String, world: ClassWorld): Boolean =
    name == owner || world.ancestors(name).contains(owner)

  private class Walk(world: ClassWorld, build: String => Boolean) {
    private val reachable    = mutable.Set.empty[Reference]
    private val targets      = mutable.Map.empty[Reference, mutable.Set[Reference]]
    private val pending      = mutable.Queue.empty[Reference]
    private val instances    = mutable.Set.empty[String]
    private val closures     = mutable.Map.empty[LambdaSite, Lambda]
    private val initialised  = mutable.Set.empty[String]
    private val sitesByOwner = mutable.Map.empty[String, mutable.Set[Reference]]
    private val receivers    = mutable.Map.empty[String, mutable.Set[Receiver]]
    private val testMethods  = mutable.Set.empty[Reference]
    private val testClasses  = mutable.Set.empty[String]
    private var testing      = false // scalafix:ok DisableSyntax.var

    def run(roots: Set[Reference], tests: Set[Reference]): CallGraph = {
      created.foreach(instantiate(_))
      upcalls.foreach(site(_))
      roots.foreach(root => enter(root))
      drain()
      testing = true
      tests.foreach(root => enter(root))
      drain()
      CallGraph(
        reachable.toSet,
        targets.view.mapValues(_.toSet).toMap,
        instances.toSet,
        closures.toMap,
        testMethods.toSet,
        testClasses.toSet
      )
    }

    private def drain(): Unit = while (pending.nonEmpty) visit(pending.dequeue())

    private def enter(root: Reference): Unit =
      world.resolve(root).foreach { target =>
        reach(reference(target))
        if (target.method.isStatic) initialise(target.owner)
        else if (world.get(target.owner).exists(file => !file.isAbstract && !file.isInterface))
          instantiate(target.owner)
        // code outside the build calls a root virtually, so the overrides of the instantiated classes run too
        if (target.method.overridable && target.method.name != "<init>") site(root)
      }

    private def reference(target: ClassWorld.Target): Reference =
      Reference(target.owner, target.method.name, target.method.descriptor)

    private def reach(method: Reference): Unit =
      if (reachable.add(method)) {
        if (testing) testMethods += method
        pending.enqueue(method)
      }

    private def reached(callee: Reference, target: Reference): Unit =
      if (targets.getOrElseUpdate(callee, mutable.Set.empty).add(target)) reach(target)

    private def visit(method: Reference): Unit =
      world.method(method).foreach(body(method, _))

    private def body(holder: Reference, method: Method): Unit = {
      method.invocations.foreach { invocation =>
        val callee = Reference(invocation.owner, invocation.name, invocation.descriptor)
        invocation.kind match {
          case Kind.Static                   => direct(callee, initialising = true)
          case Kind.Special                  => direct(callee, initialising = false)
          case Kind.Virtual | Kind.Interface =>
            if (callee.owner.startsWith("[")) direct(callee, initialising = false)
            else site(callee)
        }
      }
      method.instantiated.foreach(instantiate(_))
      method.lambdas.zipWithIndex.foreach((lambda, position) => closure(LambdaSite(holder, position), lambda))
      method.fields.filter(_.static).foreach { access =>
        world.fieldHolder(access).foreach(initialise(_))
      }
      if (method.dynamics.exists(_.owner == concatenation)) site(stringified)
    }

    private def direct(callee: Reference, initialising: Boolean): Unit =
      world.resolve(callee).foreach { target =>
        reach(reference(target))
        if (initialising) initialise(target.owner)
      }

    private def site(callee: Reference): Unit =
      if (sitesByOwner.getOrElseUpdate(callee.owner, mutable.Set.empty).add(callee))
        receivers.getOrElse(callee.owner, mutable.Set.empty).toVector.foreach(link(callee, _))

    private def link(callee: Reference, receiver: Receiver): Unit =
      receiver match {
        case Receiver.Instance(name) =>
          world.select(name, callee).foreach(target => reached(callee, reference(target)))
        case Receiver.Closure(at) => invoke(callee, closures(at))
      }

    private def invoke(callee: Reference, lambda: Lambda): Unit =
      if (runs(callee, lambda)) implementation(callee, lambda)
      else
        (lambda.interface +: lambda.markers).view.flatMap(world.select(_, callee)).find(!_.method.isAbstract)
          .foreach(target => reached(callee, reference(target)))

    private def implementation(callee: Reference, lambda: Lambda): Unit = {
      val handle = Reference(lambda.owner, lambda.name, lambda.descriptor)
      lambda.tag match {
        case Opcodes.H_INVOKEVIRTUAL | Opcodes.H_INVOKEINTERFACE => site(handle)
        case tag                                                 =>
          world.resolve(handle).foreach { target =>
            reached(callee, reference(target))
            if (tag == Opcodes.H_INVOKESTATIC) initialise(target.owner)
          }
      }
    }

    private def closure(at: LambdaSite, lambda: Lambda): Unit = {
      closures(at) = lambda
      if (lambda.tag == Opcodes.H_NEWINVOKESPECIAL) instantiate(lambda.owner)
      val types = (lambda.interface +: lambda.markers).flatMap(name => name +: world.ancestors(name))
      receive(Receiver.Closure(at), (types :+ objectClass).distinct)
    }

    private def instantiate(name: String): Unit =
      if (instances.add(name)) {
        if (testing) testClasses += name
        initialise(name)
        receive(Receiver.Instance(name), name +: world.ancestors(name))
        if (build(name)) callbacks(name)
      }

    private def callbacks(name: String): Unit =
      world.get(name).foreach { file =>
        val outside = world.ancestors(name).filterNot(build)
        file.methods.filter(method => method.overridable && !method.isAbstract && method.name != "<init>").foreach {
          method =>
            if (outside.exists(world.declares(_, method)))
              reach(Reference(name, method.name, method.descriptor))
        }
      }

    private def receive(receiver: Receiver, types: Vector[String]): Unit = {
      types.foreach(receivers.getOrElseUpdate(_, mutable.Set.empty) += receiver)
      types.foreach(tpe =>
        sitesByOwner.getOrElse(tpe, mutable.Set.empty).toVector.foreach(link(_, receiver))
      )
    }

    private def initialise(name: String): Unit =
      if (initialised.add(name))
        world.get(name).foreach { file =>
          if (!file.isInterface) {
            file.superName.foreach(initialise(_))
            world.ancestors(name).filter(ancestor =>
              world.get(ancestor).exists(parent => declaresDefault(parent))
            ).foreach(initialise(_))
          }
          if (file.methods.exists(_.name == "<clinit>")) {
            val clinit = Reference(name, "<clinit>", "()V")
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
  * references it holds. `testOnly` and `testInstantiated` are what only the tests reach and create.
  */
private[analysis] case class CallGraph(
    reachable: Set[Reference],
    dispatch: Map[Reference, Set[Reference]],
    instantiated: Set[String],
    lambdas: Map[LambdaSite, Lambda],
    testOnly: Set[Reference],
    testInstantiated: Set[String]
) {

  /** The classes and lambdas whose override of `callee` a call of it can run, which is the same for every site of
    * `callee`; it is derived from the instantiated types and lambdas rather than stored.
    */
  def receivers(callee: Reference, world: ClassWorld): Vector[Receiver] = {
    val owner  = callee.owner
    val direct = instantiated.toVector.filter(name =>
      conforms(name, owner, world) && world.select(name, callee).isDefined
    ).sorted.map(Receiver.Instance(_))
    val closures = lambdas.toVector.filter { (_, lambda) =>
      owner == objectClass || (lambda.interface +: lambda.markers).exists(conforms(_, owner, world))
    }.filter((_, lambda) =>
      runs(callee, lambda) ||
        (lambda.interface +: lambda.markers).view.flatMap(world.select(_, callee)).exists(!_.method.isAbstract)
    ).map(_._1).sortBy(site => (site.holder.toString, site.position)).map(Receiver.Closure(_))
    direct ++ closures
  }
}
