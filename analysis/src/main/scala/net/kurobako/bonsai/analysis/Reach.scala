package net.kurobako.bonsai.analysis

import net.kurobako.bonsai.analysis.ClassFiles.*
import scala.collection.mutable
import scala.reflect.NameTransformer
import scala.util.Using

object Reach {

  /** Nothing is known to be dead. */
  val empty: Reach = Reach(Set.empty, Set.empty, Set.empty, Set.empty)

  private val missingJdk =
    "the manifest names no jdk; regenerate it with the build plugin of this release (sbt: bonsaiAnalyse, maven: " +
      "bonsai:analyse), which writes a jdk line"

  private val topLevel = "$package"

  private enum Status {
    case Dead, Tested, Main

    def max(other: Status): Status = if (ordinal >= other.ordinal) this else other
  }

  /** The reach of the build `graph` describes, with the program entered where its roots say and the classes it runs
    * read from the outputs of `manifest`, its classpath and its JDK.
    */
  def of(manifest: Manifest, graph: Graph): Either[String, (Reach, Vector[String])] =
    manifest.jdk.toRight(missingJdk).flatMap { jdk =>
      val outputs   = manifest.outputs.filterNot(output => manifest.platforms(output.module))
      val libraries =
        manifest.classpath.filterNot((module, _) => manifest.platforms(module)).values.flatten.toVector.distinct
      ClassSource.of(outputs.map(_.path), libraries, Some(jdk)).map(source =>
        Using.resource(source)(source => reach(graph, outputs, manifest.platforms, ClassWorld(source.bytes(_))))
      )
    }

  private def classesOf(outputs: Vector[Output]): Map[String, Output.Scope] =
    outputs.flatMap { output =>
      Outputs.files(output.path).collect {
        case file if file.toString.endsWith(".class") =>
          output.path.relativize(file).toString.replace(java.io.File.separatorChar, '/').stripSuffix(".class") ->
            output.scope
      }
    }.toMap

  private def reach(
      graph: Graph,
      outputs: Vector[Output],
      platforms: Set[String],
      world: ClassWorld
  ): (Reach, Vector[String]) = {
    val classes = classesOf(outputs)
    val bodied  = classes.keys.toVector.map(name => name -> world.get(name).toVector.flatMap(_.methods)).toMap
    val entered = graph.roots.filter(root => graph.nodes.get(root.name).exists(!_.test))
    val tests   = for {
      (name, scope) <- classes.toVector if scope == Output.Scope.Test
      method        <- bodied(name) if !method.isAbstract
    } yield Reference(name, method.name, method.descriptor)
    val calls = CallGraph.build(
      world,
      CallGraph.roots(graph.copy(roots = entered ++ twinned(graph, platforms, Output.Scope.Main)), world),
      build = classes.contains,
      tests = tests.toSet ++ CallGraph.roots(graph.copy(roots = twinned(graph, platforms, Output.Scope.Test)), world)
    )
    val naming  = Naming(graph.nodes)
    val methods = mutable.Map.empty[String, Status]
    val reached = mutable.Set.empty[String]
    val ran     = mutable.Set.empty[String]
    classes.keys.foreach { name =>
      val states = bodied(name).map { method =>
        val reference = Reference(name, method.name, method.descriptor)
        if (!calls.reachable(reference)) Status.Dead
        else if (calls.testOnly(reference)) Status.Tested
        else Status.Main
      }
      bodied(name).zip(states).foreach { (method, state) =>
        if (!method.isAbstract) {
          val key = world.key(Reference(name, method.name, method.descriptor))
          methods.update(key, methods.get(key).fold(state)(_.max(state)))
        }
        naming.ofMethod(name, method.name).foreach { top =>
          if (state != Status.Dead) reached += top
          if (state == Status.Main) ran += top
        }
      }
      val created = calls.instantiated(name)
      naming.ofClass(name).foreach { node =>
        if (created || states.exists(_ != Status.Dead)) reached += node
        if ((created && !calls.testInstantiated(name)) || states.contains(Status.Main)) ran += node
      }
    }
    val keys   = methods.groupMap(_._2)(_._1).view.mapValues(_.toSet).toMap
    val dead   = keys.getOrElse(Status.Dead, Set.empty)
    val tested = keys.getOrElse(Status.Tested, Set.empty)
    val rooted = entered.map(_.name)
    val checks = graph.nodes.values.collect { case node if node.test => node.name }
    val every  = closure(reached ++ rooted ++ checks, graph, use => use.method.forall(!dead(_)))
    val main   = closure(ran ++ rooted, graph, use => use.method.forall(key => !dead(key) && !tested(key)))
    val unread = world.unreadable.collect {
      case (name, error) if classes.contains(name) =>
        s"class file ${name.replace('/', '.')} could not be read: $error"
    }
    (Reach(graph.nodes.keySet -- every, every -- main, dead, tested), unread.toVector.sorted)
  }

  private def twinned(graph: Graph, platforms: Set[String], scope: Output.Scope): Set[Root] =
    for {
      node   <- graph.nodes.values.toSet if platforms(node.module) && node.scope == scope
      target <- graph.edges.getOrElse(node.name, Set.empty)
      if graph.nodes.get(target).exists(other => !platforms(other.module))
    } yield Root(target, Root.Kind.Exported)

  private def closure(seeds: collection.Set[String], graph: Graph, running: Use => Boolean): Set[String] = {
    val inside  = graph.bodies.toVector.groupMap(_._1.node)(entry => entry)
    val visited = mutable.Set.empty[String]
    val pending = mutable.Queue.empty[String]
    @annotation.tailrec
    def enter(name: Option[String]): Unit = name match {
      case Some(found) if visited.add(found) =>
        pending.enqueue(found)
        enter(graph.nodes.get(found).flatMap(_.outer))
      case _ => ()
    }
    seeds.foreach(name => enter(Some(name)))
    while (pending.nonEmpty) {
      val node = pending.dequeue()
      graph.declared.getOrElse(node, Set.empty).foreach(target => enter(Some(target)))
      inside.getOrElse(node, Vector.empty).foreach((use, targets) =>
        if (running(use)) targets.foreach(target => enter(Some(target)))
      )
    }
    visited.toSet
  }

  private class Naming(nodes: Map[String, Node]) {

    private def wrapper(binary: String): Boolean = binary.stripSuffix("$").endsWith(topLevel)

    private def packaged(binary: String): String = {
      val slash = binary.lastIndexOf('/')
      if (slash < 0) "" else binary.take(slash + 1).replace('/', '.')
    }

    /** The node of a class: the longest prefix of its name, read as nested types, that the graph holds. */
    def ofClass(binary: String): Option[String] =
      Option.unless(wrapper(binary)) {
        val segments = binary.drop(binary.lastIndexOf('/') + 1).split('$').filter(_.nonEmpty).toVector
        segments.inits.filter(_.nonEmpty).map(prefix => packaged(binary) + prefix.mkString(".")).find(nodes.contains)
      }.flatten

    /** The node of the top-level definition that a method of the wrapper class of its file stands for. */
    def ofMethod(binary: String, method: String): Option[String] =
      Option.when(wrapper(binary)) {
        Vector(method, method.takeWhile(_ != '$')).map(name => packaged(binary) + NameTransformer.decode(name))
          .find(nodes.contains)
      }.flatten
  }
}

/** What the program can run, from the classes of the build: the nodes and methods no root reaches, and those only the
  * tests reach. A node a node-level reference keeps alive, as a type named only in a type position is, is live, and a
  * method that no class file of the build keys alike, as the code of a platform twin or an inline method, is too.
  *
  * Methods are named by `Keys`, so the uses the sources record meet the methods the bytecode runs.
  */
case class Reach(
    dead: Set[String],
    tested: Set[String],
    deadMethods: Set[String],
    testedMethods: Set[String]
) {

  /** Whether any root, the tests included, reaches the node. */
  def live(node: String): Boolean = !dead(node)

  /** Whether a root other than a test reaches the node. */
  def main(node: String): Boolean = !dead(node) && !tested(node)

  /** Whether any root reaches the code the use is in. */
  def liveUse(use: Use): Boolean = live(use.node) && use.method.forall(!deadMethods(_))

  /** Whether a root other than a test reaches the code the use is in. */
  def mainUse(use: Use): Boolean = liveUse(use) && main(use.node) && use.method.forall(!testedMethods(_))
}
