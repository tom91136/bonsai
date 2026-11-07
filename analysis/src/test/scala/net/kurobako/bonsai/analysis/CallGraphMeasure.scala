package net.kurobako.bonsai.analysis

import java.lang.management.ManagementFactory
import java.lang.management.MemoryType
import java.nio.file.Files
import java.nio.file.Path
import java.time.Duration
import java.time.Instant
import net.kurobako.bonsai.analysis.ClassFiles.*
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Runs the call graph over a manifest, with the arguments of the command line, and prints what it reaches by where the
  * classes come from, and what a walk that reads fewer bodies would lose.
  */
object CallGraphMeasure {
  private case class Timed[A](value: A, seconds: Double, peakMegabytes: Long, liveMegabytes: Long)

  private def timed[A](work: => A): Timed[A] = {
    val pools = ManagementFactory.getMemoryPoolMXBeans.asScala.filter(_.getType == MemoryType.HEAP)
    ManagementFactory.getMemoryMXBean.gc()
    val live = ManagementFactory.getMemoryMXBean.getHeapMemoryUsage.getUsed >> 20
    pools.foreach(_.resetPeakUsage())
    val start  = Instant.now()
    val result = work
    val taken  = Duration.between(start, Instant.now()).toMillis / 1000.0
    Timed(result, taken, pools.map(_.getPeakUsage.getUsed).sum >> 20, live)
  }

  private def classesOf(directory: Path): Vector[String] =
    Using.resource(Files.walk(directory))(_.toList.asScala.toVector).filter(_.toString.endsWith(".class")).map(path =>
      directory.relativize(path).toString.stripSuffix(".class")
    )

  private def source(outputs: Vector[Path], libraries: Vector[Path], jdk: Option[Path]): ClassSource =
    ClassSource.of(outputs, libraries, jdk).fold(error => sys.error(error), identity)

  def main(args: Array[String]): Unit = {
    val options   = CLI.parse(args.toList).fold(error => sys.error(error), identity)
    val manifest  = Manifest.read(options.manifest.get).fold(error => sys.error(error), identity)
    val extracted = timed(
      Using.resource(Parallel(math.max(options.parallelism, 4)))(parallel =>
        Extraction.read(
          manifest,
          options.generated,
          options.concurrent,
          options.reflective,
          options.hot,
          options.bounded,
          parallel
        ).fold(errors => sys.error(errors.mkString("\n")), identity)
      )
    )
    val graph = extracted.value.graph
    println(
      f"extraction: ${extracted.seconds}%.1f s, peak ${extracted.peakMegabytes} MB over ${extracted.liveMegabytes} MB live, ${graph.roots.size} root types"
    )
    val jdk     = manifest.jdk.orElse(Some(Path.of(sys.props("java.home"))))
    val outputs = manifest.outputs.filterNot(output => manifest.platforms(output.module))
    val libs = manifest.classpath.filterNot((module, _) => manifest.platforms(module)).values.flatten.toVector.distinct
    Using.Manager { use =>
      val everything = use(source(outputs.map(_.path), libs, jdk))
      val jdkOnly    = use(source(Vector.empty, Vector.empty, jdk))
      val buildNames = outputs.flatMap(output => classesOf(output.path).map(_ -> output.scope)).toMap
      val world      = ClassWorld(everything.bytes(_))
      val origin     = (name: String) =>
        if (buildNames.contains(name)) "build" else if (jdkOnly.bytes(name).isDefined) "jdk" else "library"
      val roots   = CallGraph.roots(graph, world)
      val methods = buildNames.keys.toVector.flatMap(name =>
        world.get(name).toVector.flatMap(_.methods.filterNot(m => m.isAbstract || m.isNative).map(m =>
          Reference(name, m.name, m.descriptor)
        ))
      )
      val total = methods.count(method => buildNames(method.owner) == Output.Scope.Main)
      println(s"roots: ${roots.size} methods from ${graph.roots.size} types, ${roots.count(_.name == "main")} mains")
      println(s"build classes ${buildNames.size}, methods with a body ${methods.size}, in main scope $total")

      def reachableBuild(graph: CallGraph): Set[Reference] = graph.reachable.filter(m => buildNames.contains(m.owner))

      def report(label: String, found: Timed[CallGraph]): Unit = {
        val reached = found.value
        val by      = reached.reachable.groupBy(method => origin(method.owner)).view.mapValues(_.size).toMap
        val main    = reachableBuild(reached).count(method => buildNames(method.owner) == Output.Scope.Main)
        println(
          f"$label: ${found.seconds}%.1f s, peak ${found.peakMegabytes} MB over ${found.liveMegabytes} MB live, reachable ${reached.reachable.size} " +
            s"(jdk ${by.getOrElse("jdk", 0)}, library ${by.getOrElse("library", 0)}, " +
            s"build ${by.getOrElse("build", 0)}), main-scope build methods $main of $total, " +
            s"instantiated ${reached.instantiated.size}, lambdas ${reached.lambdas.size}, " +
            s"unresolved ${reached.unresolved.size}, held references ${reached.sites.values.map(_.size.toLong).sum}, dispatch pairs ${reached.dispatch.values.map(_.size.toLong).sum}"
        )
      }

      def upcalls(graph: CallGraph, scanned: String => Boolean): Set[Reference] = {
        val overriding = graph.instantiated.filter(scanned).flatMap(world.get(_)).flatMap { file =>
          val foreign = world.ancestors(file.name).filterNot(scanned)
          file.methods.filter { method =>
            method.overridable && method.name != "<init>" &&
            foreign.exists(
              world.get(_).exists(
                _.methods.exists(other =>
                  other.name == method.name && other.descriptor == method.descriptor && other.overridable
                )
              )
            )
          }.map(method => Reference(file.name, method.name, method.descriptor))
        }
        val closures = graph.lambdas.collect {
          case (site, lambda) if scanned(site.holder.owner) => Reference(lambda.owner, lambda.name, lambda.descriptor)
        }
        overriding ++ closures
      }

      @annotation.tailrec
      def withUpcalls(scanned: String => Boolean, extra: Set[Reference]): CallGraph = {
        val next  = CallGraph.build(world, roots ++ extra, scanned)
        val grown = upcalls(next, scanned)
        if (grown.subsetOf(extra)) next else withUpcalls(scanned, extra ++ grown)
      }

      val full = timed(CallGraph.build(world, roots))
      report("full", full)
      report("full again with the classes loaded", timed(CallGraph.build(world, roots)))
      val loaded = world.loaded.groupBy(origin).view.mapValues(_.size).toMap
      println(s"classes loaded ${world.loaded.size} $loaded, missing ${world.missing.size}")
      val (slashed, mangled) = world.missing.partition(_.contains('/'))
      println(s"  missing with a package ${slashed.size}, without ${mangled.size}")
      slashed.toVector.sorted.take(8).foreach(name => println(s"  missing class $name"))
      mangled.toVector.sorted.take(3).foreach(name => println(s"  missing class $name"))
      full.value.unresolved.toVector.sortBy(_.toString).take(6).foreach(ref => println(s"  unresolved $ref"))
      val kinds = graph.roots.toVector.sorted.map { name =>
        val found = CallGraph.roots(graph.copy(roots = Set(name)), world)
        val node  = graph.nodes.get(name)
        s"$name exported=${node.exists(_.exported)} test=${node.exists(_.test)} methods=${found.size}"
      }
      kinds.foreach(line => println(s"  root $line"))
      report("seeds and upcalls only", timed(CallGraph.build(world, Set.empty)))
      val dead = methods.filter(m => buildNames(m.owner) == Output.Scope.Main && !full.value.reachable(m))
      println(s"unreachable main-scope build methods: ${dead.size}")
      dead.groupBy(_.owner.split('/').dropRight(1).mkString("/")).toVector.sortBy(-_._2.size).take(8).foreach {
        (pkg, found) => println(s"  $pkg ${found.size}")
      }
      dead.groupBy(_.owner.takeWhile(_ != '$')).toVector.sortBy(-_._2.size).take(12).foreach { (cls, found) =>
        println(s"  class $cls ${found.size}")
      }
      if (!sys.props.contains("measure.full")) {
        val isBuild  = (name: String) => buildNames.contains(name)
        val notJdk   = (name: String) => origin(name) != "jdk"
        val variants = Vector(
          "build bodies only"        -> timed(CallGraph.build(world, roots, isBuild)),
          "stop at jdk"              -> timed(CallGraph.build(world, roots, notJdk)),
          "build bodies and upcalls" -> timed(withUpcalls(isBuild, Set.empty)),
          "stop at jdk and upcalls"  -> timed(withUpcalls(notJdk, Set.empty))
        )
        val exact = reachableBuild(full.value)
        variants.foreach { (label, found) =>
          report(label, found)
          val build = reachableBuild(found.value)
          println(s"  against full: build methods missing ${exact.diff(build).size}, extra ${build.diff(exact).size}")
        }
        variants.foreach { (label, found) =>
          exact.diff(reachableBuild(found.value)).take(8).foreach(method => println(s"  missing under $label: $method"))
        }
      }
    }.get
  }
}
