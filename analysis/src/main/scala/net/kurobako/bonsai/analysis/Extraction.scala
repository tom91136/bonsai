package net.kurobako.bonsai.analysis

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.Compiler
import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.tasty.TastyUnpickler
import dotty.tools.dotc.fromtasty.ReadTasty
import dotty.tools.dotc.fromtasty.TASTYCompiler
import dotty.tools.dotc.reporting.StoreReporter
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import net.kurobako.bonsai.java.{Model => JavaModel}
import net.kurobako.bonsai.lifting.{Purity as _, *}
import scala.concurrent.Promise
import scala.util.Try
import scala.util.control.NonFatal

/** Builds the reference graph and algebraic model of a whole build from the TASTy in its outputs. */
object Extraction {

  /** The extracted model, with classes that could not be read or analysed listed as problems; `hot` holds the nodes of
    * the classes the build marks hot, when it marks any.
    */
  case class Result(
      graph: Graph,
      algebra: Algebra.Model,
      smells: Smells.Model,
      designs: Designs.Model,
      structures: Structures.Model,
      equality: Equality.Model,
      members: Members.Model,
      clones: Vector[Clones.Group],
      bytecode: Vector[Bytecode.Method],
      complexities: Vector[Complexity.Summary],
      unusedBounds: Vector[Complexity.Bounded],
      pipelines: Vector[Pipelines.Lifted],
      megamorphic: Vector[Megamorphic.Site],
      hot: Option[Set[String]],
      purity: Purity.Model,
      lints: Vector[JavaLints.Found],
      problems: Vector[String],
      nesting: Nesting.Model = Nesting.Model.empty
  )

  // The TASTy unpickler recurses over long import selector lists and deep trees.
  private val StackSize = 1L << 30

  // The compiler abandons recursion deeper than its fuel, which a chain of hundreds of calls exceeds.
  private val MaxFuel = 100000

  /** Extracts the build `manifest` describes; `concurrent` names methods that run the functions passed to them on other
    * threads, beside the standard ones, `reflective` globs the types the build names only in strings, and `hot` the
    * classes whose code runs hot, with `!` before a glob leaving out what it matches, and `bounded` the members and
    * parameters whose collections hold a declared number of elements; `facts` hold the bounds of methods the build
    * holds no code of. The stages that read files and touch no compiler state run on `parallel`.
    */
  def read(
      manifest: Manifest,
      generated: Vector[String] = Vector.empty,
      concurrent: Set[String] = Set.empty,
      reflective: Vector[String] = Vector.empty,
      hot: Vector[String] = Vector.empty,
      bounded: Vector[Complexity.Bounded] = Vector.empty,
      parallel: Parallel = Parallel.sequential
  ): Either[Vector[String], Result] = {
    val result = Promise[Either[Vector[String], Result]]()
    val thread = Thread(
      Thread.currentThread.getThreadGroup,
      () => result.complete(Try(extract(manifest, generated, concurrent, reflective, hot, bounded, parallel))): Unit,
      "bonsai-extraction",
      StackSize
    )
    thread.start()
    thread.join()
    result.future.value.fold(Left(Vector("extraction terminated abnormally")))(
      _.fold(
        error => {
          error.printStackTrace()
          Left(Vector(s"extraction failed: $error"))
        },
        identity
      )
    )
  }

  private def extract(
      manifest: Manifest,
      generated: Vector[String],
      concurrent: Set[String],
      reflective: Vector[String],
      hot: Vector[String],
      bounded: Vector[Complexity.Bounded],
      parallel: Parallel
  ): Either[Vector[String], Result] = {
    // The compiler aborts the whole run on a file whose names or sections it cannot index, so those are set aside.
    // A platform twin compiles the shared sources again, so only the classes the JVM modules lack are its own. The
    // compiler finds every class through the directories of the files it reads, in their order, so the JVM modules'
    // come first and the shared classes resolve to their copies.
    val (twins, primary) = manifest.outputs.partition(output => manifest.platforms(output.module))
    val listed           = (primary ++ twins).map(output => output -> Outputs.files(output.path))
    // Test support compiles its main sources in the test scope, so one scope can hold an empty output beside a full one.
    val sourced  = manifest.sources.map(source => (source.module, source.scope)).toSet
    val compiled = listed.collect {
      case (output, files) if files.exists(file => Set(".class", ".tasty").exists(file.toString.endsWith(_))) =>
        (output.module, output.scope)
    }.toSet
    val uncompiled = (sourced -- compiled).filter(listed.map((output, _) =>
      (output.module, output.scope)
    ).toSet).toVector
      .map((module, scope) =>
        s"$module has ${scope.id} sources but no classes; a failed compile can leave them empty and a cached compile " +
          "does not restore them, so run clean and compile again"
      )
      .sorted
    val shared = listed.collect {
      case (output, files) if !manifest.platforms(output.module) => files.map(output.path.relativize(_))
    }.flatten.toSet
    val (copies, kept) = listed.flatMap((output, files) =>
      files.collect { case file if file.toString.endsWith(".tasty") => output -> file }
    ).partition((output, file) => manifest.platforms(output.module) && shared(output.path.relativize(file)))
    val files        = kept.map(_._2)
    val checked      = files.zip(parallel.map(files)(file => Try(TastyUnpickler(Files.readAllBytes(file))).isSuccess))
    val tasty        = checked.collect { case (file, true) => file }
    val unreadable   = checked.collect { case (file, false) => file }
    val replacements = Twins.replacing(manifest, copies)
    val skipped      =
      unreadable.map(file => s"class file ${file.getFileName.toString.stripSuffix(".tasty")} could not be read")
    val roots           = manifest.outputs.flatMap(output => Outputs.entryPoints(output.path)).toSet
    val matchers        = Texts.matchers(generated)
    val reflectiveGlobs = reflective.map(Globs.qualifiedNames)
    val java            = JavaConversion.collected(manifest, matchers, parallel, reflectiveGlobs)
    val moduleUses      = Modules.uses(manifest, java.nodes, java.reflections)
    val javaCosts       = JavaCosts(java.costs, Complexity.Declared(bounded))
    if (uncompiled.nonEmpty) Left(uncompiled)
    else if (tasty.isEmpty && (skipped ++ java.problems).nonEmpty) Left(skipped ++ java.problems)
    else if (tasty.isEmpty)
      // A build of Java code alone has no TASTy to read, so the model the javac plugin collected is all of it.
      Right(Result(
        Graph(java.nodes, java.edges, roots ++ java.roots),
        Algebra.Model(java.products, java.sums, java.guarded, java.preconditions, Vector.empty),
        Smells.Model(java.methods, java.sites, java.usages, Vector.empty, Vector.empty, Vector.empty, moduleUses),
        Designs.Model.empty,
        Structures.Model.empty,
        Equality.Model.empty,
        Members.Model.empty,
        java.clones,
        Vector.empty,
        javaCosts.summaries(JavaCosts.none),
        Vector.empty,
        Vector.empty,
        Vector.empty,
        None,
        Purity.Model.empty,
        java.lints,
        Vector.empty
      ))
    else {
      // The class files are read beside the compiler, and joined where the result needs them.
      val bytecode                                 = Bytecode.reading(primary.map(_.path), parallel)
      val lines                                    = Lines()
      def collector(costs: Vector[JavaModel.Cost]) =
        Collector(
          manifest.outputs,
          manifest.libraries,
          matchers,
          concurrent,
          reflectiveGlobs,
          bounded,
          Globs.heat(hot),
          costs,
          References(_, _),
          lines
        )
      def reading(phase: Collector, files: Vector[Path], classpath: Vector[Path]) = Reader(phase).process(
        Array(
          "-from-tasty",
          "-Yretain-trees",
          "-Xmax-fuel",
          MaxFuel.toString,
          "-color:never",
          // Findings name types in one line each, which the printer would otherwise wrap.
          "-pagewidth",
          Int.MaxValue.toString,
          "-classpath",
          classpath.mkString(File.pathSeparator)
        ) ++ files.map(_.toString),
        StoreReporter()
      ).allErrors.map(_.message).distinct
      // The compiler resolves a class once per run, so the run reads the classpaths of the modules in turn, the JVM
      // modules' first, and a class several platforms' copies of a library hold resolves to the first copy.
      val modules = (primary ++ twins).map(_.module).distinct ++ manifest.classpath.keys.toVector.sorted
      val merged  = Classpath.evicted(manifest.classpathOf(modules))
      val phase   = collector(java.costs)
      val errors  = reading(phase, tasty, merged)
      // The replaced copies resolve against their own twin first, and the rest of the build behind it.
      val behind   = primary.map(_.path) ++ merged
      val outputs  = manifest.outputs.groupMap(_.module)(_.path)
      val absorbed = replacements.flatMap { (module, files) =>
        val twin  = collector(Vector.empty)
        val own   = outputs.getOrElse(module, Vector.empty)
        val found = reading(twin, files, own ++ manifest.classpathOf(Seq(module)) ++ behind)
        phase.absorb(twin)
        found
      }
      val problems = skipped ++ java.problems ++ phase.problems ++ errors ++ absorbed
      val graph    = JavaConversion.merge(phase.graph(roots), java)
      // A platform twin replaces some sources with its own of the same name, so the types in them cannot move.
      val twinned = manifest.sources.filter(source => manifest.platforms(source.module)).groupMap(_.module)(_.path)
        .values.map(paths => (paths.toSet, paths.map(_.getFileName).toSet)).toVector
      val replaced = manifest.sources.collect {
        case source
            if !manifest.platforms(source.module) &&
              twinned.exists((paths, names) => !paths(source.path) && names(source.path.getFileName)) =>
          source.path
      }
      val structures = phase.structures.copy(extents =
        phase.structures.extents.filter((key, _) =>
          graph.nodes.get(key).forall(node => !replaced.exists(_.endsWith(node.source)))
        )
      )
      // The model of the Java code joins the model of the Scala code, so that the rules read both alike.
      val algebra = phase.algebra.copy(
        products = phase.algebra.products ++ java.products,
        sums = phase.algebra.sums ++ java.sums,
        sites = phase.algebra.sites ++ java.guarded,
        preconditions = phase.algebra.preconditions ++ java.preconditions
      )
      val smells = phase.smells.copy(
        methods = phase.smells.methods ++ java.methods,
        sites = phase.smells.sites ++ java.sites.map { case (name, sites) =>
          name -> (phase.smells.sites.getOrElse(name, Vector.empty) ++ sites)
        },
        usages = phase.smells.usages ++ java.usages,
        moduleUses = moduleUses
      )
      val isHot = Globs.heat(hot)
      if (graph.nodes.isEmpty && problems.nonEmpty) Left(problems)
      else Right(Result(
        graph,
        algebra,
        smells,
        phase.designs,
        structures,
        phase.equality,
        phase.memberships,
        phase.duplicates ++ java.clones,
        bytecode(),
        phase.complexities,
        phase.unusedBounds,
        phase.pipelines,
        // Megamorphic calls cost only where the JVM compiles hot code, which a platform twin never runs on.
        phase.megamorphic.filter(site =>
          graph.nodes.get(site.node).exists(node => !manifest.platforms(node.module) && isHot(node.name))
        ),
        Option.when(hot.nonEmpty)(graph.nodes.keySet.filter(isHot)),
        phase.purity,
        java.lints,
        problems,
        phase.nesting(graph.roots)
      ))
    }
  }

  private class Reader(phase: Collector) extends Driver {
    override protected def newCompiler(using Context): Compiler = Reading(phase)
  }

  private class Reading(phase: Collector) extends TASTYCompiler {
    override protected def frontendPhases: List[List[Phase]]  = List(List(Unpickling(phase)))
    override protected def picklerPhases: List[List[Phase]]   = Nil
    override protected def transformPhases: List[List[Phase]] = Nil
    override protected def backendPhases: List[List[Phase]]   = Nil
  }

  // Collection runs in the reading phase: the compiler skips later phases once any class fails to read, and one
  // class the compiler cannot read back must not hide the rest of the build.
  private class Unpickling(collector: Collector) extends ReadTasty {
    override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
      val read = units.flatMap { unit =>
        try readTASTY(unit)(using ctx.fresh.setCompilationUnit(unit)).toList
        catch {
          case NonFatal(error) =>
            collector.problem(s"$unit could not be read: $error")
            Nil
        }
      }
      collector.runOn(read)
    }
  }
}
