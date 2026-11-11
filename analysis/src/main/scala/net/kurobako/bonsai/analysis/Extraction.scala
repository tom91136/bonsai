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
import scala.annotation.tailrec
import scala.concurrent.Promise
import scala.util.Try
import scala.util.Using
import scala.util.control.NonFatal

/** Builds the reference graph and algebraic model of a whole build from the TASTy in its outputs. */
object Extraction {

  /** The extracted model, with classes that could not be read or analysed listed as problems, and what the classpath
    * lacks as notes; `hot` holds the nodes of the classes the build marks hot, when it marks any.
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
      megamorphic: Vector[Megamorphic.Found],
      shared: Vector[Megamorphic.Shared],
      hot: Option[Set[String]],
      purity: Purity.Model,
      lints: Vector[JavaLints.Found],
      problems: Vector[String],
      nesting: Nesting.Model = Nesting.Model.empty,
      reach: Reach = Reach.empty,
      notes: Vector[String] = Vector.empty,
      dictionaries: Vector[Dictionaries.Parameter] = Vector.empty,
      status: Vector[String] = Vector.empty
  )

  private[analysis] case class Settings(
      generated: Vector[String],
      concurrent: Set[String],
      reflective: Vector[String],
      hot: Vector[String],
      bounded: Vector[Complexity.Bounded],
      parallel: Parallel
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
  ): Either[Vector[String], Result] =
    readDivided(manifest, Settings(generated, concurrent, reflective, hot, bounded, parallel), Clashes.runs)

  private[analysis] def readDivided(
      manifest: Manifest,
      settings: Settings,
      division: Clashes.Division
  ): Either[Vector[String], Result] =
    Reach.source(manifest).fold(
      error => Left(Vector(error)),
      source =>
        Using.resource(source) { source =>
          val world  = ClassWorld(source.bytes(_))
          val result = Promise[Either[Vector[String], Result]]()
          val thread = Thread(
            Thread.currentThread.getThreadGroup,
            () =>
              result.complete(
                Try(settled(manifest, settings, world, division))
              ): Unit,
            "bonsai-extraction",
            StackSize
          )
          thread.start()
          thread.join()
          result.future.value.fold[Either[Vector[String], Result]](Left(Vector("extraction terminated abnormally")))(
            _.fold(
              error => {
                error.printStackTrace()
                Left(Vector(s"extraction failed: $error"))
              },
              identity
            )
          )
        }
    )

  @tailrec private def settled(
      manifest: Manifest,
      settings: Settings,
      world: ClassWorld,
      division: Clashes.Division,
      known: Set[String] = Set.empty
  ): Either[Vector[String], Result] = {
    def clashing(read: Either[Vector[String], Result]) =
      Clashes.reported(read.fold(identity, result => result.problems ++ result.notes))
    val read     = Quiet.attempt(extract(manifest, settings, world, division, known))(clashing(_).diff(known).nonEmpty)
    val reported = clashing(read)
    if (reported.isEmpty) read
    else if (reported.subsetOf(known)) Left(Clashes.unresolved(reported))
    else settled(manifest, settings, world, division, known ++ reported)
  }

  private def extract(
      manifest: Manifest,
      settings: Settings,
      world: ClassWorld,
      divide: Clashes.Division,
      known: Set[String]
  ): Either[Vector[String], Result] = {
    import settings.*
    // The compiler aborts the whole run on a file whose names or sections it cannot index, so those are set aside.
    // A platform twin compiles the shared sources again, so only the classes the JVM modules lack are its own. The
    // compiler finds every class through the directories of the files it reads, in their order, so the JVM modules'
    // come first and the shared classes resolve to their copies.
    val (twins, primary) = manifest.outputs.partition(output => manifest.platforms(output.module))
    val listed           = (primary ++ twins).map(output => output -> Outputs.files(output.path))
    // Test support compiles its main sources in the test scope, so one scope can hold an empty output beside a full one.
    val sourced  = manifest.sources.map(source => (source.module, source.scope)).toSet
    val compiled = listed.collect {
      case (output, files) if files.exists(compiledFile(_)) =>
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
    val classes        = listed.flatMap((output, files) => files.filter(_.toString.endsWith(".tasty")).map(output -> _))
    val (copies, kept) =
      classes.partition((output, file) => manifest.platforms(output.module) && shared(output.path.relativize(file)))
    val (own, jvm) = kept.partition((output, _) => manifest.platforms(output.module))
    val files      = kept.map(_._2)
    val checked    = files.zip(parallel.map(files)(file => Try(TastyUnpickler(Files.readAllBytes(file))).isSuccess))
    val readable   = checked.collect { case (file, true) => file }.toSet
    val tasty      = jvm.collect { case (_, file) if readable(file) => file }
    val division   = divide(manifest, listed, known)
    val apart      = division.getOrElse(Vector.empty).drop(1)
    val separate   = apart.flatten.toSet
    val entries    = classesBy(listed, apart.nonEmpty)
    val together   = jvm.collect { case (output, file) if readable(file) && !separate(output) => file }
    val unreadable = checked.collect { case (file, false) => file }
    val skipped    =
      unreadable.map(file => s"class file ${file.getFileName.toString.stripSuffix(".tasty")} could not be read")
    val resourced = manifest.resources.map(_.module).toSet
    val roots     =
      (manifest.outputs.filterNot(output => resourced(output.module))
        .flatMap(output => Outputs.entryPoints(output.path)) ++ Outputs.entryPoints(manifest.resources)).toSet
    val matchers        = Texts.matchers(generated) :+ manifest.generatedMatcher
    val reflectiveGlobs = reflective.map(Globs.qualifiedNames)
    val java            = JavaConversion.collected(manifest, matchers, parallel, reflectiveGlobs)
    val moduleUses      = Modules.uses(manifest, java.nodes, java.reflections)
    val javaCosts       = JavaCosts(java.costs, Complexity.Declared(bounded))
    val clashes         = division.left.getOrElse(Vector.empty)
    val status          = Option.when(apart.nonEmpty) {
      val runs = s"read in ${apart.size + 1} compiler runs"
      Clashes.because(manifest, listed, known) match {
        case Vector() => runs
        case causes   => s"$runs: ${causes.mkString("; ")}"
      }
    }.toVector
    if (uncompiled.nonEmpty) Left(uncompiled)
    else if (clashes.nonEmpty) Left(clashes)
    else if (tasty.isEmpty && (skipped ++ java.problems).nonEmpty) Left(skipped ++ java.problems)
    else if (tasty.isEmpty) {
      // A build of Java code alone has no TASTy to read, so the model the javac plugin collected is all of it.
      val graph                        = Graph(java.nodes, java.edges, roots ++ java.roots, java.bodies)
      val linked                       = Reach.link(manifest, graph, world)
      val Reach.Reached(reach, unread) = Reach.of(linked, graph)
      Right(Result(
        graph,
        Algebra.Model(java.products, java.sums, java.guarded, java.preconditions, Vector.empty, Map.empty),
        Smells.Model(java.methods, java.sites, java.usages, Vector.empty, Vector.empty, Vector.empty, moduleUses),
        Designs.Model.empty,
        Structures.Model.empty,
        Equality.Model.empty,
        Members.Model.empty,
        java.clones,
        Vector.empty,
        javaCosts.summaries(linked.summaries.callee(_)),
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        None,
        BytecodePurity.model(linked.summaries, java.nodes),
        java.lints,
        unread,
        reach = reach
      ))
    } else {
      // The class files are read beside the compiler, and joined where the result needs them.
      val bytecode = Bytecode.reading(primary.map(_.path), parallel)
      val lines    = Lines()
      def collector(
          costs: Vector[JavaModel.Cost],
          entry: Set[Root],
          link: Option[Graph => Reach.Calls],
          foreign: Set[Output] = Set.empty,
          deferred: Boolean = false
      ) =
        Collector(
          manifest.outputs,
          entries,
          manifest.libraries,
          matchers,
          concurrent,
          reflectiveGlobs,
          bounded,
          Globs.heat(hot),
          costs,
          References(_, _),
          lines,
          entry,
          link,
          foreign,
          deferred
        )
      val tastyPaths                                                              = relativePaths(manifest)
      def reading(phase: Collector, files: Vector[Path], classpath: Vector[Path]) = Reader(phase, tastyPaths).process(
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
      val outputModules = manifest.outputs.map(_.module).toSet
      val ordered       = (primary ++ twins).map(_.module).distinct ++ manifest.classpath.keys.toVector.sorted
      def behindOf(run: Set[Output], unbuilt: Set[String]) = {
        val members = run.map(_.module) ++ unbuilt
        primary.filter(run).map(_.path) ++ Classpath.evicted(manifest.classpathOf(ordered.filter(members)))
      }
      val linking    = Some((graph: Graph) => Reach.link(manifest, JavaConversion.merge(graph, java), world))
      val everything = manifest.outputs.toSet
      val phase      = collector(java.costs, roots, linking, separate, apart.nonEmpty)
      val jvmModules = primary.map(_.module).toSet
      // A twin's classes resolve against its own and its peers' first, and the rest of its run behind them. The
      // copies are read for what they reference only, and the classes of its own sources are analysed as the JVM
      // modules' are.
      def twinning(run: Set[Output], target: Collector, behind: Vector[Path]): Vector[Twinned] = {
        val outputs = manifest.outputs.filter(run).groupMap(_.module)(_.path)
        Twins.reading(
          classes.filter((output, _) => !manifest.platforms(output.module) && run(output)),
          (copies ++ own.filter((_, file) => readable(file))).filter((output, _) => run(output)),
          shared
        ).map { twin =>
          val ahead = (twin.module +: Twins.peers(manifest, jvmModules, twin.module)).flatMap(outputs.getOrElse(
            _,
            Vector.empty
          ))
          val classpath = ahead ++ manifest.classpathOf(Seq(twin.module)) ++ behind
          val copies    = collector(Vector.empty, Set.empty, None)
          val own       = collector(Vector.empty, Set.empty, linking)
          val found     = reading(copies, twin.replaced, classpath) ++ reading(own, twin.own, classpath)
          target.absorb(copies)
          target.absorb(own)
          Twinned(twin, own, copies, copies.problems ++ own.problems, found.toVector ++ copies.notes ++ own.notes)
        }
      }
      val leading    = manifest.outputs.filterNot(separate).toSet
      val behind     = behindOf(leading, manifest.classpath.keySet.diff(outputModules))
      val phaseTwins = twinning(leading, phase, behind)
      val groupRuns  = apart.map { group =>
        val own    = collector(Vector.empty, Set.empty, linking, everything -- group, deferred = true)
        val inside = behindOf(group, Set.empty)
        val twins  = twinning(group, own, inside)
        val files  = jvm.collect { case (output, file) if group(output) && readable(file) => file }
        val found  = reading(own, files, inside)
        (own, twins, found)
      }
      val groups     = groupRuns.map(_._1)
      val groupTwins = groupRuns.flatMap(_._2)
      val twinRuns   = phaseTwins ++ groupTwins
      // A twin declares some of the types of the JVM modules again, and what the sources of its run record of them is
      // theirs.
      def standing(twins: Vector[Twinned]): String => Boolean = {
        val declared = twins.flatMap(_.reading.declared).toSet
        name => declared(name) || name.indices.exists(index => name(index) == '.' && declared(name.take(index)))
      }
      val owned = phaseTwins.map(twin => Owned(twin.own, Some(standing(phaseTwins)))) ++
        groups.map(Owned(_, None)) ++
        groupRuns.flatMap((_, twins, _) => twins.map(twin => Owned(twin.own, Some(standing(twins)))))
      val readers                = phase +: (twinRuns.flatMap(twin => Vector(twin.own, twin.copies)) ++ groups)
      val errors                 = reading(phase, together, behind)
      def beside(run: Collector) = (phase +: groups).filterNot(_ eq run).foldLeft(Collector.Evidence.empty)(_ ++
        _.evidence)
      groups.foreach(run => run.conclude(beside(run)))
      groups.foreach(phase.absorb(_))
      phase.conclude(beside(phase))
      val problems = skipped ++ java.problems ++ phase.problems ++ twinRuns.flatMap(_.problems) ++
        groups.flatMap(_.problems)
      val notes = phase.notes ++ errors ++ twinRuns.flatMap(_.notes) ++
        groupRuns.flatMap((own, _, found) => found ++ own.notes)
      val graph = JavaConversion.merge(phase.graph, java)
      // A platform twin replaces some sources with its own of the same name, so the types in them cannot move.
      val twinned = manifest.sources.filter(source => manifest.platforms(source.module)).groupMap(_.module)(_.path)
        .values.map(paths => (paths.toSet, paths.map(_.getFileName).toSet)).toVector
      val replaced = manifest.sources.collect {
        case source
            if !manifest.platforms(source.module) &&
              twinned.exists((paths, names) => !paths(source.path) && names(source.path.getFileName)) =>
          source.path
      }
      // The classes of each twin's own sources are analysed apart from the JVM modules', and their models join.
      def joined[A](
          base: A,
          part: Collector => A,
          join: (A, A) => A,
          excluding: (A, String => Boolean) => A = (model: A, _: String => Boolean) => model
      ): A =
        owned.foldLeft(base)((all, run) =>
          join(all, run.stands.fold(part(run.collector))(stands => excluding(part(run.collector), stands)))
        )
      val whole      = joined(phase.structures, _.structures, _.joined(_))
      val structures = whole.copy(extents =
        whole.extents.filter((key, _) => graph.nodes.get(key).forall(node => !replaced.exists(_.endsWith(node.source))))
      )
      // The model of the Java code joins the model of the Scala code, so that the rules read both alike.
      val scala   = joined(phase.algebra, _.algebra, _.joined(_))
      val algebra = scala.copy(
        products = scala.products ++ java.products,
        sums = scala.sums ++ java.sums,
        sites = scala.sites ++ java.guarded,
        preconditions = scala.preconditions ++ java.preconditions
      )
      // A source that a twin holds in a form of its own keeps its declarations where they stand, as the twin's cannot move along.
      val jvmPaths = manifest.sources.collect { case source if !manifest.platforms(source.module) => source.path }.toSet
      val seams    = manifest.sources.collect {
        case source if manifest.platforms(source.module) && !jvmPaths(source.path) => sourceSet(source.path.toString)
      }.toSet & jvmPaths.map(path => sourceSet(path.toString))
      def replacedAt(node: Node): Boolean = node.source.nonEmpty && seams(sourceSet(node.source))
      val analysed                        = joined(phase.smells, _.smells, _.joined(_), _.excluding(_))
      val smells                          = analysed.copy(
        methods = analysed.methods ++ java.methods,
        instantiations = if (groups.isEmpty) analysed.instantiations else Smells.merged(analysed.instantiations),
        sites = analysed.sites ++ java.sites.map { case (name, sites) =>
          name -> (analysed.sites.getOrElse(name, Vector.empty) ++ sites)
        },
        usages = analysed.usages ++ java.usages,
        moduleUses = moduleUses
      )
      // A run of part of the build misses the uses and assignments in the rest of it, which the others record by name.
      def observed(part: Collector) = {
        val others = readers.filterNot(_ eq part)
        part.memberships.observed(
          others.foldLeft(Map.empty[String, Set[Use]])((all, other) => Maps.join(all, other.usages)(_ ++ _)),
          others.flatMap(_.assignments).toSet,
          phase.referrers
        )
      }
      val members = owned.foldLeft(observed(phase))((all, run) =>
        all.joined(run.stands.fold(observed(run.collector))(stands => observed(run.collector).excluding(stands)))
      )
        .pinned(name => graph.nodes.get(name).exists(replacedAt(_)))
      val isHot     = Globs.heat(hot)
      val unmatched = {
        val beside = (groups ++ twinRuns.map(_.own)).map(_.unusedBounds.toSet)
        phase.unusedBounds.filter(bound => beside.forall(_(bound)))
      }
      if (graph.nodes.isEmpty && problems.nonEmpty) Left(problems ++ notes)
      else {
        val linked                       = phase.calls.getOrElse(Reach.link(manifest, graph, world))
        val Reach.Reached(reach, unread) = Reach.of(linked, graph)
        val hotNames                     = graph.nodes.values.view.map(_.name).filter(isHot).toSet
        // Megamorphic calls cost only where the JVM compiles hot code, which a platform twin never runs on.
        val resolved = Megamorphic.resolve(
          (phase +: groups).flatMap(_.megamorphic).filter(site =>
            !(apart.nonEmpty && site.generic) &&
              graph.nodes.get(site.node).exists(node => !manifest.platforms(node.module) && hotNames(node.name))
          ),
          graph.nodes,
          linked.graph,
          world
        )
        Right(Result(
          graph,
          algebra,
          smells,
          joined(phase.designs, _.designs, _.joined(_)),
          structures,
          joined(phase.equality, _.equality, _.joined(_)),
          members.copy(calls = java.callers),
          owned.foldLeft(phase.duplicates)(_ ++ _.collector.duplicates) ++
            Clones.groups((phase +: groups).flatMap(_.candidates)) ++
            java.clones,
          bytecode(),
          owned.foldLeft(phase.complexities)(_ ++ _.collector.complexities),
          unmatched,
          owned.foldLeft(phase.pipelines)(_ ++ _.collector.pipelines),
          resolved.filterNot(_.site.settled),
          Megamorphic.shared(resolved, graph.nodes, linked.graph, world),
          Option.when(hot.nonEmpty)(graph.nodes.keySet.filter(isHot)),
          joined(phase.purity, _.purity, _.joined(_)),
          java.lints,
          problems ++ unread,
          joined(phase.nesting(graph.rooted), _.nesting(graph.rooted), _.joined(_)).pinned(replacedAt(_)),
          reach,
          notes,
          owned.foldLeft(phase.dictionaries)(_ ++ _.collector.dictionaries),
          status
        ))
      }
    }
  }

  private def compiledFile(file: Path): Boolean = Set(".class", ".tasty").exists(file.toString.endsWith(_))

  private def classesBy(listed: Vector[(Output, Vector[Path])], divided: Boolean): Map[String, Output] =
    if (!divided) Map.empty
    else
      listed.reverse.flatMap((output, files) =>
        files.filter(compiledFile(_)).map(file =>
          output.path.relativize(file).toString.replace(File.separatorChar, '/') -> output
        )
      ).toMap

  // The path of a source below its source root, or its file name where it stands in none, which a platform's sources
  // share with the JVM's when they stand for them.
  private def sourceSet(path: String): String = {
    val parts = path.split('/')
    val root  = parts.indexWhere(part => part == "scala" || part.startsWith("scala-"))
    parts.drop(if (root < 0) parts.length - 1 else root + 1).mkString("/")
  }

  // TASTy names sources relative to the build root, which the reader resolves against the working directory.
  private def relativePaths(manifest: Manifest): Vector[(String, Path)] = manifest.root.toVector.flatMap { root =>
    val base = root.toAbsolutePath.normalize
    manifest.sources.map(_.path.toAbsolutePath.normalize).filter(_.startsWith(base)).map(file =>
      base.relativize(file).toString.replace(File.separatorChar, '/') -> file
    )
  }

  private case class Owned(collector: Collector, stands: Option[String => Boolean])

  private case class Twinned(
      reading: Twins.Reading,
      own: Collector,
      copies: Collector,
      problems: Vector[String],
      notes: Vector[String]
  )

  private class Reader(phase: Collector, tastyPaths: Vector[(String, Path)]) extends Driver {
    override protected def newCompiler(using Context): Compiler = Reading(phase, tastyPaths)
  }

  private class Reading(phase: Collector, tastyPaths: Vector[(String, Path)]) extends TASTYCompiler {
    override protected def frontendPhases: List[List[Phase]]  = List(List(Unpickling(phase, tastyPaths)))
    override protected def picklerPhases: List[List[Phase]]   = Nil
    override protected def transformPhases: List[List[Phase]] = Nil
    override protected def backendPhases: List[List[Phase]]   = Nil
  }

  // Collection runs in the reading phase: the compiler skips later phases once any class fails to read, and one
  // class the compiler cannot read back must not hide the rest of the build.
  private class Unpickling(collector: Collector, tastyPaths: Vector[(String, Path)]) extends ReadTasty {
    override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
      tastyPaths.foreach((name, path) => ctx.getFile(path.toString).foreach(file => ctx.base.files(name) = file))
      val read = units.flatMap { unit =>
        try readTASTY(unit)(using ctx.fresh.setCompilationUnit(unit)).toList
        catch {
          case NonFatal(error) =>
            collector.failed(s"$unit could not be read", error)
            Nil
        }
      }
      collector.runOn(read)
    }
  }
}
