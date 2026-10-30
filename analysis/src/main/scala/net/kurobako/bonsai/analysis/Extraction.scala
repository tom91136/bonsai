package net.kurobako.bonsai.analysis

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.Compiler
import dotty.tools.dotc.Driver
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameKinds.DefaultGetterName
import dotty.tools.dotc.core.Names.DerivedName
import dotty.tools.dotc.core.Names.TermName
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Names.typeName
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.AppliedType
import dotty.tools.dotc.core.Types.ConstantType
import dotty.tools.dotc.core.Types.NamedType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeParamRef
import dotty.tools.dotc.core.Types.TypeRef
import dotty.tools.dotc.core.Types.TypeTraverser
import dotty.tools.dotc.core.tasty.AttributeUnpickler
import dotty.tools.dotc.core.tasty.Attributes
import dotty.tools.dotc.core.tasty.TastyUnpickler
import dotty.tools.dotc.fromtasty.ReadTasty
import dotty.tools.dotc.fromtasty.TASTYCompiler
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.io.AbstractFile
import dotty.tools.tasty.TastyReader
import java.io.File
import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.PathMatcher
import net.kurobako.bonsai.java.{Model => JavaModel}
import scala.collection.mutable
import scala.concurrent.Promise
import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.util.Using
import scala.util.control.NonFatal
import scala.util.matching.Regex

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
      members: Members.Model,
      clones: Vector[Clones.Group],
      bytecode: Vector[Bytecode.Method],
      complexities: Vector[Complexity.Summary],
      unusedBounds: Vector[Complexity.Bounded],
      pipelines: Vector[Pipelines.Chain],
      megamorphic: Vector[Megamorphic.Site],
      hot: Option[Set[String]],
      purity: Purity.Model,
      lints: Vector[JavaLints.Found],
      problems: Vector[String],
      nesting: Nesting.Model = Nesting.Model.empty
  )

  // The TASTy unpickler recurses over long import selector lists and deep trees.
  private val StackSize = 1L << 30

  // A hierarchy of one case is a single implementation, which another rule reports.
  private val MinimumCases = 2

  // A string naming a member, alone or at the head of a call: `Owner.member`, `pkg.Owner.member` or `Owner.member(...`.
  private val Reference = """((?:[a-z][A-Za-z0-9_]*\.)*)([A-Z][A-Za-z0-9_]*)\.([A-Za-z_][A-Za-z0-9_]*)(\(.*)?""".r

  // A string naming a type by its package and name, `pkg.Type`.
  private val QualifiedType = """((?:[a-z][A-Za-z0-9_]*\.)+)([A-Z][A-Za-z0-9_]*)""".r

  private val Width = "[0-9]+".r

  // File names read like references to members.
  private val Extensions =
    Set("scala", "sc", "sbt", "java", "class", "tasty", "jar", "md", "txt", "json", "conf", "xml")

  /** Extracts the build `manifest` describes; `concurrent` names methods that run the functions passed to them on other
    * threads, beside the standard ones, `reflective` globs the types the build names only in strings, and `hot` the
    * classes whose code runs hot, with `!` before a glob leaving out what it matches, and `bounded` the members and
    * parameters whose collections hold a declared number of elements.
    */
  def read(
      manifest: Manifest,
      generated: Vector[String] = Vector.empty,
      concurrent: Set[String] = Set.empty,
      reflective: Vector[String] = Vector.empty,
      hot: Vector[String] = Vector.empty,
      bounded: Vector[Complexity.Bounded] = Vector.empty
  ): Either[Vector[String], Result] = {
    val result = Promise[Either[Vector[String], Result]]()
    val thread = Thread(
      Thread.currentThread.getThreadGroup,
      () => result.complete(Try(extract(manifest, generated, concurrent, reflective, hot, bounded))): Unit,
      "bonsai-extraction",
      StackSize
    )
    thread.start()
    thread.join()
    result.future.value.fold(Left(Vector("extraction terminated abnormally")))(
      _.fold(error => Left(Vector(s"extraction failed: $error")), identity)
    )
  }

  private def extract(
      manifest: Manifest,
      generated: Vector[String],
      concurrent: Set[String],
      reflective: Vector[String],
      hot: Vector[String],
      bounded: Vector[Complexity.Bounded]
  ): Either[Vector[String], Result] = {
    // The compiler aborts the whole run on a file whose names or sections it cannot index, so those are set aside.
    // A platform twin compiles the shared sources again, so only the classes the JVM modules lack are its own. The
    // compiler finds every class through the directories of the files it reads, in their order, so the JVM modules'
    // come first and the shared classes resolve to their copies.
    val (twins, primary) = manifest.outputs.partition(output => manifest.platforms(output.module))
    val shared           = primary.flatMap(output => files(output.path).map(output.path.relativize(_))).toSet
    val (copies, kept)   = (primary ++ twins).flatMap(output =>
      files(output.path).collect { case file if file.toString.endsWith(".tasty") => output -> file }
    ).partition((output, file) => manifest.platforms(output.module) && shared(output.path.relativize(file)))
    val (tasty, unreadable) = kept.map(_._2).partition(file => Try(TastyUnpickler(Files.readAllBytes(file))).isSuccess)
    val replacements        = replacing(manifest, copies)
    val skipped             =
      unreadable.map(file => s"class file ${file.getFileName.toString.stripSuffix(".tasty")} could not be read")
    val roots                = manifest.outputs.flatMap(output => entryPoints(output.path)).toSet
    val matchers             = generated.map(FileSystems.getDefault.getPathMatcher(_))
    val (java, javaProblems) = collected(manifest, matchers)
    val moduleUses           = Modules.uses(manifest, java.nodes, java.reflections)
    if (tasty.isEmpty && (skipped ++ javaProblems).nonEmpty) Left(skipped ++ javaProblems)
    else if (tasty.isEmpty)
      // A build of Java code alone has no TASTy to read, so the model the javac plugin collected is all of it.
      Right(Result(
        Graph(java.nodes, java.edges, roots ++ java.roots),
        Algebra.Model(java.products, java.sums, Map.empty, Vector.empty, Vector.empty),
        Smells.Model(java.methods, java.sites, java.usages, Vector.empty, Vector.empty, Vector.empty, moduleUses),
        Designs.Model.empty,
        Structures.Model.empty,
        Members.Model.empty,
        java.clones,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        None,
        Purity.Model.empty,
        java.lints,
        Vector.empty
      ))
    else {
      def collector() =
        Collector(
          manifest.outputs,
          manifest.libraries,
          matchers,
          concurrent,
          reflective.map(qualifiedNames(_)),
          bounded
        )
      def reading(phase: Collector, files: Vector[Path], classpath: Vector[Path]) = Reader(phase).process(
        Array(
          "-from-tasty",
          "-Yretain-trees",
          "-color:never",
          // Findings name types in one line each, which the printer would otherwise wrap.
          "-pagewidth",
          Int.MaxValue.toString,
          "-classpath",
          evicted(classpath).mkString(File.pathSeparator)
        ) ++ files.map(_.toString),
        StoreReporter()
      ).allErrors.map(_.message).distinct
      val phase  = collector()
      val errors = reading(phase, tasty, manifest.classpath)
      // The replaced copies resolve against their own twin first, and the rest of the build behind it.
      val behind   = primary.map(_.path) ++ manifest.classpath
      val absorbed = replacements.map { (module, files) =>
        val twin  = collector()
        val own   = manifest.outputs.collect { case output if output.module == module => output.path }
        val found = reading(twin, files, own ++ behind)
        phase.absorb(twin)
        found
      }.flatten
      val problems = skipped ++ javaProblems ++ phase.problems ++ errors ++ absorbed
      val graph    = merge(phase.graph(roots), java)
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
        sums = phase.algebra.sums ++ java.sums
      )
      val smells = phase.smells.copy(
        methods = phase.smells.methods ++ java.methods,
        sites = phase.smells.sites ++ java.sites.map { case (name, sites) =>
          name -> (phase.smells.sites.getOrElse(name, Vector.empty) ++ sites)
        },
        usages = phase.smells.usages ++ java.usages,
        moduleUses = moduleUses
      )
      if (graph.nodes.isEmpty && problems.nonEmpty) Left(problems)
      else Right(Result(
        graph,
        algebra,
        smells,
        phase.designs,
        structures,
        phase.memberships,
        phase.duplicates ++ java.clones,
        Bytecode.read(primary.map(_.path)),
        phase.complexities,
        phase.unusedBounds,
        phase.pipelines,
        // Megamorphic calls cost only where the JVM compiles hot code, which a platform twin never runs on.
        phase.megamorphic.filter(site =>
          graph.nodes.get(site.node).exists(node => !manifest.platforms(node.module) && heated(hot, node.name))
        ),
        Option.when(hot.nonEmpty)(graph.nodes.keySet.filter(heated(hot, _))),
        phase.purity,
        java.lints,
        problems,
        phase.nesting(graph.roots)
      ))
    }
  }

  private given Ordering[Vector[BigInt]] = Ordering.Implicits.seqOrdering[Vector, BigInt]

  /** The graph the javac plugin collected for the Java code of a build, with the methods, the fields and the calls it
    * found.
    */
  private case class Collected(
      nodes: Map[String, Node],
      edges: Map[String, Set[String]],
      roots: Set[String],
      methods: Vector[Smells.Method],
      sites: Map[String, Vector[Algebra.Site]],
      usages: Vector[Smells.Usage],
      products: Vector[Algebra.Product],
      sums: Vector[Algebra.Sum],
      clones: Vector[Clones.Group],
      lints: Vector[JavaLints.Found],
      reflections: Vector[JavaModel.Reflection]
  )

  /** The graph of the Java code the javac plugin collected, which holds the nodes it found and the references between
    * them, and the methods, the fields and the calls it found.
    */
  private def collected(manifest: Manifest, generated: Vector[PathMatcher]): (Collected, Vector[String]) = {
    val nodes       = mutable.LinkedHashMap.empty[String, Node]
    val edges       = mutable.LinkedHashMap.empty[String, mutable.LinkedHashSet[String]]
    val roots       = mutable.LinkedHashSet.empty[String]
    val methods     = mutable.ArrayBuffer.empty[Smells.Method]
    val results     = mutable.LinkedHashMap.empty[String, String]
    val sites       = mutable.LinkedHashMap.empty[String, mutable.ArrayBuffer[Algebra.Site]]
    val calls       = mutable.LinkedHashMap.empty[String, (Int, Int, Int, Int)]
    val products    = mutable.ArrayBuffer.empty[Algebra.Product]
    val sums        = mutable.ArrayBuffer.empty[Algebra.Sum]
    val fragments   = mutable.ArrayBuffer.empty[(JavaModel.Fragment, Boolean)]
    val lints       = mutable.ArrayBuffer.empty[JavaLints.Found]
    val reflections = mutable.ArrayBuffer.empty[JavaModel.Reflection]
    val problems    = Vector.newBuilder[String]
    manifest.models.foreach { model =>
      JavaModel.read(model.path) match {
        case Left(problem)    => problems += problem
        case Right(collected) =>
          // The code of the test sources is marked as such, which lowers the findings that concern it.
          val test = model.scope == Output.Scope.Test
          collected.nodes.foreach { node =>
            // The public API of a library is used by the builds that depend on it, as the public API of Scala code is.
            val exported = node.exported && manifest.libraries(model.module) && model.scope == Output.Scope.Main
            nodes.getOrElseUpdate(
              node.name,
              Node(
                node.name,
                model.module,
                model.scope,
                node.source,
                node.line,
                node.generated || generated.exists(_.matches(Path.of(node.source))),
                exported,
                node.outer,
                node.suppressed
              )
            ): Unit
            // The build enters the code from outside it at a type that holds a native or a main method, and the
            // public API of a library is used by the builds that depend on it.
            if (exported || node.entry) roots += node.name
          }
          collected.methods.foreach { method =>
            val owner = method.owner
            methods += Smells.Method(
              method.name,
              owner,
              owner,
              method.parameters.map(parameter => field(parameter)),
              // A method that overrides a method of a supertype is one it inherits rather than declares.
              overriding = method.overriding,
              // The public API of a library is used by the builds that depend on it, so the callers in this build do
              // not settle it.
              method.exported && manifest.libraries(model.module) && model.scope == Output.Scope.Main &&
                nodes.get(owner).forall(_.exported),
              test = test,
              method.stored,
              Set.empty,
              inline = false,
              suppressed = method.suppressed
            )
            // The type the method returns, which the rules that need every call site read by.
            returns(method).foreach(returned => results(method.name) = returned)
          }
          collected.fields.groupBy(_.owner).foreach { (owner, fields) =>
            products += Algebra.Product(
              owner,
              owner,
              fields.map(found => field(found.parameter)),
              Set.empty,
              Algebra.Predicate.Unknown,
              Algebra.Access.Open,
              exported = false
            )
          }
          // A record is a product, which the analysis reads alike with a case class.
          collected.products.foreach { product =>
            products += Algebra.Product(
              product.name,
              product.node,
              product.fields.map(field),
              product.parents,
              Algebra.Predicate.Unknown,
              Algebra.Access.Open,
              product.exported
            )
          }
          // A sealed type is a sum, which the analysis reads alike with a sealed trait. The fields of a variant are
          // those of the product of it, which a record that extends the sealed type is.
          collected.sums.foreach { sum =>
            sums += Algebra.Sum(
              sum.name,
              sum.node,
              sum.variants.map { variant =>
                Algebra.Variant(
                  variant.name,
                  products.find(_.name == variant.name).map(_.fields).getOrElse(Vector.empty)
                )
              }
            )
          }
          collected.sites.foreach { site =>
            val callee = site.callee
            sites.getOrElseUpdate(callee, mutable.ArrayBuffer.empty) += Algebra.Site(
              site.arguments.map(argument),
              test = test,
              // A call the callee makes of itself, whose arguments the method itself fixes.
              own = site.caller == callee,
              site.location
            )
            // The calls of a method, those of them that drop the result, those in tests and those that invert it, which
            // the rules that need every call site read alike with the calls of the Scala code.
            val (total, dropped, tests, inverted) = calls.getOrElse(callee, (0, 0, 0, 0))
            calls(callee) = (
              total + 1,
              dropped + (if (site.discarded) 1 else 0),
              tests + (if (test) 1 else 0),
              inverted + (if (site.negated) 1 else 0)
            )
          }
          collected.edges.foreach { edge =>
            edges.getOrElseUpdate(edge.from, mutable.LinkedHashSet.empty) += edge.to: Unit
          }
          collected.fragments.foreach(fragment => fragments += fragment -> test): Unit
          // The lints of the Java code are reported as findings of their own, with the module and the scope of the
          // model they were found in.
          collected.lints.foreach(lint =>
            lints += JavaLints.Found(lint, model.module, model.scope, generated.exists(_.matches(Path.of(lint.path))))
          ): Unit
          collected.reflections.foreach(reflection => reflections += reflection): Unit
      }
    }
    // The usages of the methods of the Java code, which the rules that need every call site read alike with the usages
    // of the methods of the Scala code.
    val usages = methods.toVector.flatMap { method =>
      for {
        returns                           <- results.get(method.name)
        (total, dropped, tests, inverted) <- calls.get(method.name)
      } yield Smells.Usage(method, returns, total, dropped, tests, inverted)
    }
    (
      Collected(
        nodes.toMap,
        edges.view.mapValues(_.toSet).toMap,
        roots.toSet,
        methods.toVector,
        sites.view.mapValues(_.toVector).toMap,
        usages,
        products.toVector,
        sums.toVector,
        // The copies of the code of the Java code are of the whole build, as the same block of code written in two
        // modules of it is as much a copy as one written twice in one.
        JavaFragments.groups(fragments),
        lints.toVector,
        reflections.toVector
      ),
      problems.result()
    )
  }

  /** The file a method of the Java code is in, which is the file of the type that holds it. */
  private def file(method: JavaModel.Method, nodes: collection.Map[String, Node]): String =
    nodes.get(method.owner).map(_.source).getOrElse {
      // The place the method is in names the file it is in, which the model of the type does not tell when the type is
      // not in the model.
      val location = method.location
      val index    = location.lastIndexOf(':')
      if (index < 0) location else location.take(index)
    }

  /** The type a method of the Java code returns, unless it returns nothing or returns the type that holds it, whose
    * result the code that calls it is meant to drop.
    */
  private def returns(method: JavaModel.Method): Option[String] = {
    val returned = method.returns
    if (returned.isEmpty || returned == "void") None
    else if (returned == method.owner.split('.').last) None
    else Some(returned)
  }

  /** The argument a call passes, which the analysis reads the values the callers pass by. */
  private def argument(argument: String): Algebra.Argument =
    argument match {
      case "true"                               => Algebra.Argument.True
      case "false"                              => Algebra.Argument.False
      case "absent"                             => Algebra.Argument.Absent
      case "unknown"                            => Algebra.Argument.Unknown
      case value if value.startsWith("number:") => Algebra.Argument.Number(BigDecimal(value.drop("number:".length)))
      case value if value.startsWith("text:")   => Algebra.Argument.Text(value.drop("text:".length))
      case value if value.startsWith("symbol:") => Algebra.Argument.Tag(value.drop("symbol:".length))
      case _                                    => Algebra.Argument.Present
    }

  /** The parameter of a method, or the field of a type, which the analysis compares the methods of a build by. */
  private def field(parameter: JavaModel.Parameter): Algebra.Field =
    Algebra.Field(
      parameter.name,
      shape(parameter.kind, parameter.shape),
      if (parameter.kind == "other" && parameter.shape == "java.lang.String") Algebra.Kind.Text
      else kind(parameter.kind, parameter.cases),
      parameter.primitive
    )

  /** The shape of a type, which the analysis compares the parameters of the methods by. */
  private def shape(kind: String, name: String): Polynomial = kind match {
    case "flag"     => Polynomial.constant(2)
    case "optional" => Polynomial.One + Polynomial.atom(name)
    case _          => Polynomial.atom(name)
  }

  /** The kind of a type, which the analysis reads the parameters of the methods by. */
  private def kind(kind: String, cases: Vector[String]): Algebra.Kind = kind match {
    case "flag"     => Algebra.Kind.Flag
    case "number"   => Algebra.Kind.Number
    case "optional" => Algebra.Kind.Optional
    case "tag"      => Algebra.Kind.Tag(cases)
    case _          => Algebra.Kind.Other
  }

  /** The graph of the build, which holds the nodes of the Scala code and of the Java code alike. */
  private def merge(graph: Graph, collected: Collected): Graph = {
    val edges = collected.edges.foldLeft(graph.edges) { case (edges, (from, tos)) =>
      edges.updated(from, edges.getOrElse(from, Set.empty) ++ tos)
    }
    Graph(graph.nodes ++ collected.nodes, edges, graph.roots ++ collected.roots)
  }

  /** Keeps the newest version of each repository artifact, as dependency resolution would for a single classpath. */
  def evicted(classpath: Vector[Path]): Vector[Path] = {
    val versioned = classpath.flatMap(path => artifact(path).map((directory, version) => (directory, version, path)))
    val newest    = versioned.groupBy(_._1).values.map(_.maxBy(_._2)._3).toSet
    classpath.filter(path => artifact(path).isEmpty || newest(path))
  }

  /** The artifact directory and version of a jar in a Maven-style repository layout. */
  private def artifact(path: Path): Option[(Path, (Vector[BigInt], String))] = for {
    versionDirectory  <- Option(path.getParent)
    artifactDirectory <- Option(versionDirectory.getParent)
    version = versionDirectory.getFileName.toString
    if path.getFileName.toString == s"${artifactDirectory.getFileName}-$version.jar"
  } yield artifactDirectory ->
    (version.split("[.-]").toVector.map(_.takeWhile(_.isDigit)).takeWhile(_.nonEmpty).map(BigInt(_)), version)

  private def qualifiedNames(glob: String): Regex = Intended.qualifiedNames(glob)

  private def heated(globs: Vector[String], name: String): Boolean = {
    val (excluded, included) = globs.partition(_.startsWith("!"))
    included.exists(qualifiedNames(_).matches(s"$name.")) &&
    !excluded.exists(glob => qualifiedNames(glob.drop(1)).matches(s"$name."))
  }

  /** The TASTy files of each platform twin that replace a class of the JVM modules, which the twin compiled from a
    * source of its own: one no source of the JVM modules is. The TASTy records the source it came from.
    */
  private def replacing(manifest: Manifest, copies: Vector[(Output, Path)]): Vector[(String, Vector[Path])] = {
    val primary = manifest.sources.filterNot(source => manifest.platforms(source.module)).map(_.path)
    val own     = mutable.Map.empty[String, Boolean]
    def replaced(file: Path): Boolean = sourceFile(file).exists(source =>
      own.getOrElseUpdate(source, !primary.exists(_.endsWith(Path.of(source))))
    )
    copies.filter((_, file) => replaced(file)).groupMap(_._1.module)(_._2).toVector
  }

  private def sourceFile(tasty: Path): Option[String] =
    Try(TastyUnpickler(Files.readAllBytes(tasty)).unpickle(Attribution)).toOption.flatten.flatMap(_.sourceFile)

  private object Attribution extends TastyUnpickler.SectionUnpickler[Attributes]("Attributes") {
    def unpickle(reader: TastyReader, names: TastyUnpickler.NameTable): Attributes =
      AttributeUnpickler(reader, names).attributes
  }

  private def files(directory: Path): Vector[Path] =
    if (!Files.isDirectory(directory)) Vector.empty
    else Using.resource(Files.walk(directory))(_.toList.asScala.toVector.filter(Files.isRegularFile(_)))

  // Classes instantiated reflectively by ServiceLoader, the compiler plugin loader or sbt plugin discovery.
  private def entryPoints(directory: Path): Vector[String] = {
    val services = files(directory.resolve("META-INF/services")).flatMap(Files.readAllLines(_).asScala)
      .map(_.takeWhile(_ != '#').trim)
    def lines(file: Path) = Some(file).filter(Files.isRegularFile(_)).toVector.flatMap(Files.readAllLines(_).asScala)
    val plugins           = lines(directory.resolve("plugin.properties")).collect {
      case line if line.startsWith("pluginClass=") => line.stripPrefix("pluginClass=").trim
    }
    val autoplugins = lines(directory.resolve("sbt/sbt.autoplugins")).map(_.trim)
    (services ++ plugins ++ autoplugins).filter(_.nonEmpty).map(_.stripSuffix("$").replace('$', '.'))
  }

  /** A construction or call kept until every precondition, binding and default argument of the build is known. */
  private case class Pending(
      callee: Symbol,
      method: Symbol,
      arguments: List[tpd.Tree],
      test: Boolean,
      own: Boolean,
      location: String
  )

  /** A member of a build class whose references decide whether it is used, test-only or wider than needed. */
  private case class Definition(
      name: String,
      node: String,
      deferred: Boolean,
      public: Boolean,
      variable: Boolean,
      info: Node,
      suppressed: Set[String],
      field: Boolean = false
  )

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

  private class Collector(
      outputs: Vector[Output],
      libraries: Set[String],
      generated: Vector[PathMatcher],
      concurrent: Set[String],
      reflective: Vector[Regex],
      bounded: Vector[Complexity.Bounded]
  ) extends Phase, Placements.Resolver {
    private val roots      = outputs.map(output => output -> output.path.toAbsolutePath.normalize)
    private val nodes      = mutable.Map.empty[String, Node]
    private val reflected  = mutable.Set.empty[String]
    private val edges      = mutable.Map.empty[String, mutable.Set[String]]
    private val mainRoots  = mutable.Set.empty[String]
    private val registered = mutable.Map.empty[Symbol, Option[String]]
    private val keys       = mutable.Map.empty[Symbol, Option[String]]
    private val parameters = mutable.Map.empty[Symbol, Vector[Algebra.Field]]
    private val tracked    = mutable.Map.empty[Symbol, Boolean]
    private val products   = mutable.Map.empty[Symbol, Algebra.Product]
    private val required   = mutable.Map.empty[Symbol, Algebra.Product]
    private val sums       = mutable.Map.empty[String, Algebra.Sum]
    private val pending    = mutable.ArrayBuffer.empty[Pending]
    private val sites      = mutable.Map.empty[String, Vector[Algebra.Site]]
    private val bindings   = mutable.Map.empty[Symbol, tpd.Tree]
    private val defaults   = mutable.Map.empty[Symbol, tpd.Tree]
    private val failures   = mutable.ArrayBuffer.empty[String]
    private val methods    = mutable.Map.empty[Symbol, Smells.Method]
    private val overridden = mutable.Map.empty[Symbol, Int]
    private val results    = mutable.Map.empty[Symbol, Option[String]]
    private val calls      = mutable.Map.empty[Symbol, (Int, Int)]
    private val discards   = mutable.Map.empty[Symbol, Int]
    // The applications the code inverts with `!`, and how many calls of each method it inverts.
    private val negations    = mutable.Set.empty[tpd.Tree]
    private val negatedCalls = mutable.Map.empty[Symbol, Int]
    private val abstracts    = mutable.Map.empty[Symbol, (String, String, String)]
    private val implements   = mutable.Map.empty[Symbol, mutable.Set[(String, String)]]
    private val designed     = mutable.ArrayBuffer.empty[Designs.Method]
    private val aliases      = mutable.ArrayBuffer.empty[Designs.Alias]
    private val stuttering   = mutable.Map.empty[Symbol, (String, String)]
    private val echoes       = mutable.Map.empty[Symbol, Int]
    private val literals     = mutable.ArrayBuffer.empty[(String, String, String, String, Node)]
    private val typeNames    = mutable.ArrayBuffer.empty[(String, Node)]
    private val mixins       = mutable.Set.empty[Symbol]
    private val asTypes      = mutable.Set.empty[Symbol]
    private val spliced      = mutable.Set.empty[Span]
    private val applied      = mutable.ArrayBuffer.empty[Structures.Applied]
    private val built        = mutable.Map.empty[Symbol, Int]
    private val grouped      = mutable.ArrayBuffer.empty[Structures.Member]
    private val detachable   = mutable.Set.empty[Symbol]
    private val dispatched   = mutable.ArrayBuffer.empty[Structures.Dispatch]
    private val extents      = mutable.Map.empty[String, (Int, Int)]
    private val structured   = mutable.ArrayBuffer.empty[Structures.Model]
    // Abstractions some class implements, beside those only lambdas implement.
    private val subclassed      = mutable.Set.empty[Symbol]
    private val specialised     = mutable.Set.empty[Symbol]
    private val generics        = mutable.Map.empty[Symbol, (String, String, Vector[String])]
    private val arguments       = mutable.Map.empty[(Symbol, Int), mutable.Map[String, Int]]
    private val testUses        = mutable.Map.empty[(Symbol, Int), Int]
    private val varied          = mutable.Set.empty[(Symbol, Int)]
    private val clones          = mutable.ArrayBuffer.empty[Clones.Group]
    private val members         = mutable.Map.empty[Symbol, Definition]
    private val users           = mutable.Map.empty[Symbol, mutable.Set[String]]
    private var usage           = Map.empty[String, Set[String]]
    private val assigned        = mutable.Set.empty[Symbol]
    private val explicit        = mutable.Map.empty[(Symbol, String, String), mutable.ArrayBuffer[Algebra.Site]]
    private val memberList      = mutable.ArrayBuffer.empty[Members.Member]
    private val givens          = mutable.Map.empty[Symbol, (String, Node)]
    private val named           = mutable.Set.empty[Symbol]
    private val givenList       = mutable.ArrayBuffer.empty[Members.Given]
    private val nested          = mutable.Set.empty[String]
    private val exposed         = mutable.Set.empty[String]
    private val nestedList      = mutable.ArrayBuffer.empty[Members.Nested]
    private val bodies          = mutable.Map.empty[Symbol, tpd.DefDef]
    private val initialisers    = mutable.Map.empty[Symbol, tpd.Tree]
    private val costed          = mutable.ArrayBuffer.empty[Symbol]
    private val complexityList  = mutable.ArrayBuffer.empty[Complexity.Summary]
    private val unusedList      = mutable.ArrayBuffer.empty[Complexity.Bounded]
    private val pipelineList    = mutable.ArrayBuffer.empty[Pipelines.Chain]
    private val classes         = mutable.LinkedHashSet.empty[Symbol]
    private val templates       = mutable.Map.empty[Symbol, tpd.Template]
    private val reachable       = mutable.Map.empty[Symbol, Option[Vector[Symbol]]]
    private val subclasses      = mutable.Map.empty[Symbol, mutable.ArrayBuffer[Symbol]]
    private val purities        = mutable.ArrayBuffer.empty[Purity.Model]
    private val hierarchies     = mutable.ArrayBuffer.empty[(Symbol, String, String)]
    private val bodied          = mutable.Set.empty[Symbol]
    private val enumerations    = mutable.ArrayBuffer.empty[Algebra.Enumeration]
    private val dispatches      = mutable.Map.empty[(Symbol, Symbol), (String, Type)]
    private val megamorphicList = mutable.ArrayBuffer.empty[Megamorphic.Site]
    private val placements      = mutable.ArrayBuffer.empty[Placements]

    def phaseName: String = "bonsai-collect"

    protected def run(using Context): Unit = ()

    override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
      val fragments = Fragments()
      val placing   = Placements(this)
      placements += placing
      units.foreach { unit =>
        try {
          References(this, fragments).traverse(unit.tpdTree)
          placing.traverse(unit.tpdTree)
        } catch { case NonFatal(error) => problem(s"${unit.source.path} could not be analysed: $error") }
      }
      qualifiedUses()
      enumerations ++= hierarchies.flatMap((parent, name, key) => enumeration(parent, name, key))
      val subjects = products.view.mapValues(product => product.name -> product.fields.size).toMap ++
        methods.view.mapValues(method => method.name -> method.fields.size) ++
        required.view.mapValues(product => product.name -> product.fields.size)
      for {
        (callee, recorded) <- pending.groupBy(_.callee)
        (name, arity)      <- subjects.get(callee)
        aligned = recorded.filter(_.arguments.size == arity) if aligned.nonEmpty
      } sites(name) = aligned.map(call =>
        Algebra.Site(call.arguments.map(classify(_)).toVector, call.test, call.own, call.location)
      ).toVector
      for {
        call              <- pending if call.method.name != nme.copy && defaulted(call.method)
        (argument, index) <- call.arguments.zipWithIndex if !omitted(argument)
        default           <- defaults.get(defaultGetter(call.method, index))
        value             <- same(argument, default)
        parameter         <- fieldsOf(call.callee).lift(index)
      } explicit.getOrElseUpdate((call.callee, parameter.name, value), mutable.ArrayBuffer.empty) +=
        Algebra.Site(Vector.empty, call.test, call.own, call.location)
      pending.clear()
      usage = users.toVector.groupMapReduce((symbol, _) => memberName(symbol))((_, nodes) => nodes.toSet)(_ ++ _)
      memberList ++= members.map { (symbol, definition) =>
        val bases = symbol.allOverriddenSymbols.toVector
        Members.Member(
          definition.name,
          definition.node,
          users.get(symbol).fold(Set.empty[String])(_.toSet),
          bases.exists(base => key(base).isEmpty || users.get(base).exists(_.nonEmpty)),
          bases.exists(key(_).isDefined),
          bases.nonEmpty,
          definition.deferred,
          definition.public,
          definition.variable && !assigned(symbol),
          detachable(symbol),
          definition.info.test,
          definition.suppressed,
          definition.field
        )
      }
      val overridden = givens.keySet.flatMap(_.allOverriddenSymbols)
      givenList ++= givens.collect {
        case (symbol, (name, info)) if symbol.allOverriddenSymbols.isEmpty && !overridden(symbol) && !clashes(symbol) =>
          Members.Given(name, info.name, named(symbol), suppressions(symbol))
      }
      val incoming = edges.toVector.flatMap((source, targets) => targets.map(_ -> source)).groupMap(_._1)(_._2)
      nestedList ++= nested.flatMap(inner =>
        nodes.get(inner).flatMap(_.outer).map(outer =>
          Members.Nested(inner, outer, incoming.getOrElse(inner, Vector.empty).toSet, exposed(inner))
        )
      )
      clones ++= fragments.groups()
      indexSubclasses()
      megamorphicList ++= dispatches.toVector.flatMap { case ((caller, callee), (node, receiver)) =>
        // Test doubles never reach a call in main code at run time.
        val test            = nodes.get(node).exists(_.test)
        val implementations = concreteSubclasses(callee.owner).flatMap { cls =>
          Option.when(
            (test || !key(cls).flatMap(nodes.get).exists(_.test)) && reaches(cls, callee.owner, receiver)
          )(
            callee.denot.matchingMember(cls.thisType)
          ).filter(implementation => implementation.exists && !implementation.is(Flags.Deferred))
        }.distinct.toVector
        Option.when(implementations.size > MaximumInlinedReceivers)(Megamorphic.Site(
          qualifiedName(caller),
          node,
          s"${callee.owner.name.toString.stripSuffix("$")}.${callee.name}",
          shown(implementations.map(_.owner)),
          suppressions(caller)
        ))
      }
      val costs = Costs(bodies, initialisers, bounded)
      complexityList ++= costed.flatMap(method =>
        costs.summary(method).flatMap(bounds =>
          key(method).map(Complexity.Summary(qualifiedName(method), _, bounds, suppressions(method)))
        )
      )
      unusedList ++= costs.unmatched
      val chains = Pipelines.Chains()
      pipelineList ++= bodies.toVector.filter((method, _) => ownMethod(method)).flatMap((method, definition) =>
        key(method).toVector.flatMap(chains.of(qualifiedName(method), _, suppressions(method), definition.rhs))
      )
      purities += purity()
      structured += structure()
      units
    }

    /** Adds what a twin's copies of replaced classes reference, which the copies of the JVM modules the node records
      * stand in for: the edges, the users of members and of nested types.
      */
    def absorb(other: Collector): Unit = {
      other.edges.foreach((source, targets) => targets.foreach(edge(source, _)))
      val incoming = other.edges.toVector.flatMap((source, targets) => targets.map(_ -> source)).groupMap(_._1)(_._2)
      memberList.mapInPlace(member =>
        member.copy(users = member.users ++ other.usage.getOrElse(member.name, Set.empty))
      )
      nestedList.mapInPlace(inner => inner.copy(users = inner.users ++ incoming.getOrElse(inner.node, Vector.empty)))
    }

    def problem(message: String): Unit = failures += message

    def problems: Vector[String] = failures.toVector

    def graph(entryPoints: Set[String]): Graph =
      Graph(
        nodes.toMap,
        edges.view.mapValues(_.toSet).toMap,
        mainRoots.toSet ++ entryPoints ++ nodes.values.filter(_.exported).map(_.name)
      )

    def algebra: Algebra.Model =
      Algebra.Model(
        products.values.toVector,
        sums.values.toVector,
        sites.toMap,
        required.values.toVector,
        enumerations.toVector
      )

    def smells: Smells.Model = Smells.Model(
      methods.toVector.map((symbol, method) =>
        if (overridden.getOrElse(symbol, 0) > 1) method.copy(shared = true) else method
      ),
      sites.toMap,
      calls.toVector.flatMap { case (symbol, (total, tests)) =>
        for {
          method <- methods.get(symbol)
          result <- results.getOrElse(symbol, None)
        } yield Smells.Usage(
          method,
          result,
          total,
          discards.getOrElse(symbol, 0),
          tests,
          negatedCalls.getOrElse(symbol, 0)
        )
      },
      abstracts.toVector.collect {
        case (symbol, (name, node, module)) if !mixins(symbol) || asTypes(symbol) =>
          Smells.Abstraction(
            name,
            node,
            module,
            implements.get(symbol).fold(Vector.empty)(_.toVector.sorted),
            functional = implements.contains(symbol) && !subclassed(symbol),
            specialised = specialised(symbol)
          )
      },
      arguments.toVector.flatMap { case ((symbol, index), types) =>
        generics.get(symbol).map((owner, node, parameters) =>
          Smells.Instantiation(
            owner,
            node,
            parameters(index),
            types.toMap,
            varied((symbol, index)),
            testUses.getOrElse((symbol, index), 0)
          )
        )
      },
      explicit.toVector.flatMap { case ((callee, parameter, value), recorded) =>
        val subject = products.get(callee).map(product => product.name.split('.').last -> product.node)
          .orElse(methods.get(callee).map(method => method.short -> method.node))
        subject.map((owner, node) =>
          Smells.Default(owner, node, parameter, value, recorded.toVector)
        )
      }
    )

    def complexities: Vector[Complexity.Summary] = complexityList.toVector

    def unusedBounds: Vector[Complexity.Bounded] = unusedList.toVector

    def pipelines: Vector[Pipelines.Chain] = pipelineList.toVector

    def megamorphic: Vector[Megamorphic.Site] = megamorphicList.toVector

    def purity: Purity.Model = purities.lastOption.getOrElse(Purity.Model.empty)

    private def purity()(using Context): Purity.Model = {
      val effects = Purity.Inference(bodies, templates, implementations(_), concurrent)
      effects.run()
      def site(method: Symbol, tree: tpd.Tree) =
        key(method).map {
          node =>
            val position = tree.sourcePos
            node -> (if (position.exists) s"${position.source.path}:${position.line + 1}" else "unknown")
        }
      Purity.Model(
        effects.pointless.flatMap((method, callee, tree) =>
          site(method, tree).map((node, location) => Purity.Discard(callee, node, location, suppressions(method)))
        ),
        effects.effectful.flatMap((method, operation, effect, tree, view) =>
          site(method, tree).map((node, location) =>
            Purity.Transform(operation, effect, node, location, view, suppressions(method))
          )
        ),
        effects.shared.flatMap((method, variable, entry, tree) =>
          site(method, tree).map((node, location) =>
            Purity.SharedWrite(variable, entry, node, location, suppressions(method))
          )
        ),
        bodies.keys.filter(ownMethod(_)).flatMap(method =>
          key(method).map(node =>
            qualifiedName(method) ->
              Purity.Method(
                node,
                effects.verdict(method),
                effects.observed(method),
                effects.role(method),
                suppressions(method)
              )
          )
        ).toMap,
        effects.library
      )
    }

    // The concrete classes deriving from each class, in definition order; every class is defined by the time calls are
    // resolved.
    private def indexSubclasses()(using Context): Unit = {
      subclasses.clear()
      classes.foreach(cls =>
        if (!cls.isOneOf(Flags.Trait | Flags.Abstract))
          cls.asClass.baseClasses.foreach(base => subclasses.getOrElseUpdate(base, mutable.ArrayBuffer.empty) += cls)
      )
    }

    private def concreteSubclasses(owner: Symbol): collection.Seq[Symbol] = subclasses.getOrElse(owner, Vector.empty)

    /** The bodies a call of `method` can run: its own, or every override in the build's main classes, which test
      * doubles never reach at run time; nothing when one of them lies outside the build.
      */
    private def implementations(method: Symbol)(using Context): Option[Vector[Symbol]] =
      reachable.getOrElseUpdate(
        method,
        if (bodies.contains(method) && (method.isEffectivelyFinal || !method.owner.isClass)) Some(Vector(method))
        else if (!classes.contains(method.owner)) None
        else {
          val overrides = concreteSubclasses(method.owner).filter(cls =>
            !key(cls).flatMap(nodes.get).exists(_.test)
          ).map(cls => method.denot.matchingMember(cls.thisType)).filter(found =>
            found.exists && !found.is(Flags.Deferred)
          ).toVector
          val all = (Option.when(bodies.contains(method))(method).toVector ++ overrides).distinct
          // An immutable val implements a method by a read, which has no effect.
          Option.when(all.nonEmpty && all.forall(target =>
            bodies.contains(target) || !target.is(Flags.Method) && !target.is(Flags.Mutable)
          ))(all)
        }
      )

    // Simple names, qualified by their enclosing name where two would read the same.
    private def shown(classes: Vector[Symbol])(using Context): Vector[String] = {
      val simple = classes.map(_.name.toString.stripSuffix("$"))
      if (simple.distinct.size == simple.size) simple
      else classes.map(cls => Finding.short(qualifiedName(cls)))
    }

    // HotSpot's inline caches hold two receiver types; a third makes a call site megamorphic.
    private val MaximumInlinedReceivers = 2

    /** Records a virtual call made inside a loop by `caller`, owned by `node`. */
    /** Whether a value of `cls` can be the receiver of static type `receiver` at a call of a member of `owner`: a class
      * extending `owner` with other concrete type arguments than the receiver's never is.
      */
    private def reaches(cls: Symbol, owner: Symbol, receiver: Type)(using Context): Boolean = {
      def concrete(tpe: Type) =
        tpe.widenDealias.typeSymbol.isClass && !tpe.existsPart(_.typeSymbol.isOneOf(Flags.TypeParam | Flags.Deferred))
      val expected = receiver.widen.baseType(owner).argInfos
      val actual   = cls.typeRef.baseType(owner).argInfos
      expected.size != actual.size || expected.zip(actual).forall((left, right) =>
        !concrete(left) || !concrete(right) || left <:< right || right <:< left
      )
    }

    def dispatch(callee: Symbol, caller: Option[Symbol], node: Option[String], receiver: Type)(using Context): Unit = {
      val universal = Set[Symbol](defn.AnyClass, defn.ObjectClass, defn.ProductClass, defn.SerializableClass)
      if (
        // A val is read through a getter, which dispatches like any other method.
        callee.isTerm && !callee.is(Flags.Module) && !callee.isConstructor && callee.owner.isClass &&
        !callee.isEffectivelyFinal && !callee.is(Flags.Private) && !universal(callee.owner) &&
        !callee.owner.derivesFrom(requiredClass("scala.Equals"))
      ) for {
        method <- caller
        key    <- node
      } dispatches((method, callee)) = (key, receiver)
    }

    // The build's own methods: members of its classes, not synthetic.
    private def ownMethod(method: Symbol)(using Context): Boolean =
      method.owner.isClass && !method.is(Flags.Synthetic) && key(method).exists(nodes.contains)

    /** Records the initialiser of an immutable member, whose size the cost bounds follow. */
    def initialiser(symbol: Symbol, definition: tpd.ValDef)(using Context): Unit =
      if (!symbol.is(Flags.Mutable)) initialisers(symbol) = definition.rhs

    /** Records a method body for cost bounds, reported for the methods of build classes. */
    def body(symbol: Symbol, definition: tpd.DefDef)(using Context): Unit =
      if (!symbol.isAnonymousFunction) {
        bodies(symbol) = definition
        if (symbol.owner.isClass && !symbol.is(Flags.Synthetic) && !symbol.isConstructor) costed += symbol
      }

    def memberships: Members.Model = Members.Model(memberList.toVector, givenList.toVector, nestedList.toVector)

    /** The declarations the nesting rule places, with `entries` the nodes code outside the build enters at. */
    def nesting(entries: Set[String]): Nesting.Model = placements.lastOption.fold(Nesting.Model.empty)(_.model(entries))

    def duplicates: Vector[Clones.Group] = clones.toVector

    def described(node: String): Option[Node] = nodes.get(node)

    def root(name: String): Unit = mainRoots += name

    /** Records the algebraic shape, invariant and construction access of a class defined in the analysed outputs. */
    def define(cls: Symbol, template: tpd.Template)(using Context): Unit = {
      classes += cls
      templates(cls) = template
      if (authored(template)) bodied += cls
      register(node(cls)).foreach { key =>
        val name = qualifiedName(cls)
        if (cls.is(Flags.CaseClass)) {
          val parents   = cls.asClass.parentSyms.filter(_.is(Flags.Sealed)).map(qualifiedName(_)).toSet
          val fields    = fieldsOf(cls)
          val invariant = Predicates.required(cls, Predicates.conditions(template.body), fields)
          products(cls) = Algebra.Product(
            name,
            key,
            fields,
            parents,
            invariant,
            access(cls, fields),
            exported(cls) && exported(cls.primaryConstructor)
          )
        } else if (cls.is(Flags.Sealed)) {
          val variants = cls.children.map { child =>
            val fields = if (child.isClass && child.is(Flags.CaseClass)) fieldsOf(child) else Vector.empty
            Algebra.Variant(qualifiedName(child), fields)
          }
          val accessors = cls.info.decls.toList.filter(member =>
            member.is(Flags.Method) && member.is(Flags.Deferred)
          ).map(_.name.toString).toSet
          sums(name) = Algebra.Sum(name, key, variants.toVector, accessors)
          if (!cls.is(Flags.Enum)) hierarchies += ((cls, name, key))
        }
        nodes.get(key).foreach { info =>
          if (cls.is(Flags.Trait) || cls.is(Flags.Abstract)) {
            // Other builds can implement what a library exports, unless it is sealed.
            if (!cls.is(Flags.Enum) && !info.test && (cls.is(Flags.Sealed) || !exported(cls))) {
              abstracts(cls) = (name, key, info.module)
              if (cls.is(Flags.Trait) && !cls.info.decls.exists(_.is(Flags.Deferred))) mixins += cls
            }
          } else
            cls.asClass.baseClasses.drop(1).foreach { parent =>
              subclassed += parent
              implement(parent, name, info.module)
              if (cls.typeRef.baseType(parent).argInfos.exists(_.widenDealias.typeSymbol.isPrimitiveValueClass))
                specialised += parent
            }
        }
      }
    }

    // Statements and members the author wrote, beyond the fields and members the compiler derives for a case class.
    private def authored(template: tpd.Template)(using Context): Boolean = template.body.exists {
      case member: tpd.MemberDef => !member.symbol.is(Flags.Synthetic) && !member.symbol.is(Flags.ParamAccessor)
      case tpd.Import(_, _)      => false
      case _                     => true
    }

    /** A sealed hierarchy an enum could state: case classes and objects without bodies, companion members or derived
      * instances, which nothing extends and which belong to no other sealed hierarchy.
      */
    private def enumeration(parent: Symbol, name: String, key: String)(using Context): Option[Algebra.Enumeration] = {
      val cases                         = parent.children.map(child => if (child.isTerm) child.moduleClass else child)
      def plain(child: Symbol): Boolean =
        child.isClass && child.is(Flags.Case) && !child.isOneOf(Flags.Sealed | Flags.Trait | Flags.Abstract) &&
          !implements.contains(child) && !bodied(child) &&
          child.asClass.parentSyms.count(_.is(Flags.Sealed)) == 1 &&
          !child.companionModule.moduleClass.info.decls.toList.exists(member =>
            member.is(Flags.Given) || !member.is(Flags.Synthetic) && !member.isConstructor
          )
      Option.when(cases.size >= MinimumCases && cases.forall(plain(_)))(Algebra.Enumeration(
        name,
        key,
        cases.size,
        cases.forall(_.owner == parent.companionModule.moduleClass),
        exported(parent)
      ))
    }

    def implement(abstraction: Symbol, name: String, module: String): Unit =
      implements.getOrElseUpdate(abstraction, mutable.Set.empty) += (name -> module): Unit

    /** Records a method signature of the build for the argument, result and parameter group checks. */
    def signature(method: Symbol, body: tpd.Tree)(using Context): Unit =
      key(method).flatMap(nodes.get).foreach { info =>
        val fields = fieldsOf(method)
        method.allOverriddenSymbols.filter(_.is(Flags.Deferred)).foreach(abstracted =>
          overridden(abstracted) = overridden.getOrElse(abstracted, 0) + 1
        )
        methods(method) = Smells.Method(
          methodName(method, fields),
          info.name,
          qualifiedName(method.owner),
          fields,
          method.allOverriddenSymbols.hasNext,
          exported(method),
          info.test,
          stored(method, body),
          method.paramSymss.filter(_.headOption.exists(_.isTerm)).flatten.zipWithIndex.collect {
            case (parameter, index) if parameter.is(Flags.Inline) => index
          }.toSet,
          method.is(Flags.Inline) || method.owner.isTerm && method.owner.is(Flags.Inline),
          suppressed = suppressions(method),
          rest = Shapes.clauses(method).drop(1).flatten
        )
      }

    /** The parameters of the first term parameter list that `body` passes only to constructors, by index. */
    private def stored(method: Symbol, body: tpd.Tree)(using Context): Set[Int] = {
      def constructs(application: tpd.Apply) = application.symbol.isConstructor ||
        application.symbol.name == nme.apply && application.symbol.is(Flags.Synthetic)
      val passed = body.filterSubTrees {
        case application: tpd.Apply => constructs(application)
        case _                      => false
      }.collect { case application: tpd.Apply => application.args }.flatten.map(Predicates.unwrap(_).symbol)
      def uses(parameter: Symbol) = body.filterSubTrees {
        case reference: tpd.Ident => reference.symbol == parameter
        case _                    => false
      }.size
      method.paramSymss.find(_.headOption.exists(_.isTerm)).getOrElse(Nil).zipWithIndex.collect {
        case (parameter, index) if uses(parameter) > 0 && uses(parameter) == passed.count(_ == parameter) => index
      }.toSet
    }

    def designs: Designs.Model = Designs.Model(designed.toVector, aliases.toVector)

    /** Records a method of a type of the build for the design rules. */
    def design(method: Symbol, body: tpd.Tree)(using Context): Unit = {
      val owner = method.owner
      // A supertype fixes the name and signature of a method that implements it.
      if (
        owner.isClass && !owner.isAnonymousClass && !method.isOneOf(Flags.Synthetic | Flags.Accessor) &&
        !method.isSetter && !method.name.is(DefaultGetterName) && !method.allOverriddenSymbols.hasNext
      )
        key(method).flatMap(nodes.get).foreach { info =>
          val parameters = method.paramSymss.find(_.headOption.exists(_.isTerm)).getOrElse(Nil)
          val first      = parameters.headOption
          designed += Designs.Method(
            qualifiedName(method),
            qualifiedName(owner),
            info.name,
            owner.is(Flags.ModuleClass),
            implementing(owner),
            !method.isOneOf(Flags.Private | Flags.Protected) && !method.privateWithin.exists,
            first.map(_.info.widenDealias.typeSymbol).filter(_.isClass).map(qualifiedName(_)),
            parameters.map(_.info.widenDealias.show).toVector,
            method.info.finalResultType.widenDealias.show,
            // An extension method is what the rule would make of it already.
            !method.is(Flags.ExtensionMethod) && first.exists(parameter =>
              receiving(method, parameter, body) && !fixedReceiver(method, parameter)
            ),
            info.test
          )
          if (!method.is(Flags.ExtensionMethod)) dispatch(method, parameters, body, info)
        }
    }

    /** Whether `parameter` is a receiver the method cannot move onto: a type that has a member of the method's name
      * already, or an inline instance of a type-parameterised trait, as an algebra or tagless-final interpreter is.
      */
    private def fixedReceiver(method: Symbol, parameter: Symbol)(using Context): Boolean = {
      val tpe = parameter.info.widenDealias
      tpe.member(method.name).exists ||
      parameter.is(Flags.Inline) && tpe.typeSymbol.is(Flags.Trait) && tpe.typeSymbol.typeParams.nonEmpty
    }

    /** Records a method whose result is a match on an enum of the build that it takes as a parameter. */
    private def dispatch(method: Symbol, parameters: List[Symbol], body: tpd.Tree, info: Node)(using Context): Unit =
      concluding(body) match {
        case tpd.Match(selector, cases) =>
          val parameter   = Predicates.unwrap(selector).symbol
          val enumeration = parameter.info.widenDealias.typeSymbol
          // A function that needs more of its owner than the enum cannot move into it.
          val anchored = body.existsSubTree {
            case reference: tpd.RefTree =>
              reference.symbol.exists && reference.symbol.owner == method.owner &&
              reference.symbol != method && reference.symbol != enumeration.companionModule
            case _ => false
          }
          if (
            parameters.contains(parameter) && enumeration.isClass && enumeration.is(Flags.Enum) &&
            !enumeration.is(Flags.Case) && method.owner != enumeration && !anchored
          )
            key(enumeration).flatMap(nodes.get).foreach { owned =>
              val children = enumeration.children.toSet
              val covered  = mutable.Set.empty[Symbol]
              cases.foreach(_.pat.foreachSubTree(pattern =>
                covered ++= List(pattern.symbol, pattern.tpe.typeSymbol, pattern.tpe.termSymbol).filter(children)
              ))
              dispatched += Structures.Dispatch(
                qualifiedName(method),
                qualifiedName(method.owner),
                info.name,
                qualifiedName(enumeration),
                parameter.name.toString,
                covered.size,
                children.size,
                method.owner == enumeration.companionModule.moduleClass || owned.source == info.source
              )
            }
        case _ => ()
      }

    @annotation.tailrec
    private def concluding(tree: tpd.Tree): tpd.Tree = Predicates.unwrap(tree) match {
      case tpd.Block(_, expression) => concluding(expression)
      case other                    => other
    }

    // Traits the compiler gives case classes and objects, which say nothing of what they implement.
    private val Markers = Set("scala.Product", "scala.Equals", "java.io.Serializable")

    // An object implementing a trait, as a type-class instance does, is more than a holder of functions.
    private def implementing(cls: Symbol)(using Context): Boolean = cls.asClass.parentSyms.exists { parent =>
      val name = parent.fullName.toString
      parent.is(Flags.Trait) && !Markers(name) && !name.startsWith("scala.deriving.")
    }

    /** Whether every method `body` calls, beside those of objects, is called on `parameter`, and nothing in it names
      * another member of the method's owner.
      */
    private def receiving(method: Symbol, parameter: Symbol, body: tpd.Tree)(using Context): Boolean = {
      val receivers = body.filterSubTrees {
        case select: tpd.Select =>
          select.symbol.is(Flags.Method) && select.qualifier.isTerm &&
          !select.qualifier.symbol.isOneOf(Flags.Module | Flags.Package)
        case _ => false
      }.collect { case select: tpd.Select => Predicates.unwrap(select.qualifier).symbol }
      val owned = body.existsSubTree {
        case reference: tpd.Ident =>
          reference.symbol.exists && reference.symbol.owner == method.owner && reference.symbol != method
        case tpd.This(_) => true
        case _           => false
      }
      receivers.nonEmpty && receivers.forall(_ == parameter) && !owned
    }

    /** Records a transparent alias beside an object of the same name. */
    def alias(symbol: Symbol)(using Context): Unit = {
      // A top-level alias belongs to its file's wrapper object, beside which the package holds the object.
      val scope     = if (symbol.owner.isTopLevelDefinitionsObject) symbol.owner.owner else symbol.owner
      val companion = scope.info.member(symbol.name.toTermName).symbol
      val target    = symbol.info.hiBound
      // An alias of a type its object holds, as the opaque type of a newtype, names what the object already defines.
      val held = target match {
        case reference: TypeRef => reference.prefix.termSymbol == companion
        case _                  => false
      }
      val forwarded = target match {
        case applied: AppliedType =>
          applied.tycon.typeSymbol.isClass && applied.argInfos.map(_.typeSymbol) == symbol.typeParams
        case _ => false
      }
      if (companion.is(Flags.Module) && !held && !forwarded)
        key(symbol).foreach { node =>
          val element    = target.widenDealias.baseType(requiredClass("scala.collection.Iterable")).argInfos.lastOption
          val operations = companion.moduleClass.info.decls.toList.count(member =>
            member.is(Flags.Method) && !member.isOneOf(Flags.Synthetic | Flags.Private) && !member.isConstructor &&
              member.paramSymss.flatten.exists(_.info.typeSymbol == symbol)
          )
          aliases += Designs.Alias(
            qualifiedName(symbol),
            node,
            target.show,
            element.exists(tpe => defn.isTupleNType(tpe.widenDealias) || tpe.typeSymbol.name.toString == "NamedTuple"),
            operations
          )
        }
      if (!companion.is(Flags.Module)) target match {
        case constructed: AppliedType if constructed.args.exists {
              case ConstantType(_) => true
              case _               => false
            } =>
          key(symbol).foreach(node =>
            applied += Structures.Applied(
              qualifiedName(symbol),
              node,
              qualifiedName(scope),
              qualifiedName(constructed.tycon.typeSymbol)
            )
          )
        case _ => ()
      }
    }

    def structures: Structures.Model = structured.lastOption.getOrElse(Structures.Model.empty)

    /** Records the extent of a class of the build, the constructions its parents make, and its fields named like it. */
    def structure(cls: Symbol, definition: tpd.TypeDef, template: tpd.Template)(using Context): Unit =
      key(cls).flatMap(nodes.get).foreach { info =>
        // An anonymous class is a construction of what it extends.
        if (!cls.isAnonymousClass) {
          template.parents.foreach(_.foreachSubTree {
            case tpd.New(tpt) => built(tpt.tpe.typeSymbol) = built.getOrElse(tpt.tpe.typeSymbol, 0) - 1
            case _            => ()
          })
          if (info.outer.isEmpty && info.name == qualifiedName(cls) && definition.span.exists) {
            // A companion spans its own lines, and a synthetic one those of its class.
            val position     = definition.sourcePos
            val (start, end) = extents.getOrElse(info.name, (position.startLine, position.endLine))
            extents(info.name) = (start.min(position.startLine), end.max(position.endLine))
          }
          if (!cls.is(Flags.ModuleClass))
            cls.asClass.paramAccessors.filter(field =>
              !field.is(Flags.Private) && stutters(cls.name.toString, field.name.toString)
            ).foreach(field => stuttering(field) = (qualifiedName(cls), info.name))
        }
      }

    // A field named like its class reads `graph.graph` wherever a value is named for its type.
    private def stutters(cls: String, field: String): Boolean =
      Vector(cls, s"${cls}s", s"${cls}es").exists(field.equalsIgnoreCase(_))

    /** Records a member of an object with what its definition names: whether it names anything else of its object, and
      * for an object that is neither a companion nor an instance, the members and types it names.
      */
    def group(member: Symbol, definition: tpd.ValOrDefDef)(using Context): Unit = {
      val owner = member.owner
      if (
        owner.is(Flags.ModuleClass) && !owner.isTopLevelDefinitionsObject &&
        !member.isOneOf(Flags.Synthetic | Flags.Module) && !member.isConstructor &&
        !member.name.is(DefaultGetterName) && !member.allOverriddenSymbols.hasNext
      )
        key(member).flatMap(nodes.get).foreach { info =>
          val siblings = mutable.Set.empty[String]
          val types    = mutable.Set.empty[String]
          definition.foreachSubTree { tree =>
            if (tree.symbol.exists && tree.symbol.owner == owner && tree.symbol != member)
              siblings += tree.symbol.name.toString
            key(tree.tpe.typeSymbol).foreach(types += _)
          }
          if (siblings.isEmpty && !types(info.name)) detachable += member
          if (!info.test && !owner.companionClass.exists && !implementing(owner))
            grouped += Structures.Member(
              member.name.toString,
              qualifiedName(owner),
              info.name,
              siblings.toSet,
              types.toSet - info.name
            )
        }
    }

    /** Records the field reads, constructions and member names in strings the structure rules count. */
    def observe(tree: tpd.Tree, from: Option[String])(using Context): Unit = tree match {
      case tpd.Select(qualifier, name)
          if tree.symbol.is(Flags.ParamAccessor) && qualifier.symbol.exists && qualifier.symbol.name == name =>
        echoes(tree.symbol) = echoes.getOrElse(tree.symbol, 0) + 1
      case tpd.New(tpt) => built(tpt.tpe.typeSymbol) = built.getOrElse(tpt.tpe.typeSymbol, 0) + 1
      case tpd.Apply(tpd.Select(context @ tpd.Apply(_, List(parts)), _), _)
          if context.tpe.widen.isRef(defn.StringContextClass) =>
        val pieces = parts match {
          case tpd.Typed(tpd.SeqLiteral(elements, _), _) => elements
          case _                                         => Nil
        }
        spliced ++= pieces.drop(1).collect {
          case literal @ tpd.Literal(constant)
              if constant.tag == Constants.StringTag && constant.stringValue.headOption.exists(_.isLetterOrDigit) =>
            literal.span
        }
      case tpd.Literal(constant) if constant.tag == Constants.StringTag && !spliced(tree.span) =>
        constant.stringValue match {
          case Reference(path, owner, member, _) if !Extensions(member) =>
            from.flatMap(nodes.get).foreach(info =>
              literals += ((constant.stringValue, path, owner, member, info))
            )
          case QualifiedType(_, _) =>
            from.flatMap(nodes.get).foreach(info => typeNames += ((constant.stringValue, info)))
          case _ => ()
        }
      case _ => ()
    }

    private def structure()(using Context): Structures.Model = {
      val types    = classes.toVector.filter(cls => !cls.isAnonymousClass && key(cls).isDefined)
      val named    = types.groupBy(_.effectiveName.toString)
      val mentions = literals.toVector.flatMap { (text, path, owner, member, info) =>
        val candidates =
          named.getOrElse(owner, Vector.empty).filter(cls => path.isEmpty || qualifiedName(cls) == path + owner)
        Option.when(candidates.nonEmpty && !candidates.exists(cls =>
          cls.info.member(termName(member)).exists || cls.info.member(typeName(member)).exists
        ))(Structures.Mention(text, owner, member, info.name, info.test))
      }
      val concretes = types.filterNot(_.is(Flags.Trait)).flatMap(cls =>
        key(cls).flatMap(nodes.get).map(info =>
          Structures.Concrete(
            qualifiedName(cls),
            info.name,
            !cls.isOneOf(Flags.Abstract | Flags.ModuleClass | Flags.Enum | Flags.Case) && built.getOrElse(cls, 0) <= 0,
            cls.asClass.parentSyms.collect {
              case parent if !parent.is(Flags.Trait) && key(parent).isDefined => qualifiedName(parent)
            }.toVector
          )
        )
      )
      Structures.Model(
        stuttering.toVector.map { case (field, (owner, node)) =>
          Structures.Stutter(owner, node, field.name.toString, echoes.getOrElse(field, 0))
        },
        mentions,
        applied.toVector,
        concretes,
        grouped.toVector,
        dispatched.toVector,
        extents.view.mapValues((start, end) => end - start + 1).toMap
      )
    }

    /** Keeps an application the code inverts with `!`, which is how a boolean method every caller negates is found. */
    def negate(application: tpd.Tree): Unit = negations += application

    /** Counts a statement whose value is dropped, for methods whose results are tracked. */
    def discard(
        statement: tpd.Tree
    )(using Context): Unit = { // `val _ = value` is typed as `value match { case _ => () }`.
      val dropped = statement match {
        case tpd.Match(selector, List(tpd.CaseDef(tpd.Ident(name), tpd.EmptyTree, _))) if name.toString == "_" =>
          selector
        case other => other
      }
      val method = invoked(dropped)
      if (result(method).isDefined) discards(method) = discards.getOrElse(method, 0) + 1
    }

    @annotation.tailrec
    private def invoked(tree: tpd.Tree)(using Context): Symbol = Predicates.unwrap(tree) match {
      case tpd.Apply(function, _)     => invoked(function)
      case tpd.TypeApply(function, _) => invoked(function)
      case other                      => other.symbol
    }

    /** Records the type arguments of a call or construction of a generic definition of the build. */
    def instantiate(target: Symbol, types: List[Type], clause: Int, owner: Option[String])(using Context): Unit = {
      val generic = Some(constructs(target)).filter(_.exists).getOrElse(target)
      // Extension methods take a type clause of their own after the extension's, each filled by its own application.
      val clauses =
        if (generic.isClass) List(generic.typeParams) else generic.paramSymss.filter(_.headOption.exists(_.isType))
      val parameters = clauses.lift(clause).getOrElse(Nil)
      val offset     = clauses.take(clause).map(_.size).sum
      // The compiler, not the caller, chooses the type arguments of a given; default getters repeat those of their
      // method, which is reported instead; and other builds give their own to what a library exports.
      if (
        parameters.nonEmpty && parameters.size == types.size && !generic.isOneOf(Flags.Synthetic | Flags.Given) &&
        !generic.name.is(DefaultGetterName) && !exported(generic)
      )
        key(generic).filter(_ => output(generic).isDefined).foreach { node =>
          generics.getOrElseUpdate(
            generic,
            (qualifiedName(generic), node, clauses.flatten.map(_.name.toString).toVector)
          ): Unit
          val test = owner.flatMap(nodes.get).exists(_.test)
          types.zipWithIndex.foreach { (argument, index) =>
            val slot = generic -> (offset + index)
            // A literal type such as the 16 of FloatingPoint[16] is an argument of its own, not its class.
            val bare = argument.dealias match {
              case literal: ConstantType => literal
              case _                     => argument.widenDealias
            }
            if (!bare.typeSymbol.isClass || bare.existsPart(_.typeSymbol.is(Flags.TypeParam))) varied += slot
            else if (!bare.isRef(defn.NothingClass)) {
              val counts = arguments.getOrElseUpdate(slot, mutable.Map.empty)
              val shown  = bare.show
              counts(shown) = counts.getOrElse(shown, 0) + 1
              if (test) testUses(slot) = testUses.getOrElse(slot, 0) + 1
            }
          }
        }
    }

    /** Whether construction is private, open beside companion factories returning the class in an effect, or open. */
    private def access(cls: Symbol, fields: Vector[Algebra.Field])(using Context): Algebra.Access = {
      @annotation.tailrec
      def isRestricted(symbol: Symbol): Boolean =
        symbol.exists && !symbol.isPackageObject && !symbol.is(Flags.Package) &&
          (symbol.is(Flags.Private) || symbol.privateWithin.exists || isRestricted(symbol.owner))
      val constructor = cls.primaryConstructor
      if (isRestricted(constructor) || isRestricted(cls)) Algebra.Access.Private
      else {
        val wrappers = Set[Symbol](defn.OptionClass, Shapes.either(), requiredClass("scala.util.Try"))
        val shapes   = fields.map(_.shape.key).sorted
        // A smart constructor takes the fields it validates; other effectful factories parse different inputs.
        val factories = cls.companionModule.moduleClass.info.decls.toList.filter { method =>
          method.is(Flags.Method) && !method.is(Flags.Synthetic) && !method.isConstructor &&
          !method.name.is(DefaultGetterName) &&
          (method.info.finalResultType.widenDealias match {
            case AppliedType(wrapper, wrapped) =>
              wrappers.contains(wrapper.typeSymbol) && wrapped.lastOption.exists(_.typeSymbol == cls)
            case _ => false
          }) && fieldsOf(method).map(_.shape.key).sorted == shapes
        }
        if (factories.isEmpty) Algebra.Access.Open
        else Algebra.Access.Factory(factories.map(method => s"${cls.name}.${method.name}").distinct.toVector)
      }
    }

    /** Records the `require` preconditions of a method over its first parameter list. */
    def precondition(method: Symbol, body: tpd.Tree)(using Context): Unit = {
      val conditions = Predicates.conditions(body match {
        case tpd.Block(statements, expression) => statements :+ expression
        case other                             => List(other)
      })
      if (conditions.nonEmpty) register(node(method)).foreach { key =>
        val fields    = fieldsOf(method)
        val condition = Predicates.required(method, conditions, fields)
        if (condition != Algebra.Predicate.Known(true)) {
          required(method) =
            Algebra.Product(
              methodName(method, fields),
              key,
              fields,
              Set.empty,
              condition,
              Algebra.Access.Open,
              exported(method)
            )
        }
      }
    }

    def bind(symbol: Symbol, value: tpd.Tree): Unit = bindings(symbol) = value

    def default(getter: Symbol, value: tpd.Tree): Unit = defaults(getter) = value

    /** Keeps a construction of a case class, or a call to a method of the build, for classification at the end. A
      * construction is `own` inside its class or companion, a call is `own` inside the method itself.
      */
    def record(application: tpd.Apply, owner: Option[String], enclosing: Symbol => Boolean)(using Context): Unit =
      application.fun match {
        case tpd.Apply(_, _) => ()
        case _               =>
          val method      = application.symbol
          val constructed = constructs(method)
          val test        = owner.flatMap(nodes.get).exists(_.test)
          if (!constructed.exists && result(method).isDefined) {
            val (total, tests) = calls.getOrElse(method, (0, 0))
            calls(method) = (total + 1, tests + (if (test) 1 else 0))
            if (negations.contains(application))
              negatedCalls(method) = negatedCalls.getOrElse(method, 0) + 1
          }
          val callee =
            if (constructed.exists && constructed.is(Flags.CaseClass)) constructed
            else if (method.is(Flags.Method) && !method.isConstructor) method
            else NoSymbol
          if (
            callee.exists &&
            tracked.getOrElseUpdate(
              callee,
              output(callee).isDefined &&
                (fieldsOf(callee).exists(_.kind != Algebra.Kind.Other) ||
                  defaulted(method) || (callee.is(Flags.Method) && fieldsOf(callee).nonEmpty))
            )
          ) {
            val position = application.sourcePos
            val location = if (position.exists) s"${position.source.path}:${position.line + 1}" else "unknown"
            val own      = if (constructed.exists) owner.isDefined && owner == key(callee) else enclosing(callee)
            pending += Pending(callee, method, application.args, test, own, location)
          }
      }

    /** Records a public or package-visible member of a build class, leaving out what the compiler writes and the API of
      * a library, which other builds use.
      */
    def member(symbol: Symbol)(using Context): Unit = {
      val owner = symbol.owner
      // Inline expansion and export forwarding hide or invent references, and setters are written through their fields.
      val excluded = Flags.Synthetic | Flags.Given | Flags.Implicit | Flags.Private | Flags.Module | Flags.Param |
        Flags.Inline | Flags.Exported
      // Term-only flags make a flag set term-only, so the flags type aliases can carry are tested on their own.
      val aliases = Flags.Synthetic | Flags.Private | Flags.Exported
      // Parameters of a case class are its fields, which patterns read without naming them, unless the class is
      // derived from by generic code that reads them all.
      val field = symbol.is(Flags.ParamAccessor) && owner.is(Flags.CaseClass) && !derived(owner)
      if (
        owner.isClass && !owner.isAnonymousClass && !owner.owner.isTerm && !symbol.isOneOf(excluded) &&
        (field || !symbol.is(Flags.ParamAccessor)) &&
        !symbol.isOneOf(aliases) &&
        !symbol.isConstructor && !symbol.isSetter && !symbol.name.is(DefaultGetterName) &&
        !symbol.isAllOf(Flags.EnumValue) &&
        !ctx.platform.isMainMethod(symbol) && !exported(symbol)
      )
        key(symbol).flatMap(nodes.get).foreach { info =>
          val public = !field && !symbol.is(Flags.Protected) && !symbol.privateWithin.exists
          members(symbol) =
            Definition(
              memberName(symbol),
              info.name,
              symbol.is(Flags.Deferred),
              public,
              symbol.is(Flags.Mutable),
              info,
              suppressions(symbol),
              field
            )
        }
    }

    // Overloads, and a method named like a type beside it, are told apart by their parameter types.
    private def memberName(symbol: Symbol)(using Context): String = {
      val decls  = symbol.owner.info.decls
      val shared = symbol.is(Flags.Method) &&
        (decls.lookupAll(symbol.name).exists(_ != symbol) || decls.lookup(symbol.name.toTypeName).exists)
      val parameters =
        if (shared) symbol.info.paramInfoss.map(_.map(_.show).mkString("(", ", ", ")")).mkString else ""
      qualifiedName(symbol) + parameters
    }

    // Generic derivation of an instance reads every field of a class, which no use shows; `CanEqual` reads none.
    private def derived(cls: Symbol)(using Context): Boolean =
      cls.companionModule.exists && cls.companionModule.moduleClass.info.decls.toList.exists { member =>
        val name = member.name.toString
        name.startsWith("derived$") && name != "derived$CanEqual"
      }

    private def wildcard(pattern: tpd.Tree): Boolean = pattern match {
      case tpd.Ident(nme.WILDCARD) => true
      case _                       => false
    }

    /** Records the fields of a case class that a pattern on it binds or tests, which read them. */
    def matched(unapply: tpd.UnApply, from: Option[String])(using Context): Unit = {
      val extractor = unapply.fun.symbol
      val cls       = extractor.owner.companionClass
      if (cls.is(Flags.CaseClass) && extractor.is(Flags.Synthetic)) {
        val fields = cls.primaryConstructor.paramSymss.find(_.headOption.exists(_.isTerm)).getOrElse(Nil)
        // A field named like a method of the class, as `copy` is, stands beside it.
        val accessors = cls.info.decls.toList.filter(_.is(Flags.ParamAccessor))
        fields.zip(unapply.patterns).foreach((field, pattern) =>
          if (!wildcard(pattern)) accessors.filter(_.name == field.name).foreach(use(_, from, inside = false))
        )
      }
    }

    // Printing, interpolating, appending or putting a value in a message calls its toString, which a case class writes
    // from every field.
    private val Shows = Set(
      "scala.Predef.println",
      "scala.Predef.print",
      "scala.Predef.assert",
      "scala.Predef.assume",
      "scala.Predef.require",
      "scala.Console.println",
      "scala.Console.print",
      "java.io.PrintStream.println",
      "java.io.PrintStream.print",
      "java.lang.String.valueOf",
      "java.lang.String.+",
      "java.lang.StringBuilder.append",
      "java.lang.StringBuffer.append",
      "scala.collection.mutable.StringBuilder.append",
      "scala.collection.mutable.StringBuilder.++=",
      "scala.StringContext.s",
      "scala.StringContext.f",
      "scala.StringContext.raw"
    )

    // These show the value they are called on, or the elements of the collection,.
    private val Renders = Set(
      "scala.collection.IterableOnceOps.mkString",
      "scala.Predef.StringFormat.formatted",
      "scala.Product.productIterator"
    )

    // Equality and hashing read every field of a case class.
    private val Compares = Set(
      "scala.Any.==",
      "scala.Any.!=",
      "scala.Any.equals",
      "scala.Any.hashCode",
      "java.lang.Object.==",
      "java.lang.Object.!=",
      "java.lang.Object.equals",
      "java.lang.Object.hashCode"
    )

    /** Records the fields a call reads by showing, comparing or hashing a value: its toString, or a printing or
      * interpolating method, or `==`.
      */
    def shown(application: tpd.Apply, from: Option[String])(using Context): Unit = {
      val callee = application.fun.symbol
      val named  = if (callee.exists) qualifiedName(callee) else ""
      val values = application.fun match {
        case tpd.Select(receiver, name) if name.toString == "toString" && application.args.isEmpty => List(receiver)
        case tpd.Select(receiver, _) if Compares(named)                                            =>
          receiver :: application.args
        case _ if Shows(named) =>
          application.args.flatMap(argument =>
            Predicates.unwrap(argument) match {
              case tpd.SeqLiteral(elements, _) => elements
              case other                       => List(other)
            }
          )
        case _ => Nil
      }
      read(values.map(_.tpe), from)
    }

    /** Records the fields a method called on a value reads by showing it or its elements, or by hashing the elements.
      */
    def rendered(select: tpd.Select, from: Option[String])(using Context): Unit = {
      val callee = select.symbol
      val name   = select.name.toString
      if (
        callee.exists &&
        (name match {
          case "mkString" | "formatted" | "productIterator" => Renders(qualifiedName(callee))
          case "distinct" => select.qualifier.tpe.widen.derivesFrom(requiredClass("scala.collection.Iterable"))
          case _          => false
        })
      ) read(List(select.qualifier.tpe), from)
    }

    /** Records the fields of the keys and elements of the hashed maps and sets a type names, which hashing reads. */
    def keyed(tpe: Type, from: Option[String])(using Context): Unit =
      KeyTypes(key => read(List(key), from)).traverse(tpe)

    private class KeyTypes(found: Type => Unit)(using Context) extends TypeTraverser {
      def traverse(tpe: Type): Unit = {
        tpe match {
          case AppliedType(constructor, key :: _) if hashed(constructor.classSymbol) => found(key)
          case _                                                                     => ()
        }
        traverseChildren(tpe)
      }
    }

    private def hashed(cls: Symbol)(using Context): Boolean = cls.isClass && !sorted(cls) &&
      (cls.derivesFrom(requiredClass("scala.collection.Map")) ||
        cls.derivesFrom(requiredClass("scala.collection.Set")) ||
        cls.derivesFrom(requiredClass("java.util.Map")) || cls.derivesFrom(requiredClass("java.util.Set")))

    private def sorted(cls: Symbol)(using Context): Boolean =
      cls.derivesFrom(requiredClass("scala.collection.SortedMap")) ||
        cls.derivesFrom(requiredClass("scala.collection.SortedSet")) ||
        cls.derivesFrom(requiredClass("java.util.SortedMap")) || cls.derivesFrom(requiredClass("java.util.SortedSet"))

    // A shown container shows its elements, whose types its type arguments name.
    private def read(types: List[Type], from: Option[String])(using Context): Unit = {
      val seen                      = mutable.Set.empty[Symbol]
      def showType(tpe: Type): Unit = {
        val widened = tpe.widen
        show(widened.classSymbol)
        widened.argInfos.foreach(showType(_))
      }
      def show(cls: Symbol): Unit = if (cls.isClass && seen.add(cls)) {
        if (cls.is(Flags.Sealed)) cls.sealedDescendants.foreach(show(_))
        if (cls.is(Flags.CaseClass) && !derived(cls))
          cls.info.decls.toList.filter(_.is(Flags.ParamAccessor)).foreach { field =>
            use(field, from, inside = false)
            showType(field.info.finalResultType)
          }
      }
      types.foreach(showType(_))
    }

    def assign(symbol: Symbol): Unit = assigned += symbol: Unit

    /** Records a visible class or object declared inside another build type. */
    def enclosed(symbol: Symbol)(using Context): Unit = {
      val hidden = Flags.Private | Flags.Protected | Flags.Synthetic
      if (
        symbol.isClass && symbol.owner.isClass && !symbol.owner.is(Flags.Package) && !symbol.isAnonymousClass &&
        !symbol.isOneOf(hidden) && !(symbol.is(Flags.ModuleClass) && symbol.sourceModule.isOneOf(hidden)) &&
        !symbol.isAllOf(Flags.EnumCase)
      ) key(symbol).foreach(nested += _)
    }

    /** Records the build types named by the signature of a visible member, other than those enclosing it. */
    def expose(symbol: Symbol)(using Context): Unit =
      if (symbol.owner.isClass && !symbol.isOneOf(Flags.Private | Flags.Synthetic | Flags.Param))
        key(symbol).foreach { from =>
          val signature = if (symbol.isClass) symbol.asClass.parentTypes else List(symbol.info)
          val named     = TypeReferences(target =>
            key(target).filterNot(Node.within(nodes, from, _)).foreach(exposed += _)
          )
          signature.foreach(named.traverse(_))
        }

    /** Records a named given that code in other files can refer to; private givens are left to scalafix. */
    def instance(symbol: Symbol)(using Context): Unit =
      if (
        symbol.isTerm && symbol.is(Flags.Given) && symbol.owner.isClass &&
        !symbol.isOneOf(Flags.Private | Flags.Synthetic | Flags.Deferred) &&
        !invented(symbol.name.toString)
      ) key(symbol).flatMap(nodes.get).foreach(info => givens(symbol) = qualifiedName(symbol) -> info)

    // Anonymous givens are named `given_T` and derived ones `derived$T`.
    private def invented(name: String): Boolean = name.startsWith("given_") || name.contains('$')

    /** Records a reference to a given by name; inferred arguments carry synthetic spans. */
    def mention(tree: tpd.Tree)(using Context): Unit = tree match {
      case tpd.Ident(_) | tpd.Select(_, _)
          if tree.symbol.is(Flags.Given) && tree.span.exists && !tree.span.isSynthetic =>
        named += tree.symbol: Unit
      case _ => ()
    }

    def mention(symbols: List[Symbol])(using Context): Unit = named ++= symbols.filter(_.is(Flags.Given)): Unit

    // The compiler names an anonymous given after its type, so a second given of that type in the owner would collide.
    private def clashes(symbol: Symbol)(using Context): Boolean =
      symbol.owner.info.decls.toList.exists(other =>
        other != symbol && other.isTerm && other.is(Flags.Given) &&
          other.info.finalResultType =:= symbol.info.finalResultType
      )

    /** Records a reference to a member from `from`, unless it comes from inside the member itself. */
    def asType(symbol: Symbol): Unit = asTypes += symbol: Unit

    /** Counts the members and types that strings name by their full qualified names as used from the node holding the
      * string. A type so named is a reflective root, and the members it declares are exempt. Rendered code calls such a
      * member by name, so it is neither unused nor movable.
      */
    private def qualifiedUses()(using Context): Unit = {
      val byName = classes.toVector.groupBy(qualifiedName(_))
      literals.foreach { (_, path, owner, member, info) =>
        if (path.nonEmpty)
          byName.getOrElse(path + owner, Vector.empty).foreach { cls =>
            val names = Vector(termName(member), typeName(member))
            names.flatMap(name =>
              cls.info.member(name).alternatives.collect {
                case alternative if alternative.symbol.exists => alternative.symbol
              }
            ).foreach { symbol =>
              use(symbol, Some(info.name), inside = false)
              refer(Some(info.name), symbol)
              members.remove(symbol): Unit
            }
          }
      }
      typeNames.foreach { (text, info) =>
        byName.getOrElse(text, Vector.empty).foreach(cls =>
          key(cls).foreach { name =>
            refer(Some(info.name), cls)
            nodes.keys.filter(candidate => candidate == name || candidate.startsWith(name + ".")).toVector.foreach {
              named =>
                reflected += named: Unit
                nodes.updateWith(named)(_.map(_.copy(exported = true))): Unit
            }
          }
        )
      }
      members.filterInPlace((symbol, _) => !exported(symbol)): Unit
    }

    def use(symbol: Symbol, from: Option[String], inside: Boolean)(using Context): Unit =
      if (!inside && symbol.exists && !symbol.isClass && symbol.owner.isClass && key(symbol).isDefined)
        from.foreach(node => users.getOrElseUpdate(symbol, mutable.Set.empty) += node: Unit)

    private def defaulted(method: Symbol)(using Context): Boolean =
      method.paramSymss.flatten.exists(_.is(Flags.HasDefault))

    // A named argument reaches the call through a binding whose value may itself be the default getter, and a quote
    // reaches a getter it cannot access through an inline accessor named after it.
    private def omitted(argument: tpd.Tree)(using Context): Boolean = {
      val symbol = Predicates.unwrap(argument).symbol
      @annotation.tailrec
      def getter(name: TermName): Boolean = name.is(DefaultGetterName) ||
        (name match {
          case derived: DerivedName => getter(derived.underlying)
          case _                    => false
        })
      symbol.exists && (getter(symbol.name.toTermName) || bindings.get(symbol).exists(omitted(_)))
    }

    // A case class `apply` shares the constructor's default getters in the companion.
    private def defaultGetter(method: Symbol, index: Int)(using Context): Symbol = {
      val owner = if (method.isConstructor) method.owner.companionModule.moduleClass else method.owner
      val own   = owner.info.decl(DefaultGetterName(method.name.asTermName, index)).symbol
      if (own.exists || !method.is(Flags.Synthetic)) own
      else owner.info.decl(DefaultGetterName(nme.CONSTRUCTOR, index)).symbol
    }

    /** The case class a primary constructor, synthetic `apply` or `copy` builds. */
    private def constructs(method: Symbol)(using Context): Symbol =
      if (method.isPrimaryConstructor || method.is(Flags.Synthetic) && method.name == nme.copy) method.owner
      else if (method.is(Flags.Synthetic) && method.name == nme.apply && method.owner.is(Flags.ModuleClass))
        method.owner.companionClass
      else NoSymbol

    /** The rendered result type of a build method whose callers might use it; fluent and `Unit` results are skipped. */
    private def result(method: Symbol)(using Context): Option[String] =
      if (!method.exists || !method.is(Flags.Method)) None
      else
        results.getOrElseUpdate(
          method, {
            val tpe     = method.info.finalResultType.widen
            val trivial = tpe.isRef(defn.UnitClass) || tpe.isRef(defn.NothingClass) || tpe.isRef(defn.NullClass)
            val fluent  = method.owner.isClass && tpe.classSymbol.exists && tpe.classSymbol.derivesFrom(method.owner)
            // an effect of Unit, as in a tagless-final F[Unit], has no value for a caller to use
            val effect = tpe match {
              case applied: AppliedType =>
                applied.args.sizeIs == 1 && applied.args.head.isRef(defn.UnitClass) &&
                (applied.tycon.isInstanceOf[TypeParamRef] || applied.tycon.typeSymbol.is(Flags.TypeParam))
              case _ => false
            }
            Option.when(output(method).isDefined && !trivial && !fluent && !effect)(tpe.show)
          }
        )

    private def methodName(method: Symbol, fields: Vector[Algebra.Field])(using Context): String =
      s"${qualifiedName(method)}(${fields.map(_.name).mkString(", ")})"

    private def fieldsOf(symbol: Symbol)(using Context): Vector[Algebra.Field] =
      parameters.getOrElseUpdate(symbol, if (symbol.isClass) Shapes.fields(symbol) else Shapes.parameters(symbol))

    // Named and default arguments reach the call through synthetic local bindings and default getters.
    private def classify(tree: tpd.Tree, depth: Int = 4)(using Context): Algebra.Argument =
      Predicates.argument(tree).getOrElse {
        val symbol = Predicates.unwrap(tree).symbol
        bindings.get(symbol).orElse(defaults.get(symbol)) match {
          case Some(value) if depth > 0 => classify(value, depth - 1)
          case _                        => Algebra.Argument.Unknown
        }
      }

    // The tree a named argument's binding or a default getter stands for.
    private def resolved(tree: tpd.Tree, depth: Int = 4)(using Context): tpd.Tree =
      bindings.get(Predicates.unwrap(tree).symbol).orElse(defaults.get(Predicates.unwrap(tree).symbol))
        .filter(_ => depth > 0).fold(tree)(resolved(_, depth - 1))

    /** The value two arguments are both known to have, shown; an option is known by its contents. */
    private def same(left: tpd.Tree, right: tpd.Tree)(using Context): Option[String] = {
      val (first, second)          = (resolved(left), resolved(right))
      def contents(tree: tpd.Tree) = Predicates.unwrap(tree) match {
        case tpd.Apply(_, List(inner)) => Some(inner)
        case _                         => None
      }
      (classify(first), classify(second)) match {
        case (Algebra.Argument.Present, Algebra.Argument.Present) =>
          contents(first).zip(contents(second)).flatMap(same(_, _)).map(inner => s"Some($inner)")
        case (value, other) => Option.when(value != Algebra.Argument.Unknown && value == other)(value.shown)
      }
    }

    def refer(from: Option[String], symbol: Symbol)(using Context): Unit =
      for {
        source <- from
        target <- key(symbol) if source != target
      } edge(source, target)

    def link(from: Option[String], targets: Iterable[String]): Unit =
      from.foreach(source => targets.filter(_ != source).foreach(edge(source, _)))

    private def edge(source: String, target: String): Unit = edges.getOrElseUpdate(source, mutable.Set.empty) += target

    /** The node key of any symbol, memoised because every tree and type of the build asks for one. */
    def key(symbol: Symbol)(using Context): Option[String] = keys.getOrElseUpdate(symbol, register(node(symbol)))

    /** Whether other builds can use `symbol` as part of the public API of a library. */
    private def exported(symbol: Symbol)(using Context): Boolean =
      key(symbol).flatMap(nodes.get).exists(node =>
        node.exported && !symbol.is(Flags.Private) && (reflected(node.name) || !symbol.privateWithin.exists)
      )

    /** The rules suppressed on a definition or, for a type, on its companion: by
      * `@SuppressWarnings("bonsai[:<rule>]")`, by `@nowarn`, bare or filtering on `msg=bonsai[:<rule>]` as for the
      * compiler plugin's warnings, and by `@unused` for the liveness rules. The compiler reports a `@nowarn` that names
      * only analysis rules as unused, as it suppresses none of its warnings.
      */
    def suppressions(symbol: Symbol)(using Context): Set[String] = {
      // A field's annotation is written on its constructor parameter, which keeps it there.
      val annotated =
        if (symbol.is(Flags.ParamAccessor))
          Set(symbol) ++ symbol.owner.primaryConstructor.paramSymss.flatten.find(_.name == symbol.name)
        else if (!symbol.isClass) Set(symbol)
        else
          Set(symbol, symbol.companionClass, symbol.companionModule, symbol.companionModule.moduleClass) ++
            Option.when(symbol.is(Flags.ModuleClass))(symbol.sourceModule)
      val Suppress                        = requiredClass("java.lang.SuppressWarnings")
      val Nowarn                          = requiredClass("scala.annotation.nowarn")
      val Unused                          = requiredClass("scala.annotation.unused")
      def strings(annotation: Annotation) = annotation.tree.filterSubTrees {
        case tpd.Literal(constant) => constant.tag == Constants.StringTag
        case _                     => false
      }.collect { case tpd.Literal(constant) => constant.stringValue }
      annotated.filter(_.exists).flatMap(_.annotations).flatMap { annotation =>
        annotation.symbol match {
          case Suppress => strings(annotation).flatMap(rules(_))
          case Nowarn   =>
            val filters = strings(annotation)
            if (filters.forall(_.isEmpty)) List("*")
            else
              filters.flatMap(_.split('&').flatMap {
                case s"msg=$pattern" => rules(pattern)
                case _               => None
              })
          case Unused => Liveness.Rules
          case _      => Nil
        }
      }
    }

    private def rules(id: String): Set[String] = net.kurobako.bonsai.java.JavaLint.resolve(id)

    /** The key of an analysed node, recording the node on first sight. */
    private def register(symbol: Symbol)(using Context): Option[String] =
      if (!symbol.exists) None
      else registered.get(symbol) match {
        case Some(name) => name
        case None       =>
          val name = output(symbol).map { output =>
            val name   = qualifiedName(symbol)
            val source = sourcePath(symbol)
            val outer  = register(node(symbol.owner))
            val line   = if (symbol.source.exists && symbol.span.exists) symbol.sourcePos.line + 1 else 0
            // Other builds can name a public type of a library, unless its enclosing type hides it. Code the build
            // only names in strings, compiled where the names are visible, can name a reflective type and the types
            // inside it whatever their qualifier.
            val reflected = reflective.exists(_.matches(s"$name.")) || outer.exists(this.reflected)
            if (reflected) this.reflected += name
            val exported = reflected || libraries(output.module) && output.scope == Output.Scope.Main &&
              (symbol :: Option.when(symbol.is(Flags.ModuleClass))(symbol.sourceModule).toList)
                .forall(part => !part.is(Flags.Private) && !part.privateWithin.exists) &&
              outer.forall(nodes.get(_).exists(_.exported))
            val entry =
              Node(
                name,
                output.module,
                output.scope,
                source,
                line,
                generated.exists(_.matches(Path.of(source))),
                exported,
                outer,
                suppressions(symbol)
              )
            // Inline expansion summons givens after pickling, so a given lives whenever its owner does.
            val instance = symbol.is(Flags.Given) || symbol.is(Flags.ModuleClass) && symbol.sourceModule.is(Flags.Given)
            if (instance) outer.foreach(edge(_, name))
            nodes.getOrElseUpdate(name, entry): Unit
            name
          }
          registered(symbol) = name
          name
      }

    @annotation.tailrec
    private def node(symbol: Symbol)(using Context): Symbol =
      if (!symbol.exists || symbol.is(Flags.Package) || symbol.isTopLevelDefinitionsObject) NoSymbol
      else if (symbol.owner.isTopLevelDefinitionsObject)
        if (symbol.is(Flags.Synthetic) || symbol.isConstructor) NoSymbol else symbol
      else if (symbol.is(Flags.Module) && symbol.isTerm) node(symbol.moduleClass)
      else if (symbol.isClass && !symbol.isAnonymousClass && !symbol.isRefinementClass) {
        val companion = symbol.companionClass
        val canonical = if (symbol.is(Flags.ModuleClass) && companion.exists) companion else symbol
        if (canonical.isAllOf(Flags.EnumCase)) node(canonical.owner) else canonical
      } else node(symbol.owner)

    private def output(symbol: Symbol)(using Context): Option[Output] =
      Option[AbstractFile](symbol.associatedFile).flatMap { file =>
        val path = Path.of(file.path).toAbsolutePath.normalize
        roots.collectFirst { case (output, root) if path.startsWith(root) => output }
      }

    private def sourcePath(symbol: Symbol)(using Context): String =
      if (symbol.source.exists) symbol.source.path
      else Option(symbol.compilationUnitInfo).flatMap(_.tastyInfo).flatMap(_.attributes.sourceFile).getOrElse("")

    private def qualifiedName(symbol: Symbol)(using Context): String =
      if (symbol.is(Flags.Package)) symbol.fullName.toString
      else if (symbol.isTopLevelDefinitionsObject) qualifiedName(symbol.owner)
      else {
        val prefix = qualifiedName(symbol.owner)
        val name   = symbol.effectiveName.toString
        if (prefix == "<empty>") name else s"$prefix.$name"
      }
  }

  private class References(collector: Collector, fragments: Fragments)(using Context) extends tpd.TreeTraverser {
    private val owners  = mutable.Stack.empty[Option[String]]
    private val methods = mutable.Stack.empty[Symbol]
    // Whether the tree being traversed runs once per iteration of a loop, and the extent of the innermost loop.
    private val looping = mutable.Stack(false)
    private val loops   = mutable.Stack(Span(0))
    // Synthetic methods such as case class `apply` and `copy` forward their parameters, which says nothing about callers.
    private val synthetic = mutable.Stack(false)
    // Inline expansion happens after pickling, so the givens it summons are only visible through their imports.
    private val imports = mutable.Stack(mutable.Set.empty[String])
    private val parents = mutable.Set.empty[tpd.Tree]
    private var parent  = false
    private val types   = TypeReferences { symbol =>
      collector.refer(owner, symbol)
      read(symbol)
      if (!parent) collector.asType(symbol)
    }

    // The methods the compiler writes for a case class read every field.
    private def read(symbol: Symbol): Unit =
      if (!(synthetic.top && symbol.is(Flags.ParamAccessor))) collector.use(symbol, owner, methods.contains(symbol))

    private def owner: Option[String] = owners.headOption.flatten

    def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
      case imported: tpd.Import =>
        collector.mention(importedSymbols(imported))
        val targets = importedSymbols(imported).flatMap(collector.key(_))
        imports.top ++= targets: Unit
        collector.link(owner, targets)
      case definition: tpd.MemberDef if definition.symbol.exists =>
        val symbol    = definition.symbol
        val enclosing = owner
        val node      = collector.key(symbol).orElse(enclosing)
        if (symbol.isClass && ctx.platform.hasMainMethod(symbol) || symbol.hasAnnotation(defn.MainAnnot))
          node.foreach(collector.root(_))
        if (node != enclosing) imports.foreach(collector.link(node, _))
        collector.instance(symbol)
        collector.enclosed(symbol)
        collector.expose(symbol)
        definition match {
          case typed @ tpd.TypeDef(_, template: tpd.Template) if symbol.isClass =>
            collector.define(symbol, template)
            collector.structure(symbol, typed, template)
          case value: tpd.ValDef if symbol.is(Flags.Synthetic) && !symbol.owner.isClass =>
            collector.bind(symbol, value.rhs)
          case method: tpd.DefDef if symbol.name.is(DefaultGetterName) => collector.default(symbol, method.rhs)
          case tpd.TypeDef(_, _) if symbol.isAliasType && !symbol.is(Flags.Opaque) =>
            collector.member(symbol)
            collector.alias(symbol)
          case method: tpd.DefDef if !symbol.is(Flags.Synthetic) && !symbol.isConstructor =>
            collector.body(symbol, method)
            collector.precondition(symbol, method.rhs)
            collector.signature(symbol, method.rhs)
            collector.design(symbol, method.rhs)
            collector.group(symbol, method)
            collector.member(symbol)
            if (symbol.owner.isClass) body(symbol, method.rhs, node)
          case value: tpd.ValDef if symbol.owner.isClass && !symbol.is(Flags.Synthetic) =>
            body(symbol, value.rhs, node)
            collector.group(symbol, value)
            collector.initialiser(symbol, value)
            collector.member(symbol)
          // A pattern definition keeps its authored right-hand side in a synthetic val.
          case value: tpd.ValDef if symbol.owner.isClass && value.rhs.span.exists && !value.rhs.span.isSynthetic =>
            body(symbol, value.rhs, node)
          case _ => ()
        }
        pushing(synthetic, synthetic.top || symbol.isAllOf(Flags.SyntheticMethod) && !symbol.isAnonymousFunction) {
          pushing(owners, node) {
            pushing(methods, symbol) {
              symbol.annotations.foreach(annotation => traverse(annotation.tree))
              traverseChildren(tree)
            }
          }
        }
      case block: tpd.Block =>
        // Local definitions are statements too, but discard nothing.
        if (!synthetic.top) block.stats.filterNot(_.isDef).foreach(collector.discard(_))
        pushing(imports, mutable.Set.empty[String])(traverseChildren(tree))
      case template: tpd.Template =>
        parents ++= template.parents
        pushing(imports, mutable.Set.empty[String])(traverseChildren(tree))
      case tpd.PackageDef(_, _) =>
        pushing(imports, mutable.Set.empty[String])(traverseChildren(tree))
      case _ =>
        tree match {
          // A call the code inverts with `!` is read as the opposite of what it gives, which every caller inverting says
          // the method itself is named as. A nullary `unary_!` is a select, not an application.
          case select: tpd.Select if select.name == nme.UNARY_! =>
            select.qualifier match {
              case argument: tpd.Apply => collector.negate(argument)
              case _                   => ()
            }
          case tpd.Apply(tpd.Select(argument: tpd.Apply, name), Nil) if name == nme.UNARY_! =>
            collector.negate(argument)
          case application: tpd.Apply if !synthetic.top =>
            collector.record(application, owner, methods.contains(_))
            collector.shown(application, owner)
          case assignment: tpd.Assign                   => collector.assign(assignment.lhs.symbol)
          case unapply: tpd.UnApply                     => collector.matched(unapply, owner)
          case generic: tpd.TypeApply if !synthetic.top =>
            collector.instantiate(generic.fun.symbol, generic.args.map(_.tpe), clause(generic.fun), owner)
          case closure: tpd.Closure if !closure.tpt.isEmpty =>
            owner.flatMap(collector.described(_)).foreach(node =>
              collector.implement(closure.tpt.tpe.classSymbol, s"a lambda in ${node.name}", node.module)
            )
          // An inline body runs as the code of each expansion, where its receivers have their own types, and a receiver
          // the loop does not change dispatches to one class throughout it.
          case select: tpd.Select
              if looping.top && !synthetic.top && !methods.exists(_.is(Flags.Inline)) && !settled(select.qualifier) =>
            collector.dispatch(
              select.symbol,
              methods.find(method => method.is(Flags.Method) && method.owner.isClass),
              owner,
              select.qualifier.tpe
            )
          case _ => ()
        }
        collector.refer(owner, tree.symbol)
        read(tree.symbol)
        collector.mention(tree)
        collector.observe(tree, owner)
        collector.keyed(tree.tpe, owner)
        tree match {
          case select: tpd.Select => collector.rendered(select, owner)
          case _                  => ()
        }
        parent = parents.contains(tree)
        types.traverse(tree.tpe)
        parent = false
        children(tree)
    }

    /** The type clauses already applied in `function`, which is the index of the clause its application fills. */
    private def clause(function: tpd.Tree): Int = function match {
      case tpd.Apply(inner, _)     => clause(inner)
      case tpd.TypeApply(inner, _) => clause(inner) + 1
      case _                       => 0
    }

    // While bodies and the function arguments of collection traversals run once per iteration.
    private def children(tree: tpd.Tree): Unit = tree match {
      case tpd.WhileDo(condition, body) =>
        pushing(loops, tree.span) {
          pushing(looping, value = true) {
            traverse(condition)
            traverse(body)
          }
        }
      case tpd.Apply(function, arguments) if Traversals.traversal(function) =>
        traverse(function)
        arguments.foreach { argument =>
          val lambda = Traversals.lambda(argument).isDefined
          pushing(loops, if (lambda) argument.span else loops.top) {
            pushing(looping, looping.top || lambda)(traverse(argument))
          }
        }
      case _ => traverseChildren(tree)
    }

    /** A receiver that is the same on every iteration of the innermost loop: this, or a value defined outside the loop
      * that nothing reassigns, or a stable path from one.
      */
    private def settled(receiver: tpd.Tree)(using Context): Boolean = Predicates.unwrap(receiver) match {
      case tpd.This(_)          => true
      case reference: tpd.Ident =>
        val symbol = reference.symbol
        symbol.isTerm && !symbol.isOneOf(Flags.Mutable | Flags.Method) && symbol.span.exists &&
        !loops.top.contains(symbol.span)
      case select: tpd.Select => select.symbol.isStableMember && !select.symbol.is(Flags.Mutable) &&
        settled(select.qualifier)
      case _ => false
    }

    private def body(symbol: Symbol, tree: tpd.Tree, node: Option[String])(using Context): Unit =
      node.flatMap(collector.described(_)).foreach(info =>
        if (!tree.isEmpty && !collector.suppressions(symbol).exists(Set("*", Finding.Rule.DuplicateCode.id)))
          fragments.add(tree, info.name, info.test, symbol.is(Flags.Method), family(symbol), specialising(symbol, tree))
      )

    /** The overloads, or methods named alike but for a width, that a method belongs to: those that take parameters of
      * different types, one per width, as numbered-family asks for.
      */
    private def family(symbol: Symbol)(using Context): String = {
      def unsized(name: String) = Width.replaceAllIn(name, "#")
      val owner                 = symbol.owner
      val name                  = unsized(symbol.name.toString)
      val alike                 = owner.info.decls.toList.filter(member =>
        member.is(Flags.Method) && unsized(member.name.toString) == name
      )
      if (symbol.is(Flags.Method) && owner.isClass && alike.map(_.info.firstParamTypes.map(_.show)).distinct.size > 1)
        s"${owner.fullName}.$name"
      else ""
    }

    // An inline method that expands inline methods on literals specialises them, as staged code does.
    private def specialising(symbol: Symbol, body: tpd.Tree)(using Context): Boolean =
      symbol.is(Flags.Inline) && body.existsSubTree {
        case application: tpd.Apply =>
          application.fun.symbol.is(Flags.Inline) && application.args.exists(argument =>
            Predicates.unwrap(argument) match {
              case tpd.Literal(_) => true
              case _              => false
            }
          )
        case _ => false
      }

    private def importedSymbols(imported: tpd.Import): List[Symbol] =
      imported.expr.symbol ::
        (for {
          selector <- imported.selectors if !selector.isWildcard
          name     <- List(selector.name.toTermName, selector.name.toTypeName)
          member   <- imported.expr.tpe.member(name).alternatives
        } yield member.symbol)

    private def pushing[A](stack: mutable.Stack[A], value: A)(body: => Unit): Unit = {
      stack.push(value)
      try body
      finally stack.pop(): Unit
    }
  }

  private class TypeReferences(refer: Symbol => Unit)(using Context) extends TypeTraverser {
    def traverse(tpe: Type): Unit = {
      refer(tpe.typeSymbol)
      refer(tpe.termSymbol)
      tpe match {
        case named: NamedType => traverse(named.prefix)
        case _                => traverseChildren(tpe)
      }
    }
  }
}
