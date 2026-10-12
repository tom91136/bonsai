package net.kurobako.baumkuchen.analysis

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
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.AppliedType
import dotty.tools.dotc.core.Types.NamedType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeTraverser
import dotty.tools.dotc.core.tasty.TastyUnpickler
import dotty.tools.dotc.fromtasty.ReadTasty
import dotty.tools.dotc.fromtasty.TASTYCompiler
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.io.AbstractFile
import java.io.File
import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.PathMatcher
import scala.collection.mutable
import scala.concurrent.Promise
import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.util.Using
import scala.util.control.NonFatal

/** Builds the reference graph and algebraic model of a whole build from the TASTy in its outputs. */
object Extraction {

  /** The extracted model, with classes that could not be read or analysed listed as problems. */
  case class Result(
      graph: Graph,
      algebra: Algebra.Model,
      smells: Smells.Model,
      members: Members.Model,
      clones: Vector[Clones.Group],
      bytecode: Vector[Bytecode.Method],
      complexities: Vector[Complexity.Summary],
      pipelines: Vector[Pipelines.Chain],
      megamorphic: Vector[Megamorphic.Site],
      purity: Purity.Model,
      problems: Vector[String]
  )

  val DefaultGenerated: Vector[String] = Vector("glob:**/src/generated/**", "glob:src/generated/**")

  // The TASTy unpickler recurses over long import selector lists and deep trees.
  private val StackSize = 1L << 30

  // A hierarchy of one case is a single implementation, which another rule reports.
  private val MinimumCases = 2

  /** Extracts the build `manifest` describes; `concurrent` names methods that run the functions passed to them on other
    * threads, beside the standard ones.
    */
  def read(
      manifest: Manifest,
      generated: Vector[String] = DefaultGenerated,
      concurrent: Set[String] = Set.empty
  ): Either[Vector[String], Result] = {
    val result = Promise[Either[Vector[String], Result]]()
    val thread = Thread(
      Thread.currentThread.getThreadGroup,
      () => result.complete(Try(extract(manifest, generated, concurrent))): Unit,
      "baumkuchen-extraction",
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
      concurrent: Set[String]
  ): Either[Vector[String], Result] = {
    // The compiler aborts the whole run on a file whose names or sections it cannot index, so those are set aside.
    val (tasty, unreadable) =
      manifest.outputs.flatMap(output => files(output.path).filter(_.toString.endsWith(".tasty")))
        .partition(file => Try(TastyUnpickler(Files.readAllBytes(file))).isSuccess)
    val skipped =
      unreadable.map(file => s"class file ${file.getFileName.toString.stripSuffix(".tasty")} could not be read")
    val roots = manifest.outputs.flatMap(output => entryPoints(output.path)).toSet
    if (tasty.isEmpty && skipped.nonEmpty) Left(skipped)
    else if (tasty.isEmpty)
      Right(Result(
        Graph(Map.empty, Map.empty, roots),
        Algebra.Model(Vector.empty, Vector.empty, Map.empty, Vector.empty, Vector.empty),
        Smells.Model(Vector.empty, Map.empty, Vector.empty, Vector.empty, Vector.empty, Vector.empty),
        Members.Model.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Purity.Model.empty,
        Vector.empty
      ))
    else {
      val phase =
        Collector(
          manifest.outputs,
          manifest.libraries,
          generated.map(FileSystems.getDefault.getPathMatcher(_)),
          concurrent
        )
      val arguments = Array(
        "-from-tasty",
        "-Yretain-trees",
        "-color:never",
        // Findings name types in one line each, which the printer would otherwise wrap.
        "-pagewidth",
        Int.MaxValue.toString,
        "-classpath",
        evicted(manifest.classpath).mkString(File.pathSeparator)
      ) ++ tasty.map(_.toString)
      val reporter = Reader(phase).process(arguments, StoreReporter())
      val problems = skipped ++ phase.problems ++ reporter.allErrors.map(_.message).distinct
      val graph    = phase.graph(roots)
      if (graph.nodes.isEmpty && problems.nonEmpty) Left(problems)
      else Right(Result(
        graph,
        phase.algebra,
        phase.smells,
        phase.memberships,
        phase.duplicates,
        Bytecode.read(manifest.outputs.map(_.path)),
        phase.complexities,
        phase.pipelines,
        phase.megamorphic,
        phase.purity,
        problems
      ))
    }
  }

  private given Ordering[Vector[BigInt]] = Ordering.Implicits.seqOrdering[Vector, BigInt]

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

  private def files(directory: Path): Vector[Path] =
    if (!Files.isDirectory(directory)) Vector.empty
    else Using.resource(Files.walk(directory))(_.toList.asScala.toVector.filter(Files.isRegularFile(_)))

  // Classes instantiated reflectively by ServiceLoader, the compiler plugin loader or sbt plugin discovery.
  private def entryPoints(directory: Path): Vector[String] = {
    val services = files(directory.resolve("META-INF/services")).flatMap(Files.readAllLines(_).asScala)
      .map(_.takeWhile(_ != '#').trim)
    val plugins = Some(directory.resolve("plugin.properties")).filter(Files.isRegularFile(_)).toVector
      .flatMap(Files.readAllLines(_).asScala).collect {
        case line if line.startsWith("pluginClass=") => line.stripPrefix("pluginClass=").trim
      }
    val autoplugins = Some(directory.resolve("sbt/sbt.autoplugins")).filter(Files.isRegularFile(_)).toVector
      .flatMap(Files.readAllLines(_).asScala).map(_.trim)
    (services ++ plugins ++ autoplugins).filter(_.nonEmpty).map(_.stripSuffix("$").replace('$', '.'))
  }

  /** A construction or call kept until every precondition, binding and default argument of the build is known. */
  private case class Pending(
      callee: Symbol,
      method: Symbol,
      arguments: List[tpd.Tree],
      test: Boolean,
      generated: Boolean,
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
      suppressed: Set[String]
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
      concurrent: Set[String]
  ) extends Phase {
    private val roots           = outputs.map(output => output -> output.path.toAbsolutePath.normalize)
    private val nodes           = mutable.Map.empty[String, Node]
    private val edges           = mutable.Map.empty[String, mutable.Set[String]]
    private val mainRoots       = mutable.Set.empty[String]
    private val registered      = mutable.Map.empty[Symbol, Option[String]]
    private val keys            = mutable.Map.empty[Symbol, Option[String]]
    private val parameters      = mutable.Map.empty[Symbol, Vector[Algebra.Field]]
    private val tracked         = mutable.Map.empty[Symbol, Boolean]
    private val products        = mutable.Map.empty[Symbol, Algebra.Product]
    private val required        = mutable.Map.empty[Symbol, Algebra.Product]
    private val sums            = mutable.Map.empty[String, Algebra.Sum]
    private val pending         = mutable.ArrayBuffer.empty[Pending]
    private val sites           = mutable.Map.empty[String, Vector[Algebra.Site]]
    private val bindings        = mutable.Map.empty[Symbol, tpd.Tree]
    private val defaults        = mutable.Map.empty[Symbol, tpd.Tree]
    private val failures        = mutable.ArrayBuffer.empty[String]
    private val methods         = mutable.Map.empty[Symbol, Smells.Method]
    private val results         = mutable.Map.empty[Symbol, Option[String]]
    private val calls           = mutable.Map.empty[Symbol, (Int, Int)]
    private val discards        = mutable.Map.empty[Symbol, Int]
    private val abstracts       = mutable.Map.empty[Symbol, (String, String, String)]
    private val implements      = mutable.Map.empty[Symbol, mutable.Set[(String, String)]]
    private val generics        = mutable.Map.empty[Symbol, (String, String, Vector[String])]
    private val arguments       = mutable.Map.empty[(Symbol, Int), mutable.Map[String, Int]]
    private val testUses        = mutable.Map.empty[(Symbol, Int), Int]
    private val varied          = mutable.Set.empty[(Symbol, Int)]
    private val clones          = mutable.ArrayBuffer.empty[Clones.Group]
    private val members         = mutable.Map.empty[Symbol, Definition]
    private val users           = mutable.Map.empty[Symbol, mutable.Set[String]]
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
    private val costed          = mutable.ArrayBuffer.empty[Symbol]
    private val complexityList  = mutable.ArrayBuffer.empty[Complexity.Summary]
    private val pipelineList    = mutable.ArrayBuffer.empty[Pipelines.Chain]
    private val classes         = mutable.ArrayBuffer.empty[Symbol]
    private val templates       = mutable.Map.empty[Symbol, tpd.Template]
    private val reachable       = mutable.Map.empty[Symbol, Option[Vector[Symbol]]]
    private val purities        = mutable.ArrayBuffer.empty[Purity.Model]
    private val hierarchies     = mutable.ArrayBuffer.empty[(Symbol, String, String)]
    private val bodied          = mutable.Set.empty[Symbol]
    private val enumerations    = mutable.ArrayBuffer.empty[Algebra.Enumeration]
    private val dispatches      = mutable.Map.empty[(Symbol, Symbol), String]
    private val megamorphicList = mutable.ArrayBuffer.empty[Megamorphic.Site]

    def phaseName: String = "baumkuchen-collect"

    protected def run(using Context): Unit = ()

    override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
      val fragments = Fragments()
      units.foreach { unit =>
        try References(this, fragments).traverse(unit.tpdTree)
        catch { case NonFatal(error) => problem(s"${unit.source.path} could not be analysed: $error") }
      }
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
        call              <- pending if !call.generated && call.method.name != nme.copy && defaulted(call.method)
        (argument, index) <- call.arguments.zipWithIndex if !omitted(argument)
        default           <- defaults.get(defaultGetter(call.method, index))
        value             <- same(argument, default)
        parameter         <- fieldsOf(call.callee).lift(index)
      } explicit.getOrElseUpdate((call.callee, parameter.name, value), mutable.ArrayBuffer.empty) +=
        Algebra.Site(Vector.empty, call.test, call.own, call.location)
      pending.clear()
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
          definition.info.test,
          definition.info.generated,
          definition.suppressed
        )
      }
      val overridden = givens.keySet.flatMap(_.allOverriddenSymbols)
      givenList ++= givens.collect {
        case (symbol, (name, info)) if symbol.allOverriddenSymbols.isEmpty && !overridden(symbol) && !clashes(symbol) =>
          Members.Given(name, info.name, named(symbol), info.test, info.generated, suppressions(symbol))
      }
      val incoming = edges.toVector.flatMap((source, targets) => targets.map(_ -> source)).groupMap(_._1)(_._2)
      nestedList ++= nested.flatMap(inner =>
        nodes.get(inner).flatMap(_.outer).map(outer =>
          Members.Nested(inner, outer, incoming.getOrElse(inner, Vector.empty).toSet, exposed(inner))
        )
      )
      clones ++= fragments.groups()
      megamorphicList ++= dispatches.toVector.flatMap { case ((caller, callee), node) =>
        // Test doubles never reach a call in main code at run time.
        val test            = nodes.get(node).exists(_.test)
        val implementations = classes.toVector.flatMap { cls =>
          Option.when(
            !cls.isOneOf(Flags.Trait | Flags.Abstract) && cls.derivesFrom(callee.owner) &&
              (test || !key(cls).flatMap(nodes.get).exists(_.test))
          )(
            callee.denot.matchingMember(cls.thisType)
          ).filter(implementation => implementation.exists && !implementation.is(Flags.Deferred))
        }.distinct
        Option.when(implementations.size > MaximumInlinedReceivers)(Megamorphic.Site(
          qualifiedName(caller),
          node,
          s"${callee.owner.name.toString.stripSuffix("$")}.${callee.name}",
          shown(implementations.map(_.owner))
        ))
      }
      val costs = Costs(bodies)
      complexityList ++= costed.flatMap(method =>
        Option(costs.superLinear(method)).filter(_.nonEmpty).flatMap(terms =>
          key(method).map(Complexity.Summary(qualifiedName(method), _, terms))
        )
      )
      val chains = Chains()
      pipelineList ++= bodies.toVector.flatMap { (method, definition) =>
        for {
          node <- key(method)
          if method.owner.isClass && !method.is(Flags.Synthetic) && nodes.get(node).exists(!_.generated)
        } yield chains.of(qualifiedName(method), node, definition.rhs)
      }.flatten
      purities += purity()
      units
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
      methods.values.toVector,
      sites.toMap,
      calls.toVector.flatMap { case (symbol, (total, tests)) =>
        for {
          method <- methods.get(symbol)
          result <- results.getOrElse(symbol, None)
        } yield Smells.Usage(method, result, total, discards.getOrElse(symbol, 0), tests)
      },
      abstracts.toVector.map { case (symbol, (name, node, module)) =>
        Smells.Abstraction(name, node, module, implements.get(symbol).fold(Vector.empty)(_.toVector.sorted))
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

    def pipelines: Vector[Pipelines.Chain] = pipelineList.toVector

    def megamorphic: Vector[Megamorphic.Site] = megamorphicList.toVector

    def purity: Purity.Model = purities.lastOption.getOrElse(Purity.Model.empty)

    private def purity()(using Context): Purity.Model = {
      val effects = Purity.Inference(bodies, templates, implementations(_), concurrent)
      effects.run()
      def site(method: Symbol, tree: tpd.Tree) =
        key(method).filterNot(node => nodes.get(node).exists(_.generated)).map {
          node =>
            val position = tree.sourcePos
            node -> (if (position.exists) s"${position.source.path}:${position.line + 1}" else "unknown")
        }
      val authored = bodies.keys.filter(method =>
        method.owner.isClass && !method.is(Flags.Synthetic) && key(method).flatMap(nodes.get).exists(!_.generated)
      )
      Purity.Model(
        effects.pointless.flatMap((method, callee, tree) =>
          site(method, tree).map((node, location) => Purity.Discard(callee, node, location))
        ),
        effects.effectful.flatMap((method, operation, effect, tree, view) =>
          site(method, tree).map((node, location) => Purity.Transform(operation, effect, node, location, view))
        ),
        effects.shared.flatMap((method, variable, entry, tree) =>
          site(method, tree).map((node, location) => Purity.SharedWrite(variable, entry, node, location))
        ),
        authored.flatMap(method =>
          key(method).map(node =>
            qualifiedName(method) ->
              Purity.Method(node, effects.verdict(method), effects.observed(method), effects.role(method))
          )
        ).toMap,
        effects.library
      )
    }

    /** The bodies a call of `method` can run: its own, or every override in the build's main classes, which test
      * doubles never reach at run time; nothing when one of them lies outside the build.
      */
    private def implementations(method: Symbol)(using Context): Option[Vector[Symbol]] =
      reachable.getOrElseUpdate(
        method,
        if (bodies.contains(method) && (method.isEffectivelyFinal || !method.owner.isClass)) Some(Vector(method))
        else if (!classes.contains(method.owner)) None
        else {
          val overrides = classes.toVector.filter(cls =>
            !cls.isOneOf(Flags.Trait | Flags.Abstract) && cls.derivesFrom(method.owner) &&
              !key(cls).flatMap(nodes.get).exists(_.test)
          ).map(cls => method.denot.matchingMember(cls.thisType)).filter(found =>
            found.exists && !found.is(Flags.Deferred)
          )
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
    def dispatch(callee: Symbol, caller: Option[Symbol], node: Option[String])(using Context): Unit = {
      val universal = Set[Symbol](defn.AnyClass, defn.ObjectClass, defn.ProductClass, defn.SerializableClass)
      if (
        // A val is read through a getter, which dispatches like any other method.
        callee.isTerm && !callee.is(Flags.Module) && !callee.isConstructor && callee.owner.isClass &&
        !callee.isEffectivelyFinal && !callee.is(Flags.Private) && !universal(callee.owner) &&
        !callee.owner.derivesFrom(requiredClass("scala.Equals"))
      ) for {
        method <- caller
        key    <- node
      } dispatches((method, callee)) = key
    }

    /** Records a method body for cost bounds, reported for the methods of build classes. */
    def body(symbol: Symbol, definition: tpd.DefDef)(using Context): Unit =
      if (!symbol.isAnonymousFunction) {
        bodies(symbol) = definition
        if (symbol.owner.isClass && !symbol.is(Flags.Synthetic) && !symbol.isConstructor) costed += symbol
      }

    def memberships: Members.Model = Members.Model(memberList.toVector, givenList.toVector, nestedList.toVector)

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
          sums(name) = Algebra.Sum(name, key, variants.toVector)
          if (!cls.is(Flags.Enum) && !nodes.get(key).exists(_.generated)) hierarchies += ((cls, name, key))
        }
        nodes.get(key).foreach { info =>
          if (cls.is(Flags.Trait) || cls.is(Flags.Abstract)) {
            // Other builds can implement what a library exports, unless it is sealed.
            if (!cls.is(Flags.Enum) && !info.generated && !info.test && (cls.is(Flags.Sealed) || !exported(cls)))
              abstracts(cls) = (name, key, info.module)
          } else cls.asClass.baseClasses.drop(1).foreach(implement(_, name, info.module))
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
    def signature(method: Symbol)(using Context): Unit =
      key(method).flatMap(nodes.get).foreach { info =>
        val fields = fieldsOf(method)
        methods(method) = Smells.Method(
          methodName(method, fields),
          info.name,
          qualifiedName(method.owner),
          fields,
          method.allOverriddenSymbols.hasNext,
          exported(method),
          info.generated,
          info.test
        )
      }

    /** Counts a statement whose value is dropped, for methods whose results are tracked. */
    def discard(statement: tpd.Tree)(using Context): Unit = {
      // `val _ = value` is typed as `value match { case _ => () }`.
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
            val bare = argument.widenDealias
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
      val constructor = cls.primaryConstructor
      if (constructor.is(Flags.Private) || constructor.privateWithin.exists) Algebra.Access.Private
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
                  defaulted(method))
            )
          ) {
            val position  = application.sourcePos
            val location  = if (position.exists) s"${position.source.path}:${position.line + 1}" else "unknown"
            val own       = if (constructed.exists) owner.isDefined && owner == key(callee) else enclosing(callee)
            val generated = owner.flatMap(nodes.get).exists(_.generated)
            pending += Pending(callee, method, application.args, test, generated, own, location)
          }
      }

    /** Records a public or package-visible member of a build class, leaving out what the compiler writes and the API of
      * a library, which other builds use.
      */
    def member(symbol: Symbol)(using Context): Unit = {
      val owner = symbol.owner
      // Inline expansion and export forwarding hide or invent references, and setters are written through their fields.
      val excluded = Flags.Synthetic | Flags.ParamAccessor | Flags.Given | Flags.Implicit | Flags.Private |
        Flags.Module | Flags.Param | Flags.Inline | Flags.Exported
      // Term-only flags make a flag set term-only, so the flags type aliases can carry are tested on their own.
      val aliases = Flags.Synthetic | Flags.Private | Flags.Exported
      if (
        owner.isClass && !owner.isAnonymousClass && !owner.owner.isTerm && !symbol.isOneOf(excluded) &&
        !symbol.isOneOf(aliases) &&
        !symbol.isConstructor && !symbol.isSetter && !symbol.name.is(DefaultGetterName) &&
        !symbol.isAllOf(Flags.EnumValue) &&
        !ctx.platform.isMainMethod(symbol) && !exported(symbol)
      )
        key(symbol).flatMap(nodes.get).foreach { info =>
          val public = !symbol.is(Flags.Protected) && !symbol.privateWithin.exists
          val decls  = owner.info.decls
          // Overloads, and a method named like a type beside it, are told apart by their parameter types.
          val shared = symbol.is(Flags.Method) &&
            (decls.lookupAll(symbol.name).exists(_ != symbol) || decls.lookup(symbol.name.toTypeName).exists)
          val parameters =
            if (shared) symbol.info.paramInfoss.map(_.map(_.show).mkString("(", ", ", ")")).mkString else ""
          members(symbol) =
            Definition(
              qualifiedName(symbol) + parameters,
              info.name,
              symbol.is(Flags.Deferred),
              public,
              symbol.is(Flags.Mutable),
              info,
              suppressions(symbol)
            )
        }
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
            Option.when(output(method).isDefined && !trivial && !fluent)(tpe.show)
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
      key(symbol).flatMap(nodes.get).exists(_.exported) && !symbol.is(Flags.Private) && !symbol.privateWithin.exists

    /** The rules suppressed on a definition or, for a type, on its companion: by
      * `@SuppressWarnings("baumkuchen[:<rule>]")`, by `@nowarn`, bare or filtering on `msg=baumkuchen[:<rule>]` as for
      * the compiler plugin's warnings, and by `@unused` for the liveness rules.
      */
    private def suppressions(symbol: Symbol)(using Context): Set[String] = {
      val annotated =
        if (!symbol.isClass) Set(symbol)
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
          case Suppress => strings(annotation).flatMap(rule(_))
          case Nowarn   =>
            val filters = strings(annotation)
            if (filters.forall(_.isEmpty)) List("*")
            else
              filters.flatMap(_.split('&').flatMap {
                case s"msg=$pattern" => rule(pattern)
                case _               => None
              })
          case Unused => Liveness.Rules
          case _      => Nil
        }
      }
    }

    private def rule(id: String): Option[String] = id match {
      case "baumkuchen"         => Some("*")
      case s"baumkuchen:$check" => Some(check)
      case _                    => None
    }

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
            // Other builds can name a public type of a library, unless its enclosing type hides it.
            val exported = libraries(output.module) && output.scope == Output.Scope.Main &&
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
    // Whether the tree being traversed runs once per iteration of a loop.
    private val looping = mutable.Stack(false)
    // Synthetic methods such as case class `apply` and `copy` forward their parameters, which says nothing about callers.
    private val synthetic = mutable.Stack(false)
    // Inline expansion happens after pickling, so the givens it summons are only visible through their imports.
    private val imports = mutable.Stack(mutable.Set.empty[String])
    private val types   = TypeReferences { symbol =>
      collector.refer(owner, symbol)
      collector.use(symbol, owner, methods.contains(symbol))
    }

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
          case tpd.TypeDef(_, template: tpd.Template) if symbol.isClass => collector.define(symbol, template)
          case value: tpd.ValDef if symbol.is(Flags.Synthetic) && !symbol.owner.isClass =>
            collector.bind(symbol, value.rhs)
          case method: tpd.DefDef if symbol.name.is(DefaultGetterName) => collector.default(symbol, method.rhs)
          case tpd.TypeDef(_, _) if symbol.isAliasType && !symbol.is(Flags.Opaque)        => collector.member(symbol)
          case method: tpd.DefDef if !symbol.is(Flags.Synthetic) && !symbol.isConstructor =>
            collector.body(symbol, method)
            collector.precondition(symbol, method.rhs)
            collector.signature(symbol)
            collector.member(symbol)
            if (symbol.owner.isClass) body(method.rhs, node)
          case value: tpd.ValDef if symbol.owner.isClass && !symbol.is(Flags.Synthetic) =>
            body(value.rhs, node)
            collector.member(symbol)
          // A pattern definition keeps its authored right-hand side in a synthetic val.
          case value: tpd.ValDef if symbol.owner.isClass && value.rhs.span.exists && !value.rhs.span.isSynthetic =>
            body(value.rhs, node)
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
      case tpd.PackageDef(_, _) | _: tpd.Template =>
        pushing(imports, mutable.Set.empty[String])(traverseChildren(tree))
      case _ =>
        tree match {
          case application: tpd.Apply if !synthetic.top => collector.record(application, owner, methods.contains(_))
          case assignment: tpd.Assign                   => collector.assign(assignment.lhs.symbol)
          case generic: tpd.TypeApply if !synthetic.top =>
            collector.instantiate(generic.fun.symbol, generic.args.map(_.tpe), clause(generic.fun), owner)
          case closure: tpd.Closure if !closure.tpt.isEmpty =>
            owner.flatMap(collector.described(_)).foreach(node =>
              collector.implement(closure.tpt.tpe.classSymbol, s"a lambda in ${node.name}", node.module)
            )
          case select: tpd.Select if looping.top && !synthetic.top =>
            collector.dispatch(
              select.symbol,
              methods.find(method => method.is(Flags.Method) && method.owner.isClass),
              owner
            )
          case _ => ()
        }
        collector.refer(owner, tree.symbol)
        collector.use(tree.symbol, owner, methods.contains(tree.symbol))
        collector.mention(tree)
        types.traverse(tree.tpe)
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
        pushing(looping, value = true) {
          traverse(condition)
          traverse(body)
        }
      case tpd.Apply(function, arguments) if Traversals.traversal(function) =>
        traverse(function)
        arguments.foreach(argument =>
          pushing(looping, looping.top || Traversals.lambda(argument).isDefined)(traverse(argument))
        )
      case _ => traverseChildren(tree)
    }

    // Generated code is expected to repeat itself, so only authored bodies are hashed for duplicates.
    private def body(tree: tpd.Tree, node: Option[String]): Unit =
      node.flatMap(collector.described(_)).filterNot(_.generated).foreach(info =>
        if (!tree.isEmpty) fragments.add(tree, info.name, info.test)
      )

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
