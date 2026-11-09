package net.kurobako.bonsai.analysis

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.MissingType
import dotty.tools.dotc.core.NameKinds.DefaultGetterName
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Names.typeName
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.AppliedType
import dotty.tools.dotc.core.Types.TypeRef
import java.nio.file.PathMatcher
import net.kurobako.bonsai.java.{Model => JavaModel}
import net.kurobako.bonsai.lifting.{Purity as _, *}
import scala.annotation.tailrec
import scala.annotation.targetName
import scala.collection.mutable
import scala.util.control.NonFatal
import scala.util.matching.Regex

private[analysis] object Collector {

  /** What a twin's collector leaves for the collector it joins. */
  object Absorbed {
    val empty: Absorbed = Absorbed(Symbols.Recorded.empty, Map.empty, Set.empty)
  }

  case class Absorbed(symbols: Symbols.Recorded, nodes: Map[String, Node], roots: Set[Root]) {
    @targetName("merge")
    def ++(other: Absorbed): Absorbed = Absorbed(symbols ++ other.symbols, other.nodes ++ nodes, roots ++ other.roots)
  }

  // A hierarchy of one case is a single implementation, which another rule reports.
  private val MinimumCases = 2

  /** A member of a build class whose references decide whether it is used, test-only or wider than needed. */
  private case class Definition(
      name: String,
      node: String,
      deferred: Boolean,
      public: Boolean,
      variable: Boolean,
      info: Node,
      suppressed: Set[String],
      field: Boolean = false,
      alias: Boolean = false,
      key: Option[String] = None,
      line: Int = 0
  )
}

private[analysis] class Collector(
    outputs: Vector[Output],
    libraries: Set[String],
    generated: Vector[PathMatcher],
    concurrent: Set[String],
    reflective: Vector[Regex],
    bounded: Vector[Complexity.Bounded],
    hot: String => Boolean,
    javaCosts: Vector[JavaModel.Cost],
    references: (Components, Fragments) => Context ?=> tpd.TreeTraverser,
    lines: Lines,
    entryPoints: Set[Root],
    link: Option[Graph => Reach.Calls]
) extends Phase, Recorder {
  import Collector.*

  private val symbols     = Symbols(outputs, libraries, generated, reflective, lines)
  private val callSites   = CallSites(symbols, lines)
  private val reads       = Reads(symbols, (symbol, from) => use(symbol, from))
  private val derivations = Derivations(symbol => symbols.key(symbol).isDefined)
  private val dispatching = Dispatch(symbols, bodies.contains(_))
  private val dictionary  = Dictionaries(symbols)
  private val structure   = StructureObserver(symbols, () => dispatching.classes, lines)
  private val comparing   = Comparisons(symbols, () => dispatching.classes, lines)
  import symbols.*

  private val products       = mutable.Map.empty[Symbol, Algebra.Product]
  private val required       = mutable.Map.empty[Symbol, (String, Algebra.Product)]
  private val sums           = mutable.Map.empty[String, Algebra.Sum]
  private val failures       = mutable.ArrayBuffer.empty[String]
  private val remarks        = mutable.ArrayBuffer.empty[String]
  private val methods        = mutable.Map.empty[Symbol, Smells.Method]
  private val overridden     = mutable.Map.empty[Symbol, Int]
  private val designed       = mutable.ArrayBuffer.empty[Designs.Method]
  private val aliases        = mutable.ArrayBuffer.empty[Designs.Alias]
  private val clones         = mutable.ArrayBuffer.empty[Clones.Group]
  private val members        = mutable.Map.empty[Symbol, Definition]
  private val users          = mutable.Map.empty[Symbol, mutable.Set[Use]]
  private val usage          = mutable.Map.empty[String, Set[Use]]
  private val assigned       = mutable.Set.empty[Symbol]
  private val assignedNames  = mutable.Set.empty[String]
  private val memberList     = mutable.ArrayBuffer.empty[Members.Member]
  private val givens         = mutable.Map.empty[Symbol, (String, Node)]
  private val named          = mutable.Set.empty[Symbol]
  private val givenList      = mutable.ArrayBuffer.empty[Members.Given]
  private val unlisted       = mutable.Set.empty[Symbol]
  private val nested         = mutable.Set.empty[String]
  private val exposed        = mutable.Set.empty[String]
  private val nestedList     = mutable.ArrayBuffer.empty[Members.Nested]
  private val bodies         = mutable.Map.empty[Symbol, tpd.DefDef]
  private val constructors   = mutable.Map.empty[Symbol, tpd.DefDef]
  private val initialisers   = mutable.Map.empty[Symbol, tpd.Tree]
  private val costed         = mutable.ArrayBuffer.empty[Symbol]
  private val complexityList = mutable.ArrayBuffer.empty[Complexity.Summary]
  private val unusedList     = mutable.ArrayBuffer.empty[Complexity.Bounded]
  private val pipelineList   = mutable.ArrayBuffer.empty[Pipelines.Lifted]
  private val dictionaryList = mutable.ArrayBuffer.empty[Dictionaries.Parameter]
  private val templates      = mutable.Map.empty[Symbol, tpd.Template]
  // The phase runs once, and its results are read by the extraction that follows.
  private var purityModel  = Purity.Model.empty        // scalafix:ok DisableSyntax.var
  private var linked       = Option.empty[Reach.Calls] // scalafix:ok DisableSyntax.var
  private var twinned      = Collector.Absorbed.empty  // scalafix:ok DisableSyntax.var
  private val hierarchies  = mutable.ArrayBuffer.empty[(Symbol, String, String)]
  private val bodied       = mutable.Set.empty[Symbol]
  private val enumerations = mutable.ArrayBuffer.empty[Algebra.Enumeration]
  private var placements   = Option.empty[Placements]  // scalafix:ok DisableSyntax.var
  private val callees      = mutable.Map.empty[Symbol, Callee]

  def phaseName: String = "bonsai-collect"

  protected def run(using Context): Unit = ()

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
    val fragments  = Fragments(lines)
    val placing    = Placements(symbols, lines)
    val library    = Library()
    val components =
      Components(
        symbols,
        callSites,
        reads,
        derivations,
        dispatching,
        structure,
        comparing,
        library,
        dictionary,
        this
      )
    placements = Some(placing)
    units.foreach { unit =>
      try {
        given Context = ctx.withSource(unit.source)
        structure.enter(unit.tpdTree)
        references(components, fragments).traverse(unit.tpdTree)
        placing.traverse(unit.tpdTree)
      } catch { case NonFatal(error) => failed(s"${unit.source.path} could not be analysed", error) }
    }
    qualifiedUses()
    symbols.absorb(twinned.symbols)
    products.mapValuesInPlace((cls, product) =>
      if (derivations.derived(cls)) product.copy(origin = Algebra.Origin.Derived) else product
    )
    enumerations ++= hierarchies.flatMap((parent, name, key) => enumeration(parent, name, key))
    callees ++= products.view.map((cls, product) =>
      cls -> Callee(product.name, product.fields.size, Finding.simple(product.name), product.node, Set.empty, 0)
    ).toMap ++ methods.view.map((symbol, method) =>
      symbol -> Callee(method.key, method.fields.size, method.short, method.node, method.suppressed, method.line)
    )
    callSites.align(callees)
    usage ++= users.toVector.groupMapReduce((symbol, _) => memberName(symbol))((_, nodes) => nodes.toSet)(_ ++ _)
    assignedNames ++= assigned.map(memberName(_))
    memberList ++= members.collect {
      case (symbol, definition) if !(definition.field && derivations.derived(symbol.owner)) =>
        val bases = (symbol.allOverriddenSymbols ++ dispatching.overridden(symbol)).toVector
        Members.Member(
          definition.name,
          definition.node,
          users.get(symbol).fold(Set.empty[Use])(_.toSet),
          bases.exists(base => key(base).isEmpty || users.get(base).exists(_.nonEmpty)),
          bases.exists(key(_).isDefined),
          bases.nonEmpty,
          definition.deferred,
          definition.public,
          definition.variable && !assigned(symbol),
          structure.detachable(symbol),
          definition.info.test,
          definition.suppressed,
          definition.field,
          unlisted(symbol),
          definition.alias,
          definition.key,
          definition.line,
          bases.map(memberName(_))
        )
    }
    val overridden = givens.keySet.flatMap(_.allOverriddenSymbols)
    givenList ++= givens.collect {
      case (symbol, (name, info)) if symbol.allOverriddenSymbols.isEmpty && !overridden(symbol) && !clashes(symbol) =>
        Members.Given(name, info.name, named(symbol), suppressions(symbol))
    }
    val incoming = symbols.incoming
    nestedList ++= nested.flatMap(inner =>
      nodes.get(inner).flatMap(_.outer).map(outer =>
        Members.Nested(inner, outer, incoming.getOrElse(inner, Vector.empty).toSet, exposed(inner))
      )
    )
    clones ++= fragments.groups()
    dispatching.resolve()
    dictionaryList ++= dictionary.parameters()
    linked = link.map(_(graph))
    val bytecode = linked.fold(BytecodeEffects.none)(_.summaries)
    val declared = Complexity.Declared(bounded)
    val costs    = Costs(
      bodies ++ constructors,
      initialisers,
      declared,
      JavaCosts(javaCosts, declared),
      library,
      lines,
      bytecode
    )
    complexityList ++= costed.flatMap(method =>
      try
        costs.summary(method).flatMap(bounds =>
          key(method).map(Complexity.Summary(qualifiedName(method), _, bounds, suppressions(method)))
        )
      catch {
        case NonFatal(error) =>
          failed(s"${qualifiedName(method)} could not be costed", error)
          None
      }
    )
    complexityList ++= costs.javaSummaries
    unusedList ++= declared.unmatched
    val effects = inference(library, bytecode)
    val chains  = Pipelines.Chains(effects, lines, hot)
    pipelineList ++= bodies.view.filter((method, _) => ownMethod(method)).flatMap((method, definition) =>
      key(method).toVector.flatMap(
        chains.lifted(method, qualifiedName(method), _, suppressions(method), definition.rhs)
      )
    )
    purityModel = purity(effects)
    structure.finish()
    comparing.finish()
    units
  }

  /** Adds what a twin's classes reference, which the copies of the JVM modules the node records stand in for: the edges
    * and the nodes. A twin is read before this collector, which joins it as it finishes.
    */
  def absorb(other: Collector): Unit =
    twinned = twinned ++ Collector.Absorbed(other.symbols.recorded, other.graph.nodes, other.graph.roots)

  /** The nodes with an edge to each node, with those of the twins this collector absorbed. */
  def referrers: Map[String, Vector[String]] = symbols.incoming

  /** The uses of members by name. */
  def usages: Map[String, Set[Use]] = usage.toMap

  /** The names of the variables assigned after their initialisation. */
  def assignments: Set[String] = assignedNames.toSet

  def problem(message: String): Unit = failures += message

  /** Records why `message` could not be done: a note when the classpath lacks a class, which a build may leave off, and
    * a problem otherwise.
    */
  def failed(message: String, error: Throwable): Unit = error match {
    case _: MissingType => note(s"$message: $error")
    case _              => problem(s"$message: $error")
  }

  def problems: Vector[String] = failures.toVector

  def note(message: String): Unit = remarks += message

  def notes: Vector[String] = remarks.toVector

  def graph: Graph = {
    val own = symbols.graph(entryPoints)
    own.copy(nodes = twinned.nodes ++ own.nodes, roots = own.roots ++ twinned.roots)
  }

  /** What linking the build to its class files gave, when the collector linked them. */
  def calls: Option[Reach.Calls] = linked

  def algebra: Algebra.Model =
    Algebra.Model(
      products.values.toVector.sortBy(_.name),
      sums.values.toVector.sortBy(_.name),
      callSites.recorded,
      required.values.toMap,
      enumerations.toVector,
      callSites.expanded
    )

  def smells: Smells.Model = {
    val defaults = callSites.defaulting(callees)
    Smells.Model(
      methods.toVector.map((symbol, method) =>
        if (overridden.getOrElse(symbol, 0) > 1) method.copy(shared = true) else method
      ).sortBy(method => (method.name, method.node)),
      callSites.recorded,
      callSites.usages(methods),
      dispatching.abstractions,
      callSites.instantiations,
      defaults,
      spares = callSites.spared(defaults)
    )
  }

  def complexities: Vector[Complexity.Summary] = complexityList.toVector

  def unusedBounds: Vector[Complexity.Bounded] = unusedList.toVector

  def pipelines: Vector[Pipelines.Lifted] = pipelineList.toVector

  def megamorphic: Vector[Megamorphic.Site] = dispatching.megamorphic

  def dictionaries: Vector[Dictionaries.Parameter] = dictionaryList.toVector

  def purity: Purity.Model = purityModel

  private def inference(library: Library, bytecode: BytecodeEffects.Summaries)(using Context): Purity.Inference = {
    val effects = Purity.Inference(bodies, templates, dispatching.implementations(_), concurrent, library, bytecode)
    effects.run()
    effects.unreadable.foreach(note(_))
    effects
  }

  private def purity(effects: Purity.Inference)(using Context): Purity.Model = {
    def site(method: Symbol, tree: tpd.Tree) =
      key(method).map(node => (node, lines.location(tree.sourcePos), lines.line(tree.sourcePos)))
    Purity.Model(
      effects.pointless.flatMap((method, callee, tree) =>
        site(method, tree).map((node, location, line) =>
          Purity.Discard(callee, node, location, line, suppressions(method))
        )
      ),
      effects.effectful.flatMap((method, operation, effect, tree, view, clock) =>
        site(method, tree).map((node, location, line) =>
          Purity.Transform(operation, effect, node, location, line, view, clock, suppressions(method))
        )
      ),
      effects.shared.flatMap((method, variable, entry, tree) =>
        site(method, tree).map((node, location, _) =>
          Purity.SharedWrite(variable, entry, node, location, suppressions(method))
        )
      ),
      bodies.keys.view.filter(ownMethod(_)).flatMap(method =>
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
      effects.library,
      effects.reordered.flatMap((method, operator, input, pure, whole) =>
        site(method, whole).map((node, location, line) =>
          Purity.Ordered(short(method), operator, shown(input), shown(pure), node, location, line, suppressions(method))
        )
      ),
      effects.invariant.flatMap((method, operation, call) =>
        site(method, call).map((node, location, line) =>
          Purity.Invariant(short(method), operation, shown(call), node, location, line, suppressions(method))
        )
      ),
      effects.returns
    )
  }

  private def short(method: Symbol)(using Context): String = Finding.short(qualifiedName(method))

  // The source text of a tree, or its callee where the source is not at hand or no longer the one compiled.
  private def shown(tree: tpd.Tree)(using Context): String = {
    val position = tree.sourcePos
    val text     = if (position.exists) position.source.content() else Array.empty[Char]
    val written  =
      if (position.exists && position.end <= text.length) String(text, position.start, position.end - position.start)
      else ""
    val name    = tree.symbol.name.toString
    val current = written.nonEmpty && written.count(_ == '(') == written.count(_ == ')') &&
      (!name.forall(character => character.isLetterOrDigit || character == '_') || name == "apply" ||
        written.contains(name))
    if (current) written.linesIterator.map(_.trim).mkString(" ") else Finding.short(qualifiedName(tree.symbol))
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

  /** Records a constructor for cost bounds; the primary one runs the parent calls and the statements of the class body
    * as well.
    */
  def constructor(symbol: Symbol, definition: tpd.DefDef, template: Option[tpd.Template])(using Context): Unit = {
    val rhs = template.fold(definition.rhs) { body =>
      val statements = body.parents.collect { case call: tpd.Apply => call } ++
        body.body.filter(statement => statement.isInstanceOf[tpd.ValDef] || !statement.isDef)
      tpd.Block(statements.filterNot(_.isInstanceOf[tpd.Import]), tpd.unitLiteral)
    }
    constructors(symbol) = tpd.cpy.DefDef(definition)(rhs = rhs)
    if (!symbol.is(Flags.Synthetic)) costed += symbol
  }

  def memberships: Members.Model = Members.Model(memberList.toVector, givenList.toVector, nestedList.toVector)

  /** The declarations the nesting rule places, with `entries` the nodes code outside the build enters at. */
  def nesting(entries: Set[String]): Nesting.Model = placements.fold(Nesting.Model.empty)(_.model(entries))

  def duplicates: Vector[Clones.Group] = clones.toVector

  /** Records the algebraic shape, invariant and construction access of a class defined in the analysed outputs. */
  def define(cls: Symbol, template: tpd.Template)(using Context): Unit = {
    dispatching.declare(cls)
    templates(cls) = template
    constructor(template.constr.symbol, template.constr, Some(template))
    unlisted ++= Registries.unlisted(template)
    if (authored(template)) bodied += cls
    register(node(cls)).foreach { key =>
      val name = qualifiedName(cls)
      if (cls.is(Flags.CaseClass)) {
        val parents = cls.asClass.parentSyms.view.collect {
          case parent if parent.is(Flags.Sealed) => qualifiedName(parent)
        }.toSet
        val fields    = fieldsOf(cls)
        val invariant = Predicates.required(cls, requirements(template.body), fields)
        val returns   = cls.info.decls.toList.view.collect {
          case method if method.is(Flags.Method) => method.info.finalResultType.widenDealias.typeSymbol.name.toString
        }.toSet
        products(cls) = Algebra.Product(
          name,
          key,
          fields,
          parents,
          invariant,
          access(cls, fields),
          Algebra.Origin.of(exported(cls) && exported(cls.primaryConstructor)),
          returns = returns
        )
      } else if (cls.is(Flags.Sealed)) {
        val variants = cls.children.map { child =>
          val fields = if (child.isClass && child.is(Flags.CaseClass)) fieldsOf(child) else Vector.empty
          Algebra.Variant(qualifiedName(child), fields)
        }
        val accessors = cls.info.decls.toList.view.collect {
          case member if member.is(Flags.Method) && member.is(Flags.Deferred) => member.name.toString
        }.toSet
        sums(name) = Algebra.Sum(name, key, variants.toVector, accessors)
        if (!cls.is(Flags.Enum)) hierarchies += ((cls, name, key))
      }
      nodes.get(key).foreach(dispatching.classify(cls, name, key, _))
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
    val cases                         = Symbols.childClasses(parent)
    def plain(child: Symbol): Boolean =
      child.isClass && child.is(Flags.Case) && !child.isOneOf(Flags.Sealed | Flags.Trait | Flags.Abstract) &&
        !dispatching.implemented(child) && !bodied(child) &&
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

  /** Records a method signature of the build for the argument, result and parameter group checks. */
  def signature(method: Symbol, body: tpd.Tree)(using Context): Unit =
    nodeOf(method).foreach { info =>
      val fields = fieldsOf(method)
      method.allOverriddenSymbols.filter(_.is(Flags.Deferred)).foreach(abstracted =>
        overridden(abstracted) = overridden.getOrElse(abstracted, 0) + 1
      )
      methods(method) = Smells.Method(
        methodName(method, fields),
        methodKey(method, fields),
        info.name,
        qualifiedName(method.owner),
        fields,
        method.allOverriddenSymbols.hasNext || Symbols.facade(method.enclosingClass),
        exported(method),
        info.test,
        stored(method, body),
        Calls.termClauses(method).flatten.zipWithIndex.collect {
          case (parameter, index) if parameter.is(Flags.Inline) => index
        }.toSet,
        method.is(Flags.Inline) || method.owner.isTerm && method.owner.is(Flags.Inline),
        suppressed = suppressions(method),
        rest = Shapes.clauses(method).drop(1).flatten,
        tested = Some(tested(method, body)),
        folded = folded(method, body),
        line = lines.line(method.sourcePos),
        entry = entry(method)
      )
    }

  private def entry(method: Symbol)(using Context): Vector[String] =
    if (!method.annotations.exists(_.symbol.fullName.toString == "mainargs.main")) Vector.empty
    else {
      val shapes = Shapes.clauses(method).flatten.map(field => field.name -> field.shape).toMap
      Calls.termClauses(method).flatten.toVector.flatMap { parameter =>
        parameter.annotations.find(_.symbol.fullName.toString == "mainargs.arg").map { annotation =>
          val shape = shapes.get(parameter.name.toString).fold(parameter.info.widenExpr.show)(_.toString)
          s"@${shown(annotation.tree)} ${parameter.name}: $shape"
        }
      }
    }

  private def literal(argument: Algebra.Argument): Boolean = argument match {
    case Algebra.Argument.Absent | Algebra.Argument.Present | Algebra.Argument.Unknown => false
    case _                                                                             => true
  }

  /** The optional parameters with a default of the first term parameter list that `body` only folds to one constant, by
    * index.
    */
  private def folded(method: Symbol, body: tpd.Tree)(using Context): Map[Int, String] = {
    @tailrec
    def bare(tree: tpd.Tree): tpd.Tree = tree match {
      case tpd.TypeApply(function, _) => bare(function)
      case other                      => other
    }
    @tailrec
    def receiver(tree: tpd.Tree): Option[Symbol] = Calls.peel(tree) match {
      case reference @ tpd.Ident(_)     => Some(reference.symbol)
      case tpd.Apply(function, List(_)) =>
        bare(function) match {
          case tpd.Select(inner, name) if name.toString == "map" => receiver(inner)
          case _                                                 => None
        }
      case _ => None
    }
    def folding(function: tpd.Tree, name: String): Option[Symbol] = bare(function) match {
      case tpd.Select(inner, called) if called.toString == name && function.symbol.owner == defn.OptionClass =>
        receiver(inner)
      case _ => None
    }
    val optional = Calls.termClauses(method).headOption.getOrElse(Nil).zipWithIndex.filter((parameter, _) =>
      parameter.is(Flags.HasDefault) && parameter.info.widenDealias.isRef(defn.OptionClass)
    )
    if (optional.isEmpty) Map.empty
    else {
      val folds = body.filterSubTrees {
        case tpd.Apply(tpd.Apply(function, List(_)), List(_)) => folding(function, "fold").isDefined
        case tpd.Apply(function, List(_))                     => folding(function, "getOrElse").isDefined
        case _                                                => false
      }.flatMap {
        case tpd.Apply(tpd.Apply(function, List(default)), List(_)) => folding(function, "fold").map(_ -> default)
        case tpd.Apply(function, List(default))                     => folding(function, "getOrElse").map(_ -> default)
        case _                                                      => None
      }
      optional.flatMap { (parameter, index) =>
        val uses = body.filterSubTrees {
          case reference: tpd.Ident => reference.symbol == parameter
          case _                    => false
        }.size
        folds.collect { case (`parameter`, default) => Predicates.argument(default) } match {
          case constants if constants.sizeIs == uses && constants.forall(_.exists(literal(_))) =>
            constants.distinct match {
              case List(Some(only)) => Some(index -> only.shown)
              case _                => None
            }
          case _ => None
        }
      }.toMap
    }
  }

  /** The parameters of the first term parameter list that the first statement of `body` tests, or that `body` only
    * passes to calls of methods of the build other than constructors, by index.
    */
  private def tested(method: Symbol, body: tpd.Tree)(using Context): Set[Int] = {
    val first = body match {
      case tpd.Block(statement :: _, _) => statement
      case tpd.Block(Nil, expression)   => expression
      case other                        => other
    }
    val branched = first match {
      case tpd.If(test, _, _) =>
        val used = test.filterSubTrees {
          case tpd.Ident(_) => true
          case _            => false
        }.map(_.symbol).toSet
        Calls.termClauses(method).headOption.getOrElse(Nil).zipWithIndex.collect {
          case (parameter, index) if used(parameter) => index
        }.toSet
      case _ => Set.empty
    }
    branched ++ passed(method, body)(application => !constructs(application) && output(application.symbol).isDefined)
  }

  private def constructs(application: tpd.Apply)(using Context): Boolean = application.symbol.isConstructor ||
    application.symbol.name == nme.apply && application.symbol.is(Flags.Synthetic)

  /** The parameters of the first term parameter list that `body` passes only to constructors, by index. */
  private def stored(method: Symbol, body: tpd.Tree)(using Context): Set[Int] = passed(method, body)(constructs(_))

  private def passed(method: Symbol, body: tpd.Tree)(accept: tpd.Apply => Boolean)(using Context): Set[Int] = {
    val arguments = body.filterSubTrees {
      case application: tpd.Apply => accept(application)
      case _                      => false
    }.collect { case application: tpd.Apply => application.args }.flatten.map(Calls.peel(_).symbol)
    val passes                  = arguments.groupMapReduce(identity)(_ => 1)(_ + _)
    def uses(parameter: Symbol) = body.filterSubTrees {
      case reference: tpd.Ident => reference.symbol == parameter
      case _                    => false
    }.size
    Calls.termClauses(method).headOption.getOrElse(Nil).zipWithIndex.collect {
      case (parameter, index) if uses(parameter) > 0 && uses(parameter) == passes.getOrElse(parameter, 0) => index
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
      nodeOf(method).foreach { info =>
        val parameters = Calls.termClauses(method).headOption.getOrElse(Nil)
        val first      = parameters.headOption
        designed += Designs.Method(
          qualifiedName(method),
          qualifiedName(owner),
          info.name,
          owner.is(Flags.ModuleClass),
          StructureObserver.implementing(owner),
          !method.isOneOf(Flags.Private | Flags.Protected) && !method.privateWithin.exists,
          first.map(_.info.widenDealias.typeSymbol).collect {
            case symbol if symbol.isClass => qualifiedName(symbol)
          },
          parameters.map(_.info.widenDealias.show).toVector,
          method.info.finalResultType.widenDealias.show,
          // An extension method is what the rule would make of it already.
          !method.is(Flags.ExtensionMethod) && first.exists(parameter =>
            receiving(method, parameter, body) && !fixedReceiver(method, parameter) && !scalar(parameter)
          ),
          info.test,
          lines.line(method.sourcePos)
        )
        structure.method(method, body, info)
        if (!method.is(Flags.ExtensionMethod)) structure.dispatch(method, parameters, body, info)
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

  /** Whether `parameter` is a primitive or a `String`, whose operations the library offers rather than the build. */
  private def scalar(parameter: Symbol)(using Context): Boolean = {
    val symbol = parameter.info.widenDealias.typeSymbol
    symbol.isPrimitiveValueClass || symbol == defn.StringClass
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
    }.collect { case select: tpd.Select => Calls.peel(select.qualifier).symbol }
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
      case constructed: AppliedType => structure.alias(symbol, scope, constructed)
      case _                        => ()
    }
  }

  def structures: Structures.Model = structure.structures

  def equality: Equality.Model = comparing.equality

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

  /** The conditions of the `require` calls among `statements`. */
  private def requirements(statements: List[tpd.Tree])(using Context): List[tpd.Tree] = statements.collect {
    case tpd.Apply(function, condition :: _)
        if function.symbol.name.toString == "require" &&
          function.symbol.owner == defn.ScalaPredefModule.moduleClass =>
      condition
  }

  /** Records the `require` preconditions of a method over its first parameter list. */
  def precondition(method: Symbol, body: tpd.Tree)(using Context): Unit = {
    val conditions = requirements(body match {
      case tpd.Block(statements, expression) => statements :+ expression
      case other                             => List(other)
    })
    if (conditions.nonEmpty) register(node(method)).foreach { key =>
      val fields    = fieldsOf(method)
      val condition = Predicates.required(method, conditions, fields)
      if (condition != Algebra.Predicate.Known(true)) {
        required(method) = methodKey(method, fields) -> Algebra.Product(
          methodName(method, fields),
          key,
          fields,
          Set.empty,
          condition,
          Algebra.Access.Open,
          Algebra.Origin.of(exported(method))
        )
      }
    }
  }

  /** Records a public or package-visible member of a build class, leaving out what the compiler writes and the API of a
    * library, which other builds use.
    */
  def member(symbol: Symbol)(using Context): Unit = {
    val owner = symbol.owner
    // Inline expansion and export forwarding hide or invent references, and setters are written through their fields.
    val excluded = Flags.Synthetic | Flags.Given | Flags.Implicit | Flags.Private | Flags.Module | Flags.Param |
      Flags.Inline | Flags.Exported
    // Term-only flags make a flag set term-only, so the flags type aliases can carry are tested on their own.
    val aliases = Flags.Synthetic | Flags.Private | Flags.Exported
    // Parameters of a case class are its fields, which patterns read without naming them.
    val field = symbol.is(Flags.ParamAccessor) && owner.is(Flags.CaseClass)
    if (
      owner.isClass && !owner.isAnonymousClass && !owner.owner.isTerm && !symbol.isOneOf(excluded) &&
      (field || !symbol.is(Flags.ParamAccessor)) &&
      !symbol.isOneOf(aliases) &&
      !symbol.isConstructor && !symbol.isSetter && !symbol.name.is(DefaultGetterName) &&
      !symbol.isAllOf(Flags.EnumValue) &&
      !ctx.platform.isMainMethod(symbol) && !exported(symbol)
    )
      nodeOf(symbol).foreach { info =>
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
            field,
            symbol.isAliasType,
            Option.when(symbol.isTerm)(Keys.of(symbol)).flatten,
            lineOf(symbol)
          )
      }
  }

  // A type that private or a qualifier already confines, which nothing inside it can be seen beyond.
  private def confined(cls: Symbol)(using Context): Boolean =
    cls.isClass &&
      (cls.privateWithin.exists || cls.isOneOf(Flags.Private | Flags.Protected) ||
        cls.is(Flags.ModuleClass) && cls.sourceModule.isOneOf(Flags.Private | Flags.Protected))

  private def memberName(symbol: Symbol)(using Context): String = Placements.signed(symbol, qualifiedName(symbol))

  /** Records the fields of a case class that a pattern on it binds or tests, which read them. */
  def matched(unapply: tpd.UnApply, from: Option[Use])(using Context): Unit = {
    val extractor = unapply.fun.symbol
    val cls       = Calls.owner(extractor).companionClass
    if (cls.is(Flags.CaseClass) && extractor.is(Flags.Synthetic)) {
      val fields = Calls.termClauses(cls.primaryConstructor).headOption.getOrElse(Nil)
      // A field named like a method of the class, as `copy` is, stands beside it.
      val accessors = cls.info.decls.toList.filter(_.is(Flags.ParamAccessor)).groupBy(_.name)
      fields.zip(unapply.patterns).foreach((field, pattern) =>
        if (!PatternShapes.ignored(pattern)) accessors.getOrElse(field.name, Nil).foreach(use(_, from))
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
      !symbol.ownersIterator.exists(confined(_)) && !symbol.isAllOf(Flags.EnumCase)
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
        if (!symbol.ownersIterator.exists(confined(_))) structure.expose(symbol, from)
      }

  /** Records a named given that code in other files can refer to; private givens are left to scalafix. */
  def instance(symbol: Symbol)(using Context): Unit =
    if (
      symbol.isTerm && symbol.is(Flags.Given) && symbol.owner.isClass &&
      !symbol.isOneOf(Flags.Private | Flags.Synthetic | Flags.Deferred) &&
      !invented(symbol.name.toString)
    ) nodeOf(symbol).foreach(info => givens(symbol) = qualifiedName(symbol) -> info)

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

  /** Counts the members and types that strings name by their full qualified names as used from the node holding the
    * string. A type so named is a reflective root, and the members it declares are exempt. Rendered code calls such a
    * member by name, so it is neither unused nor movable.
    */
  private def qualifiedUses()(using Context): Unit = {
    val byName = dispatching.classes.toVector.groupBy(qualifiedName(_))
    structure.literals.foreach { (_, path, owner, member, info) =>
      if (path.nonEmpty)
        byName.getOrElse(path + owner, Vector.empty).foreach { cls =>
          val names = Vector(termName(member), typeName(member))
          names.flatMap(name =>
            cls.info.member(name).alternatives.collect {
              case alternative if alternative.symbol.exists => alternative.symbol
            }
          ).foreach { symbol =>
            use(symbol, Some(Use(info.name)))
            symbols.refer(Some(Use(info.name)), symbol)
            members.remove(symbol): Unit
          }
        }
    }
    structure.typeNames.foreach { (text, info) =>
      byName.getOrElse(text, Vector.empty).foreach(cls =>
        key(cls).foreach { name =>
          symbols.refer(Some(Use(info.name)), cls)
          symbols.reflect(name)
        }
      )
    }
    dispatching.classes.foreach(cls =>
      if (exported(cls)) cls.asClass.baseClasses.drop(1).foreach(base => symbols.expose(base))
    )
    members.filterInPlace((symbol, _) => !exported(symbol)): Unit
  }

  def use(symbol: Symbol, from: Option[Use])(using Context): Unit =
    if (symbol.exists && !symbol.isClass && symbol.owner.isClass && key(symbol).isDefined)
      from.foreach(user => users.getOrElseUpdate(symbol, mutable.Set.empty) += user: Unit)
}
