package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.io.AbstractFile
import java.nio.file.Path
import java.nio.file.PathMatcher
import net.kurobako.bonsai.collections.*
import net.kurobako.bonsai.java.*
import net.kurobako.bonsai.lifting.*
import scala.annotation.targetName
import scala.collection.mutable
import scala.util.matching.Regex

private[analysis] object Symbols {

  /** The references a registry recorded, without the symbols they came from. */
  object Recorded {
    val empty: Recorded = Recorded(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)
  }

  case class Recorded(
      edges: Map[String, Set[String]],
      bodies: Map[Use, Set[String]],
      incoming: Map[String, Vector[String]],
      inlines: Map[String, Set[Use]],
      splices: Map[String, Set[Use]],
      arguments: Map[String, Set[(Use, String)]]
  ) {
    @targetName("merge")
    def ++(other: Recorded): Recorded = Recorded(
      Maps.join(edges, other.edges)(_ ++ _),
      Maps.join(bodies, other.bodies)(_ ++ _),
      Maps.join(incoming, other.incoming)(_ ++ _),
      Maps.join(inlines, other.inlines)(_ ++ _),
      Maps.join(splices, other.splices)(_ ++ _),
      Maps.join(arguments, other.arguments)(_ ++ _)
    )
  }

  // A Scala.js facade describes a JavaScript API, which Scala does not implement; the library may be off the classpath.
  def facade(cls: Symbol)(using Context): Boolean = {
    val native = Classes.named("scala.scalajs.js.native")
    val any    = Classes.named("scala.scalajs.js.Any")
    native.exists && cls.hasAnnotation(native) || any.exists && cls.derivesFrom(any)
  }

  def childClasses(parent: Symbol)(using Context): List[Symbol] =
    parent.children.map(child => if (child.isTerm) child.moduleClass else child)
}

/** The nodes of the build that its symbols name, and the references between them. */
private[analysis] class Symbols(
    outputs: Vector[Output],
    libraries: Set[String],
    generated: Vector[PathMatcher],
    reflective: Vector[Regex],
    lines: Lines
) {
  private val roots      = outputs.map(output => output -> output.path.toAbsolutePath.normalize)
  private val known      = mutable.Map.empty[String, Node]
  private val reflected  = mutable.Set.empty[String]
  private val edges      = mutable.Map.empty[String, mutable.Set[String]]
  private val bodies     = mutable.Map.empty[Use, mutable.Set[String]]
  private val inlines    = mutable.Map.empty[String, mutable.Set[Use]]
  private val splices    = mutable.Map.empty[String, mutable.Set[Use]]
  private val arguments  = mutable.Map.empty[String, mutable.Set[(Use, String)]]
  private val mainRoots  = mutable.Set.empty[String]
  private val registered = mutable.Map.empty[Symbol, Option[String]]
  private val keys       = java.util.HashMap[Symbol, Option[String]]()
  private val parameters = mutable.Map.empty[Symbol, Vector[Algebra.Field]]
  private val names      = mutable.Map.empty[Symbol, String]
  private val children   = mutable.Map.empty[String, mutable.Set[String]]

  val nodes: collection.Map[String, Node] = known

  def described(node: String): Option[Node] = known.get(node)

  def root(name: String): Unit = mainRoots += name

  def graph(entryPoints: Set[Root]): Graph =
    Graph(
      known.toMap,
      edges.view.mapValues(_.toSet).toMap,
      mainRoots.map(Root(_, Root.Kind.Main)).toSet ++ entryPoints ++ known.values.view.collect {
        case node if node.exported =>
          Root(node.name, if (reflected(node.name)) Root.Kind.Reflective else Root.Kind.Exported)
      },
      bodies.view.mapValues(_.toSet).toMap,
      inlines.view.mapValues(_.toSet).toMap,
      splices.view.mapValues(_.toSet).toMap,
      arguments.view.mapValues(_.toSet).toMap
    )

  /** The nodes with an edge to each node. */
  def incoming: Map[String, Vector[String]] =
    (edges.toVector ++ bodies.toVector.map((use, targets) => use.node -> targets))
      .flatMap((source, targets) => targets.map(_ -> source)).groupMap(_._1)(_._2)

  def recorded: Symbols.Recorded = Symbols.Recorded(
    edges.view.mapValues(_.toSet).toMap,
    bodies.view.mapValues(_.toSet).toMap,
    incoming,
    inlines.view.mapValues(_.toSet).toMap,
    splices.view.mapValues(_.toSet).toMap,
    arguments.view.mapValues(_.toSet).toMap
  )

  /** Adds the edges another registry recorded. */
  def absorb(other: Symbols.Recorded): Unit = {
    other.edges.foreach((source, targets) => targets.foreach(edge(source, _)))
    other.bodies.foreach((use, targets) => bodies.getOrElseUpdate(use, mutable.Set.empty) ++= targets)
    other.inlines.foreach((key, callers) => inlines.getOrElseUpdate(key, mutable.Set.empty) ++= callers)
    other.splices.foreach((key, targets) => splices.getOrElseUpdate(key, mutable.Set.empty) ++= targets)
    other.arguments.foreach((key, named) => arguments.getOrElseUpdate(key, mutable.Set.empty) ++= named)
  }

  /** Marks the node `prefix` names, and the nodes inside it, as reached by name from strings and as exported. */
  def reflect(prefix: String): Unit =
    (known.get(prefix).map(_ => prefix).toVector ++ children.getOrElse(prefix, Nil)).foreach { named =>
      reflected += named
      known(named) = known(named).copy(exported = true)
    }

  def expose(symbol: Symbol)(using Context): Unit =
    key(symbol).foreach(named => known(named) = known(named).copy(exported = true))

  def methodName(method: Symbol, fields: Vector[Algebra.Field])(using Context): String =
    s"${qualifiedName(method)}(${fields.map(_.name).mkString(", ")})"

  def methodKey(method: Symbol, fields: Vector[Algebra.Field])(using Context): String =
    Keys.of(method).getOrElse(methodName(method, fields))

  def fieldsOf(symbol: Symbol)(using Context): Vector[Algebra.Field] =
    parameters.getOrElseUpdate(symbol, if (symbol.isClass) Shapes.fields(symbol) else Shapes.parameters(symbol))

  def refer(from: Option[Use], symbol: Symbol)(using Context): Unit =
    from.foreach { user =>
      key(symbol).filter(_ != user.node).foreach { target =>
        user.method.fold(edge(user.node, target))(_ => bodies.getOrElseUpdate(user, mutable.Set.empty) += target: Unit)
      }
    }

  def link(from: Option[String], targets: Iterable[String]): Unit =
    from.foreach(source => targets.filter(_ != source).foreach(edge(source, _)))

  def inlined(key: String): Unit = inlines.getOrElseUpdate(key, mutable.Set.empty): Unit

  def expanded(key: String, caller: Use): Unit = inlines.getOrElseUpdate(key, mutable.Set.empty) += caller: Unit

  def spliced(key: String, target: Use): Unit = splices.getOrElseUpdate(key, mutable.Set.empty) += target: Unit

  def argued(key: String, caller: Use, node: String): Unit =
    arguments.getOrElseUpdate(key, mutable.Set.empty) += caller -> node: Unit

  // Edges arrive in runs from one source, so its set is kept to skip the lookup.
  // scalafix:off DisableSyntax.var, DisableSyntax.null
  private var latestSource                       = ""
  private var latestTargets: mutable.Set[String] = null

  private def edge(source: String, target: String): Unit = {
    if (latestTargets == null || latestSource != source) {
      latestTargets = edges.getOrElseUpdate(source, mutable.Set.empty)
      latestSource = source
    }
    latestTargets += target: Unit
  }
  // scalafix:on DisableSyntax.var, DisableSyntax.null

  /** The node key of any symbol, memoised because every tree and type of the build asks for one. */
  def key(symbol: Symbol)(using Context): Option[String] = {
    val cached = keys.get(symbol)
    // The map is a java one, where absent is null and a lookup allocates nothing.
    if (cached ne null) cached // scalafix:ok DisableSyntax.null
    else {
      val created = register(node(symbol))
      keys.put(symbol, created)
      created
    }
  }

  def nodeOf(symbol: Symbol)(using Context): Option[Node] = key(symbol).flatMap(known.get)

  /** Whether other builds can use `symbol` as part of the public API of a library. */
  def exported(symbol: Symbol)(using Context): Boolean =
    nodeOf(symbol).exists(node =>
      node.exported && !symbol.is(Flags.Private) && (reflected(node.name) || !symbol.privateWithin.exists)
    )

  private val UnusedRules =
    List(Finding.Rule.Unused, Finding.Rule.TestOnly, Finding.Rule.UnusedMember, Finding.Rule.TestOnlyMember).map(_.id)

  /** The rules suppressed on a definition or, for a type, on its companion: by `@SuppressWarnings("bonsai[:<rule>]")`,
    * by `@nowarn`, bare or filtering on `msg=bonsai[:<rule>]` as for the compiler plugin's warnings, and by `@unused`
    * for the liveness rules. The compiler reports a `@nowarn` that names only analysis rules as unused, as it
    * suppresses none of its warnings.
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
    val Suppress                        = Classes.required("java.lang.SuppressWarnings")
    val Nowarn                          = Classes.required("scala.annotation.nowarn")
    val Unused                          = Classes.required("scala.annotation.unused")
    def strings(annotation: Annotation) = annotation.tree.filterSubTrees {
      case tpd.Literal(constant) => constant.tag == Constants.StringTag
      case _                     => false
    }.collect { case tpd.Literal(constant) => constant.stringValue }
    annotated.view.filter(_.exists).flatMap(_.annotations).flatMap { annotation =>
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
        case Unused => UnusedRules
        case _      => Nil
      }
    }.toSet
  }

  private def rules(id: String): Set[String] = LintId.resolve(id)

  /** The line `symbol` is declared at, or 0 when unknown. */
  def lineOf(symbol: Symbol)(using Context): Int =
    if (symbol.source.exists && symbol.span.exists) lines.line(symbol.source, symbol.span.point) else 0

  /** The key of an analysed node, recording the node on first sight. */
  def register(symbol: Symbol)(using Context): Option[String] =
    if (!symbol.exists) None
    else registered.get(symbol) match {
      case Some(name) => name
      case None       =>
        val name = output(symbol).map { output =>
          val name   = qualifiedName(symbol)
          val source = sourcePath(symbol)
          val outer  = register(node(symbol.owner))
          val line   = lineOf(symbol)
          // Other builds can name a public type of a library, unless its enclosing type hides it. Code the build
          // only names in strings, compiled where the names are visible, can name a reflective type and the types
          // inside it whatever their qualifier.
          val reflected = reflective.exists(Globs.matches(_, name)) || outer.exists(this.reflected)
          if (reflected) this.reflected += name
          val exported = reflected || libraries(output.module) && output.scope == Output.Scope.Main &&
            (symbol :: Option.when(symbol.is(Flags.ModuleClass))(symbol.sourceModule).toList)
              .forall(part => !part.is(Flags.Private) && !part.privateWithin.exists) &&
            outer.forall(nodes.get(_).exists(_.exported))
          val isGenerated = Outputs.isGenerated(generated, source) ||
            symbol.annotations.exists(annotation =>
              Generated.Annotations(annotation.symbol.fullName.toString)
            ) ||
            outer.exists(nodes.get(_).exists(_.generated))
          val entry =
            Node(
              name,
              output.module,
              output.scope,
              source,
              line,
              isGenerated,
              exported,
              outer,
              suppressions(symbol)
            )
          // Inline expansion summons givens after pickling, so a given lives whenever its owner does.
          val instance = symbol.is(Flags.Given) || symbol.is(Flags.ModuleClass) && symbol.sourceModule.is(Flags.Given)
          if (instance) outer.foreach(edge(_, name))
          if (!known.contains(name)) {
            known(name) = entry
            name.indices.filter(name(_) == '.').foreach(index =>
              children.getOrElseUpdate(name.take(index), mutable.Set.empty) += name
            )
          }
          name
        }
        registered(symbol) = name
        name
    }

  @annotation.tailrec
  final def node(symbol: Symbol)(using Context): Symbol =
    if (!symbol.exists || symbol.is(Flags.Package) || symbol.isTopLevelDefinitionsObject) NoSymbol
    else if (symbol.owner.isTopLevelDefinitionsObject)
      if (symbol.is(Flags.Synthetic) || symbol.isConstructor) NoSymbol else symbol
    else if (symbol.is(Flags.Module) && symbol.isTerm) node(symbol.moduleClass)
    else if (symbol.isClass && !symbol.isAnonymousClass && !symbol.isRefinementClass) {
      val companion = symbol.companionClass
      val canonical = if (symbol.is(Flags.ModuleClass) && companion.exists) companion else symbol
      if (canonical.isAllOf(Flags.EnumCase)) node(canonical.owner) else canonical
    } else node(symbol.owner)

  private val outputsOf = mutable.HashMap.empty[String, Option[Output]]

  def output(symbol: Symbol)(using Context): Option[Output] =
    Option[AbstractFile](symbol.associatedFile).flatMap(file =>
      outputsOf.getOrElseUpdate(file.path, located(file.path))
    )

  private val rooted: Map[Path, (Int, Output)] =
    roots.zipWithIndex.reverse.map { case ((output, root), index) => root -> (index, output) }.toMap

  private def located(file: String): Option[Output] =
    Iterator.iterate(Option(Path.of(file).toAbsolutePath.normalize))(_.flatMap(path => Option(path.getParent)))
      .takeWhile(_.isDefined).flatten.flatMap(rooted.get).minByOption(_._1).map(_._2)

  private def sourcePath(symbol: Symbol)(using Context): String =
    if (symbol.source.exists) symbol.source.path
    else Option(symbol.compilationUnitInfo).flatMap(_.tastyInfo).flatMap(_.attributes.sourceFile).getOrElse("")

  def qualifiedName(symbol: Symbol)(using Context): String = names.get(symbol) match {
    case Some(name) => name
    case None       =>
      val name =
        if (symbol.is(Flags.Package)) symbol.fullName.toString
        else if (symbol.isTopLevelDefinitionsObject) qualifiedName(symbol.owner)
        else {
          val prefix = qualifiedName(symbol.owner)
          val simple = symbol.effectiveName.toString
          if (prefix == "<empty>") simple else s"$prefix.$simple"
        }
      names(symbol) = name
      name
  }
}
