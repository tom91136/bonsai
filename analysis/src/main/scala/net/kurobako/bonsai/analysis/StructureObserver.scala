package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameKinds.DefaultGetterName
import dotty.tools.dotc.core.NameOps.stripModuleClassSuffix
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Names.typeName
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.AppliedType
import dotty.tools.dotc.core.Types.ConstantType
import dotty.tools.dotc.core.Types.NamedType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeTraverser
import dotty.tools.dotc.util.Spans.Span
import net.kurobako.bonsai.lifting.*
import scala.annotation.targetName
import scala.collection.mutable

private[analysis] object StructureObserver {

  object Evidence {
    val empty: Evidence = Evidence(Map.empty, Map.empty, Vector.empty, Vector.empty, Map.empty, Set.empty)
  }

  /** What a run records for the runs beside it: the constructions and field reads of classes it did not read, the
    * strings it holds that name members and types, the strings that spell a member, and the names of its classes.
    */
  case class Evidence(
      built: Map[String, Int],
      echoes: Map[(String, String), Int],
      named: Vector[(String, String, String, String, Node)],
      typed: Vector[(String, Node)],
      spelled: Map[String, Set[String]],
      classes: Set[String]
  ) {
    @targetName("merge")
    def ++(other: Evidence): Evidence = Evidence(
      Maps.join(built, other.built)(_ + _),
      Maps.join(echoes, other.echoes)(_ + _),
      named ++ other.named,
      typed ++ other.typed,
      Maps.join(spelled, other.spelled)(_ ++ _),
      classes ++ other.classes
    )
  }

  // A string naming a member, alone or at the head of a call: `Owner.member`, `pkg.Owner.member` or `Owner.member(...`.
  private val Reference = """((?:[a-z][A-Za-z0-9_]*\.)*)([A-Z][A-Za-z0-9_]*)\.([A-Za-z_][A-Za-z0-9_]*)(\(.*)?""".r

  private val Identifier = """[A-Za-z_][A-Za-z0-9_]*""".r

  // A string naming a type by its package and name, `pkg.Type`.
  private val QualifiedType = """((?:[a-z][A-Za-z0-9_]*\.)+)([A-Z][A-Za-z0-9_]*)""".r

  // File names read like references to members.
  private val Extensions =
    Set("scala", "sc", "sbt", "java", "class", "tasty", "jar", "md", "txt", "json", "conf", "xml")

  // Traits the compiler gives case classes and objects, which say nothing of what they implement.
  private val Markers = Set("scala.Product", "scala.Equals", "java.io.Serializable")

  private val ValueTypes = Set(
    "java.lang.Boolean",
    "java.lang.Byte",
    "java.lang.Character",
    "java.lang.Short",
    "java.lang.Integer",
    "java.lang.Long",
    "java.lang.Float",
    "java.lang.Double",
    "scala.math.BigInt",
    "scala.math.BigDecimal",
    "java.math.BigInteger",
    "java.math.BigDecimal",
    "scala.collection.StringOps",
    "scala.runtime.RichBoolean",
    "scala.runtime.RichByte",
    "scala.runtime.RichChar",
    "scala.runtime.RichDouble",
    "scala.runtime.RichFloat",
    "scala.runtime.RichInt",
    "scala.runtime.RichLong",
    "scala.runtime.RichShort"
  )

  // An object implementing a trait, as a type-class instance does, is more than a holder of functions.
  def implementing(cls: Symbol)(using Context): Boolean = cls.asClass.parentSyms.exists { parent =>
    val name = parent.fullName.toString
    parent.is(Flags.Trait) && !Markers(name) && !name.startsWith("scala.deriving.")
  }

  // A field named like its class reads `graph.graph` wherever a value is named for its type.
  private def stutters(cls: String, field: String): Boolean =
    Vector(cls, s"${cls}s", s"${cls}es").exists(field.equalsIgnoreCase(_))

  // The types a type names, not the objects that hold them: `Scheduler.Listener` names `Listener` only.
  private class NamedTypes(record: Symbol => Unit)(using Context) extends TypeTraverser {
    def traverse(tpe: Type): Unit = tpe match {
      case named: NamedType => record(named.symbol)
      case _                => traverseChildren(tpe)
    }
  }

  // The symbols of `owner` a type names, following the aliases of `owner` to what they stand for.
  private class OwnedTypes(owner: Symbol, record: Symbol => Unit)(using Context) extends TypeTraverser {
    private val seen = mutable.Set.empty[Symbol]

    def traverse(tpe: Type): Unit = tpe match {
      case named: NamedType =>
        val symbol = named.symbol
        if (symbol.exists && symbol.owner == owner && seen.add(symbol)) {
          record(symbol)
          if (symbol.isAliasType) traverse(symbol.info)
        }
        traverseChildren(tpe)
      case _ => traverseChildren(tpe)
    }
  }

  @annotation.tailrec
  private def concluding(tree: tpd.Tree)(using Context): tpd.Tree = Calls.peel(tree) match {
    case tpd.Block(_, expression) => concluding(expression)
    case other                    => other
  }
}

/** The structure model: the extents, constructions, field reads, string mentions and object groupings the structure
  * rules count. It owns the strings code spells out, which `literals` and `typeNames` give to reflective-use
  * resolution, and the members `detachable` tells apart for member listing.
  */
private[analysis] class StructureObserver(
    symbols: Symbols,
    classes: () => Iterable[Symbol],
    lines: Lines,
    foreign: Set[Output]
) {
  import StructureObserver.*
  import symbols.*

  private val stuttering  = mutable.Map.empty[Symbol, (String, String)]
  private val echoes      = mutable.Map.empty[Symbol, Int]
  private val spelled     = mutable.Map.empty[String, mutable.Set[String]]
  private val spliced     = mutable.Set.empty[Span]
  private val applied     = mutable.ArrayBuffer.empty[Structures.Applied]
  private val built       = mutable.Map.empty[Symbol, Int]
  private val grouped     = mutable.ArrayBuffer.empty[Structures.Member]
  private val detached    = mutable.Set.empty[Symbol]
  private val nested      = mutable.Map.empty[(String, String), Set[String]]
  private val dispatched  = mutable.ArrayBuffer.empty[Structures.Dispatch]
  private val extents     = mutable.Map.empty[String, (Int, Int)]
  private val named       = mutable.ArrayBuffer.empty[(String, String, String, String, Node)]
  private val typed       = mutable.ArrayBuffer.empty[(String, Node)]
  private val mirrors     = mutable.ArrayBuffer.empty[Counterparts.Mirror]
  private val signed      = mutable.Set.empty[Structures.Signature]
  private val wrapping    = Wrappers(symbols, lines)
  private val apartBuilt  = mutable.Map.empty[String, Int]
  private val apartEchoes = mutable.Map.empty[(String, String), Int]
  private var received    = StructureObserver.Evidence.empty // scalafix:ok DisableSyntax.var

  private def elsewhere(symbol: Symbol)(using Context): Boolean = foreign.nonEmpty && output(symbol).exists(foreign)

  private def construct(symbol: Symbol, change: Int)(using Context): Unit =
    if (elsewhere(symbol)) apartBuilt(identified(symbol)) = apartBuilt.getOrElse(identified(symbol), 0) + change
    else built(symbol) = built.getOrElse(symbol, 0) + change

  /** What this run records for the runs beside it, which hold the definitions or the strings it concerns. */
  def evidence(using Context): StructureObserver.Evidence =
    if (foreign.isEmpty) StructureObserver.Evidence.empty
    else
      StructureObserver.Evidence(
        apartBuilt.toMap,
        apartEchoes.toMap,
        named.toVector,
        typed.toVector,
        spelled.view.mapValues(_.toSet).toMap,
        classes().filter(cls => !cls.isAnonymousClass && key(cls).isDefined).map(_.effectiveName.toString).toSet
      )
  // Built once every class is declared, when the phase finishes.
  private var model = Structures.Model.empty // scalafix:ok DisableSyntax.var

  /** Takes the evidence the runs beside this one recorded of what it defines. */
  def receive(others: StructureObserver.Evidence): Unit = received = others

  /** The strings naming `Owner.member`, as text, package path, owner, member and the node holding the string. */
  def literals: Iterable[(String, String, String, String, Node)] = named ++ received.named

  /** The strings naming a type by package and name, with the node holding the string. */
  def typeNames: Iterable[(String, Node)] = typed ++ received.typed

  def structures: Structures.Model = model

  /** Makes the libraries a compilation unit imports the ones its methods are checked for bypassed wrappers. */
  def enter(unit: tpd.Tree)(using Context): Unit = wrapping.enter(unit)

  /** Records the build types named by the signature of the visible member `symbol` of the type `from`, and whether each
    * is named by its result or parents rather than its parameters.
    */
  def expose(symbol: Symbol, from: String)(using Context): Unit = {
    def record(result: Boolean)(types: List[Type]) = {
      val named = NamedTypes(target => key(target).foreach(signed += Structures.Signature(from, _, result)))
      types.foreach(named.traverse(_))
    }
    if (symbol.isClass) record(true)(symbol.asClass.parentTypes)
    else {
      record(false)(symbol.info.paramInfoss.flatten)
      record(true)(List(symbol.info.finalResultType))
    }
  }

  /** Whether a member of an object names nothing else of its object and uses no type of the build but its own. */
  def detachable(member: Symbol): Boolean = detached(member)

  /** Records the extent of a class of the build, the constructions its parents make, and its fields named like it. */
  def define(cls: Symbol, definition: tpd.TypeDef, template: tpd.Template)(using Context): Unit =
    nodeOf(cls).foreach { info =>
      // An anonymous class is a construction of what it extends.
      if (!cls.isAnonymousClass) {
        template.parents.foreach(_.foreachSubTree {
          case tpd.New(tpt) => construct(tpt.tpe.typeSymbol, -1)
          case _            => ()
        })
        if (info.outer.isEmpty && info.name == qualifiedName(cls) && definition.span.exists) {
          // A companion spans its own lines, and a synthetic one those of its class.
          val first        = lines.line(definition.source, definition.span.start)
          val last         = lines.line(definition.source, definition.span.end)
          val (start, end) = extents.getOrElse(info.name, (first, last))
          extents(info.name) = (start.min(first), end.max(last))
        }
        if (cls.owner.is(Flags.ModuleClass)) {
          val within = (qualifiedName(cls.owner), cls.name.stripModuleClassSuffix.toString)
          nested(within) = nested.getOrElse(within, Set.empty) ++ reached(template, cls.owner).map(_.name.toString)
        }
        if (!cls.is(Flags.ModuleClass))
          cls.asClass.paramAccessors.filter(field =>
            !field.is(Flags.Private) && stutters(cls.name.toString, field.name.toString)
          ).foreach(field => stuttering(field) = (qualifiedName(cls), info.name))
      }
    }

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
      nodeOf(member).foreach { info =>
        val siblings   = reached(definition, owner).filter(_ != member).map(_.name.toString)
        val types      = mutable.Set.empty[String]
        val qualifiers = mutable.Set.empty[tpd.Tree]
        definition.foreachSubTree { tree =>
          tree match {
            case tpd.Select(qualifier, _) if qualifier.tpe.widen.typeSymbol.is(Flags.ModuleClass) =>
              qualifiers += qualifier
            case _ => ()
          }
          if (!qualifiers(tree))
            usedTypes(tree.tpe)(used => if (used != owner) named(used).foreach(types += _))
        }
        if (siblings.isEmpty && !types(info.name)) detached += member
        if (
          !info.test && !owner.companionClass.exists && !implementing(owner) &&
          !owner.asClass.derivesFrom(requiredClass("scala.Enumeration"))
        )
          grouped += Structures.Member(
            member.name.toString,
            qualifiedName(owner),
            info.name,
            siblings,
            types.toSet
          )
      }
  }

  private def reached(tree: tpd.Tree, owner: Symbol)(using Context): Set[Symbol] = {
    val symbols   = mutable.Set.empty[Symbol]
    val traverser = OwnedTypes(owner, symbols += _)
    tree.foreachSubTree { sub =>
      if (sub.symbol.exists && sub.symbol.owner == owner) symbols += sub.symbol
      traverser.traverse(sub.tpe)
    }
    symbols.toSet
  }

  private def nestedUses(owner: String, names: Set[String]): Set[String] = {
    @annotation.tailrec
    def close(found: Set[String]): Set[String] = {
      val more = found.flatMap(name => nested.getOrElse((owner, name), Set.empty))
      if (more.subsetOf(found)) found else close(found ++ more)
    }
    close(names)
  }

  private def usedTypes(tpe: Type)(record: Symbol => Unit)(using Context): Unit =
    tpe.widen match {
      case applied: AppliedType if wrapper(applied.tycon.typeSymbol) =>
        applied.args.foreach(usedTypes(_)(record))
      case applied: AppliedType =>
        record(applied.tycon.typeSymbol)
      case widened if widened.hasSimpleKind =>
        val symbol = widened.typeSymbol
        val target = if (symbol.isAliasType && predefined(symbol.owner)) widened.dealias.typeSymbol else symbol
        if (!unnamed(target) && !valueType(target) && !(target.isClass && structural(target))) record(target)
      case _ =>
    }

  private def wrapper(symbol: Symbol)(using Context): Boolean =
    symbol.isAliasType && predefined(symbol.owner) || symbol.isClass && (key(symbol).isEmpty || structural(symbol))

  private def structural(symbol: Symbol)(using Context): Boolean =
    !symbol.isDefinedInCurrentRun &&
      (defn.isTupleClass(symbol) || defn.isFunctionClass(symbol) ||
        symbol == defn.ArrayClass || symbol == Classes.required("scala.collection.ArrayOps") ||
        symbol.asClass.derivesFrom(defn.OptionClass) ||
        symbol.asClass.derivesFrom(Classes.required("scala.collection.IterableOnce")))

  private def named(symbol: Symbol)(using Context): Option[String] =
    key(symbol).orElse(Option.when(symbol.isClass)(qualifiedName(symbol)))

  private def predefined(owner: Symbol)(using Context): Boolean =
    owner == defn.ScalaPredefModuleClass || owner == defn.ScalaPackageObject.moduleClass

  private def unnamed(symbol: Symbol)(using Context): Boolean =
    symbol.is(Flags.Package) ||
      Set[Symbol](defn.AnyClass, defn.AnyValClass, defn.ObjectClass, defn.NothingClass, defn.NullClass, defn.UnitClass)
        .contains(symbol)

  private def valueType(symbol: Symbol)(using Context): Boolean =
    symbol == defn.StringClass || symbol == defn.StringContextClass || symbol == defn.StringContextModule.moduleClass ||
      symbol.isPrimitiveValueClass || symbol.isClass && ValueTypes(qualifiedName(symbol))

  /** Records an alias of `scope` that fills a type constructor with literal arguments. */
  def alias(symbol: Symbol, scope: Symbol, target: AppliedType)(using Context): Unit =
    if (
      target.args.exists {
        case ConstantType(_) => true
        case _               => false
      }
    )
      key(symbol).foreach(node =>
        applied += Structures.Applied(
          qualifiedName(symbol),
          node,
          qualifiedName(scope),
          qualifiedName(target.tycon.typeSymbol)
        )
      )

  /** Records a method whose result is a match on an enum of the build that it takes as a parameter. */
  def dispatch(method: Symbol, parameters: List[Symbol], body: tpd.Tree, info: Node)(using Context): Unit =
    concluding(body) match {
      case tpd.Match(selector, cases) =>
        val parameter   = Calls.peel(selector).symbol
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
          nodeOf(enumeration).foreach { owned =>
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

  // The class, object or member a symbol stands for, where the compiler adds constructors and companions.
  private def definition(symbol: Symbol)(using Context): Symbol = {
    val owner = if (symbol.isConstructor || symbol.is(Flags.Synthetic)) symbol.owner else symbol
    if (owner.is(Flags.ModuleClass)) owner.companionClass.orElse(owner.sourceModule) else owner
  }

  // The enum or sealed parent whose case `symbol` is, and the name of the case.
  private def alternative(tree: tpd.Tree)(using Context): Option[(Symbol, String)] = {
    @annotation.tailrec
    def head(tree: tpd.Tree): tpd.Tree = Calls.peel(tree) match {
      case tpd.Apply(function, _)      => head(function)
      case tpd.TypeApply(function, _)  => head(function)
      case tpd.Bind(_, inner)          => head(inner)
      case tpd.Typed(inner, _)         => head(inner)
      case tpd.UnApply(function, _, _) => head(function)
      case other                       => other
    }
    val found = head(tree)
    List(
      found.symbol,
      found.tpe.typeSymbol,
      found.tpe.termSymbol
    ).filter(_.exists).map(definition(_)).collectFirst(Function.unlift {
      cls =>
        val shape  = if (cls.is(Flags.Module)) cls.moduleClass else cls
        val parent =
          if (cls.isAllOf(Flags.EnumCase) || cls.owner.linkedClass.is(Flags.Enum) && shape.is(Flags.Case))
            Some(cls.owner.linkedClass)
          else if (shape.isClass && shape.is(Flags.Case)) shape.asClass.parentSyms.find(_.is(Flags.Sealed))
          else None
        parent.filter(_.exists).map(_ -> cls.name.toString)
    })
  }

  private def mirror(method: Symbol, info: Node, cases: List[tpd.CaseDef])(using Context): Unit = {
    val pairs = cases.flatMap { clause =>
      for {
        (from, name) <- alternative(clause.pat)
        (to, other)  <- alternative(concluding(clause.body))
        if name == other && from != to
      } yield (from, to, name)
    }
    pairs.groupBy((from, to, _) => (from, to)).foreach { case ((from, to), found) =>
      mirrors += Counterparts.Mirror(
        qualifiedName(method),
        info.name,
        lines.line(method.sourcePos),
        qualifiedName(from),
        qualifiedName(to),
        found.map(_._3).sorted.toVector
      )
    }
  }

  /** Records the raw calls a library wrapper stands for, and the matches of production code that translate cases. */
  def method(method: Symbol, body: tpd.Tree, info: Node)(using Context): Unit = {
    wrapping.method(method, body, info)
    if (!info.test && !info.generated) {
      body.foreachSubTree {
        case tpd.Match(_, cases) => mirror(method, info, cases)
        case _                   => ()
      }
    }
  }

  /** Records the field reads, constructions and member names in strings the structure rules count. */
  def observe(tree: tpd.Tree, from: Option[String])(using Context): Unit = tree match {
    case tpd.Select(qualifier, name)
        if tree.symbol.is(Flags.ParamAccessor) && qualifier.symbol.exists && qualifier.symbol.name == name =>
      if (elsewhere(tree.symbol)) {
        val field = (qualifiedName(tree.symbol.owner), tree.symbol.name.toString)
        apartEchoes(field) = apartEchoes.getOrElse(field, 0) + 1
      } else echoes(tree.symbol) = echoes.getOrElse(tree.symbol, 0) + 1
    case tpd.New(tpt) => construct(tpt.tpe.typeSymbol, 1)
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
      if (Identifier.matches(constant.stringValue))
        from.foreach(spelled.getOrElseUpdate(constant.stringValue, mutable.Set.empty) += _)
      constant.stringValue match {
        case Reference(path, owner, member, _) if !Extensions(member) =>
          from.flatMap(nodes.get).foreach(info =>
            named += ((constant.stringValue, path, owner, member, info))
          )
        case QualifiedType(_, _) =>
          from.flatMap(nodes.get).foreach(info => typed += ((constant.stringValue, info)))
        case _ => ()
      }
    case _ => ()
  }

  /** Builds the model, once every class is declared. */
  def finish(others: StructureObserver.Evidence)(using Context): Unit = {
    val types    = classes().toVector.filter(cls => !cls.isAnonymousClass && key(cls).isDefined)
    val byName   = types.groupBy(_.effectiveName.toString)
    val mentions = (named.toVector ++ others.named).flatMap { (text, path, owner, member, info) =>
      // A nested type goes by its simple name only inside the type that holds it.
      def nameable(cls: Symbol) =
        cls.owner.is(Flags.Package) || key(cls.topLevelClass).exists(Node.within(nodes, info.name, _))
      val candidates = byName.getOrElse(owner, Vector.empty)
        .filter(cls => (path.isEmpty || qualifiedName(cls) == path + owner) && nameable(cls))
      def spells(cls: Symbol) =
        key(cls).exists(owner =>
          (spelled.getOrElse(member, Set.empty) ++ others.spelled.getOrElse(member, Set.empty))
            .exists(Node.within(nodes, _, owner))
        )
      Option.when(candidates.nonEmpty && !others.classes(owner) && !candidates.exists(cls =>
        cls.info.member(termName(member)).exists || cls.info.member(typeName(member)).exists || spells(cls)
      ))(Structures.Mention(text, owner, member, info.name, info.test))
    }
    val concretes = types.view.filterNot(_.is(Flags.Trait)).flatMap(cls =>
      nodeOf(cls).map(info =>
        Structures.Concrete(
          qualifiedName(cls),
          info.name,
          !cls.isOneOf(Flags.Abstract | Flags.ModuleClass | Flags.Enum | Flags.Case) &&
            built.getOrElse(cls, 0) + others.built.getOrElse(identified(cls), 0) <= 0,
          cls.asClass.parentSyms.collect {
            case parent if !parent.is(Flags.Trait) && key(parent).isDefined => qualifiedName(parent)
          }.toVector
        )
      )
    ).toVector
    val families = types.filter(cls =>
      cls.owner.is(Flags.ModuleClass) && !cls.owner.isTopLevelDefinitionsObject &&
        !cls.owner.linkedClass.is(Flags.Enum) &&
        !cls.isOneOf(Flags.ModuleClass | Flags.Synthetic | Flags.Private) && !cls.isAllOf(Flags.EnumCase)
    ).groupBy(_.owner).toVector.flatMap((owner, nested) =>
      for {
        info <- nodeOf(owner) if !info.test && !info.generated
      } yield Counterparts.Family(
        qualifiedName(owner),
        info.name,
        lines.line(owner.sourcePos),
        nested.map(_.name.toString).toSet
      )
    )
    model = Structures.Model(
      stuttering.toVector.map { case (field, (owner, node)) =>
        Structures.Stutter(
          owner,
          node,
          field.name.toString,
          echoes.getOrElse(field, 0) + others.echoes.getOrElse((owner, field.name.toString), 0)
        )
      },
      mentions,
      applied.toVector,
      concretes,
      grouped.toVector.map(member => member.copy(siblings = nestedUses(member.owner, member.siblings))),
      dispatched.toVector,
      extents.view.mapValues((start, end) => end - start + 1).toMap,
      Counterparts.Model(mirrors.toVector, families),
      wrapping.bypasses,
      signed.toVector
    )
  }
}
