package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.AndType
import dotty.tools.dotc.core.Types.OrType
import dotty.tools.dotc.core.Types.Type
import net.kurobako.bonsai.lifting.*
import scala.collection.mutable

private[analysis] object Comparisons {

  private val Strategies = Set(
    "scala.math.Ordering",
    "scala.math.PartialOrdering",
    "scala.math.Equiv",
    "java.util.Comparator",
    "java.lang.Comparable"
  )

  private val MutableLibraries = Set("ujson.Value", "ujson.Obj", "ujson.Arr")

  // Types whose equality is that of the values they hold.
  private val Containers = Vector(
    "scala.collection.IterableOnce",
    "scala.collection.ArrayOps",
    "scala.Option",
    "scala.util.Either",
    "scala.util.Try",
    "scala.Product",
    "java.util.Collection",
    "java.util.Map"
  )

  // Collection operations that compare elements by equality.
  private val Elementwise = Set(
    "contains",
    "containsSlice",
    "containsAll",
    "indexOf",
    "lastIndexOf",
    "indexOfSlice",
    "lastIndexOfSlice",
    "distinct",
    "diff",
    "intersect",
    "sameElements",
    "startsWith",
    "endsWith",
    "removeAll",
    "retainAll"
  )

  private val Assertions = Set(
    "assertEquals",
    "assertNotEquals",
    "assertResult",
    "assertSameElements",
    "shouldBe",
    "shouldEqual",
    "mustBe",
    "mustEqual"
  )

  private val Binary = Set("==", "!=", "equals")

  private val Hashing = Set("hashCode", "##")
}

/** The case classes of the build with fields that make their equality unstable, and the places that compare values:
  * with `==` and the other methods of equality and hashing, as set elements and map keys, in operations that look
  * elements up by equality, against stable identifiers in patterns, and in assertions.
  */
private[analysis] class Comparisons(symbols: Symbols, classes: () => Iterable[Symbol], lines: Lines) {
  import Comparisons.*
  import symbols.*

  private val cases       = mutable.ArrayBuffer.empty[Symbol]
  private val comparisons = mutable.LinkedHashMap.empty[(Set[Symbol], String, String), (Int, Node)]
  // Built once every class is declared, when the phase finishes.
  private var model = Equality.Model.empty // scalafix:ok DisableSyntax.var

  def equality: Equality.Model = model

  def define(cls: Symbol)(using Context): Unit =
    if (cls.is(Flags.CaseClass) && !cls.is(Flags.ModuleClass)) cases += cls

  /** Records the comparison a tree makes. */
  def observe(tree: tpd.Tree, from: Option[String])(using Context): Unit = tree match {
    case tpd.Apply(select @ tpd.Select(receiver, name), List(argument))
        if Binary(name.toString) && universal(select.symbol) && !nulled(receiver, argument) =>
      record(List(receiver, argument), s"by $name", tree, from)
    case select @ tpd.Select(receiver, name) if Hashing(name.toString) && universal(select.symbol) =>
      record(List(receiver), s"by $name", tree, from)
    case tpd.Apply(tpd.Select(receiver, name), arguments)
        if Elementwise(name.toString) && container(receiver.tpe) =>
      record(receiver :: arguments, s"by $name", tree, from)
    case select @ tpd.Select(receiver, name) if Elementwise(name.toString) && container(receiver.tpe) =>
      record(List(receiver), s"by $name", select, from)
    case application: tpd.Apply if application.symbol.exists && Assertions(application.symbol.name.toString) =>
      val receiver = application.fun match {
        case tpd.Select(receiver, _) => List(receiver)
        case _                       => Nil
      }
      record(receiver ++ application.args, s"by ${application.symbol.name}", tree, from)
    case caseDef: tpd.CaseDef =>
      record(identifiers(caseDef.pat), "by a stable identifier pattern", caseDef.pat, from)
    case _ => ()
  }

  /** Records the key types of the hashed maps and sets a tree's type names. */
  def keyed(keys: List[Type], tree: tpd.Tree, from: Option[String])(using Context): Unit =
    if (keys.nonEmpty) {
      val types = keys.flatMap(elements(_)).toSet
      if (types.nonEmpty) store(types, "as a set element or map key", tree, from)
    }

  /** Builds the model, once every class is declared. */
  def finish()(using Context): Unit = {
    val objects = classes().filter(_.is(Flags.ModuleClass)).toVector
    model = Equality.Model(
      cases.toVector.flatMap(cls =>
        nodeOf(cls).map { info =>
          val parameters = Calls.termClauses(cls.primaryConstructor).headOption.getOrElse(Nil)
          Equality.Case(
            qualifiedName(cls),
            info.name,
            parameters.flatMap(parameter =>
              reason(parameter.info.widenExpr, objects).map(Equality.Field(parameter.name.toString, _))
            ).toVector,
            cls.asClass.baseClasses.drop(1).collect { case base if key(base).isDefined => qualifiedName(base) }.toSet,
            parameters.flatMap(parameter => elements(parameter.info.widenExpr)).map(qualifiedName(_)).toSet,
            suppressions(cls)
          )
        }
      ),
      comparisons.view.map { case ((types, how, _), (line, info)) =>
        val site = if (line > 0) s"${Finding.short(info.name)} (line $line)" else Finding.short(info.name)
        Equality.Comparison(types.map(qualifiedName(_)), how, site, info.test)
      }.toVector
    )
  }

  private def record(trees: List[tpd.Tree], how: String, at: tpd.Tree, from: Option[String])(using Context): Unit = {
    val types = trees.flatMap(tree => elements(tree.tpe)).toSet
    if (types.nonEmpty) store(types, how, at, from)
  }

  private def store(types: Set[Symbol], how: String, at: tpd.Tree, from: Option[String])(using Context): Unit =
    from.flatMap(nodes.get).foreach(info =>
      comparisons.getOrElseUpdate((types, how, info.name), (lines.line(at.sourcePos), info)): Unit
    )

  private def universal(method: Symbol)(using Context): Boolean = {
    def root(symbol: Symbol) = symbol.owner == defn.AnyClass || symbol.owner == defn.ObjectClass
    method.exists && (root(method) || method.allOverriddenSymbols.exists(root(_)))
  }

  private def nulled(trees: tpd.Tree*): Boolean = trees.exists {
    case tpd.Literal(constant) => constant.tag == Constants.NullTag
    case _                     => false
  }

  private def container(tpe: Type)(using Context): Boolean = Classes.derives(tpe.widenDealias, Containers*)

  /** The build classes whose values a comparison of `tpe` compares: the type itself, and what the containers it names
    * hold.
    */
  private def elements(tpe: Type)(using Context): Set[Symbol] = tpe.widenDealias match {
    case or: OrType   => elements(or.tp1) ++ elements(or.tp2)
    case and: AndType => elements(and.tp1) ++ elements(and.tp2)
    case widened      =>
      val cls = widened.classSymbol
      if (!cls.isClass) Set.empty
      else if (key(cls).isDefined) Set(cls)
      else if (container(widened)) widened.argInfos.flatMap(argument => elements(argument.hiBound)).toSet
      else Set.empty
  }

  /** The stable identifiers a pattern compares the scrutinee with. */
  private def identifiers(pattern: tpd.Tree)(using Context): List[tpd.Tree] = pattern match {
    case tpd.Bind(_, body)             => identifiers(body)
    case tpd.Typed(body, _)            => identifiers(body)
    case tpd.Alternative(alternatives) => alternatives.flatMap(identifiers(_))
    case tpd.UnApply(_, _, patterns)   => patterns.flatMap(identifiers(_))
    case reference @ (tpd.Ident(_) | tpd.Select(_, _))
        if reference.symbol.isTerm && reference.symbol.name != nme.WILDCARD &&
          !reference.symbol.isOneOf(Flags.Module | Flags.Package | Flags.Enum) =>
      List(reference)
    case _ => Nil
  }

  private def reason(tpe: Type, objects: Vector[Symbol])(using Context): Option[String] = {
    val widened = tpe.widen.dealias
    val symbol  = widened.classSymbol
    if (widened.isRef(defn.ArrayClass)) Some("arrays compare by reference")
    else if (defn.isFunctionType(widened)) Some("functions compare by reference")
    else if (lambdaShaped(widened, symbol, objects))
      Some("single-method traits that lambdas implement compare by reference")
    else if (changing(symbol)) Some("mutable fields change the value after construction")
    else None
  }

  private def changing(symbol: Symbol)(using Context): Boolean = {
    val name = symbol.fullName.toString
    symbol.exists &&
    (name.startsWith("scala.collection.mutable.") || name == "java.util.ArrayList" ||
      name == "java.util.HashMap" || name == "java.util.HashSet" || name == "java.util.LinkedList" ||
      MutableLibraries(name))
  }

  // A trait some object of the build implements is a strategy, not something lambdas stand in for.
  private def lambdaShaped(tpe: Type, symbol: Symbol, objects: Vector[Symbol])(using Context): Boolean =
    symbol.exists && symbol.is(Flags.Trait) && !symbol.is(Flags.Sealed) && !Strategies(symbol.fullName.toString) &&
      !objects.exists(_.derivesFrom(symbol)) &&
      (tpe.possibleSamMethods match {
        case Seq(method) => method.info.paramInfoss.nonEmpty
        case _           => false
      })
}
