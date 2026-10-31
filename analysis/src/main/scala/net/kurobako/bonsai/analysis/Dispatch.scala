package net.kurobako.bonsai.analysis

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.Type
import net.kurobako.bonsai.lifting.Calls
import scala.collection.mutable

/** The classes of the build, what implements each abstraction, and the calls that dispatch to more than one class.
  * `bodied` tells whether the build holds the body of a method.
  */
private[analysis] class Dispatch(symbols: Symbols, bodied: Symbol => Boolean) {
  import symbols.*

  private val declared   = mutable.LinkedHashSet.empty[Symbol]
  private val reachable  = mutable.Map.empty[Symbol, Option[Vector[Symbol]]]
  private val subclasses = mutable.Map.empty[Symbol, mutable.ArrayBuffer[Symbol]]
  private val dispatches = mutable.Map.empty[(Symbol, Symbol), (String, Type)]
  private val abstracts  = mutable.Map.empty[Symbol, (String, String, String)]
  private val implements = mutable.Map.empty[Symbol, mutable.Set[(String, String)]]
  private val mixins     = mutable.Set.empty[Symbol]
  private val asTypes    = mutable.Set.empty[Symbol]
  // Abstractions some class implements, beside those only lambdas implement.
  private val subclassed  = mutable.Set.empty[Symbol]
  private val specialised = mutable.Set.empty[Symbol]
  private val sites       = mutable.ArrayBuffer.empty[Megamorphic.Site]

  val classes: collection.Set[Symbol] = declared

  def declare(cls: Symbol): Unit = declared += cls: Unit

  def implemented(abstraction: Symbol): Boolean = implements.contains(abstraction)

  def megamorphic: Vector[Megamorphic.Site] = sites.toVector

  def asType(symbol: Symbol): Unit = asTypes += symbol: Unit

  def implement(abstraction: Symbol, name: String, module: String): Unit =
    implements.getOrElseUpdate(abstraction, mutable.Set.empty) += (name -> module): Unit

  /** Records the abstraction a trait or abstract class `cls` is, or the abstractions a concrete class implements. */
  def classify(cls: Symbol, name: String, key: String, info: Node)(using Context): Unit =
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

  def abstractions: Vector[Smells.Abstraction] =
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
    }

  /** Resolves the calls recorded to the classes that can run them, once every class is declared. */
  def resolve()(using Context): Unit = {
    indexSubclasses()
    sites ++= dispatches.toVector.flatMap { case ((caller, callee), (node, receiver)) =>
      // Test doubles never reach a call in main code at run time.
      val test            = nodes.get(node).exists(_.test)
      val implementations = concreteSubclasses(callee.owner).view.filter(cls =>
        (test || !nodeOf(cls).exists(_.test)) && reaches(cls, callee.owner, receiver)
      ).flatMap(concreteMember(callee, _)).toVector.distinct
      Option.when(implementations.size > MaximumInlinedReceivers)(Megamorphic.Site(
        qualifiedName(caller),
        node,
        s"${callee.owner.name.toString.stripSuffix("$")}.${callee.name}",
        shown(implementations.map(_.owner)),
        suppressions(caller)
      ))
    }
  }

  // The concrete classes deriving from each class, in definition order; every class is defined by the time calls are
  // resolved.
  private def indexSubclasses()(using Context): Unit = {
    subclasses.clear()
    declared.foreach(cls =>
      if (!cls.isOneOf(Flags.Trait | Flags.Abstract))
        cls.asClass.baseClasses.foreach(base => subclasses.getOrElseUpdate(base, mutable.ArrayBuffer.empty) += cls)
    )
  }

  private def concreteSubclasses(owner: Symbol): collection.Seq[Symbol] = subclasses.getOrElse(owner, Vector.empty)

  private def concreteMember(method: Symbol, cls: Symbol)(using Context): Option[Symbol] =
    Some(method.denot.matchingMember(cls.thisType)).filter(found => found.exists && !found.is(Flags.Deferred))

  /** The bodies a call of `method` can run: its own, or every override in the build's main classes, which test doubles
    * never reach at run time; nothing when one of them lies outside the build.
    */
  def implementations(method: Symbol)(using Context): Option[Vector[Symbol]] =
    reachable.getOrElseUpdate(
      method,
      if (bodied(method) && (method.isEffectivelyFinal || !method.owner.isClass)) Some(Vector(method))
      else if (!declared.contains(Calls.owner(method))) None
      else {
        val overrides =
          concreteSubclasses(method.owner).view.filter(cls => !nodeOf(cls).exists(_.test)).flatMap(concreteMember(
            method,
            _
          ))
            .toVector
        val all = (Option.when(bodied(method))(method).toVector ++ overrides).distinct
        // An immutable val implements a method by a read, which has no effect.
        Option.when(all.nonEmpty && all.forall(target =>
          bodied(target) || !target.is(Flags.Method) && !target.is(Flags.Mutable)
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

  /** Whether a value of `cls` can be the receiver of static type `receiver` at a call of a member of `owner`: a class
    * extending `owner` with other concrete type arguments than the receiver's never is.
    */
  private def reaches(cls: Symbol, owner: Symbol, receiver: Type)(using Context): Boolean = {
    def open(tpe: Type) =
      !tpe.widenDealias.typeSymbol.isClass || tpe.existsPart(_.typeSymbol.isOneOf(Flags.TypeParam | Flags.Deferred))
    val expected = receiver.widen.baseType(owner).argInfos
    val actual   = cls.typeRef.baseType(owner).argInfos
    expected.size != actual.size || expected.zip(actual).forall((left, right) =>
      open(left) || open(right) || left <:< right || right <:< left
    )
  }
  private def universal(owner: Symbol)(using Context): Boolean =
    owner == defn.AnyClass || owner == defn.ObjectClass || owner == defn.ProductClass || owner == defn.SerializableClass
  def record(callee: Symbol, caller: Option[Symbol], node: Option[String], receiver: Type)(using Context): Unit =
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
