package net.kurobako.bonsai.analysis

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.Type
import net.kurobako.bonsai.lifting.*
import scala.annotation.targetName
import scala.collection.mutable

private[analysis] object Dispatch {

  object Evidence {
    val empty: Evidence = Evidence(Map.empty, Set.empty, Set.empty, Set.empty, Set.empty)
  }

  /** What a run records of the abstractions another run reads, by their nodes: the implementations it found, and
    * whether some class implements them, a class implementing them fixes a primitive type argument or is an object, and
    * a type names them.
    */
  case class Evidence(
      implemented: Map[String, Vector[(String, String)]],
      subclassed: Set[String],
      specialised: Set[String],
      objectParents: Set[String],
      asTypes: Set[String]
  ) {
    @targetName("merge")
    def ++(other: Evidence): Evidence = Evidence(
      Maps.join(implemented, other.implemented)(_ ++ _),
      subclassed ++ other.subclassed,
      specialised ++ other.specialised,
      objectParents ++ other.objectParents,
      asTypes ++ other.asTypes
    )
  }
}

/** The classes of the build, what implements each abstraction, and the calls that dispatch to more than one class.
  * `bodied` tells whether the build holds the body of a method.
  */
private[analysis] class Dispatch(symbols: Symbols, bodied: Symbol => Boolean, foreign: Set[Output]) {
  import symbols.*

  private val declared   = mutable.LinkedHashSet.empty[Symbol]
  private val reachable  = mutable.Map.empty[(Symbol, Symbol), Option[Vector[Symbol]]]
  private val subclasses = mutable.Map.empty[Symbol, mutable.ArrayBuffer[Symbol]]
  private val dispatches = mutable.Map.empty[(Symbol, Symbol), (String, Type, Boolean)]
  private val abstracts  = mutable.Map.empty[Symbol, (String, String, String)]
  private val implements = mutable.Map.empty[Symbol, mutable.Map[Symbol, (String, String)]]
  private val mixins     = mutable.Set.empty[Symbol]
  private val asTypes    = mutable.Set.empty[Symbol]
  // Abstractions some class implements, beside those only lambdas implement.
  private val subclassed    = mutable.Set.empty[Symbol]
  private val specialised   = mutable.Set.empty[Symbol]
  private val prioritised   = mutable.Set.empty[Symbol]
  private val objectParents = mutable.Set.empty[Symbol]
  private val sites         = mutable.ArrayBuffer.empty[Megamorphic.Site]
  private val apart         = mutable.Map.empty[String, mutable.ArrayBuffer[(String, String)]]
  private val apartKinds    = mutable.Map.empty[Kind, mutable.Set[String]]

  private enum Kind { case Subclassed, Specialised, ObjectParent, AsType }

  private def elsewhere(symbol: Symbol)(using Context): Option[String] =
    if (foreign.isEmpty || !output(symbol).exists(foreign)) None else key(symbol)

  private def note(kind: Kind, symbol: Symbol, local: mutable.Set[Symbol])(using Context): Unit =
    elsewhere(symbol).fold(local += symbol)(node => apartKinds.getOrElseUpdate(kind, mutable.Set.empty) += node): Unit

  private var received = Dispatch.Evidence.empty // scalafix:ok DisableSyntax.var

  def receive(others: Dispatch.Evidence): Unit = received = others

  private def remote(owner: Symbol)(using Context): Boolean = key(owner).exists(received.subclassed)

  /** What this run records of the abstractions that it did not read. */
  def evidence: Dispatch.Evidence = Dispatch.Evidence(
    apart.view.mapValues(_.toVector).toMap,
    apartKinds.getOrElse(Kind.Subclassed, Set.empty).toSet,
    apartKinds.getOrElse(Kind.Specialised, Set.empty).toSet,
    apartKinds.getOrElse(Kind.ObjectParent, Set.empty).toSet,
    apartKinds.getOrElse(Kind.AsType, Set.empty).toSet
  )

  val classes: collection.Set[Symbol] = declared

  def declare(cls: Symbol)(using Context): Unit =
    if (declared.add(cls) && !cls.isOneOf(Flags.Trait | Flags.Abstract))
      cls.asClass.baseClasses.foreach(base => subclasses.getOrElseUpdate(base, mutable.ArrayBuffer.empty) += cls)

  def implemented(abstraction: Symbol): Boolean = implements.contains(abstraction)

  def megamorphic: Vector[Megamorphic.Site] = sites.toVector

  def asType(symbol: Symbol)(using Context): Unit = note(Kind.AsType, symbol, asTypes)

  def implement(abstraction: Symbol, implementation: Symbol, name: String, module: String)(using Context): Unit =
    elsewhere(abstraction).fold(
      implements.getOrElseUpdate(abstraction, mutable.Map.empty) += (implementation -> (name -> module))
    )(node => apart.getOrElseUpdate(node, mutable.ArrayBuffer.empty) += (name -> module)): Unit

  /** Records the abstraction a trait or abstract class `cls` is, or the abstractions a concrete class implements. */
  def classify(cls: Symbol, name: String, key: String, info: Node)(using Context): Unit =
    if (cls.is(Flags.Trait) || cls.is(Flags.Abstract)) {
      // Other builds can implement what a library exports, unless it is sealed.
      if (!cls.is(Flags.Enum) && !info.test && !Symbols.facade(cls) && (cls.is(Flags.Sealed) || !exported(cls))) {
        abstracts(cls) = (name, key, info.module)
        if (cls.is(Flags.Trait) && !cls.info.decls.exists(_.is(Flags.Deferred))) mixins += cls
        if (cls.info.decls.exists(_.isOneOf(Flags.Implicit | Flags.Given))) prioritised += cls
      }
    } else
      cls.asClass.baseClasses.drop(1).foreach { parent =>
        note(Kind.Subclassed, parent, subclassed)
        implement(parent, cls, name, info.module)
        if (cls.is(Flags.ModuleClass)) note(Kind.ObjectParent, parent, objectParents)
        if (cls.typeRef.baseType(parent).argInfos.exists(_.widenDealias.typeSymbol.isPrimitiveValueClass))
          note(Kind.Specialised, parent, specialised)
      }

  def abstractions: Vector[Smells.Abstraction] = {
    val others = received
    abstracts.toVector.collect {
      case (symbol, (name, node, module)) if !mixins(symbol) || asTypes(symbol) || others.asTypes(node) =>
        val found = implements.get(symbol).fold(Vector.empty)(_.values.toVector) ++
          others.implemented.getOrElse(node, Vector.empty)
        Smells.Abstraction(
          name,
          node,
          module,
          found.sorted,
          functional = found.nonEmpty && !subclassed(symbol) && !others.subclassed(node),
          specialised = specialised(symbol) || others.specialised(node),
          prioritised = prioritised(symbol) && (objectParents(symbol) || others.objectParents(node))
        )
    }
  }

  /** Resolves the calls recorded to the keys class files name them by, once every class is declared. */
  def resolve()(using Context): Unit =
    sites ++= dispatches.toVector.flatMap { case ((caller, callee), (node, receiver, settled)) =>
      // Test doubles never reach a call in main code at run time.
      val test    = nodes.get(node).exists(_.test)
      val classes = concreteSubclasses(callee.owner)
      val kept    = classes.filter(reaches(_, callee.owner, receiver))
      val members =
        kept.view.filter(cls => test || !nodeOf(cls).exists(_.test)).flatMap(concreteMember(callee, _)).toVector
      val cls = Some(receiver.widen.classSymbol).filter(found => found.exists && found.derivesFrom(callee.owner))
      Keys.at(callee, cls.getOrElse(callee.owner)).map(key =>
        Megamorphic.Site(
          qualifiedName(caller),
          node,
          s"${callee.owner.name.toString.stripSuffix("$")}.${callee.name}",
          key,
          Keys.of(caller),
          settled,
          classes.diff(kept).map(Calls.qualified(_)).toSet,
          members.nonEmpty && members.forall(stableRead(_)),
          suppressions(caller),
          callee.owner.typeParams.nonEmpty
        )
      )
    }

  private def concreteSubclasses(owner: Symbol): collection.Seq[Symbol] = subclasses.getOrElse(owner, Vector.empty)

  private def concreteMember(method: Symbol, cls: Symbol)(using Context): Option[Symbol] =
    Some(method.denot.matchingMember(cls.thisType)).filter(found => found.exists && !found.is(Flags.Deferred))

  def overridden(member: Symbol)(using Context): collection.View[Symbol] =
    concreteSubclasses(member.owner).view.flatMap(cls =>
      cls.asClass.baseClasses.view
        .filterNot(base => base.derivesFrom(member.owner) || member.owner.derivesFrom(base))
        .map(member.matchingDecl(_, cls.thisType))
    ).filter(_.exists)

  /** The bodies a call of `method` on a receiver of the static class `receiver` can run: its own, or every override in
    * the build's main classes that extend the receiver's, which test doubles never reach at run time; nothing when one
    * of them lies outside the build.
    */
  def implementations(method: Symbol, receiver: Option[Symbol])(using Context): Option[Vector[Symbol]] = {
    val from =
      if (!method.exists) method
      else receiver.filter(cls => cls.exists && cls.isClass && cls.derivesFrom(method.owner)).getOrElse(method.owner)
    reachable.getOrElseUpdate(
      method -> from,
      if (bodied(method) && (method.isEffectivelyFinal || !method.owner.isClass)) Some(Vector(method))
      else if (!declared.contains(Calls.owner(method)) || remote(Calls.owner(method))) None
      else {
        def main(owner: Symbol) = concreteSubclasses(owner).filter(cls => !nodeOf(cls).exists(_.test))
        val classes             = Some(main(from)).filter(_.nonEmpty).getOrElse(main(method.owner))
        val overrides           = classes.view.flatMap(concreteMember(method, _)).toVector
        val all                 = (Option.when(bodied(method))(method).toVector ++ overrides).distinct
        Option.when(all.nonEmpty && all.forall(target => bodied(target) || immutableRead(target)))(all)
      }
    )
  }

  // An immutable val implements a method by a read, which has no effect.
  private def immutableRead(member: Symbol)(using Context): Boolean =
    !member.is(Flags.Method) && !member.is(Flags.Mutable)

  // A lazy val runs its initialiser on the first read.
  private def stableRead(member: Symbol)(using Context): Boolean =
    immutableRead(member) && !member.is(Flags.Lazy)

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
  def record(callee: Symbol, caller: Option[Symbol], node: Option[String], receiver: Type, settled: Boolean)(using
      Context
  ): Unit =
    if (
      // A val is read through a getter, which dispatches like any other method.
      callee.isTerm && !callee.is(Flags.Module) && !callee.isConstructor && callee.owner.isClass &&
      !callee.isEffectivelyFinal && !callee.is(Flags.Private) && !universal(callee.owner) &&
      !callee.owner.derivesFrom(requiredClass("scala.Equals"))
    ) for {
      method <- caller
      key    <- node
    } dispatches((method, callee)) = dispatches.get((method, callee)) match {
      case Some(previous @ (_, _, false)) if settled => previous
      case Some((_, _, before))                      => (key, receiver, settled && before)
      case None                                      => (key, receiver, settled)
    }
}
