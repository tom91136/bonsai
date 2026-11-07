package net.kurobako.bonsai.analysis

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.AppliedType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeParamRef

/** What the cost model knows of library types. */
private class Library(using Context) {
  private val IterableOnce   = requiredClass("scala.collection.IterableOnce")
  private val ArrayOps       = requiredClass("scala.collection.ArrayOps")
  private val StringOps      = requiredClass("scala.collection.StringOps")
  private val LinearSeq      = requiredClass("scala.collection.LinearSeq")
  private val Seq            = requiredClass("scala.collection.Seq")
  private val Range          = requiredClass("scala.collection.immutable.Range")
  private val ListBuffer     = requiredClass("scala.collection.mutable.ListBuffer")
  private val ArrayBuffer    = requiredClass("scala.collection.mutable.ArrayBuffer")
  private val List           = requiredClass("scala.collection.immutable.List")
  private val Vector         = requiredClass("scala.collection.immutable.Vector")
  private val ImmSeq         = requiredClass("scala.collection.immutable.Seq")
  private val ImmIndexed     = requiredClass("scala.collection.immutable.IndexedSeq")
  private val ImmSet         = requiredClass("scala.collection.immutable.Set")
  private val ImmMap         = requiredClass("scala.collection.immutable.Map")
  private val MutableIter    = requiredClass("scala.collection.mutable.Iterable")
  private val JavaCollection = requiredClass("java.util.Collection")
  private val JavaMap        = requiredClass("java.util.Map")
  private val Traversals     = Set(IterableOnce, defn.ArrayClass, ArrayOps, StringOps)
  private val FlatMap        = termName("flatMap")

  // Containers whose flatMap runs at once, so a recursion through it is recursion on the stack and no loop.
  private val Strict = Set(
    IterableOnce,
    defn.ArrayClass,
    defn.OptionClass,
    requiredClass("scala.util.Either"),
    requiredClass("scala.util.Try"),
    requiredClass("java.util.Optional")
  )

  private def derives(tpe: Type, cls: Symbol): Boolean = tpe.widen.derivesFrom(cls)

  /** Collections and arrays, but not options. */
  def sized(tpe: Type): Boolean =
    !derives(tpe, defn.OptionClass) && (derives(tpe, IterableOnce) || derives(tpe, defn.ArrayClass))

  def text(tpe: Type): Boolean = derives(tpe, defn.StringClass) || derives(tpe, StringOps)

  /** Whether values of a type can be traversed: collections and arrays, but not options. */
  def traversal(tpe: Type): Boolean =
    !derives(tpe, defn.OptionClass) && Traversals.exists(derives(tpe, _))

  /** Whether a value of the type, or of the result type of a method, is an effect: a computation that runs later, which
    * is a type with a `flatMap` that no strict container is, or an abstract type constructor as a tagless `F[_]` is.
    */
  def deferred(tpe: Type): Boolean = {
    val result = tpe.widenTermRefExpr.finalResultType.widenDealias
    !Strict.exists(derives(result, _)) &&
    (result match {
      case applied: AppliedType =>
        applied.tycon match {
          case TypeParamRef(_, _) => true
          case tycon => tycon.typeSymbol.isOneOf(Flags.TypeParam | Flags.Deferred) || result.member(FlatMap).exists
        }
      case _ => result.member(FlatMap).exists
    })
  }

  def linked(tpe: Type): Boolean = derives(tpe, LinearSeq)

  def array(tpe: Type): Boolean = derives(tpe, defn.ArrayClass) || derives(tpe, ArrayOps)

  def javaContainer(tpe: Type): Boolean = derives(tpe, JavaCollection) || derives(tpe, JavaMap)

  def growable(tpe: Type): Boolean = derives(tpe, MutableIter)

  def listBuffer(tpe: Type): Boolean = derives(tpe, ListBuffer)

  def arrayBuffer(tpe: Type): Boolean = derives(tpe, ArrayBuffer)

  def searchable(tpe: Type): Boolean = (derives(tpe, Seq) || derives(tpe, ArrayOps)) && !derives(tpe, Range)

  /** Whether converting with `name` hands the receiver back as it is. */
  def unchanged(name: String, tpe: Type): Boolean = name match {
    case "toList"       => derives(tpe, List)
    case "toVector"     => derives(tpe, Vector)
    case "toSeq"        => derives(tpe, ImmSeq)
    case "toIndexedSeq" => derives(tpe, ImmIndexed)
    case "toSet"        => derives(tpe, ImmSet)
    case "toMap"        => derives(tpe, ImmMap)
    case _              => false
  }
}
