package net.kurobako.bonsai.lifting

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.Type
import net.kurobako.bonsai.collections.Kind

/** What the laws need to know of a type, read from the type itself. */
private[lifting] class Kinds(using Context) {
  private val ArrayOps     = Classes.required("scala.collection.ArrayOps")
  private val StringOps    = Classes.required("scala.collection.StringOps")
  private val IterableOnce = Classes.required("scala.collection.IterableOnce")
  private val Iterable     = Classes.required("scala.collection.Iterable")
  private val View         = Classes.required("scala.collection.View")
  private val LazyList     = Classes.required("scala.collection.immutable.LazyList")
  private val Iterator     = Classes.required("scala.collection.Iterator")
  private val SeqOps       = Classes.required("scala.collection.SeqOps")
  private val SeqMap       = Classes.required("scala.collection.SeqMap")
  private val SortedOps    = Classes.required("scala.collection.SortedOps")
  private val MapClass     = Classes.required("scala.collection.Map")
  private val SetClass     = Classes.required("scala.collection.Set")
  private val Product      = Classes.required("scala.Product")

  private val Insertion = Set("immutable.ListSet", "immutable.VectorSet", "mutable.LinkedHashSet")

  private val Operations: Vector[Symbol] = Vector(
    Classes.required("scala.collection.IterableOnceOps"),
    ArrayOps,
    StringOps,
    defn.OptionClass,
    defn.ArrayClass,
    defn.PartialFunctionClass
  )

  val scalar: Kind = Kind(library = false)

  /** A receiver known only to be called with a standard collection method. */
  private val opaque: Kind = Kind(library = true, strict = false, ordered = false)

  /** The pair of collections `unzip` gives, which a component then selects from. */
  val pair: Kind = Kind(library = true, ordered = false)

  /** Whether `symbol`, the method of a call, is one the standard collections define. */
  def library(symbol: Symbol): Boolean = symbol.exists && {
    val owner = symbol.owner
    owner.isClass && inScala(owner) && Operations.exists(owner.derivesFrom(_))
  }

  /** Whether `symbol` is a method of the `WithFilter` a guard of a for-comprehension gives. */
  def guarded(symbol: Symbol): Boolean = symbol.exists && symbol.owner.name.toString == "WithFilter" &&
    inScala(symbol.owner)

  private def inScala(symbol: Symbol): Boolean = symbol.fullName.toString.startsWith("scala.")

  def tuple(tpe: Type): Boolean = tpe.widenDealias.classSymbol.derivesFrom(Product) &&
    tpe.widenDealias.classSymbol.fullName.toString.startsWith("scala.Tuple")

  /** The kind of a value of type `tpe`, when it is a collection, an option, an array or a string. */
  def of(tpe: Type): Option[Kind] = {
    val widened = tpe.widenTermRefExpr.finalResultType.widenDealias
    val array   = widened.derivesFrom(defn.ArrayClass) || widened.derivesFrom(ArrayOps)
    val string  = widened.derivesFrom(defn.StringClass) || widened.derivesFrom(StringOps)
    val option  = widened.derivesFrom(defn.OptionClass)
    Option.when(array || string || option || widened.derivesFrom(IterableOnce)) {
      val symbol   = widened.typeSymbol
      val concrete = symbol.isClass
      val name     = if (concrete) standardName(symbol, array, string) else ""
      val lazily   = widened.derivesFrom(View) || widened.derivesFrom(LazyList) || widened.derivesFrom(Iterator)
      val set      = widened.derivesFrom(SetClass)
      if (!concrete && !array && !string) opaque
      else if (symbol == Iterable || symbol == IterableOnce) opaque.copy(name = name)
      else
        Kind(
          library = inScala(symbol) || array || string,
          strict = !lazily,
          ordered = array || string || option || widened.derivesFrom(Iterator) || widened.derivesFrom(SeqOps) ||
            widened.derivesFrom(SeqMap) || widened.derivesFrom(SortedOps) || set && Insertion(name),
          distinct = set,
          keyed = widened.derivesFrom(MapClass),
          single = option,
          name = name
        )
    }
  }

  private def standardName(symbol: Symbol, array: Boolean, string: Boolean): String =
    if (array) "Array"
    else if (string) "String"
    else Calls.qualified(symbol).stripPrefix("scala.collection.").stripPrefix("scala.")
}
