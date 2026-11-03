package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.AppliedType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeTraverser
import net.kurobako.bonsai.lifting.*
import scala.collection.mutable

/** The reads of the fields of a case class that showing, comparing and hashing a value make, passed to `use` as uses of
  * the fields.
  */
private[analysis] class Reads(symbols: Symbols, use: (Symbol, Option[String]) => Context ?=> Unit) {
  import symbols.*

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
      case _ if Shows(named) => Predicates.spread(application.args)
      case _                 => Nil
    }
    read(values.map(_.tpe), from)
  }

  /** Records the field a `selectDynamic` call with a literal name reads on a receiver whose type is built from the case
    * class declaring it.
    */
  def selected(application: tpd.Apply, from: Option[String])(using Context): Unit = application match {
    case tpd.Apply(tpd.Select(receiver, name), List(tpd.Literal(Constant(field: String))))
        if name == nme.selectDynamic =>
      val target = termName(field)
      Derivations.cases(receiver.tpe).foreach(
        _.info.decls.filter(member => member.is(Flags.ParamAccessor) && member.name == target).foreach(use(_, from))
      )
    case _ => ()
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
        case "distinct" => select.qualifier.tpe.widen.derivesFrom(Classes.required("scala.collection.Iterable"))
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
    derivesFromAny(cls, "scala.collection.Map", "scala.collection.Set", "java.util.Map", "java.util.Set")

  private def sorted(cls: Symbol)(using Context): Boolean =
    derivesFromAny(
      cls,
      "scala.collection.SortedMap",
      "scala.collection.SortedSet",
      "java.util.SortedMap",
      "java.util.SortedSet"
    )

  private def derivesFromAny(cls: Symbol, names: String*)(using Context): Boolean =
    names.exists(name => cls.derivesFrom(Classes.required(name)))

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
      if (cls.is(Flags.CaseClass))
        cls.info.decls.filter(_.is(Flags.ParamAccessor)).foreach { field =>
          use(field, from)
          showType(field.info.finalResultType)
        }
    }
    types.foreach(showType(_))
  }
}
