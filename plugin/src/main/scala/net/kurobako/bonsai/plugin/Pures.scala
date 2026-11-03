package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.Type
import net.kurobako.bonsai.lifting.*

/** Expressions that read immutable values and call the standard library without effects, which evaluating later, once
  * or not at all changes nothing: no variable, mutable collection or iterator is involved, and no method that fails on
  * some input, such as `get` and `head`, or division by anything but a literal, is called.
  */
private object Pures {
  private val Failing = Set(
    "get",
    "head",
    "last",
    "tail",
    "init",
    "apply",
    "reduce",
    "reduceLeft",
    "reduceRight",
    "max",
    "min",
    "maxBy",
    "minBy",
    "toInt",
    "toLong",
    "toShort",
    "toByte",
    "toFloat",
    "toDouble",
    "toBoolean",
    "/",
    "%",
    "div",
    "rem",
    "next",
    "error",
    "sliding",
    "grouped"
  )
  private val Output  = Set("???")
  private val Strings = Set(
    "length",
    "isEmpty",
    "toUpperCase",
    "toLowerCase",
    "trim",
    "contains",
    "startsWith",
    "endsWith",
    "concat",
    "replace",
    "split",
    "equals",
    "hashCode",
    "toString",
    "indexOf",
    "lastIndexOf",
    "+"
  )
  private val Outside = List(
    "scala.io.",
    "scala.sys.",
    "scala.concurrent.",
    "scala.runtime.",
    "scala.util.Random",
    "scala.util.Using",
    "scala.util.Properties",
    "scala.Console",
    "java."
  )

  def apply(tree: tpd.Tree)(using Context): Boolean = !tree.existsSubTree(volatile(_)) && pure(tree)

  private def volatile(tree: tpd.Tree)(using Context): Boolean = tree.isTerm && outside(tree.tpe.widen)

  private def outside(tpe: Type)(using Context): Boolean = {
    val symbol = tpe.dealias.classSymbol
    symbol.exists && {
      val name = Calls.qualified(symbol)
      name != "java.lang.String" && Outside.exists(name.startsWith(_)) || Purity.stateful(tpe)
    }
  }

  private def pure(tree: tpd.Tree)(using Context): Boolean = Calls.lambda(tree) match {
    case Some(definition) => pure(definition.rhs)
    case None             =>
      tree match {
        case _: (tpd.Literal | tpd.This)       => true
        case tpd.Typed(expression, _)          => pure(expression)
        case tpd.Inlined(_, Nil, inlined)      => pure(inlined)
        case tpd.Block(statements, expression) =>
          statements.forall {
            case value: tpd.ValDef => !value.symbol.isOneOf(Flags.Mutable | Flags.Lazy) && pure(value.rhs)
            case _                 => false
          } && pure(expression)
        case tpd.If(condition, left, right)                          => pure(condition) && pure(left) && pure(right)
        case _: (tpd.Ident | tpd.Select | tpd.Apply | tpd.TypeApply) => reference(tree)
        case _                                                       => false
      }
  }

  private def reference(tree: tpd.Tree)(using Context): Boolean = tree match {
    case tpd.Apply(function, arguments)    => reference(function) && arguments.forall(pure(_))
    case tpd.TypeApply(function, _)        => reference(function)
    case select @ tpd.Select(qualifier, _) =>
      (if (select.symbol.is(Flags.Method)) allowed(select.symbol) else field(select.symbol)) &&
      (qualifier.isInstanceOf[tpd.New] || pure(qualifier))
    case identifier: tpd.Ident =>
      if (identifier.symbol.is(Flags.Method)) allowed(identifier.symbol) else field(identifier.symbol)
    case _ => false
  }

  private def field(symbol: Symbol)(using Context): Boolean = {
    val name = symbol.name.toString
    Purity.Syntactic.value(symbol) || symbol.owner.exists && defn.isTupleClass(symbol.owner) && name.startsWith("_")
  }

  private def allowed(symbol: Symbol)(using Context): Boolean = {
    val owner = Calls.qualified(symbol.owner)
    val name  = symbol.name.toString
    symbol.exists && !symbol.info.finalResultType.isRef(defn.UnitClass) && !Output(name) &&
    (if (owner == "java.lang.String") Strings(name)
     else
       owner.startsWith("scala.") && !Outside.exists(owner.startsWith(_)) &&
       (!Failing(name) || name == "apply" && symbol.owner.is(Flags.ModuleClass)))
  }
}
