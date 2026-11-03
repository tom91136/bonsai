package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import net.kurobako.bonsai.lifting.Calls
import scala.collection.mutable

/** Methods that answer from a map held by their object, and compute and store the answer on a miss. A recursive search
  * written this way does its work once per key over the life of the map, however many times it is called.
  *
  * A method is memoised when its body is `map.getOrElse(key, miss(key))` or `map.getOrElseUpdate(key, miss)`, where
  * `map` is an immutable member that outlives the call, `key` is a parameter of the method, and the miss reaches the
  * method again. For `getOrElse` the miss must also be a call of a method of the build that passes `key` on and stores
  * its own parameter in `map`. A miss that stores the key further down the call chain, or stores something derived from
  * it, is not recognised.
  */
private class Memos(definitions: collection.Map[Symbol, tpd.DefDef], subtrees: Children)(using Context) {
  private val Lookups = Set("getOrElse", "getOrElseUpdate")
  private val Stores  = Set("update", "put", "getOrElseUpdate")

  private val known = mutable.Map.empty[Symbol, Boolean]

  def apply(method: Symbol): Boolean =
    known.getOrElseUpdate(method, method.owner.isClass && definitions.get(method).exists(memoised(method, _)))

  /** Whether the object a call is made on lives past every loop of the caller. */
  def outliving(qualifier: tpd.Tree): Boolean = Calls.peel(qualifier) match {
    case tpd.This(_) => true
    case reference   => reference.symbol.isStatic || member(reference)
  }

  private def member(tree: tpd.Tree): Boolean = tree match {
    case select: tpd.Select =>
      select.symbol.owner.isClass && !select.symbol.is(Flags.Mutable) && outliving(select.qualifier)
    case _ => false
  }

  private def memoised(method: Symbol, definition: tpd.DefDef): Boolean =
    Calls.peel(definition.rhs) match {
      case application: tpd.Apply =>
        (Calls.selected(application.fun), Calls.arguments(application)) match {
          case (Some(select), List(key, miss))
              if Lookups(select.name.toString) && member(Calls.peel(select.qualifier)) &&
                definition.termParamss.flatten.exists(_.symbol == Calls.peel(key).symbol) =>
            val memo = Calls.peel(select.qualifier).symbol
            (select.name.toString == "getOrElseUpdate" || stores(miss, memo, Calls.peel(key).symbol)) &&
            reaches(miss, method)
          case _ => false
        }
      case _ => false
    }

  private def stores(miss: tpd.Tree, memo: Symbol, key: Symbol): Boolean = Calls.peel(miss) match {
    case application: tpd.Apply =>
      definitions.get(application.fun.symbol).exists(callee =>
        Calls.arguments(application).zip(callee.termParamss.flatten).exists((argument, parameter) =>
          Calls.peel(argument).symbol == key && exists(callee.rhs) {
            case inner: tpd.Apply =>
              Calls.selected(inner.fun).exists(select =>
                Stores(select.name.toString) && Calls.peel(select.qualifier).symbol == memo &&
                  Calls.arguments(inner).headOption.exists(Calls.peel(_).symbol == parameter.symbol)
              )
            case _ => false
          }
        )
      )
    case _ => false
  }

  private def reaches(from: tpd.Tree, target: Symbol): Boolean = {
    val seen                            = mutable.Set.empty[Symbol]
    def search(tree: tpd.Tree): Boolean = exists(tree) {
      case reference: tpd.RefTree =>
        reference.symbol == target ||
        (seen.add(reference.symbol) && definitions.get(reference.symbol).exists(callee => search(callee.rhs)))
      case _ => false
    }
    search(from)
  }

  private def exists(tree: tpd.Tree)(test: tpd.Tree => Boolean): Boolean =
    test(tree) || subtrees.of(tree).exists(exists(_)(test))
}
