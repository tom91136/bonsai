package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol

/** Methods returning an effect that run again until the effect stops: one that continues into itself through `flatMap`
  * or `>>`, repeats itself with `foreverM`, or steps with `tailRecM`. Each pass of such a loop runs the whole body, as
  * a pass of a `while` does. Only a continuation that names the method itself counts, so a recursion through another
  * method, mutual recursion among them, is not found.
  */
private[analysis] object Recursion {
  private val Continuing = Set("flatMap", ">>")
  private val Repeating  = Set("foreverM", "tailRecM")

  /** Whether `method`, whose body is `body`, loops. */
  def loop(method: Symbol, body: tpd.Tree, library: Library)(using Context): Boolean =
    method.is(Flags.Method) && library.deferred(method.info) && body.existsSubTree(continues(_, method, library))

  private def continues(tree: tpd.Tree, method: Symbol, library: Library)(using Context): Boolean = tree match {
    case application: tpd.Apply =>
      Continuing(called(application.fun)) && library.deferred(application.tpe) &&
      application.args.exists(refers(_, method))
    case reference @ (tpd.Ident(_) | tpd.Select(_, _)) => Repeating(reference.symbol.name.decode.toString)
    case _                                             => false
  }

  private def refers(tree: tpd.Tree, method: Symbol)(using Context): Boolean = tree.existsSubTree {
    case reference @ (tpd.Ident(_) | tpd.Select(_, _)) => reference.symbol == method
    case _                                             => false
  }

  @annotation.tailrec
  private def called(function: tpd.Tree)(using Context): String = function match {
    case tpd.Apply(inner, _)     => called(inner)
    case tpd.TypeApply(inner, _) => called(inner)
    case other                   => other.symbol.name.decode.toString
  }
}
