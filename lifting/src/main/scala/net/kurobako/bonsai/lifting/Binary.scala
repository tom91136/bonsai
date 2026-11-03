package net.kurobako.bonsai.lifting

import dotty.tools.dotc.ast.tpd

object Binary {
  def unapply(tree: tpd.Tree): Option[Binary] = tree match {
    case tpd.Apply(tpd.Select(left, name), List(right)) => Some(Binary(tree, left, name.toString, right))
    // From Scala 3.10 `until` and `to` on `Int` are extension methods of its companion, applied to the left in a clause
    // of their own.
    case tpd.Apply(tpd.Apply(tpd.Select(_, name), List(left)), List(right)) =>
      Some(Binary(tree, left, name.toString, right))
    case _ => None
  }
}

/** A call of a binary operator: `tree` is `left operator right`. */
class Binary(val tree: tpd.Tree, val left: tpd.Tree, val operator: String, val right: tpd.Tree)
