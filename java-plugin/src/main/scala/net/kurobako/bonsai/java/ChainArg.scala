package net.kurobako.bonsai.java

import net.kurobako.bonsai.collections.*

/** An argument of a step, which is a function written in place, a reference to a method or a plain expression. */
private class ChainArg(
    val text: String,
    val pure: Boolean,
    override val identity: Boolean,
    override val literal: Option[Any],
    override val comparison: Option[Comparison] = None
) extends Arg {
  def applied(value: String): String = if (identity) value else s"($text).apply($value)"
}
