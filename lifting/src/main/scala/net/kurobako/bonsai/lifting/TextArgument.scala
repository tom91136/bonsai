package net.kurobako.bonsai.lifting

import net.kurobako.bonsai.collections.*

/** An argument that is rewritten source text rather than a tree; it is never taken to be pure. */
private[lifting] class TextArgument(val text: String) extends Arg {
  def pure: Boolean = false

  def applied(value: String): String = s"($text)($value)"
}
