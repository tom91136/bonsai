package net.kurobako.bonsai.java

import com.sun.source.tree.*
import net.kurobako.bonsai.collections.*

/** A chain of calls as the laws see it, with the expression that runs each step: `ends(i)` is the call, or the
  * comparison or negation of one, that completes step `i`. A chain whose first call names no receiver runs on `this`,
  * which `chain.source` then stands for.
  */
case class Lifted(chain: Chain, ends: Vector[ExpressionTree], implicitThis: Boolean = false) {

  /** The text of the expression the step `from` runs on. */
  def receiver(from: Int): String = if (from == 0) chain.source.text else ends(from - 1).toString
}
