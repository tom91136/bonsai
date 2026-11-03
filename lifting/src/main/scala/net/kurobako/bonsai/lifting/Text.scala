package net.kurobako.bonsai.lifting

import net.kurobako.bonsai.collections.*

/** Small facts about source text, which rewrites need without parsing it. */
private[lifting] object Text {

  /** Whether a call can follow `text` directly: it has no operator or keyword outside brackets, and whitespace only
    * beside a dot.
    */
  private def selectable(text: String): Boolean = {
    val flat = Source.outline(text)
    val bare = flat.zipWithIndex.forall { (c, at) =>
      !c.isWhitespace || flat.take(at).reverse.dropWhile(_.isWhitespace).headOption.contains('.') ||
      flat.drop(at).dropWhile(_.isWhitespace).headOption.contains('.')
    }
    bare && !flat.headOption.exists("!-+~".contains(_)) && !flat.startsWith("new ")
  }

  /** `text` written so a call can follow it. */
  def operand(text: String): String = if (selectable(text)) text else s"($text)"
}
