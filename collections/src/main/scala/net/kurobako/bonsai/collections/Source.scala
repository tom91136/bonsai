package net.kurobako.bonsai.collections

import scala.annotation.tailrec
import scala.collection.mutable

/** Small facts about source text that rewrites need, without parsing it. */
object Source {

  private val looserThanOr =
    """=>|\bif\b|\bmatch\b|\belse\b|\btry\b|\bthrow\b|\bwhile\b|\byield\b|\bfor\b|(?<![=!<>])=(?![=>])""".r
  private val looserThanAnd = ("""\|\|""" + "|" + looserThanOr.regex).r

  /** `text` with everything inside brackets and string literals replaced by underscores. */
  def outline(text: String): String = {
    val out = mutable.StringBuilder(text.length)
    @tailrec def scan(index: Int, depth: Int, quoted: Boolean, escaped: Boolean): Unit = if (index < text.length) {
      val c = text.charAt(index)
      if (quoted) {
        out += '_'
        scan(index + 1, depth, !(!escaped && c == '"'), !escaped && c == '\\')
      } else {
        c match {
          case '"'             => out += '_'; scan(index + 1, depth, true, false)
          case '(' | '[' | '{' => out += '_'; scan(index + 1, depth + 1, false, false)
          case ')' | ']' | '}' => out += '_'; scan(index + 1, depth - 1, false, false)
          case _               => out += (if (depth > 0) '_' else c); scan(index + 1, depth, false, false)
        }
      }
    }
    scan(0, 0, false, false)
    out.toString
  }

  /** `text` as an operand of `&&` or `||`, parenthesised when it binds looser than the operator. */
  def operand(text: String, operator: String): String = {
    val loose = if (operator == "&&") looserThanAnd else looserThanOr
    if (loose.findFirstIn(outline(text)).isEmpty) text else s"($text)"
  }

  /** `text` as the argument of a function written as a template over its parameter, parenthesised unless it is a single
    * token or call.
    */
  def argument(text: String): String = {
    val flat = outline(text)
    if (flat.exists(_.isWhitespace) || flat.headOption.exists("!-+~".contains(_))) s"($text)" else text
  }
}
