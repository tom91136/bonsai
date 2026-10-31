package net.kurobako.bonsai.collections

/** Small facts about source text that rewrites need, without parsing it. */
object Source {

  private val looserThanOr =
    """=>|\bif\b|\bmatch\b|\belse\b|\btry\b|\bthrow\b|\bwhile\b|\byield\b|\bfor\b|(?<![=!<>])=(?![=>])""".r
  private val looserThanAnd = ("""\|\|""" + "|" + looserThanOr.regex).r

  /** `text` with everything inside brackets and string literals replaced by underscores. */
  def outline(text: String): String = {
    var depth   = 0
    var quoted  = false
    var escaped = false
    text.map { c =>
      if (quoted) {
        if (escaped) escaped = false else if (c == '\\') escaped = true else if (c == '"') quoted = false
        '_'
      } else
        c match {
          case '"'             => quoted = true; '_'
          case '(' | '[' | '{' => depth += 1; '_'
          case ')' | ']' | '}' => depth -= 1; '_'
          case _               => if (depth > 0) '_' else c
        }
    }
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
