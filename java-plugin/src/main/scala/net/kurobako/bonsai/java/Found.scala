package net.kurobako.bonsai.java

import com.sun.source.tree.*

/** What a lint finds in the code of a tree, which the analysis reports as a finding of its own.
  *
  * @param detail
  *   what is wrong with the code
  * @param at
  *   the code the finding is about, which is the code the lint was asked about unless it says otherwise
  * @param offset
  *   where in the file the finding is placed, for a finding that is of no tree of its own such as a comment, or none
  * @param id
  *   the id the finding is reported under, for a lint that reports under several, or empty for the id of the lint
  */
case class Found(detail: String, at: Tree, offset: Int = -1, id: String = "") {

  /** The id the finding is reported under, which the lint that found it is the default of. */
  def reportedAs(lint: Lint): String = if (id.isEmpty) lint.id else id
}
