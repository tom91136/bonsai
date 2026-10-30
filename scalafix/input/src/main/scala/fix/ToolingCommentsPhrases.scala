/*
rule = ToolingComments
ToolingComments.phrases = ["would\\s+otherwise", "\\bthe\\s+expansion\\b"]
 */
package fix

object ToolingCommentsPhrases {
  val Bound = 1 /* assert: ToolingComments.tooling-comment*/ // built once, which would otherwise happen twice

  val Expanded = 2 /* assert: ToolingComments.tooling-comment*/ // bound outside the expansion

  val Linted = 3 /* assert: ToolingComments.tooling-comment*/ // the linter stays quiet

  // the polynomial is evaluated once per lane
  val Plain = 4
}
