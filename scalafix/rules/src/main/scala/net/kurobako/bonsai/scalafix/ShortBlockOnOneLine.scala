package net.kurobako.bonsai.scalafix

import scala.meta.*
import scala.meta.tokens.Token
import scalafix.v1.*

/** Writes an if or else block of one statement and one result on one line, separated by a semicolon, when it fits.
  *
  * Blocks with comments, nested blocks, or other than two single-line terms are left alone. So is a block whose opening
  * brace starts its line, which scalafmt joins back onto the keyword, and a block that shares its line with the other
  * branch. The then-branch of an `if` with an `else` is left alone too, so `else` never follows a one-line block.
  */
class ShortBlockOnOneLine extends SyntacticRule("ShortBlockOnOneLine") {
  private val maxColumn = 120

  override def fix(using document: SyntacticDocument): Patch = document.tree.collect { case branch: Term.If =>
    val branches = if (hasElse(branch)) List(branch.elsep) else List(branch.thenp, branch.elsep)
    val replaced = branches.flatMap {
      case block: Term.Block => oneLine(block).map(Patch.replaceTree(block, _))
      case _                 => None
    }
    (replaced ++ Option.when(replaced.nonEmpty)(lint(
      "one-line-block",
      "write the block on one line, separating the statement from the result with a semicolon",
      branch.pos
    ))).asPatch
  }.asPatch

  // a missing else is a zero-width unit, so an explicit else () still counts
  private def hasElse(branch: Term.If): Boolean = branch.elsep.pos.start != branch.elsep.pos.end

  private def oneLine(block: Term.Block): Option[String] = block.stats match {
    case List(statement: Term, result: Term)
        if block.pos.startLine != block.pos.endLine &&
          statement.pos.startLine == statement.pos.endLine && result.pos.startLine == result.pos.endLine &&
          !block.stats.exists(_.collect { case _: Term.Block => true }.nonEmpty) &&
          !block.tokens.exists { case _: Token.Comment => true; case _ => false } =>
      val text             = s"{ ${statement.pos.text}; ${result.pos.text} }"
      val (prefix, suffix) = surroundings(block)
      Option.when(
        prefix.trim.nonEmpty && !prefix.contains('}') && !suffix.contains('{') &&
          prefix.length + text.length + suffix.length <= maxColumn
      )(text)
    case _ => None
  }

  private def surroundings(block: Term.Block): (String, String) = {
    val chars     = block.pos.input.chars
    val lineStart = chars.lastIndexOf('\n', block.pos.start - 1) + 1
    val lineEnd   = chars.indexOf('\n', block.pos.end) match {
      case -1  => chars.length
      case end => end
    }
    (
      String(chars, lineStart, block.pos.start - lineStart),
      String(chars, block.pos.end, lineEnd - block.pos.end)
    )
  }
}
