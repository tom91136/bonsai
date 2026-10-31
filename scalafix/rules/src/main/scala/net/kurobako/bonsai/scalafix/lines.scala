package net.kurobako.bonsai.scalafix

import scala.meta.*
import scala.meta.tokens.Token
import scalafix.v1.*

private object lines {

  /** Removes the tokens from `first` to `last` with the whitespace that would leave a gap: the whole line when they are
    * alone on it, else the whitespace on one side.
    */
  def remove(tokens: Vector[Token], first: Int, last: Int): Patch = {
    val start      = tokens.lastIndexWhere(!_.is[Token.HSpace], first - 1) + 1
    val end        = tokens.indexWhere(!_.is[Token.HSpace], last + 1)
    val leading    = tokens(start - 1).is[Token.AtEOL] || tokens(start - 1).is[Token.BOF]
    val trailing   = tokens(end).is[Token.AtEOL] || tokens(end).is[Token.EOF]
    val blanks     = tokens.indexWhere(!_.is[Token.Whitespace], last + 1)
    val lineEnd    = tokens.lastIndexWhere(_.is[Token.AtEOL], blanks - 1)
    val (from, to) =
      if (leading && trailing) (start, lineEnd)
      else if (leading) (first, end - 1)
      else (start, last)
    Patch.removeTokens(tokens.slice(from, to + 1))
  }
}
