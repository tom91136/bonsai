package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scala.meta.tokens.Token
import scalafix.v1.*

/** Braces around a simple interpolated name are redundant when the text after them cannot continue the name.
  *
  * `s"${name}.size()"` and `s"$name.size()"` are the same string, while `s"${name}_suffix"` needs the braces because
  * `$name_suffix` would name one identifier.
  */
class InterpolationIdioms extends SyntacticRule("InterpolationIdioms") {
  private val Interpolators = Set("s", "raw")

  override def fix(using document: SyntacticDocument): Patch = document.tree.collect {
    case interpolation: Term.Interpolate if Interpolators(interpolation.prefix.value) =>
      interpolation.args.flatMap {
        case block @ Term.Block(List(name: Term.Name))
            if !name.value.startsWith("`") && safe(document.input.text, name) =>
          val open = interpolation.tokens.collectFirst {
            case token: Token.LeftBrace if token.start == block.pos.start => token
          }
          val close = interpolation.tokens.collectFirst {
            case token: Token.RightBrace if token.end == block.pos.end => token
          }
          for {
            left  <- open
            right <- close
          } yield Patch.removeToken(left) + Patch.removeToken(right) +
            lint("redundant-interpolation", s"the braces around ${name.value} are redundant", name.pos)
        case _ => None
      }.asPatch
  }.asPatch

  /** Whether the text after the closing brace cannot be read as part of the interpolated name. */
  private def safe(text: String, name: Term.Name): Boolean = {
    val close = text.indexOf('}', name.pos.end)
    close >= 0 && text.lift(close + 1).exists { character =>
      !(character.isLetterOrDigit || character == '_')
    }
  }
}
