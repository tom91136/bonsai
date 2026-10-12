package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

private object lint {
  def apply(id: String, message: String, position: Position): Patch = Patch.lint(Diagnostic(id, message, position))
}
