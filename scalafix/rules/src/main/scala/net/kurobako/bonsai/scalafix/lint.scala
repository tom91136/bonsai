package net.kurobako.bonsai.scalafix

import scala.meta.*
import scalafix.v1.*

private object lint {
  def apply(id: String, message: String, position: Position): Patch = Patch.lint(Diagnostic(id, message, position))

  def replace(tree: Tree, replacement: String, id: String, message: String): Patch =
    Patch.replaceTree(tree, replacement) + apply(id, message, tree.pos)
}
