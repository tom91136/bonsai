package net.kurobako.bonsai.plugin

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.CodeAction
import dotty.tools.dotc.reporting.ErrorMessageID
import dotty.tools.dotc.reporting.Message
import dotty.tools.dotc.reporting.MessageKind

/** A message with code actions for editors to offer. `-rewrite` applies them for lint warnings in some compilers only,
  * so the checks apply them themselves.
  */
private class Fixable(text: String, fixes: List[CodeAction])(using Context)
    extends Message(ErrorMessageID.NoExplanationID) {
  def kind: MessageKind                                 = MessageKind.NoKind
  def msg(using Context): String                        = text
  def explain(using Context): String                    = ""
  override def actions(using Context): List[CodeAction] = fixes
}
