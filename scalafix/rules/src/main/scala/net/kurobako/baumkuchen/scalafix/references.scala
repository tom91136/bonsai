package net.kurobako.baumkuchen.scalafix

import scala.meta.*

private object references {
  // Scala 3 can attach the same symbol to both a selection and its final name
  def isSelectionName(reference: Tree): Boolean = reference.parent match {
    case Some(selection: Term.Select) => selection.name == reference
    case Some(selection: Type.Select) => selection.name == reference
    case _                            => false
  }
}
