package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import scala.collection.mutable

private object Fields {
  val Checks: Set[Check] = Set(Check.WriteOnlyField)
}

/** Reports private fields nothing reads for a purpose: written, checked by `require` or `assert` as the object is
  * built, or shown and compared by the methods describing the object, but never otherwise used. Every use of a private
  * field is in its unit. Constructor parameters are left alone, since one only checked as the object is built is not
  * kept.
  */
private class Fields(checks: Checks)(using Context) extends tpd.TreeTraverser {
  private enum Read {
    case Written, Checked, Described
  }

  private val Descriptions = Set("toString", "equals", "hashCode", "canEqual")
  private val Validations  = Set("require", "assert", "assume")

  private val candidates = mutable.Set.empty[Symbol]
  private val reads      = mutable.Map.empty[Symbol, mutable.Set[Read]]
  private val used       = mutable.Set.empty[Symbol]
  // The definitions around the tree being traversed, innermost first, and the fields assignments around it update; a
  // field read only to compute its own next value is still never read.
  private val enclosing = mutable.Stack.empty[Symbol]
  private val updating  = mutable.Stack.empty[Symbol]
  private val checked   = mutable.Stack.empty[Option[Symbol]]

  def run(tree: tpd.Tree): Unit = {
    candidates ++= tree.filterSubTrees {
      case field: tpd.ValDef =>
        val symbol = field.symbol
        symbol.exists && symbol.owner.isClass && !symbol.owner.is(Flags.Case) && symbol.is(Flags.Private) &&
        !symbol.isOneOf(Flags.Synthetic | Flags.ParamAccessor | Flags.Module | Flags.Given)
      case _ => false
    }.map(_.symbol)
    if (candidates.nonEmpty) {
      traverse(tree)
      candidates.filterNot(used).foreach(field =>
        reads.get(field).filter(_.nonEmpty).foreach(purposes =>
          checks(
            Check.WriteOnlyField,
            if (purposes == Set(Read.Written)) s"${field.name} is written but never read; remove it"
            else
              s"${field.name} is only ${purposes.map(_.toString.toLowerCase).toList.sorted.mkString(" and ")}, " +
                "never used; remove it, or use it",
            field.srcPos
          )
        )
      )
    }
  }

  override def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
    case definition: tpd.MemberDef if definition.symbol.exists =>
      enclosing.push(definition.symbol)
      try traverseChildren(tree)
      finally enclosing.pop(): Unit
    case tpd.Assign(target, value) if candidates(target.symbol) =>
      record(target.symbol, Read.Written)
      target match {
        case tpd.Select(qualifier, _) => traverse(qualifier)
        case _                        => ()
      }
      updating.push(target.symbol)
      try traverse(value)
      finally updating.pop(): Unit
    case application: tpd.Apply if validation(application.fun.symbol)        => checking(tree)
    case inlined: tpd.Inlined if validation(inlined.call.symbol)             => checking(tree)
    case reference: (tpd.Ident | tpd.Select) if candidates(reference.symbol) =>
      val field = reference.symbol
      if (updating.contains(field)) record(field, Read.Written)
      else purpose(field).fold(used += field: Unit)(record(field, _))
      traverseChildren(tree)
    case _ => traverseChildren(tree)
  }

  private def record(field: Symbol, purpose: Read): Unit =
    reads.getOrElseUpdate(field, mutable.Set.empty) += purpose: Unit

  private def validation(symbol: Symbol)(using Context): Boolean =
    Validations(symbol.name.toString) && symbol.owner.name.toString.startsWith("Predef")

  // A check in the statements of a class runs once, as the object is built.
  private def checking(tree: tpd.Tree)(using Context): Unit = {
    checked.push(enclosing.headOption.filter(_.isClass))
    try traverseChildren(tree)
    finally checked.pop(): Unit
  }

  // Reads in a check the class makes as it is built, outside the functions the check passes on, or in the methods
  // describing an object.
  private def purpose(field: Symbol): Option[Read] =
    if (checked.headOption.flatten.contains(field.owner) && enclosing.headOption.contains(field.owner))
      Some(Read.Checked)
    else
      enclosing.find(_.owner == field.owner).filter(definition => Descriptions(definition.name.toString))
        .map(_ => Read.Described)
}
