package net.kurobako.bonsai.lifting

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.Type

/** The patterns of generators and of the functions the compiler writes for them. */
private[lifting] object PatternShapes {
  private def ignored(pattern: tpd.Tree): Boolean = pattern match {
    case tpd.Ident(name) => name.toString == "_"
    case _               => false
  }

  def bound(pattern: tpd.Tree): Boolean = pattern match {
    case bind: tpd.Bind => ignored(bind.body)
    case _              => false
  }

  def simple(pattern: tpd.Tree): Boolean = ignored(pattern) || bound(pattern)

  /** Whether `pattern` matches every value of the type `scrutinee`: a wildcard, a binding, and a tuple or case class of
    * such, but not a type test or an extractor of a type with other cases.
    */
  def irrefutable(pattern: tpd.Tree, scrutinee: Type)(using Context): Boolean = pattern match {
    case tpd.Ident(name)                    => name.toString == "_"
    case tpd.Bind(_, inner)                 => irrefutable(inner, scrutinee)
    case tpd.Typed(inner, tpt)              => scrutinee <:< tpt.tpe && irrefutable(inner, scrutinee)
    case tpd.UnApply(function, _, patterns) =>
      val symbol = pattern.tpe.widen.classSymbol
      val tuple  = defn.isTupleClass(symbol)
      symbol.exists && function.symbol.name.toString == "unapply" && function.symbol.owner.companionClass == symbol &&
      (tuple || symbol.is(Flags.CaseClass) && function.symbol.is(Flags.Synthetic)) &&
      scrutinee.widen <:< pattern.tpe.widen && {
        val components = scrutinee.widen.dealias.argInfos
        if (tuple && components.sizeIs == patterns.size) patterns.zip(components).forall(irrefutable(_, _))
        else patterns.forall(simple(_))
      }
    case _ => false
  }
}
