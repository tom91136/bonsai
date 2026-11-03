package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.ExprType
import dotty.tools.dotc.util.Spans.Span
import net.kurobako.bonsai.lifting.*
import scala.collection.mutable

private object Lazies {
  val Checks: Set[Check] = Set(Check.HandWrittenLazy)
}

/** Reports a parameterless method that fills a private variable on its first call and returns it afterwards: guarded by
  * a flag variable that it sets, or by the variable still being `null`. Nothing else writes or reads the variables, so
  * a `lazy val` does the same. The fix replaces the method and the variables with the `lazy val`.
  */
private class Lazies(checks: Checks)(using Context) {
  private val text = Edits.source

  private case class Fill(flag: Option[Symbol], cache: Symbol, value: tpd.Tree)

  def run(tree: tpd.Tree): Unit = {
    val references = mutable.Map.empty[Symbol, List[Span]]
    val variables  = mutable.Map.empty[Symbol, tpd.ValDef]
    val methods    = mutable.ListBuffer.empty[tpd.DefDef]
    tree.foreachSubTree {
      case reference: (tpd.Ident | tpd.Select) if tracked(reference.symbol) =>
        references(reference.symbol) = reference.span :: references.getOrElse(reference.symbol, Nil)
      case variable: tpd.ValDef if tracked(variable.symbol) => variables(variable.symbol) = variable
      case method: tpd.DefDef if candidate(method)          => methods += method: Unit
      case _                                                => ()
    }
    methods.foreach { method =>
      fill(method).foreach { found =>
        val owned  = found.cache :: found.flag.toList
        val inside = owned.forall(variable =>
          variables.contains(variable) && references.getOrElse(variable, Nil).forall(method.span.contains)
        )
        if (inside && initial(found, variables) && !mentions(found.value, owned)) report(method, found, variables)
      }
    }
  }

  private def tracked(symbol: Symbol): Boolean =
    symbol.exists && symbol.is(Flags.Mutable) && symbol.is(Flags.Private) && symbol.owner.isClass &&
      symbol.annotations.isEmpty

  private def candidate(method: tpd.DefDef): Boolean = {
    val symbol = method.symbol
    symbol.exists && symbol.owner.isClass && symbol.info.isInstanceOf[ExprType] &&
    !symbol.isOneOf(Flags.Override | Flags.Deferred | Flags.Synthetic | Flags.Inline | Flags.Implicit | Flags.Given) &&
    symbol.annotations.isEmpty && !symbol.allOverriddenSymbols.hasNext && symbol.isEffectivelyFinal &&
    method.source == ctx.compilationUnit.source && method.span.exists
  }

  private def reference(tree: tpd.Tree): Option[Symbol] = Calls.peelInlined(tree) match {
    case id: tpd.Ident                                                                          => Some(id.symbol)
    case select @ tpd.Select(qualifier: tpd.This, _) if qualifier.symbol == select.symbol.owner => Some(select.symbol)
    case _                                                                                      => None
  }

  private def statements(tree: tpd.Tree): List[tpd.Tree] = tree match {
    case tpd.Block(stats, expression) => stats :+ expression
    case other                        => List(other)
  }

  private def fill(method: tpd.DefDef): Option[Fill] = method.rhs match {
    case tpd.Block(List(tpd.If(condition, assigned, otherwise)), result)
        if otherwise.isInstanceOf[tpd.Literal] =>
      val writes = statements(assigned).collect { case write: tpd.Assign => write }
      val all    = statements(assigned)
      if (writes.size != all.size) None
      else
        (condition, reference(result)) match {
          case (tpd.Select(flag, name), Some(cache)) if name.toString == "unary_!" =>
            reference(flag).flatMap { guard =>
              val set    = writes.filter(write => reference(write.lhs).contains(guard))
              val stored = writes.filter(write => reference(write.lhs).contains(cache))
              Option.when(
                writes.size == 2 && set.size == 1 && stored.size == 1 && guard != cache &&
                  set.head.rhs.isInstanceOf[tpd.Literal] &&
                  set.head.rhs.asInstanceOf[tpd.Literal].const == Constant(true)
              )(Fill(Some(guard), cache, stored.head.rhs))
            }
          case (tpd.Apply(tpd.Select(checked, name), List(tpd.Literal(Constant(null)))), Some(cache))
              if (name.toString == "==" || name.toString == "eq") && reference(checked).contains(cache) =>
            Option.when(writes.size == 1 && reference(writes.head.lhs).contains(cache))(
              Fill(None, cache, writes.head.rhs)
            )
          case _ => None
        }
    case _ => None
  }

  private def initial(found: Fill, variables: collection.Map[Symbol, tpd.ValDef]): Boolean = {
    def literal(variable: Symbol, constant: Constant): Boolean =
      variables.get(variable).exists(_.rhs match {
        case tpd.Literal(value) => value == constant
        case _                  => false
      })
    found.flag.fold(literal(found.cache, Constant(null)))(literal(_, Constant(false)))
  }

  private def mentions(tree: tpd.Tree, variables: List[Symbol]): Boolean =
    tree.existsSubTree {
      case reference: (tpd.Ident | tpd.Select) => variables.contains(reference.symbol)
      case _                                   => false
    }

  private def report(method: tpd.DefDef, found: Fill, variables: collection.Map[Symbol, tpd.ValDef]): Unit = {
    val keyword  = text.slice(method.span.start, method.nameSpan.start)
    val declared =
      if (method.tpt.span.exists && method.tpt.span.start < method.tpt.span.end) Trees.text(method.tpt)
      else method.tpt.tpe.show
    val fix =
      if (
        keyword.matches("(?s).*\\bdef\\s+") && found.value.span.exists && found.value.span.start < found.value.span.end
      )
        Edits.replace(
          method.span.start,
          method.span.end,
          keyword.replaceAll("def\\s+$", "lazy val ") + s"${method.name}: $declared = ${Trees.text(found.value)}"
        ) :: (found.cache :: found.flag.toList).flatMap(variable => variables.get(variable).map(Edits.remove(text, _)))
      else Nil
    checks(
      Check.HandWrittenLazy,
      s"${method.name} fills ${(found.cache :: found.flag.toList).map(_.name).mkString(" and ")} on its first call; " +
        "make it a lazy val",
      method.srcPos,
      fix
    )
  }
}
