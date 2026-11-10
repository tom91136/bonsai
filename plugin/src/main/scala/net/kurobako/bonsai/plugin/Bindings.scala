package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.rewrites.Rewrites.ActionPatch
import net.kurobako.bonsai.lifting.*

private object Bindings {
  val Checks: Set[Check] = Set(Check.BindingInOneBranch)
}

/** Reports a `val` of a block that only one branch of a later `if` or `match` uses, when its initialiser reads
  * immutable values and calls the standard library without effects, so computing it in that branch is the same program
  * that computes it less often. The fix moves the definition into the branch.
  */
private class Bindings(checks: Checks)(using Context) {
  private val text = Edits.source

  def run(tree: tpd.Tree): Unit = tree.foreachSubTree {
    case block: tpd.Block => scan(block)
    case _                => ()
  }

  private def scan(block: tpd.Block): Unit = (block.stats :+ block.expr).tails.foreach {
    case (definition: tpd.ValDef) :: rest if candidate(definition) =>
      rest.filter(uses(definition.symbol, _)) match {
        case List(branching) => report(definition, branching)
        case _               => ()
      }
    case _ => ()
  }

  private def candidate(definition: tpd.ValDef): Boolean = {
    val symbol = definition.symbol
    symbol.exists && !symbol.isOneOf(
      Flags.Mutable | Flags.Lazy | Flags.Synthetic | Flags.Param | Flags.Given | Flags.Implicit | Flags.Inline |
        Flags.Private
    ) && symbol.annotations.isEmpty && definition.span.exists && definition.source == ctx.compilationUnit.source &&
    Trees.text(definition).startsWith("val ") && computes(definition.rhs) && Pures(definition.rhs)
  }

  private def computes(rhs: tpd.Tree): Boolean = rhs.existsSubTree(_.isInstanceOf[tpd.Apply])

  private def uses(value: Symbol, tree: tpd.Tree): Boolean =
    tree.existsSubTree {
      case reference: tpd.Ident => reference.symbol == value
      case _                    => false
    }

  private case class Branching(bodies: List[tpd.Tree], elsewhere: List[tpd.Tree], patterns: List[tpd.Tree])

  private def branching(statement: tpd.Tree): Option[Branching] = Calls.peelInlined(statement) match {
    case head: tpd.If if !head.isInline =>
      @annotation.tailrec
      def walk(node: tpd.If, found: List[tpd.If]): List[tpd.If] = node.elsep match {
        case next: tpd.If if !next.isInline => walk(next, node :: found)
        case _                              => (node :: found).reverse
      }
      val chain = walk(head, Nil)
      Some(Branching(chain.map(_.thenp) :+ chain.last.elsep, chain.map(_.cond), Nil))
    case branch: tpd.Match if !branch.isInline =>
      Some(Branching(branch.cases.map(_.body), branch.selector :: branch.cases.map(_.guard), branch.cases.map(_.pat)))
    case _ => None
  }

  private def report(definition: tpd.ValDef, statement: tpd.Tree): Unit =
    branching(statement).foreach { branches =>
      val value  = definition.symbol
      val inside = branches.bodies.zipWithIndex.filter((body, _) => uses(value, body))
      if (inside.size == 1 && !branches.elsewhere.exists(uses(value, _))) {
        val (body, index) = inside.head
        val bound         = branches.patterns.lift(index).fold(Set.empty[Name])(pattern =>
          pattern.filterSubTrees(_.isInstanceOf[tpd.Bind]).map(_.asInstanceOf[tpd.Bind].name).toSet
        )
        val free = definition.rhs.filterSubTrees(_.isInstanceOf[tpd.Ident]).map(_.asInstanceOf[tpd.Ident].name).toSet
        checks(
          Check.BindingInOneBranch,
          s"${definition.name} is only used in one branch; define it there",
          definition.srcPos,
          if (
            bound.intersect(free).isEmpty && body.span.exists && body.source == ctx.compilationUnit.source &&
            body.span.start < body.span.end
          )
            Edits.remove(text, definition) :: placement(definition, body)
          else Nil
        )
      }
    }

  private def placement(definition: tpd.ValDef, body: tpd.Tree): List[ActionPatch] = {
    val binding = Trees.text(definition)
    val raw     = body.span.start
    val start   =
      if (text.startsWith("=>", raw)) text.indexWhere(character => !character.isWhitespace, raw + 2) else raw
    if (text.charAt(start) == '{') {
      val after = text.indexWhere(character => character != ' ' && character != '\t', start + 1)
      if (after >= 0 && text.charAt(after) == '\n') {
        val first  = text.indexWhere(character => !character.isWhitespace, after)
        val indent = text.slice(after + 1, first)
        List(Edits.insert(start + 1, s"\n$indent$binding"))
      } else List(Edits.insert(start + 1, s" $binding;"))
    } else List(Edits.insert(start, s"{ $binding; "), Edits.insert(body.span.end, " }"))
  }
}
