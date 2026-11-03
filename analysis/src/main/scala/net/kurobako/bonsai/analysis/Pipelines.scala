package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import net.kurobako.bonsai.collections.Laws
import net.kurobako.bonsai.collections as core
import net.kurobako.bonsai.lifting

/** Chains of collection calls in one method, whose intermediate collections and passes the laws of the shared algebra
  * find fewer of.
  */
object Pipelines {

  /** A chain of collection calls in one method, lifted to the algebra the laws work on; `lines` holds, for each step,
    * the line where the expression of the whole chain up to and including that step starts.
    */
  case class Lifted(name: String, node: String, chain: core.Chain, lines: Vector[Int], suppressed: Set[String])

  def findings(chains: Vector[Lifted], nodes: Map[String, Node]): Vector[Finding] = chains.flatMap { lifted =>
    Laws.outcomes(lifted.chain).view.filter(_.analysis).flatMap(outcome =>
      Finding.at(
        nodes,
        lifted.node,
        Finding.Rule.IntermediateCollections,
        s"${Finding.short(lifted.name)}: ${outcome.message} (line ${lifted.lines(outcome.from)})",
        Finding.Level.Info,
        suppressed = lifted.suppressed
      )
    )
  }

  /** Lifts the chains of collection calls in each method body to the shared algebra, with the purity of their functions
    * from `effects`.
    */
  class Chains(effects: Purity.Inference, lines: Lines)(using Context) {
    private val lifter = lifting.Lifter(Inferred(effects))

    // A nested method is lifted on its own when the build declares it; a local one is not lifted at all.
    private def descend(tree: tpd.Tree): Boolean = tree match {
      case definition: tpd.DefDef => definition.symbol.isAnonymousFunction
      case _                      => true
    }

    /** The chains of collection calls in the body of `method`, each lifted with its lines in the source, which a source
      * the reader cannot find has none of.
      */
    def lifted(
        method: Symbol,
        name: String,
        node: String,
        suppressed: Set[String],
        body: tpd.Tree
    ): Vector[Pipelines.Lifted] =
      if (body.source.content().isEmpty) Vector.empty
      else
        lifter.chains(body, method, descend).map { found =>
          Pipelines.Lifted(
            name,
            node,
            found.chain,
            found.ends.map(extent => lines.line(found.file, extent.start)),
            suppressed
          )
        }
  }

  private class Inferred(effects: Purity.Inference) extends lifting.Purity {
    def lambda(owner: Symbol, definition: tpd.DefDef)(using Context): Boolean = effects.silent(owner, definition)

    def method(symbol: Symbol)(using Context): Boolean = effects.silent(symbol)
  }
}
