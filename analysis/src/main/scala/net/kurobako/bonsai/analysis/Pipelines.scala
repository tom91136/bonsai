package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import net.kurobako.bonsai.*
import net.kurobako.bonsai.collections.*
import net.kurobako.bonsai.collections as core

/** Chains of collection calls in one method, whose intermediate collections and passes the laws of the shared algebra
  * find fewer of.
  */
object Pipelines {

  /** A chain of collection calls in one method, lifted to the algebra the laws work on; `lines` holds, for each step,
    * the line where the expression of the whole chain up to and including that step starts.
    */
  case class Lifted(
      name: String,
      node: String,
      chain: core.Chain,
      lines: Vector[Int],
      suppressed: Set[String],
      defines: Option[String] = None,
      minuend: Option[String] = None
  )

  def findings(chains: Vector[Lifted], nodes: Map[String, Node]): Vector[Finding] =
    laws(chains, nodes) ++ repeats(chains, nodes) ++ complements(chains, nodes)

  private def laws(chains: Vector[Lifted], nodes: Map[String, Node]): Vector[Finding] = chains.flatMap { lifted =>
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

  private def lead(lifted: Lifted, method: String): Option[Step] =
    lifted.chain.steps.headOption.filter(step =>
      step.method == method && step.receiver.library && step.receiver.strict && !step.receiver.single &&
        step.args.sizeIs == 1 && step.args.head.pure
    )

  private val SizePreserving = Set("map", "zipWithIndex", "reverse", "sorted", "sortBy", "sortWith")

  /** A `count` subtracted from the size of the collection its subject keeps one element of per element, which counts
    * what the predicate fails on in the subject.
    */
  private def complements(chains: Vector[Lifted], nodes: Map[String, Node]): Vector[Finding] =
    chains.groupBy(lifted => (lifted.node, lifted.name)).values.toVector.flatMap { method =>
      method.flatMap { counting =>
        for {
          step    <- lead(counting, "count")
          minuend <- counting.minuend
          mapping <- method.find { candidate =>
            candidate.defines.contains(counting.chain.source.text) && candidate.chain.source.pure &&
            candidate.chain.source.text == minuend && candidate.chain.steps.forall(step =>
              SizePreserving(step.method) && step.receiver.library && step.receiver.strict &&
                !step.receiver.distinct && !step.receiver.keyed && !step.receiver.single
            )
          }
          finding <- Finding.at(
            nodes,
            counting.node,
            Finding.Rule.ComplementCount,
            s"${Finding.short(counting.name)}: $minuend minus ${counting.chain.source.text}.count (line " +
              s"${counting.lines.head}) counts the failures of ${step.args.head.text}, as ${counting.chain.source.text} " +
              s"keeps one element per element of $minuend (line ${mapping.lines.head}); count with the predicate negated",
            Finding.Level.Info,
            suppressed = counting.suppressed
          )
        } yield finding
      }
    }

  /** An `exists` that tests the collection again for the predicate of a `filter` of the same method, which the
    * `nonEmpty` of the filtered collection already answers.
    */
  private def repeats(chains: Vector[Lifted], nodes: Map[String, Node]): Vector[Finding] =
    chains.groupBy(lifted => (lifted.node, lifted.name)).values.toVector.flatMap { method =>
      method.flatMap { existing =>
        for {
          tested    <- lead(existing, "exists")
          filtering <- method.find { candidate =>
            candidate.chain.source.pure && candidate.chain.source.text == existing.chain.source.text &&
            lead(candidate, "filter").exists(_.args.head.text == tested.args.head.text)
          }
          finding <- Finding.at(
            nodes,
            existing.node,
            Finding.Rule.RepeatedPredicate,
            s"${Finding.short(existing.name)}: exists on ${existing.chain.source.text} (line ${existing.lines.head}) " +
              s"repeats the filter of line ${filtering.lines.head}; the nonEmpty of the filtered collection answers it",
            Finding.Level.Info,
            suppressed = existing.suppressed
          )
        } yield finding
      }
    }

  /** Lifts the chains of collection calls in each method body to the shared algebra, with the purity of their functions
    * from `effects`.
    */
  class Chains(effects: Purity.Inference, lines: Lines, hot: String => Boolean)(using Context) {
    private val lifter = lifting.Lifter(Inferred(effects), hot)

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
        lifter.chains(body, method, descend(_)).map { found =>
          Pipelines.Lifted(
            name,
            node,
            found.chain,
            found.ends.map(extent => lines.line(found.file, extent.start)),
            suppressed,
            found.defines,
            found.minuend
          )
        }
  }

  private class Inferred(effects: Purity.Inference) extends lifting.Purity {
    def lambda(owner: Symbol, definition: tpd.DefDef)(using Context): Boolean = effects.silent(owner, definition)

    def method(symbol: Symbol)(using Context): Boolean = effects.silent(symbol)
  }
}
