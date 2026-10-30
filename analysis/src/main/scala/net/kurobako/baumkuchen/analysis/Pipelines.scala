package net.kurobako.baumkuchen.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.Type
import scala.collection.mutable

/** Chains of strict collection transformations in one method, whose intermediate collections a view avoids. */
object Pipelines {

  /** The transformations of one chain, in the order they run. */
  case class Chain(name: String, node: String, methods: Vector[String], suppressed: Set[String])

  def findings(chains: Vector[Chain], nodes: Map[String, Node]): Vector[Finding] = chains.flatMap { chain =>
    Finding.at(
      nodes,
      chain.node,
      Finding.Rule.IntermediateCollections,
      s"${Finding.short(chain.name)} chains ${chain.methods.mkString(" then ")}; each step builds an intermediate " +
        "collection, so use .view or a single pass",
      Finding.Level.Info,
      suppressed = chain.suppressed
    )
  }

  /** Finds the longest chain of strict transformations in each method body. */
  class Chains(using Context) {
    private val subtrees                   = Children()
    private val Transformations: Set[Name] = Set("map", "flatMap", "filter", "filterNot", "collect").map(termName(_))
    private val ArrayOps                   = requiredClass("scala.collection.ArrayOps")
    private val Iterable                   = requiredClass("scala.collection.Iterable")
    private val View                       = requiredClass("scala.collection.View")
    private val LazyList                   = requiredClass("scala.collection.immutable.LazyList")

    def of(name: String, node: String, suppressed: Set[String], body: tpd.Tree): Vector[Pipelines.Chain] = {
      val found                      = mutable.ArrayBuffer.empty[Vector[String]]
      val spine                      = mutable.Set.empty[tpd.Tree]
      def walk(tree: tpd.Tree): Unit = tree match {
        // A nested method is scanned on its own when the build declares it; a local one is not scanned at all.
        case definition: tpd.DefDef if !definition.symbol.isAnonymousFunction => ()
        case _                                                                =>
          if (!spine(tree))
            chain(tree).filter(_._1.size > 1).foreach { (methods, steps) =>
              found += methods
              spine ++= steps
            }
          subtrees.of(tree).foreach(walk(_))
      }
      walk(body)
      found.toVector.map(methods => Pipelines.Chain(name, node, methods, suppressed))
    }

    /** The transformations ending at `tree`, in the order they run, with the applications they run through. */
    private def chain(tree: tpd.Tree): Option[(Vector[String], Vector[tpd.Apply])] = tree match {
      case application: tpd.Apply =>
        transformation(application).map { select =>
          val outer = select.name.toString
          chain(converted(select.qualifier)) match {
            case Some((methods, steps)) => (methods :+ outer, application +: steps)
            case None                   => (Vector(outer), Vector(application))
          }
        }
      case _ => None
    }

    // Only the application that names the transformation counts, not one that supplies its implicit arguments.
    private def transformation(tree: tpd.Tree): Option[tpd.Select] = tree match {
      case tpd.Apply(function: (tpd.Select | tpd.TypeApply), _) =>
        Traversals.selected(function).filter(select =>
          Transformations(select.name) && strict(select.qualifier.tpe.widen)
        )
      case _ => None
    }

    @annotation.tailrec
    private def converted(tree: tpd.Tree): tpd.Tree = tree match {
      case tpd.Apply(function, List(wrapped)) if function.symbol.is(Flags.Implicit)       => converted(wrapped)
      case tpd.Apply(inner: tpd.Apply, arguments) if arguments.forall(_.span.isSynthetic) => converted(inner)
      case other                                                                          => other
    }

    private def strict(receiver: Type): Boolean =
      receiver.derivesFrom(ArrayOps) ||
        receiver.derivesFrom(Iterable) && !receiver.derivesFrom(View) && !receiver.derivesFrom(LazyList)
  }
}
