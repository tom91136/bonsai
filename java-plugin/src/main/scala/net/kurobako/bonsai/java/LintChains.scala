package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.tree.Tree.Kind
import net.kurobako.bonsai.collections.*

/** The lints of the chains of calls on the collections, streams and optionals of the Java code.
  *
  * The chain a call ends is lifted to the algebra of the collection laws, whose outcomes the lint reports under the id
  * of the law, in the words the law has for Java and with the code that replaces the calls where the law has one.
  */
object LintChains {

  val lints: Vector[Lint] = Vector(Chains)

  private object Chains extends Lint {
    val id               = "collection-chains"
    val kinds: Set[Kind] = Set(Tree.Kind.METHOD_INVOCATION)

    def check(context: Context, tree: Tree): Vector[Found] =
      if (generated(context)) Vector.empty
      else
        ChainLifter.lift(context).toVector.flatMap { lifted =>
          Laws.outcomes(lifted.chain).flatMap(found(context, lifted, _))
        }

    private def defines(context: Context, outcome: Outcome): Boolean =
      outcome.law.startsWith("size-to-") && context.ancestors.collectFirst { case method: MethodTree => method }
        .exists(method => method.getName.contentEquals("isEmpty") && method.getParameters.isEmpty)

    private def found(context: Context, lifted: Lifted, outcome: Outcome): Option[Found] =
      outcome.java.filterNot(_ => defines(context, outcome)).map { java =>
        val replacement = java.render.map(render => render(lifted.receiver(outcome.from))).map { code =>
          if (lifted.implicitThis && outcome.from == 0) code.replaceFirst("^(!?)this\\.", "$1") else code
        }
        Found(
          replacement.fold(java.message)(code => s"${java.message}: $code"),
          lifted.ends(outcome.until - 1),
          id = outcome.law
        )
      }
  }

  private def generated(context: Context): Boolean = {
    val holder = Iterator.iterate(context.path)(_.getParentPath).takeWhile(_ != null).find { path =>
      path.getLeaf match {
        case _: ClassTree | _: MethodTree => true
        case _                            => false
      }
    }
    holder.exists(path => Collector.generated(context.trees.getElement(path)))
  }
}
