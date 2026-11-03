package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Symbols.Symbol
import net.kurobako.bonsai.lifting.*
import scala.collection.mutable

private object RoundTrips {
  val Checks: Set[Check] = Set(Check.StringRoundTrip)
}

/** Reports a string built by an interpolation and parsed back, in the same method, by a pattern interpolation with the
  * same parts, when the strings the pattern matches come from the interpolation through values of the method. The
  * values the string was built from are at hand, so parsing it only recovers them. A pattern over strings from
  * elsewhere is left alone.
  */
private class RoundTrips(checks: Checks)(using Context) {
  private case class Render(scope: Symbol, parts: List[String], tree: tpd.Tree)
  private case class Parse(scope: Symbol, parts: List[String], tree: tpd.Tree, source: tpd.Tree)

  private val MinimumParts = 3

  def run(tree: tpd.Tree): Unit = {
    val renders = mutable.ListBuffer.empty[Render]
    val parses  = mutable.ListBuffer.empty[Parse]
    val values  = mutable.Map.empty[Symbol, tpd.Tree]
    val origins = mutable.Map.empty[Symbol, tpd.Tree]
    val scopes  = mutable.Stack.empty[Symbol]
    new tpd.TreeTraverser {
      def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
        case method: tpd.DefDef if !method.symbol.isAnonymousFunction =>
          scopes.push(method.symbol)
          try traverseChildren(tree)
          finally scopes.pop(): Unit
        case value: tpd.ValDef =>
          if (value.symbol.exists && !value.rhs.isEmpty) values(value.symbol) = value.rhs
          traverseChildren(tree)
        case application @ tpd.Apply(function, arguments) =>
          Calls.selected(function).foreach { select =>
            arguments.foreach(argument =>
              Calls.lambda(argument).foreach(_.termParamss.flatten.foreach(parameter =>
                origins(parameter.symbol) = select.qualifier
              ))
            )
          }
          interpolation(application, "s").foreach(parts => renders += Render(scope, parts, application))
          traverseChildren(tree)
        case matched: tpd.Match =>
          matched.cases.foreach(_.pat.foreachSubTree {
            case pattern: tpd.UnApply =>
              extractor(pattern).foreach(parts =>
                parses += Parse(scope, parts, pattern, origin(matched.selector))
              )
            case _ => ()
          })
          traverseChildren(tree)
        case _ => traverseChildren(tree)
      }
      private def scope: Symbol                        = scopes.headOption.getOrElse(ctx.owner)
      private def origin(selector: tpd.Tree): tpd.Tree = selector match {
        case reference: tpd.Ident => origins.getOrElse(reference.symbol, selector)
        case _                    => selector
      }
    }.traverse(tree)
    parses.foreach { parse =>
      if (parse.parts.size >= MinimumParts && parse.parts.exists(_.nonEmpty))
        renders.find(render =>
          render.scope == parse.scope && render.parts == parse.parts && flows(render.tree, parse.source, values)
        ).foreach(_ =>
          checks(
            Check.StringRoundTrip,
            "the string is built from its parts and parsed back with the same format; use the parts",
            parse.tree.srcPos
          )
        )
    }
  }

  private def flows(render: tpd.Tree, source: tpd.Tree, values: collection.Map[Symbol, tpd.Tree]): Boolean = {
    @annotation.tailrec
    def search(pending: List[tpd.Tree], seen: Set[Symbol]): Boolean = pending match {
      case Nil          => false
      case tree :: rest =>
        tree.existsSubTree(_ eq render) || {
          val fresh = tree.filterSubTrees(_.isInstanceOf[tpd.Ident]).map(_.symbol).distinct.filterNot(seen)
          search(rest ++ fresh.flatMap(values.get), seen ++ fresh)
        }
    }
    search(List(source), Set.empty)
  }

  private def literals(tree: tpd.Tree): Option[List[String]] =
    tree.filterSubTrees(_.isInstanceOf[tpd.SeqLiteral]).collectFirst {
      case sequence: tpd.SeqLiteral if sequence.elems.forall(_.isInstanceOf[tpd.Literal]) =>
        sequence.elems.collect { case tpd.Literal(Constant(part: String)) => part }
    }

  private def interpolation(application: tpd.Apply, name: String): Option[List[String]] =
    application.fun match {
      case select @ tpd.Select(context, method)
          if method.toString == name && select.symbol.owner.fullName.toString == "scala.StringContext" =>
        literals(context)
      case _ => None
    }

  private def extractor(pattern: tpd.UnApply): Option[List[String]] =
    Option.when(pattern.fun.symbol.name.toString == "unapplySeq" &&
      Calls.qualified(pattern.fun.symbol.owner).startsWith("scala.StringContext"))(
      literals(pattern.fun)
    ).flatten
}
