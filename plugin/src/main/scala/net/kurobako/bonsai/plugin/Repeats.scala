package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.MethodicType
import dotty.tools.dotc.core.Types.Type
import net.kurobako.bonsai.effects.Axioms
import net.kurobako.bonsai.effects.Kind
import net.kurobako.bonsai.lifting.Calls
import net.kurobako.bonsai.lifting.Purity
import scala.collection.mutable

/** Reports a chain of library reads on values that no one assigns, written out twice or more in one method, where a
  * value computes it once. A chain is a read when each call is one of the standard library on immutable receivers and
  * arguments; the occurrences are those the first of them reaches whenever the others run, so a val before it stands
  * for all of them.
  */
private class Repeats(checks: Checks)(using Context) {
  private case class Site(tree: tpd.Tree, path: List[Int])

  private val Cheap = Set(
    "size",
    "length",
    "isEmpty",
    "nonEmpty",
    "head",
    "last",
    "headOption",
    "lastOption",
    "get",
    "getOrElse",
    "isDefined",
    "apply",
    "contains",
    "keys",
    "values",
    "toString",
    "equals",
    "hashCode",
    "unary_!",
    "==",
    "!=",
    "+",
    "-",
    "*",
    "/",
    "%",
    "<",
    ">",
    "<=",
    ">=",
    "&&",
    "||"
  )

  private val Impure = Set(Kind.Random, Kind.Output, Kind.Input, Kind.Writes)

  private val Foreign = List(
    "scala.collection.mutable",
    "scala.util.Random",
    "scala.Console",
    "scala.sys",
    "scala.concurrent",
    "scala.io",
    "scala.util.control",
    "scala.util.Try"
  )

  private val MinimumWork = 2
  private val axioms      = Calls()

  def run(tree: tpd.Tree): Unit = new tpd.TreeTraverser {
    def traverse(tree: tpd.Tree)(using Context): Unit = {
      tree match {
        case definition: tpd.DefDef
            if !definition.symbol.isAnonymousFunction && !definition.symbol.is(Flags.Synthetic) &&
              !definition.rhs.isEmpty =>
          method(definition)
        case _ => ()
      }
      traverseChildren(tree)
    }
  }.traverse(tree)

  private def method(definition: tpd.DefDef)(using Context): Unit = {
    val sites   = mutable.ListBuffer.empty[(String, Site)]
    var regions = 0
    new tpd.TreeTraverser {
      private var path: List[Int] = Nil

      private def region(body: => Unit): Unit = {
        regions += 1
        val before = path
        path = path :+ regions
        try body
        finally path = before
      }

      def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
        case nested: tpd.DefDef if nested.symbol.isAnonymousFunction => region(traverse(nested.rhs))
        case _: (tpd.DefDef | tpd.TypeDef)                           => ()
        case tpd.If(condition, left, right)                          =>
          traverse(condition)
          region(traverse(left))
          region(traverse(right))
        case tpd.Match(selector, cases) =>
          traverse(selector)
          cases.foreach(clause =>
            region {
              traverse(clause.guard)
              traverse(clause.body)
            }
          )
        case tpd.Apply(tpd.Select(left, name), List(right)) if name.toString == "&&" || name.toString == "||" =>
          traverse(left)
          region(traverse(right))
        case _: tpd.WhileDo => region(traverseChildren(tree))
        case _: tpd.Try     => region(traverseChildren(tree))
        case _              =>
          tree match {
            case _: (tpd.Apply | tpd.TypeApply | tpd.Select)
                if tree.span.exists && !tree.tpe.widen.isInstanceOf[MethodicType] =>
              key(tree, Map.empty).filter(_._2 >= MinimumWork).foreach((text, _) => sites += text -> Site(tree, path))
            case _ => ()
          }
          traverseChildren(tree)
      }
    }.traverse(definition.rhs)
    report(definition, sites.toList)
  }

  private def report(definition: tpd.DefDef, sites: List[(String, Site)]): Unit = {
    val groups = sites.groupMap(_._1)(_._2).values.view.map(_.distinctBy(_.tree.span)).flatMap { occurrences =>
      val base = occurrences.minBy(site => (site.path.size, site.tree.span.start))
      val same = occurrences.filter(_.path.startsWith(base.path))
      Option.when(same.sizeIs >= 2)((base, same))
    }.toList
    val taken = mutable.ListBuffer.empty[tpd.Tree]
    groups.sortBy((base, _) => -(base.tree.span.end - base.tree.span.start)).foreach { (_, same) =>
      val remaining = same.filterNot(site => taken.exists(outer => outer.span.contains(site.tree.span)))
      if (remaining.sizeIs >= 2) {
        taken ++= remaining.map(_.tree)
        val first = remaining.minBy(_.tree.span.start).tree
        checks(
          Check.RepeatedSubexpression,
          s"${shown(first)} is evaluated ${remaining.size} times in ${definition.name}; bind it to a val",
          first.srcPos
        )
      }
    }
  }

  private def shown(tree: tpd.Tree): String = {
    val text = Trees.text(tree)
    if (text.length <= 60 && !text.contains('\n')) text
    else s"${Calls.selected(tree).fold(tree.symbol.name.toString)(_.name.toString)}(...)"
  }

  /** The chain as a text that equal chains share and the number of its calls that do work, or none where it reads
    * anything but standard library calls on values.
    */
  private def key(tree: tpd.Tree, binders: Map[Symbol, Int]): Option[(String, Int)] =
    if (!sound(tree.tpe)) None
    else
      tree match {
        case tpd.Literal(constant) => Some((s"${constant.tag}:${constant.value}", 0))
        case _: tpd.This           => Some(("this", 0))
        case reference: tpd.Ident  =>
          val symbol = reference.symbol
          if (binders.contains(symbol)) Some((s"$$${binders(symbol)}", 0))
          else if (value(symbol)) Some((s"#${symbol.id}", 0))
          else None
        case select @ tpd.Select(qualifier, name) =>
          val symbol = select.symbol
          if (symbol.is(Flags.Module)) Some((s"#${symbol.id}", 0))
          else if (value(symbol)) key(qualifier, binders).map((text, work) => (s"$text.${symbol.id}", work))
          else if (library(symbol, qualifier))
            key(qualifier, binders).map((text, work) => (s"$text.${symbol.id}", work + effort(name.toString)))
          else None
        case tpd.Apply(function, arguments) =>
          for {
            called <- key(function, binders)
            passed <- sequence(arguments.map(argument(_, binders)))
          } yield (s"${called._1}(${passed.map(_._1).mkString(",")})", called._2 + passed.map(_._2).sum)
        case tpd.TypeApply(function, _) => key(function, binders)
        case tpd.Typed(expression, _)   => key(expression, binders)
        case tpd.Block(Nil, expression) => key(expression, binders)
        case _                          => None
      }

  private def argument(tree: tpd.Tree, binders: Map[Symbol, Int]): Option[(String, Int)] =
    Calls.lambda(tree) match {
      case Some(definition) =>
        val parameters = definition.termParamss.flatten.map(_.symbol)
        val inner = binders ++ parameters.zipWithIndex.map((parameter, index) => parameter -> (binders.size + index))
        key(Calls.peelInlined(definition.rhs), inner).map((text, work) => (s"fn${parameters.size}:$text", work))
      case None => key(tree, binders)
    }

  private def sequence(parts: List[Option[(String, Int)]]): Option[List[(String, Int)]] =
    parts.foldRight(Option(List.empty[(String, Int)]))((part, rest) =>
      for {
        p <- part
        r <- rest
      } yield p :: r
    )

  private def effort(name: String): Int = if (Cheap(name)) 0 else 1

  private def value(symbol: Symbol): Boolean =
    symbol.exists && symbol.isTerm && !symbol.isOneOf(Flags.Method | Flags.Mutable) && !Trees.byName(symbol)

  private def library(symbol: Symbol, qualifier: tpd.Tree): Boolean = symbol.exists && symbol.is(Flags.Method) && {
    val owner = symbol.owner.fullName.toString
    (owner.startsWith("scala.") || owner == "java.lang.String") && !Foreign.exists(owner.startsWith) &&
    !Axioms.effects(axioms.of(symbol, Some(qualifier))).exists(effect => Impure(effect.kind))
  }

  private def sound(tpe: Type): Boolean = {
    val widened = tpe.widen
    widened.isInstanceOf[MethodicType] ||
    !(widened.isRef(defn.UnitClass) || widened.isRef(defn.NothingClass) || Purity.stateful(widened))
  }
}
