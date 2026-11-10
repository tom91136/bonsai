package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.ExprType
import dotty.tools.dotc.core.Types.Type
import net.kurobako.bonsai.lifting.*

private object Trees {

  /** The source text of a tree, as written. */
  def text(tree: tpd.Tree)(using Context): String = {
    val position = tree.sourcePos
    position.source.content.slice(position.start, position.end).mkString
  }

  /** The parameters an application fills: of a curried method, the list after those applied inside it. */
  def parameters(application: tpd.Apply)(using Context): List[Symbol] = {
    @annotation.tailrec
    def depth(function: tpd.Tree, applied: Int): Int = function match {
      case tpd.Apply(inner, _)     => depth(inner, applied + 1)
      case tpd.TypeApply(inner, _) => depth(inner, applied)
      case _                       => applied
    }
    Calls.termClauses(application.fun.symbol).lift(depth(application.fun, 0)).getOrElse(Nil)
  }

  /** The elements of a collection built in place by the apply of a companion, as `Set(a, b)` or `Vector(a, b)`. */
  def built(tree: tpd.Tree)(using Context): Option[List[tpd.Tree]] = {
    val collection = Calls.peelInlined(tree).tpe.widen
    if (collection.derivesFrom(defn.ArrayClass) || Classes.derives(collection, "scala.collection.Iterable"))
      varargs(Calls.peelInlined(tree))
    else None
  }

  // Companions such as Array's take a ClassTag in an implicit argument list after the elements, and Array takes its
  // first primitive element apart from the rest.
  @annotation.tailrec
  private def varargs(tree: tpd.Tree)(using Context): Option[List[tpd.Tree]] = tree match {
    case tpd.Apply(inner: tpd.Apply, _) if inner.tpe.widen.isImplicitMethod => varargs(inner)
    case tpd.Apply(function, arguments :+ last)
        if Calls.selected(function).exists(_.qualifier.tpe.widen.classSymbol.is(Flags.Module)) &&
          function.symbol.name == nme.apply =>
      Calls.peelInlined(last) match {
        case tpd.SeqLiteral(elements, element) if arguments.forall(_.tpe <:< element.tpe) => Some(arguments ++ elements)
        case _                                                                            => None
      }
    case _ => None
  }

  /** A literal, or a case of an enum or a case object. */
  def constant(tree: tpd.Tree)(using Context): Boolean = Calls.peelInlined(tree) match {
    case _: tpd.Literal => true
    case other          => enumerated(other)
  }

  def enumerated(tree: tpd.Tree)(using Context): Boolean = Calls.peelInlined(tree) match {
    case reference @ (_: tpd.Ident | _: tpd.Select) =>
      val symbol = reference.symbol
      symbol.isTerm && symbol.is(Flags.Case) && (symbol.is(Flags.Enum) || symbol.is(Flags.Module))
    case _ => false
  }

  /** Whether `tree` is written in the source being compiled, not generated or inlined from another. */
  def real(tree: tpd.Tree)(using Context): Boolean =
    tree.span.exists && tree.span.start < tree.span.end && tree.source == ctx.compilationUnit.source

  def constructs(symbol: Symbol)(using Context): Boolean =
    symbol.isConstructor || symbol.name == nme.apply && symbol.is(Flags.Synthetic)

  /** Whether a class or object enclosing `symbol` satisfies `test`. */
  def enclosedBy(symbol: Symbol, test: Symbol => Boolean)(using Context): Boolean =
    symbol.ownersIterator.exists(owner => owner.isClass && !owner.is(Flags.Package) && test(owner))

  /** Whether a type is one of the primitives the JVM boxes: a number, a character or a Boolean. */
  def primitive(tpe: Type)(using Context): Boolean = {
    val symbol = tpe.widen.dealias.typeSymbol
    symbol == defn.BooleanClass || defn.ScalaNumericValueClasses().contains(symbol)
  }

  def byName(parameter: Symbol)(using Context): Boolean =
    parameter.exists && !parameter.is(Flags.StableRealizable) && parameter.info.isInstanceOf[ExprType]

  /** Option, Either and Try hold a failure, or an absent value, as a value. */
  def optional(tpe: Type)(using Context): Boolean =
    tpe.derivesFrom(defn.OptionClass) || Classes.derives(tpe, "scala.util.Either", "scala.util.Try")

  /** Whether the functions passed to a call run before it returns, as those passed to Option, Either, Try and strict
    * collections do.
    */
  def eager(function: tpd.Tree)(using Context): Boolean = Calls.selected(function).exists { select =>
    val receiver = select.qualifier.tpe.widen
    optional(receiver) || Classes.derives(receiver, "scala.collection.IterableOnce", "scala.collection.ArrayOps")
  }
}
