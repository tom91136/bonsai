package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.ExprType
import dotty.tools.dotc.core.Types.Type

private object Trees {

  /** The source text of a tree, as written. */
  def text(tree: tpd.Tree)(using Context): String = {
    val position = tree.sourcePos
    position.source.content.slice(position.start, position.end).mkString
  }

  private val Preceded = """(?s).*[(,]\s*[A-Za-z_$][A-Za-z0-9_$]*\s*=\s*""".r
  private val Labelled = """(?s)[A-Za-z_$][A-Za-z0-9_$]*\s*=[^=>].*""".r

  /** Whether an argument was written as `name = value`, which typing can erase, as overload resolution does. Its span
    * may start at the value or at the name.
    */
  def named(argument: tpd.Tree)(using Context): Boolean = argument.span.exists && !argument.span.isSynthetic && {
    val start = argument.span.start
    Preceded.matches(argument.source.content.slice((start - 256).max(0), start).mkString) ||
    Labelled.matches(text(argument))
  }

  /** The parameters an application fills: of a curried method, the list after those applied inside it. */
  def parameters(application: tpd.Apply)(using Context): List[Symbol] = {
    @annotation.tailrec
    def depth(function: tpd.Tree, applied: Int): Int = function match {
      case tpd.Apply(inner, _)     => depth(inner, applied + 1)
      case tpd.TypeApply(inner, _) => depth(inner, applied)
      case _                       => applied
    }
    application.fun.symbol.paramSymss.filter(_.headOption.exists(_.isTerm)).lift(depth(application.fun, 0))
      .getOrElse(Nil)
  }

  /** The method a function passed to a call defines, seen through the blocks and ascriptions around it. */
  @annotation.tailrec
  def lambda(tree: tpd.Tree)(using Context): Option[tpd.DefDef] = tree match {
    case tpd.Block(List(definition: tpd.DefDef), _: tpd.Closure) => Some(definition)
    case tpd.Block(Nil, expression)                              => lambda(expression)
    case tpd.Typed(expression, _)                                => lambda(expression)
    case tpd.Inlined(_, _, expansion)                            => lambda(expansion)
    case _                                                       => None
  }

  /** The elements of a collection built in place by the apply of a companion, as `Set(a, b)` or `Vector(a, b)`. */
  def built(tree: tpd.Tree)(using Context): Option[List[tpd.Tree]] = {
    val collection = unwrap(tree).tpe.widen
    if (collection.derivesFrom(requiredClass("scala.collection.Iterable")) || collection.derivesFrom(defn.ArrayClass))
      varargs(unwrap(tree))
    else None
  }

  // Companions such as Array's take a ClassTag in an implicit argument list after the elements, and Array takes its
  // first primitive element apart from the rest.
  @annotation.tailrec
  private def varargs(tree: tpd.Tree)(using Context): Option[List[tpd.Tree]] = tree match {
    case tpd.Apply(inner: tpd.Apply, _) if inner.tpe.widen.isImplicitMethod => varargs(inner)
    case tpd.Apply(function, arguments :+ last)
        if selected(function).exists(_.qualifier.tpe.widen.classSymbol.is(Flags.Module)) &&
          function.symbol.name == nme.apply =>
      unwrap(last) match {
        case tpd.SeqLiteral(elements, element) if arguments.forall(_.tpe <:< element.tpe) => Some(arguments ++ elements)
        case _                                                                            => None
      }
    case _ => None
  }

  /** A literal, or a case of an enum or a case object. */
  def constant(tree: tpd.Tree)(using Context): Boolean = unwrap(tree) match {
    case _: tpd.Literal => true
    case other          => enumerated(other)
  }

  def enumerated(tree: tpd.Tree)(using Context): Boolean = unwrap(tree) match {
    case reference @ (_: tpd.Ident | _: tpd.Select) =>
      val symbol = reference.symbol
      symbol.isTerm && symbol.is(Flags.Case) && (symbol.is(Flags.Enum) || symbol.is(Flags.Module))
    case _ => false
  }

  // Curried methods such as foldLeft take their function in a later argument list.
  @annotation.tailrec
  def selected(function: tpd.Tree): Option[tpd.Select] = function match {
    case select: tpd.Select      => Some(select)
    case tpd.TypeApply(inner, _) => selected(inner)
    case tpd.Apply(inner, _)     => selected(inner)
    case _                       => None
  }

  /** An expression without the blocks, ascriptions and inline wrappers around it. */
  @annotation.tailrec
  def unwrap(tree: tpd.Tree)(using Context): tpd.Tree = tree match {
    case tpd.Block(Nil, expression)   => unwrap(expression)
    case tpd.Typed(expression, _)     => unwrap(expression)
    case tpd.Inlined(_, _, expansion) => unwrap(expansion)
    case other                        => other
  }

  /** The full name of a symbol; static members of Java classes belong to a module class, whose name ends in `$`. */
  def qualified(symbol: Symbol)(using Context): String = symbol.fullName.toString.replace("$.", ".")

  /** Whether a type is one of the primitives the JVM boxes: a number, a character or a Boolean. */
  def primitive(tpe: Type)(using Context): Boolean = {
    val symbol = tpe.widen.dealias.typeSymbol
    symbol == defn.BooleanClass || defn.ScalaNumericValueClasses().contains(symbol)
  }

  def byName(parameter: Symbol)(using Context): Boolean = parameter.exists && parameter.info.isInstanceOf[ExprType]

  /** Option, Either and Try hold a failure, or an absent value, as a value. */
  def optional(tpe: Type)(using Context): Boolean =
    tpe.derivesFrom(defn.OptionClass) || tpe.derivesFrom(requiredClass("scala.util.Either")) ||
      tpe.derivesFrom(requiredClass("scala.util.Try"))

  /** Whether the functions passed to a call run before it returns, as those passed to Option, Either, Try and strict
    * collections do.
    */
  def eager(function: tpd.Tree)(using Context): Boolean = selected(function).exists { select =>
    val receiver = select.qualifier.tpe.widen
    optional(receiver) || receiver.derivesFrom(requiredClass("scala.collection.IterableOnce")) ||
    receiver.derivesFrom(requiredClass("scala.collection.ArrayOps"))
  }
}
