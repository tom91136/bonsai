package net.kurobako.baumkuchen.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.Type
import scala.collection.mutable

/** Upper bounds on the work of build methods, as polynomials in the sizes of their collection parameters.
  *
  * A traversal costs its receiver's size times the work of its function, linear operations cost their receiver's size,
  * and a call costs its callee's bound with the argument sizes substituted. Loops of unknown length, recursion and
  * sizes that cannot be followed contribute the atom `?`, and terms containing it are never reported.
  */
private class Costs(definitions: collection.Map[Symbol, tpd.DefDef])(using Context) {
  private val Unknown   = Polynomial.atom("?")
  private val summaries = mutable.Map.empty[Symbol, Polynomial]
  private val pending   = mutable.Set.empty[Symbol]
  private val subtrees  = Children()

  private val Preserving = Set(
    "map",
    "filter",
    "filterNot",
    "collect",
    "sortBy",
    "sortWith",
    "sorted",
    "reverse",
    "distinct",
    "toList",
    "toVector",
    "toSeq",
    "toArray",
    "toIndexedSeq",
    "view",
    "iterator",
    "indices",
    "zipWithIndex",
    "tail",
    "init",
    "take",
    "drop",
    "takeWhile",
    "dropWhile",
    "slice"
  )
  private val Searches   = Set("contains", "indexOf", "lastIndexOf")
  private val Aggregates = Set(
    "sum",
    "product",
    "max",
    "min",
    "mkString",
    "toList",
    "toVector",
    "toSet",
    "toMap",
    "toArray",
    "toSeq",
    "distinct",
    "reverse",
    "sorted"
  )
  private val Walks = Set("apply", "length", "size", "last")

  /** The highest-degree super-linear terms of `method` that arise in its own body rather than in a callee's. */
  def superLinear(method: Symbol): Vector[Vector[(String, Int)]] = {
    val local = definitions.get(method).fold(Polynomial.Zero)(definition =>
      cost(definition.rhs, parameters(definition), callee => linear(bound(callee)))
    )
    val terms = known(local).filter(_.map(_._2).sum > 1)
    val most  = terms.map(_.map(_._2).sum).maxOption.getOrElse(0)
    terms.filter(_.map(_._2).sum == most)
  }

  /** The bound of `method` in the sizes of its own parameters; zero outside the build. */
  private def bound(method: Symbol): Polynomial = summaries.get(method) match {
    case Some(known)             => known
    case None if pending(method) => Unknown
    case None                    => definitions.get(method).fold(Polynomial.Zero) { definition =>
        pending += method
        val result =
          try cost(definition.rhs, parameters(definition), bound(_))
          finally pending -= method
        summaries(method) = result
        result
      }
  }

  private def parameters(definition: tpd.DefDef): Map[Symbol, Polynomial] =
    definition.termParamss.flatten.collect {
      case parameter if sized(parameter.tpt.tpe) => parameter.symbol -> Polynomial.atom(name(parameter.symbol))
    }.toMap

  private def name(parameter: Symbol): String = s"${parameter.name}#${parameter.id}"

  private def sized(tpe: Type): Boolean = {
    val widened = tpe.widen
    !widened.derivesFrom(defn.OptionClass) &&
    (widened.derivesFrom(requiredClass("scala.collection.IterableOnce")) || widened.derivesFrom(defn.ArrayClass))
  }

  private def traversal(tpe: Type): Boolean = {
    val widened = tpe.widen
    sized(widened) || widened.derivesFrom(requiredClass("scala.collection.ArrayOps")) ||
    widened.derivesFrom(requiredClass("scala.collection.StringOps"))
  }

  private def cost(tree: tpd.Tree, sizes: Map[Symbol, Polynomial], callee: Symbol => Polynomial): Polynomial =
    tree match {
      case tpd.Block(statements, expression) =>
        val (total, scope) = statements.foldLeft((Polynomial.Zero, sizes)) { case ((total, scope), statement) =>
          statement match {
            case value: tpd.ValDef =>
              (
                total + cost(value.rhs, scope, callee),
                size(value.rhs, scope).fold(scope)(bound => scope.updated(value.symbol, bound))
              )
            case _: tpd.DefDef => (total, scope)
            case other         => (total + cost(other, scope, callee), scope)
          }
        }
        total + cost(expression, scope, callee)
      case tpd.WhileDo(condition, body) =>
        Unknown * (Polynomial.One + cost(condition, sizes, callee) + cost(body, sizes, callee))
      case application: tpd.Apply => applied(application, sizes, callee)
      case select: tpd.Select     => cost(select.qualifier, sizes, callee) + operation(select, sizes)
      case _: tpd.DefDef          => Polynomial.Zero
      case _                      => Polynomial.sum(subtrees.of(tree).map(cost(_, sizes, callee)))
    }

  private def applied(
      application: tpd.Apply,
      sizes: Map[Symbol, Polynomial],
      callee: Symbol => Polynomial
  ): Polynomial = {
    val method    = application.fun.symbol
    val select    = selected(application.fun)
    val arguments = argumentLists(application)
    val functions = arguments.flatMap(lambda(_))
    val bodies    = Polynomial.sum(functions.map(cost(_, sizes, callee)))
    val plain     = Polynomial.sum(arguments.filter(lambda(_).isEmpty).map(cost(_, sizes, callee)))
    val receiver  = select.fold(Polynomial.Zero)(select => cost(select.qualifier, sizes, callee))
    val work      = select match {
      case Some(traversed) if functions.nonEmpty && traversal(traversed.qualifier.tpe) =>
        size(traversed.qualifier, sizes).getOrElse(Unknown) * (Polynomial.One + bodies)
      case _ if definitions.contains(method) =>
        val bindings = method.paramSymss.filter(_.headOption.exists(_.isTerm)).flatten.zip(arguments).collect {
          case (parameter, argument) if sized(parameter.info) =>
            name(parameter) -> size(argument, sizes).getOrElse(Unknown)
        }.toMap
        bodies + substitute(callee(method), bindings)
      case _ => bodies + select.fold(Polynomial.Zero)(operation(_, sizes))
    }
    receiver + plain + work
  }

  /** The cost of a method that traverses its receiver without a function argument. */
  private def operation(select: tpd.Select, sizes: Map[Symbol, Polynomial]): Polynomial = {
    val receiver = select.qualifier.tpe.widen
    val name     = select.name.toString
    val scanned  =
      if (Walks(name)) receiver.derivesFrom(requiredClass("scala.collection.LinearSeq"))
      else if (Searches(name))
        (receiver.derivesFrom(requiredClass("scala.collection.Seq")) ||
          receiver.derivesFrom(requiredClass("scala.collection.ArrayOps"))) &&
        !receiver.derivesFrom(requiredClass("scala.collection.immutable.Range"))
      else Aggregates(name) && traversal(receiver)
    if (scanned) size(select.qualifier, sizes).getOrElse(Unknown) else Polynomial.Zero
  }

  /** The size of a collection-valued expression, when it follows from a parameter or local value. */
  private def size(tree: tpd.Tree, sizes: Map[Symbol, Polynomial]): Option[Polynomial] = tree match {
    case tpd.Ident(_)                                                             => sizes.get(tree.symbol)
    case tpd.Typed(expression, _)                                                 => size(expression, sizes)
    case tpd.Apply(function, List(wrapped)) if function.symbol.is(Flags.Implicit) => size(wrapped, sizes)
    case application: tpd.Apply => selected(application.fun).filter(select => Preserving(select.name.toString))
        .flatMap(select => size(select.qualifier, sizes))
    case select: tpd.Select if Preserving(select.name.toString) => size(select.qualifier, sizes)
    case _                                                      => None
  }

  @annotation.tailrec
  private def selected(function: tpd.Tree): Option[tpd.Select] = function match {
    case select: tpd.Select      => Some(select)
    case tpd.TypeApply(inner, _) => selected(inner)
    case tpd.Apply(inner, _)     => selected(inner)
    case _                       => None
  }

  private def argumentLists(application: tpd.Apply): List[tpd.Tree] = application.fun match {
    case inner: tpd.Apply                   => argumentLists(inner) ++ application.args
    case tpd.TypeApply(inner: tpd.Apply, _) => argumentLists(inner) ++ application.args
    case _                                  => application.args
  }

  /** The body of an anonymous function argument. */
  @annotation.tailrec
  private def lambda(tree: tpd.Tree): Option[tpd.Tree] = tree match {
    case tpd.Block(List(definition: tpd.DefDef), _: tpd.Closure) => Some(definition.rhs)
    case tpd.Block(Nil, expression)                              => lambda(expression)
    case tpd.Typed(expression, _)                                => lambda(expression)
    case tpd.Inlined(_, _, expansion)                            => lambda(expansion)
    case _                                                       => None
  }

  private def substitute(bound: Polynomial, sizes: Map[String, Polynomial]): Polynomial =
    Polynomial.sum(bound.terms.map { (monomial, count) =>
      Polynomial.constant(count) * Polynomial.product(monomial.toVector.map {
        case (Polynomial.Factor.Atom(atom), power) =>
          Polynomial.product(Vector.fill(power)(sizes.getOrElse(atom, Unknown)))
        case _ => Unknown
      })
    })

  private def linear(bound: Polynomial): Polynomial =
    Polynomial(bound.terms.filter((monomial, _) => monomial.values.sum <= 1))

  /** The terms of a bound over parameter sizes alone, without unknown or collapsed atoms. */
  private def known(bound: Polynomial): Vector[Vector[(String, Int)]] = bound.terms.keys.toVector.flatMap { monomial =>
    val atoms = monomial.toVector.map {
      case (Polynomial.Factor.Atom(atom), power) if atom.contains('#') => Some(atom.takeWhile(_ != '#') -> power)
      case _                                                           => None
    }
    Option.when(atoms.forall(_.isDefined))(atoms.flatten)
  }
}
