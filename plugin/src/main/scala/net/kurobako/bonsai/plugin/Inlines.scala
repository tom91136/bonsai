package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.Type
import net.kurobako.bonsai.lifting.*

/** Parameters of inline methods. A by-value parameter is bound once at each expansion, so a function it receives is
  * allocated as a closure and called through `apply`; an inline one is substituted at every use, so its argument runs
  * once per use, or not at all where a use is skipped; and only an inline parameter can expand the abstract inline
  * members of its type.
  */
private class Inlines(checks: Checks)(using Context) {

  /** Function parameters an inline method only calls, and abstract inline members it calls on by-value parameters. */
  def definition(method: tpd.DefDef): Unit =
    if (method.symbol.is(Flags.Inline) && !method.symbol.is(Flags.Deferred) && !method.rhs.isEmpty)
      method.termParamss.flatten.filterNot(_.symbol.is(Flags.Inline)).foreach { parameter =>
        val symbol = parameter.symbol
        if (functional(symbol.info) && called(symbol, method.rhs))
          checks(
            Check.InlineFunctionParameter,
            s"${symbol.name} is only called, so each call site allocates a closure for it and calls it through " +
              "apply; make it an inline parameter, which expands the function where it is called",
            parameter.srcPos
          )
        method.rhs.filterSubTrees {
          case tpd.Select(reference: tpd.Ident, _) => reference.symbol == symbol
          case _                                   => false
        }.collect { case select: tpd.Select if select.symbol.isAllOf(Flags.Inline | Flags.Deferred) => select }
          .foreach(select =>
            checks(
              Check.InlineInstanceParameter,
              s"${select.name} is an abstract inline method, which expands only on an inline ${symbol.name}; make " +
                s"${symbol.name} an inline parameter",
              select.srcPos
            )
          )
      }

  /** Arguments an inline method's expansion evaluates more than once, or whose effects run only on some paths. A macro
    * reads its arguments rather than running them.
    */
  def call(application: tpd.Apply): Unit = {
    val method = application.fun.symbol
    if (!reading(method)) body(method).foreach { body =>
      bound(application).foreach { (parameter, argument) =>
        if (parameter.is(Flags.Inline) && !cheap(argument)) {
          val evaluations = Evaluations(parameter, Set(method -> parameter)).apply(Count.Zero, body)
          // A by-name parameter declares that its argument runs only where it is needed.
          val skipped = evaluations.least == 0 && evaluations.most > 0 && effectful(argument) &&
            !Trees.byName(parameter)
          if (evaluations.most > 1 || skipped) {
            val evaluated =
              if (evaluations.most > 1) s"${evaluations.most} times on some path"
              else "only where its use is reached, so its effects run only there"
            checks(
              Check.InlineParameterReuse,
              s"the expansion of ${method.name} evaluates this argument to ${parameter.name} $evaluated; bind it to " +
                s"a val first, or make ${parameter.name} a by-value parameter",
              argument.srcPos
            )
          }
        }
      }
    }
  }

  /** The parameter each argument is passed to; the typer puts named arguments in order, but keeps their names. */
  private def bound(application: tpd.Apply): List[(Symbol, tpd.Tree)] =
    Trees.parameters(application).zip(application.args.map {
      case tpd.NamedArg(_, argument) => argument
      case argument                  => argument
    })

  // Skipping what only computes a value saves the work; skipping an assignment or a call made for its effect loses it.
  private def effectful(argument: tpd.Tree): Boolean = argument.existsSubTree {
    case _: tpd.Assign          => true
    case application: tpd.Apply => application.tpe.widen.isRef(defn.UnitClass)
    case _                      => false
  }

  private def body(method: Symbol): Option[tpd.Tree] =
    method.getAnnotation(defn.BodyAnnot).map(_.tree).filterNot(Calls.peelInlined(_).isEmpty)

  // Macros and compile-time operations take the tree of their argument at expansion, not its value.
  private def reading(method: Symbol): Boolean =
    method.is(Flags.Inline) && body(method).exists(_.existsSubTree {
      case tpd.Splice(_) => true
      case _             => false
    }) || method.owner.fullName.toString.startsWith("scala.compiletime")

  private object Count {
    val Zero: Count = Count(0, 0)
    val Once: Count = Count(1, 1)
  }

  /** The fewest and the most times one path through an expansion evaluates a parameter. */
  private case class Count(least: Int, most: Int) {
    def +(other: Count): Count  = Count(least + other.least, most + other.most)
    def *(other: Count): Count  = Count(least * other.least, most * other.most)
    def or(other: Count): Count = Count(least.min(other.least), most.max(other.most))
    // What runs only on some paths, or whenever a function is called, may not run at all.
    def optional: Count = Count(0, most)
  }

  /** How often a path through an expansion evaluates `parameter`: once per use, never in what a macro reads, as often
    * as one of the branches of an if or match, of which one runs, possibly not at all in the right operand of `&&` and
    * `||` and in a function body, and as often as an inline method it is passed on to evaluates its own parameter.
    * `forwarded` holds the parameters already being counted.
    */
  private class Evaluations(parameter: Symbol, forwarded: Set[(Symbol, Symbol)]) extends tpd.TreeAccumulator[Count] {
    def apply(total: Count, tree: tpd.Tree)(using Context): Count = total +
      (tree match {
        case reference: tpd.Ident            => if (reference.symbol == parameter) Count.Once else Count.Zero
        case tpd.If(condition, thenp, elsep) => apply(Count.Zero, condition) + branches(List(thenp, elsep))
        case tpd.Match(selector, cases)      => apply(Count.Zero, selector) + branches(cases)
        case tpd.Apply(tpd.Select(left, name), List(right)) if name == nme.ZAND || name == nme.ZOR =>
          apply(Count.Zero, left) + apply(Count.Zero, right).optional
        case application: tpd.Apply if reading(application.fun.symbol)         => apply(Count.Zero, application.fun)
        case application: tpd.Apply if application.fun.symbol.is(Flags.Inline) =>
          val callee = application.fun.symbol
          apply(Count.Zero, application.fun) +
            bound(application).map { (inner, argument) =>
              val uses = apply(Count.Zero, argument)
              if (uses.most > 0 && inner.is(Flags.Inline)) uses * expanded(callee, inner) else uses
            }.foldLeft(Count.Zero)(_ + _)
        case tree if Calls.lambda(tree).isDefined => foldOver(Count.Zero, tree).optional
        case _                                    => foldOver(Count.Zero, tree)
      })

    private def branches(trees: List[tpd.Tree])(using Context): Count =
      trees.map(apply(Count.Zero, _)).reduceOption(_.or(_)).getOrElse(Count.Zero)

    // A parameter passed on runs as often as the callee evaluates it, and at least once where its body is unknown.
    private def expanded(callee: Symbol, inner: Symbol)(using Context): Count =
      if (forwarded(callee -> inner)) Count.Once
      else body(callee).fold(Count.Once)(Evaluations(inner, forwarded + (callee -> inner)).apply(Count.Zero, _))
  }

  // A SAM type stands in for a function type, as a trait with one abstract method that lambdas implement.
  private def functional(tpe: Type): Boolean = defn.isFunctionType(tpe) || tpe.classSymbol.is(Flags.Trait) && {
    val abstracts = tpe.abstractTermMembers
    abstracts.size == 1 && !abstracts.exists(_.symbol.is(Flags.Inline))
  }

  /** Whether every use of `parameter` in `body` calls it. */
  private def called(parameter: Symbol, body: tpd.Tree): Boolean = {
    val uses  = references(parameter, body)
    val calls = body.filterSubTrees {
      case tpd.Apply(tpd.Select(reference: tpd.Ident, _), _) => reference.symbol == parameter
      case _                                                 => false
    }
    uses.nonEmpty && uses.size == calls.size
  }

  private def references(parameter: Symbol, body: tpd.Tree): List[tpd.Tree] = body.filterSubTrees {
    case reference: tpd.Ident => reference.symbol == parameter
    case _                    => false
  }

  // Evaluating a literal, a stable path, a function or arithmetic on them again changes nothing and costs little, as
  // does an implicit conversion of one, which refines a literal; and a parameter passed on, inline or not, is judged
  // where its own argument is given.
  private def cheap(argument: tpd.Tree): Boolean = tpd.isIdempotentExpr(argument) ||
    (Calls.peelInlined(argument) match {
      case reference: tpd.Ident if reference.symbol.is(Flags.Param)                              => true
      case tpd.Apply(tpd.Select(operand, _), operands) if operand.tpe.widen.isPrimitiveValueType =>
        cheap(operand) && operands.forall(cheap(_))
      case tpd.Apply(function, List(operand)) if function.symbol.is(Flags.Implicit) => cheap(operand)
      case _                                                                        => false
    })
}
