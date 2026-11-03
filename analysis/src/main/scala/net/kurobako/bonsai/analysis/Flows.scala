package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.Type
import net.kurobako.bonsai.collections.*
import net.kurobako.bonsai.lifting.*
import scala.collection.mutable

private object Flows {

  /** The sinks of the expressions of a method, and of the local values and parameters it reads. */
  class Flow(sinks: collection.Map[tpd.Tree, Sink], uses: collection.Map[Symbol, Sink]) {
    def of(tree: tpd.Tree): Sink = sinks.getOrElse(tree, Sink.Returned)

    def use(symbol: Symbol): Sink = uses.getOrElse(symbol, Sink.Dropped)
  }
}

/** Escape analysis over the bodies of the build: for each expression, whether its value is kept past the point it is
  * built at. A value is kept when the method returns it, stores it in a field or a variable of an outer method, or
  * accumulates it in a collection or variable that outlives the turn of a loop. A value that only reaches a call that
  * returns nothing it could hold, or a callee that does not keep that parameter, is dropped.
  *
  * A call outside the build holds its arguments in its result, unless it mutates a collection, which keeps them in the
  * receiver. A result that cannot hold a reference, such as a number or a string, holds nothing.
  */
private class Flows(
    library: Library,
    definitions: collection.Map[Symbol, tpd.DefDef],
    owned: Symbol => Boolean,
    subtrees: Children
)(using Context) {
  import Flows.*

  private val flows   = mutable.Map.empty[Symbol, Flow]
  private val pending = mutable.Set.empty[Symbol]

  def of(method: Symbol): Flow = flows.get(method) match {
    case Some(known) => known
    case None        =>
      pending += method
      val computed =
        try Pass(definitions(method)).run()
        finally pending -= method
      flows(method) = computed
      computed
  }

  /** What a call of `method` does with its argument at `index`: returns it, or stores it. */
  private def fate(method: Symbol, index: Int): Sink =
    if (pending(method)) Sink(0, returned = true, stored = true)
    else
      definitions(method).termParamss.flatten.lift(index).fold(Sink.Returned)(parameter =>
        of(method).use(parameter.symbol)
      )

  private def scalar(tpe: Type): Boolean = {
    val cls = tpe.widenExpr.widen.classSymbol
    cls == defn.StringClass || cls == defn.NothingClass || cls.isPrimitiveValueClass
  }

  private class Pass(definition: tpd.DefDef) {
    private val sinks    = mutable.Map.empty[tpd.Tree, Sink]
    private val uses     = mutable.Map.empty[Symbol, Sink]
    private val declared = mutable.Map.empty[Symbol, Int]
    private var changed  = false

    def run(): Flow = {
      var rounds = 0
      while (rounds == 0 || (changed && rounds < 8)) {
        changed = false
        rounds += 1
        flow(definition.rhs, Sink.Returned, 0)
      }
      Flow(sinks, uses)
    }

    private def use(symbol: Symbol, sink: Sink): Unit =
      if (symbol.isTerm && !symbol.is(Flags.Method)) {
        val previous = uses.get(symbol)
        val joined   = previous.fold(sink)(_.join(sink))
        if (!previous.contains(joined)) {
          uses(symbol) = joined
          changed = true
        }
      }

    /** Where what is assigned to or bound as `symbol` goes: where it is read, and for a variable the loops it lives
      * across.
      */
    private def held(symbol: Symbol): Sink = {
      val read = uses.getOrElse(symbol, Sink.Dropped)
      if (symbol.is(Flags.Mutable)) read.join(Sink(declared.getOrElse(symbol, 0), read.returned, read.stored))
      else read
    }

    private def target(tree: tpd.Tree): Sink = tree match {
      case ident: tpd.Ident if declared.contains(ident.symbol) => held(ident.symbol)
      case _                                                   => Sink.Stored
    }

    private def carried(tree: tpd.Tree, sink: Sink): Sink = if (scalar(tree.tpe)) Sink.Dropped else sink

    private def statements(trees: List[tpd.Tree], depth: Int): Unit = trees.foreach {
      case value: tpd.ValDef =>
        declared(value.symbol) = depth
        flow(value.rhs, held(value.symbol), depth)
      case tpd.DefDef(_, _, _, _) => ()
      case other                  => flow(other, Sink.Dropped, depth)
    }

    private def binds(pattern: tpd.Tree): Boolean = pattern match {
      case tpd.Bind(_, _)              => true
      case tpd.Typed(inner, _)         => binds(inner)
      case tpd.UnApply(_, _, patterns) => patterns.exists(binds(_))
      case tpd.Alternative(patterns)   => patterns.exists(binds(_))
      case _                           => false
    }

    private def flow(tree: tpd.Tree, sink: Sink, depth: Int): Unit = {
      sinks(tree) = sink
      tree match {
        case tpd.Block(trees, expression) =>
          statements(trees, depth)
          flow(expression, sink, depth)
        case tpd.Inlined(_, bindings, expansion) =>
          statements(bindings, depth)
          flow(expansion, sink, depth)
        case tpd.If(condition, thenp, elsep) =>
          flow(condition, Sink.Dropped, depth)
          flow(thenp, sink, depth)
          flow(elsep, sink, depth)
        case tpd.Match(selector, cases) =>
          flow(selector, if (cases.exists(branch => binds(branch.pat))) sink else Sink.Dropped, depth)
          cases.foreach { branch =>
            flow(branch.guard, Sink.Dropped, depth)
            flow(branch.body, sink, depth)
          }
        case tpd.WhileDo(condition, body) =>
          flow(condition, Sink.Dropped, depth + 1)
          flow(body, Sink.Dropped, depth + 1)
        case tpd.Try(block, cases, finalizer) =>
          flow(block, sink, depth)
          cases.foreach(branch => flow(branch.body, sink, depth))
          flow(finalizer, Sink.Dropped, depth)
        case tpd.Return(expression, _) => flow(expression, Sink.Returned, depth)
        case tpd.Assign(lhs, rhs)      =>
          flow(rhs, target(lhs), depth)
          lhs match {
            case select: tpd.Select => flow(select.qualifier, Sink.Dropped, depth)
            case _                  => ()
          }
        case tpd.Typed(inner, _)         => flow(inner, sink, depth)
        case tpd.NamedArg(_, inner)      => flow(inner, sink, depth)
        case tpd.TypeApply(inner, _)     => flow(inner, sink, depth)
        case tpd.SeqLiteral(elements, _) => elements.foreach(flow(_, sink, depth))
        case application: tpd.Apply      => applied(application, sink, depth)
        case select: tpd.Select          => flow(select.qualifier, carried(select, sink), depth)
        case ident: tpd.Ident            => use(ident.symbol, sink)
        case tpd.DefDef(_, _, _, _)      => ()
        case _                           => subtrees.of(tree).foreach(flow(_, Sink.Returned, depth))
      }
    }

    private def holder(select: tpd.Select): Boolean =
      library.traversal(select.qualifier.tpe) || library.javaContainer(select.qualifier.tpe)

    private def applied(application: tpd.Apply, sink: Sink, depth: Int): Unit = {
      val method    = application.fun.symbol
      val select    = Calls.selected(application.fun)
      val result    = carried(application, sink)
      val internal  = owned(method)
      val traversal = !internal && select.exists(selected => library.traversal(selected.qualifier.tpe))
      val name      = method.name.toString
      val callee    = Option.when(definitions.contains(method) && !method.isConstructor)(method)
      select.foreach(selected => flow(selected.qualifier, result, depth))
      val mutator = select.filter(selected => Methods.Mutators(name) && !internal && holder(selected))
      Calls.arguments(application).zipWithIndex.foreach { (argument, index) =>
        Calls.lambda(argument) match {
          case Some(function) =>
            val body =
              if (!traversal) result
              else if (Methods.Accumulating(name)) result.accumulated(depth)
              else Sink.Dropped
            flow(function.rhs, body, if (traversal) depth + 1 else depth)
          case None =>
            val into =
              mutator match {
                case Some(selected) => target(selected.qualifier)
                case None           =>
                  callee.fold(result) { found =>
                    val used = fate(found, index)
                    (if (used.returned) result else Sink.Dropped).join(if (used.stored) Sink.Stored else Sink.Dropped)
                  }
              }
            flow(argument, into, depth)
        }
      }
    }
  }
}
