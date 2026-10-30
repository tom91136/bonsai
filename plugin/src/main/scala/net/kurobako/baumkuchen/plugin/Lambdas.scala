package net.kurobako.baumkuchen.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import scala.collection.mutable

/** Reports indirection that only passes a call on: a private single-method trait nothing but lambdas implement, however
  * many, which a function type would state, and a private or local method that takes a function, applies it once on
  * every path through its body to arguments its callers have, and is only ever given lambdas forwarding to a method, so
  * the caller can pass the result instead. A conditional or by-name application stays, since passing the result would
  * evaluate it eagerly. Single-method traits in hot classes are left alone, since they are how primitives avoid boxing
  * there, and so is one whose method a function type would box, outside the classes the build leaves cold, since that
  * function type is what unspecialised-function reports.
  */
private class Lambdas(checks: Checks, hot: String => Boolean, cold: String => Boolean)(using Context) {
  private val closures = mutable.Map.empty[Symbol, mutable.ListBuffer[tpd.Closure]]
  private val calls    = mutable.Map.empty[Symbol, mutable.ListBuffer[tpd.Apply]]
  private val mentions = mutable.Map.empty[Symbol, Int]
  private val extended = mutable.Set.empty[Symbol]

  def run(tree: tpd.Tree): Unit = {
    val definitions = mutable.ListBuffer.empty[tpd.MemberDef]
    tree.foreachSubTree {
      case member: tpd.MemberDef => definitions += member: Unit
      case _                     => ()
    }
    val traits  = definitions.collect { case template: tpd.TypeDef if single(template.symbol) => template.symbol }
    val methods = definitions.collect { case method: tpd.DefDef if candidate(method) => method }
    val tracked = methods.map(_.symbol).toSet
    Collector(traits.toSet, tracked).traverse(tree)
    traits.filter(symbol =>
      !extended(symbol) && closures.contains(symbol) && !(boxing(symbol) && !coldOwner(symbol))
    ).foreach { symbol =>
      checks(Check.LambdaTrait, s"${symbol.name} is implemented only by lambdas; use a function type", symbol.srcPos)
    }
    methods.foreach(method => applied(method))
  }

  // A private trait with one abstract method and nothing else, outside the classes the build marks hot.
  private def single(symbol: Symbol): Boolean =
    symbol.isClass && symbol.is(Flags.Trait) && (symbol.is(Flags.Private) || symbol.owner.isTerm) &&
      !symbol.is(Flags.Synthetic) && !hotOwner(symbol) && {
        val members = symbol.info.decls.toList.filter(member => member.isTerm && !member.isConstructor)
        members.size == 1 && members.head.is(Flags.Deferred) && members.head.is(Flags.Method)
      }

  // The trait's function type would box a primitive on every call, as unspecialised-function finds.
  private def boxing(symbol: Symbol): Boolean = {
    val method     = symbol.info.decls.toList.filter(member => member.isTerm && !member.isConstructor).head
    val parameters = method.info.paramInfoss.flatten
    val result     = method.info.finalResultType
    (result :: parameters).exists(tpe => Trees.primitive(tpe) && !tpe.isRef(defn.BooleanClass)) &&
    !defn.isSpecializableFunctionSAM(parameters, result)
  }

  private def coldOwner(symbol: Symbol): Boolean = symbol.ownersIterator.exists(owner =>
    owner.isClass && !owner.is(Flags.Package) && cold(Trees.qualified(owner).stripSuffix("$"))
  )

  private def hotOwner(symbol: Symbol): Boolean = symbol.ownersIterator.exists(owner =>
    owner.isClass && !owner.is(Flags.Package) && hot(Trees.qualified(owner).stripSuffix("$"))
  )

  private def candidate(method: tpd.DefDef): Boolean = {
    val symbol = method.symbol
    symbol.exists && !symbol.isOneOf(Flags.Synthetic | Flags.Inline | Flags.Override) && !symbol.isConstructor &&
    (symbol.is(Flags.Private) || symbol.owner.isTerm) && !symbol.allOverriddenSymbols.hasNext &&
    method.termParamss.size == 1 && !hotOwner(symbol)
  }

  private def applied(method: tpd.DefDef): Unit = {
    val symbol = method.symbol
    val sites  = calls.getOrElse(symbol, mutable.ListBuffer.empty).toList
    if (sites.nonEmpty && mentions.getOrElse(symbol, 0) == sites.size)
      method.termParamss.head.zipWithIndex.foreach { (parameter, index) =>
        if (invokable(parameter.symbol)) {
          val uses = Uses(parameter.symbol, method)
          uses.traverse(method.rhs)
          if (
            uses.single && uses.arguments.exists(_.nonEmpty) &&
            sites.forall(site => site.args.lift(index).exists(forwarding(_)))
          )
            checks(
              Check.FunctionAppliedOnce,
              s"${parameter.name} is applied once, to arguments the callers of ${symbol.name} have, and only ever given " +
                "lambdas that forward to a method; pass the result instead of the function",
              parameter.srcPos
            )
        }
      }
  }

  // A function type, or a single-method trait lambdas implement, whose parameters are all values.
  private def invokable(parameter: Symbol): Boolean = {
    val tpe = parameter.info.dealias
    !Trees.byName(parameter) && (defn.isFunctionType(tpe) && tpe.argInfos.size > 1 || single(tpe.classSymbol))
  }

  private def forwarding(argument: tpd.Tree): Boolean = Trees.lambda(argument).exists(lambda => call(lambda.rhs))

  // One call whose function and arguments are plain references.
  private def call(body: tpd.Tree): Boolean = Trees.unwrap(body) match {
    case tpd.Apply(function, arguments) => reference(function) && arguments.forall(reference(_))
    case _                              => false
  }

  @annotation.tailrec
  private def reference(tree: tpd.Tree): Boolean = Trees.unwrap(tree) match {
    case tpd.Literal(_) | tpd.Ident(_) | tpd.This(_) => true
    case tpd.Select(qualifier, _)                    => reference(qualifier)
    case tpd.TypeApply(function, _)                  => reference(function)
    case _                                           => false
  }

  private class Collector(traits: Set[Symbol], methods: Set[Symbol]) extends tpd.TreeTraverser {
    override def traverse(tree: tpd.Tree)(using Context): Unit = {
      tree match {
        case closure: tpd.Closure if traits(closure.tpt.tpe.classSymbol) =>
          closures.getOrElseUpdate(closure.tpt.tpe.classSymbol, mutable.ListBuffer.empty) += closure
        case template: tpd.Template =>
          template.parents.foreach(parent => if (traits(parent.tpe.classSymbol)) extended += parent.tpe.classSymbol)
        case application: tpd.Apply =>
          val callee = application.fun match {
            case tpd.TypeApply(inner, _) => inner.symbol
            case inner                   => inner.symbol
          }
          if (methods(callee)) calls.getOrElseUpdate(callee, mutable.ListBuffer.empty) += application
        case reference: (tpd.Ident | tpd.Select) if methods(reference.symbol) =>
          mentions(reference.symbol) = mentions.getOrElse(reference.symbol, 0) + 1
        case _ => ()
      }
      traverseChildren(tree)
    }
  }

  /** The applications of a function parameter on every path through the body of its method, outside anything that runs
    * more than once or only sometimes: loops, local methods, lazy values, branches, `try`, and by-name arguments.
    */
  private class Uses(parameter: Symbol, method: tpd.DefDef) extends tpd.TreeTraverser {
    private val mentioned = mutable.ListBuffer.empty[tpd.Tree]
    private val found     = mutable.ListBuffer.empty[tpd.Apply]
    private var deferred  = 0

    // Arguments the method computes, like a result it only converts, are a step its callers would otherwise repeat.
    def single: Boolean = mentioned.size == 1 && found.size == 1 &&
      found.head.args.forall(argument => reference(argument) && own(argument)) && !converted(found.head)

    private def converted(application: tpd.Apply)(using Context): Boolean = Trees.unwrap(method.rhs) match {
      case tpd.Apply(_, List(argument)) => Trees.unwrap(argument) eq application
      case _                            => false
    }

    def arguments: Option[List[tpd.Tree]] = found.headOption.map(_.args)

    // An argument is available to the caller when it names a parameter, a value from outside the method, or a literal.
    private def own(argument: tpd.Tree)(using Context): Boolean = !argument.existsSubTree {
      case reference: (tpd.Ident | tpd.Select) =>
        reference.symbol.exists && reference.symbol != parameter && reference.symbol.isTerm &&
        !reference.symbol.is(Flags.Param) && reference.symbol.ownersIterator.contains(method.symbol)
      case _ => false
    }

    private def within(body: => Unit): Unit = {
      deferred += 1
      try body
      finally deferred -= 1
    }

    override def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
      case application @ tpd.Apply(tpd.Select(receiver, _), _)
          if receiver.symbol == parameter && application.symbol.is(Flags.Deferred) =>
        mentioned += application
        if (deferred == 0) found += application
        application.args.foreach(traverse(_))
      case reference: tpd.Ident if reference.symbol == parameter => mentioned += reference
      case tpd.If(condition, thenp, elsep)                       =>
        traverse(condition)
        within { traverse(thenp); traverse(elsep) }
      case tpd.Match(selector, cases) =>
        traverse(selector)
        within(cases.foreach(traverse(_)))
      case application: tpd.Apply =>
        traverse(application.fun)
        val parameters = Trees.parameters(application)
        val callee     = application.fun.symbol
        val short      = callee == defn.Boolean_&& || callee == defn.Boolean_||
        application.args.zipWithIndex.foreach { (argument, index) =>
          if (short || parameters.lift(index).exists(Trees.byName(_))) within(traverse(argument))
          else traverse(argument)
        }
      case _: tpd.DefDef | _: tpd.WhileDo | _: tpd.Try                => within(traverseChildren(tree))
      case definition: tpd.ValDef if definition.symbol.is(Flags.Lazy) => within(traverseChildren(tree))
      case _                                                          => traverseChildren(tree)
    }
  }
}
