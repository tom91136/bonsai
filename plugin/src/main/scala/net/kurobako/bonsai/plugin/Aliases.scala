package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.MethodType
import dotty.tools.dotc.core.Types.PolyType
import dotty.tools.dotc.core.Types.Type
import scala.collection.mutable

/** Reports leftovers of reshuffling code to satisfy detectors: a private, local or inline method that only calls
  * another with exactly its own parameters, an object whose members all only pass their parameters on to other objects,
  * and a parameter of a private or local method marked `@unused`, which nothing in the method needs. A method that is
  * overloaded, overrides, is annotated, differs in visibility from what it calls, or shows up as a value, where its
  * parameters must fit a function type, is left alone.
  */
private class Aliases(checks: Checks)(using Context) {
  private val Unused = requiredClass("scala.annotation.unused")

  def run(tree: tpd.Tree): Unit = {
    val methods = mutable.ListBuffer.empty[tpd.DefDef]
    tree.foreachSubTree {
      case method: tpd.DefDef if candidate(method)                  => methods += method: Unit
      case owner: tpd.TypeDef if owner.symbol.is(Flags.ModuleClass) => facade(owner)
      case _                                                        => ()
    }
    if (methods.nonEmpty) {
      val scan = Scan(methods.map(_.symbol).toSet)
      scan.traverse(tree)
      methods.foreach { method =>
        forwarding(method)
        unused(method, scan)
      }
    }
  }

  private def candidate(method: tpd.DefDef): Boolean = {
    val symbol = method.symbol
    symbol.exists && !method.rhs.isEmpty && !symbol.isOneOf(Flags.Synthetic | Flags.Override | Flags.Deferred) &&
    !symbol.isConstructor && symbol.annotations.forall(internal) && !symbol.allOverriddenSymbols.hasNext &&
    hidden(symbol) &&
    !(symbol.owner.isClass && symbol.owner.info.decls.lookupAll(symbol.name).size > 1)
  }

  // The compiler keeps the body of an inline method in an annotation of its own.
  private def internal(annotation: Annotation): Boolean =
    annotation.symbol.owner.fullName.toString == "scala.annotation.internal"

  private def hidden(symbol: Symbol): Boolean = symbol.is(Flags.Private) || symbol.owner.isTerm

  private def forwarding(method: tpd.DefDef): Unit = {
    val symbol     = method.symbol
    val parameters = method.termParamss.map(_.map(_.symbol))
    val flat       = parameters.flatten
    if (flat.nonEmpty && !flat.exists(parameter => Trees.byName(parameter) || parameter.is(Flags.HasDefault))) {
      call(Trees.unwrap(method.rhs), Nil, Nil).foreach { (head, typeArguments, argumentLists) =>
        val target = head.symbol
        val same   = argumentLists.size == parameters.size &&
          argumentLists.zip(parameters).forall((arguments, own) =>
            arguments.size == own.size && arguments.zip(own).forall((argument, parameter) =>
              argument.isInstanceOf[tpd.Ident] && argument.symbol == parameter
            )
          )
        val generic = typeArguments.forall(_.tpe.typeSymbol.isType) &&
          (typeArguments.isEmpty || typeArguments.map(_.tpe.typeSymbol) == method.leadingTypeParams.map(_.symbol))
        if (
          same && generic && target.exists && target != symbol && target.is(Flags.Method) && !target.isConstructor &&
          !target.name.toString.startsWith("inline$") &&
          hidden(target) == hidden(symbol) && target.is(Flags.Protected) == symbol.is(Flags.Protected) &&
          (hidden(symbol) || !hidden(target)) &&
          method.rhs.tpe.widen =:= method.tpt.tpe && stable(head)
        )
          checks(
            Check.ForwardingAlias,
            s"${symbol.name} only passes its parameters to ${target.name}; call ${target.name} directly",
            method.srcPos
          )
      }
    }
  }

  private def facade(owner: tpd.TypeDef): Unit = owner.rhs match {
    case template: tpd.Template if !owner.symbol.isPackageObject =>
      val stats = template.body.filter {
        case method: tpd.DefDef => !method.symbol.isConstructor && !method.symbol.is(Flags.Synthetic)
        case _: tpd.Import      => false
        case _                  => true
      }
      val methods = stats.collect { case method: tpd.DefDef => method }
      if (
        stats.nonEmpty && methods.size == stats.size && methods.forall(passes(_).isDefined) &&
        methods.exists(_.termParamss.exists(_.nonEmpty))
      )
        checks(
          Check.ForwardingAlias,
          s"${owner.symbol.sourceModule.name} only passes its parameters to other objects; call their members directly",
          owner.srcPos
        )
    case _ => ()
  }

  private def passes(method: tpd.DefDef): Option[Symbol] = {
    val symbol     = method.symbol
    val parameters = method.termParamss.flatten.map(_.symbol)
    call(Trees.unwrap(method.rhs), Nil, Nil) match {
      case Some((select: tpd.Select, Nil, List(arguments)))
          if visible(symbol) && arguments.map(_.symbol) == parameters && arguments.forall(_.isInstanceOf[tpd.Ident]) &&
            stable(select) && select.symbol.is(Flags.Method) && select.symbol != symbol &&
            method.rhs.tpe.widen =:= method.tpt.tpe =>
        Some(select.symbol)
      case _ => None
    }
  }

  private def visible(symbol: Symbol): Boolean =
    !symbol.isOneOf(Flags.Private | Flags.Protected | Flags.Implicit | Flags.Given) && !symbol.privateWithin.exists &&
      !symbol.allOverriddenSymbols.hasNext

  // The function of a chain of applications, its type arguments, and the arguments of each clause in order.
  @annotation.tailrec
  private def call(
      tree: tpd.Tree,
      typeArguments: List[tpd.Tree],
      argumentLists: List[List[tpd.Tree]]
  ): Option[(tpd.Tree, List[tpd.Tree], List[List[tpd.Tree]])] = tree match {
    case tpd.Apply(function, arguments)             => call(function, typeArguments, arguments :: argumentLists)
    case tpd.TypeApply(function, types)             => call(function, types, argumentLists)
    case reference @ (_: tpd.Ident | _: tpd.Select) => Some((reference, typeArguments, argumentLists))
    case _                                          => None
  }

  // A receiver that is a path, which calling the target directly reaches as well.
  @annotation.tailrec
  private def stable(reference: tpd.Tree): Boolean = reference match {
    case tpd.Select(qualifier, _) => qualifier match {
        case _: (tpd.Ident | tpd.This) => true
        case inner: tpd.Select         => stable(inner)
        case _                         => false
      }
    case _: tpd.Ident => true
    case _            => false
  }

  private def dependent(tpe: Type): Boolean = tpe match {
    case poly: PolyType     => dependent(poly.resType)
    case method: MethodType => method.isParamDependent || method.isResultDependent || dependent(method.resType)
    case _                  => false
  }

  private def unused(method: tpd.DefDef, scan: Scan): Unit = {
    val symbol = method.symbol
    if (hidden(symbol) && !scan.values(symbol) && !dependent(symbol.info)) {
      val clauses = method.termParamss
      clauses.flatten.zipWithIndex.foreach { (parameter, index) =>
        val own = parameter.symbol
        if (
          own.hasAnnotation(Unused) && !own.isOneOf(Flags.Given | Flags.Implicit | Flags.HasDefault | Flags.Erased) &&
          !method.rhs.existsSubTree(_.symbol == own) &&
          scan.calls.getOrElse(symbol, Nil).forall(site =>
            site.flatten.lift(index).forall(argument => !scan.forwarded(argument))
          )
        )
          checks(
            Check.UnusedPrivateParam,
            s"${own.name} of ${symbol.name} is marked @unused and nothing uses it; remove the parameter",
            parameter.srcPos
          )
      }
    }
  }

  /** How the candidates are referenced: as the function of a call, with the arguments of each clause, or as a value. */
  private class Scan(candidates: Set[Symbol]) extends tpd.TreeTraverser {
    val values = mutable.Set.empty[Symbol]
    val calls  = mutable.Map.empty[Symbol, List[List[List[tpd.Tree]]]]

    // An argument that is a parameter of a lambda written for the call is how a method becomes a function value.
    def forwarded(argument: tpd.Tree)(using Context): Boolean = {
      val symbol = Trees.unwrap(argument).symbol
      symbol.exists && symbol.is(Flags.Param) && symbol.owner.isAnonymousFunction
    }

    override def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
      case application: tpd.Apply =>
        call(application, Nil, Nil) match {
          case Some((head, _, argumentLists)) if candidates(head.symbol) =>
            calls(head.symbol) = calls.getOrElse(head.symbol, Nil) :+ argumentLists
            head match {
              case tpd.Select(qualifier, _) => traverse(qualifier)
              case _                        => ()
            }
            argumentLists.flatten.foreach(traverse(_))
          case _ => traverseChildren(tree)
        }
      case reference @ (_: tpd.Ident | _: tpd.Select) if candidates(reference.symbol) =>
        values += reference.symbol: Unit
        traverseChildren(tree)
      case _ => traverseChildren(tree)
    }
  }
}
