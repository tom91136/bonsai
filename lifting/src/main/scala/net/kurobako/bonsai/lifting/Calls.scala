package net.kurobako.bonsai.lifting

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameKinds.DefaultGetterName
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import net.kurobako.bonsai.effects.*

/** Presents the calls of typed trees to the effect axioms. */
class Calls(using Context) {

  /** `method` called on `qualifier`, if it has one. */
  def of(method: Symbol, qualifier: Option[tpd.Tree]): Axioms.Call = {
    val operand = qualifier.map { tree =>
      val tpe = Calls.unconverted(tree).tpe.widen
      Axioms.Operand(
        name => Classes.named(name).exists && tpe.derivesFrom(Classes.named(name)),
        tree.symbol.is(Flags.Module)
      )
    }
    Axioms.Call(
      Calls.qualified(Calls.owner(method)),
      method.name.toString,
      method.isConstructor,
      operand,
      method.is(Flags.Synthetic) || method.name.is(DefaultGetterName),
      method.is(Flags.JavaDefined),
      Calls.qualified(method.info.finalResultType.dealias.typeSymbol)
    )
  }
}

object Calls {

  /** The fully qualified name of a symbol, as the axioms spell it: modules' classes end in `$`, as do the owners of
    * Java statics.
    */
  def qualified(symbol: Symbol)(using Context): String =
    if (symbol.exists) names.get().computeIfAbsent(symbol, _ => spelled(symbol)) else ""

  private def spelled(symbol: Symbol)(using Context): String =
    symbol.fullName.toString.replace("$.", ".").stripSuffix("$")

  // Symbols are compared by identity and held weakly, so the names of a finished compilation go with it.
  private val names = ThreadLocal.withInitial(() => java.util.WeakHashMap[Symbol, String]())

  /** The owner of `symbol`, or `NoSymbol` where the classpath lacks the class `symbol` was read from. */
  def owner(symbol: Symbol)(using Context): Symbol = if (symbol.exists) symbol.owner else NoSymbol

  /** The term parameter lists of a method. */
  def termClauses(method: Symbol)(using Context): List[List[Symbol]] =
    method.paramSymss.filter(_.headOption.exists(_.isTerm))

  @annotation.tailrec
  def unconverted(tree: tpd.Tree)(using Context): tpd.Tree = peel(tree) match {
    case tpd.Apply(conversion, List(wrapped)) if conversion.symbol.is(Flags.Implicit) => unconverted(wrapped)
    case other                                                                        => other
  }

  private val strictPeels: PartialFunction[tpd.Tree, tpd.Tree] = {
    case tpd.NamedArg(_, argument)                             => argument
    case tpd.Inlined(_, inlined, expansion) if inlined.isEmpty => expansion
  }

  private val lenientPeels: PartialFunction[tpd.Tree, tpd.Tree] = { case tpd.Inlined(_, _, expansion) => expansion }

  /** Peels the wrappers that leave the value of a tree unchanged: named arguments, inlined expansions without bindings,
    * type ascriptions and blocks without statements.
    */
  def peel(tree: tpd.Tree)(using Context): tpd.Tree = peelWith(tree, strictPeels)

  /** As `peel`, but also peels inlined expansions with bindings. */
  def peelInlined(tree: tpd.Tree)(using Context): tpd.Tree = peelWith(tree, lenientPeels)

  @annotation.tailrec
  private def peelWith(tree: tpd.Tree, extra: PartialFunction[tpd.Tree, tpd.Tree])(using Context): tpd.Tree =
    if (extra.isDefinedAt(tree)) peelWith(extra(tree), extra)
    else
      tree match {
        case tpd.Typed(expression, _)   => peelWith(expression, extra)
        case tpd.Block(Nil, expression) => peelWith(expression, extra)
        case other                      => other
      }

  /** The arguments of each application in a chain of applications, innermost first. */
  def arguments(application: tpd.Apply): List[tpd.Tree] = application.fun match {
    case inner: tpd.Apply                   => arguments(inner) ++ application.args
    case tpd.TypeApply(inner: tpd.Apply, _) => arguments(inner) ++ application.args
    case _                                  => application.args
  }

  /** The method a function written in place defines. */
  def lambda(tree: tpd.Tree)(using Context): Option[tpd.DefDef] = peelInlined(tree) match {
    case tpd.Block(List(definition: tpd.DefDef), _: tpd.Closure) => Some(definition)
    case _                                                       => None
  }

  def tuple(symbol: Symbol)(using Context): Boolean =
    symbol.exists && symbol.name.toString == "apply" && defn.isTupleClass(symbol.owner.linkedClass)

  def identity(symbol: Symbol)(using Context): Boolean =
    symbol.exists && symbol.name.toString == "identity" && symbol.owner == defn.ScalaPredefModuleClass

  // Packages whose companions build collections that can be mutated.
  private val MutablePackages = Vector("scala.collection.mutable.", "java.util.")

  /** Whether `tree` builds a new object, mutable collection, array or iterator, which no one else holds yet. */
  def allocates(tree: tpd.Tree)(using Context): Boolean = {
    val (function, _) = spine(peel(tree))
    val method        = function.symbol
    val name          = method.name.toString
    val factory       = selected(function).map(_.qualifier.symbol).exists(module =>
      module.is(Flags.Module) &&
        (module == defn.ArrayModule || MutablePackages.exists(qualified(module).startsWith(_)))
    )
    method.isConstructor || Set("iterator", "toArray", "toBuffer", "clone")(name) ||
    factory && Set("apply", "empty", "from", "ofDim", "fill", "tabulate", "newBuilder")(name)
  }

  /** What a call on `qualifier` acts on; `fresh` holds for the locals of the method that hold a value it allocated. */
  def receiver(qualifier: Option[tpd.Tree], fresh: Symbol => Boolean)(using Context): Receiver[Symbol] =
    qualifier.map(unconverted(_)) match {
      case None | Some(tpd.This(_))                              => Receiver.This
      case Some(reference: tpd.Ident) if fresh(reference.symbol) => Receiver.Fresh
      case Some(reference: tpd.Ident)                            => Receiver.Other(Some(reference.symbol))
      case Some(allocation) if allocates(allocation)             => Receiver.Fresh
      case Some(_)                                               => Receiver.Other(None)
    }

  /** `kind` for an access to a field `target`: acting on the object it is selected from, which no one else sees when
    * the method allocated it.
    */
  def field(target: tpd.Tree, kind: Kind, cause: String, fresh: Symbol => Boolean)(using
      Context
  ): Option[Effect[Symbol]] = target match {
    case tpd.Select(qualifier, _) => Effect.on(receiver(Some(qualifier), fresh), kind, cause)
    case _                        => Some(Effect(kind, cause, onThis = true))
  }

  /** The function of a chain of applications and type applications, and the nodes applied to it, innermost first. */
  def spine(tree: tpd.Tree): (tpd.Tree, List[tpd.Tree]) = {
    @annotation.tailrec
    def go(inner: tpd.Tree, nodes: List[tpd.Tree]): (tpd.Tree, List[tpd.Tree]) = inner match {
      case tpd.Apply(function, _)     => go(function, inner :: nodes)
      case tpd.TypeApply(function, _) => go(function, inner :: nodes)
      case other                      => (other, nodes)
    }
    go(tree, Nil)
  }

  /** The selection a call goes through, if it is made on a qualifier. */
  @annotation.tailrec
  def selected(function: tpd.Tree): Option[tpd.Select] = function match {
    case select: tpd.Select      => Some(select)
    case tpd.TypeApply(inner, _) => selected(inner)
    case tpd.Apply(inner, _)     => selected(inner)
    case _                       => None
  }
}
