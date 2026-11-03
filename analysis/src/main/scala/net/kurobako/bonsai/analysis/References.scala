package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameKinds.DefaultGetterName
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.util.Spans.Span
import net.kurobako.bonsai.analysis.Placements.*
import net.kurobako.bonsai.lifting.*
import scala.collection.mutable

private[analysis] class References(components: Components, fragments: Fragments)(using Context)
    extends tpd.TreeTraverser {
  import components.*
  private val owners  = mutable.Stack.empty[Option[String]]
  private val methods = mutable.Stack.empty[Symbol]
  private val entered = mutable.Set.empty[Symbol]
  // Whether the tree being traversed runs once per iteration of a loop, and the extent of the innermost loop.
  private val looping = mutable.Stack(false)
  private val loops   = mutable.Stack(Span(0))
  // Synthetic methods such as case class `apply` and `copy` forward their parameters, which says nothing about callers.
  private val synthetic = mutable.Stack(false)
  // Inline expansion happens after pickling, so the givens it summons are only visible through their imports.
  private val imports   = mutable.Stack(mutable.Set.empty[String])
  private val methodsOf = mutable.HashMap.empty[Symbol, Map[String, List[Symbol]]]
  private val parents   = mutable.Set.empty[tpd.Tree]
  // Read by the callback of `types` while it traverses the type of one tree.
  private var parent = false // scalafix:ok DisableSyntax.var
  private val types  = TypeReferences { symbol =>
    symbols.refer(owner, symbol)
    read(symbol)
    if (!parent) dispatching.asType(symbol)
  }

  // The methods the compiler writes for a case class read every field.
  private def read(symbol: Symbol): Unit =
    if (!(synthetic.top && symbol.is(Flags.ParamAccessor)) && !entered(symbol)) recorder.use(symbol, owner)

  private def owner: Option[String] = if (owners.isEmpty) None else owners.top

  def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
    case imported: tpd.Import =>
      recorder.mention(importedSymbols(imported))
      val targets = importedSymbols(imported).flatMap(symbols.key(_))
      imports.top ++= targets: Unit
      symbols.link(owner, targets)
    case definition: tpd.MemberDef if definition.symbol.exists =>
      val symbol    = definition.symbol
      val enclosing = owner
      val node      = symbols.key(symbol).orElse(enclosing)
      if (symbol.isClass && ctx.platform.hasMainMethod(symbol) || symbol.hasAnnotation(defn.MainAnnot))
        node.foreach(symbols.root(_))
      if (node != enclosing) imports.foreach(symbols.link(node, _))
      recorder.instance(symbol)
      recorder.enclosed(symbol)
      recorder.expose(symbol)
      definition match {
        case typed @ tpd.TypeDef(_, template: tpd.Template) if symbol.isClass =>
          recorder.define(symbol, template)
          structure.define(symbol, typed, template)
        case value: tpd.ValDef if symbol.is(Flags.Synthetic) && !symbol.owner.isClass =>
          callSites.bind(symbol, value.rhs)
        case method: tpd.DefDef if symbol.name.is(DefaultGetterName) => callSites.default(symbol, method.rhs)
        case tpd.TypeDef(_, _) if symbol.isAliasType && !symbol.is(Flags.Opaque) =>
          recorder.member(symbol)
          recorder.alias(symbol)
        case method: tpd.DefDef if !symbol.is(Flags.Synthetic) && !symbol.isConstructor =>
          recorder.body(symbol, method)
          recorder.precondition(symbol, method.rhs)
          recorder.signature(symbol, method.rhs)
          recorder.design(symbol, method.rhs)
          structure.group(symbol, method)
          recorder.member(symbol)
          if (symbol.owner.isClass) body(symbol, method.rhs, node)
        case value: tpd.ValDef if symbol.owner.isClass && !symbol.is(Flags.Synthetic) =>
          body(symbol, value.rhs, node)
          structure.group(symbol, value)
          recorder.initialiser(symbol, value)
          recorder.member(symbol)
        // A pattern definition keeps its authored right-hand side in a synthetic val.
        case value: tpd.ValDef if symbol.owner.isClass && value.rhs.span.exists && !value.rhs.span.isSynthetic =>
          body(symbol, value.rhs, node)
        case method: tpd.DefDef if symbol.isConstructor && !symbol.isPrimaryConstructor =>
          recorder.constructor(symbol, method, None)
        case _ => ()
      }
      pushing(synthetic, synthetic.top || symbol.isAllOf(Flags.SyntheticMethod) && !symbol.isAnonymousFunction) {
        pushing(owners, node) {
          pushing(methods, symbol) {
            val added = entered.add(symbol)
            try {
              symbol.annotations.foreach(annotation => traverse(annotation.tree))
              traverseChildren(tree)
            } finally if (added) entered -= symbol: Unit
          }
        }
      }
    case block: tpd.Block =>
      // Local definitions are statements too, but discard nothing.
      if (!synthetic.top) block.stats.filterNot(_.isDef).foreach(callSites.discard(_))
      pushing(imports, mutable.Set.empty[String])(traverseChildren(tree))
    case template: tpd.Template =>
      parents ++= template.parents
      pushing(imports, mutable.Set.empty[String])(traverseChildren(tree))
    case tpd.PackageDef(_, _) =>
      pushing(imports, mutable.Set.empty[String])(traverseChildren(tree))
    case _ =>
      tree match {
        // A call the code inverts with `!` is read as the opposite of what it gives, which every caller inverting says
        // the method itself is named as. A nullary `unary_!` is a select, not an application.
        case select: tpd.Select if select.name == nme.UNARY_!                  => callSites.negate(select.qualifier)
        case tpd.Apply(tpd.Select(argument, name), Nil) if name == nme.UNARY_! => callSites.negate(argument)
        case application: tpd.Apply if !synthetic.top                          =>
          callSites.record(application, owner, entered)
          reads.shown(application, owner)
          reads.selected(application, owner)
        case assignment: tpd.Assign                   => recorder.assign(assignment.lhs.symbol)
        case unapply: tpd.UnApply                     => recorder.matched(unapply, owner)
        case generic: tpd.TypeApply if !synthetic.top =>
          callSites.instantiate(generic.fun.symbol, generic.args.map(_.tpe), clause(generic.fun), owner)
        case closure: tpd.Closure if !closure.tpt.isEmpty =>
          owner.flatMap(symbols.described(_)).foreach(node =>
            dispatching.implement(closure.tpt.tpe.classSymbol, s"a lambda in ${node.name}", node.module)
          )
        // An inline body runs as the code of each expansion, where its receivers have their own types, and a receiver
        // the loop does not change dispatches to one class throughout it.
        case select: tpd.Select
            if looping.top && !synthetic.top && !methods.exists(_.is(Flags.Inline)) && !settled(select.qualifier) =>
          dispatching.record(
            select.symbol,
            methods.find(method => method.is(Flags.Method) && method.owner.isClass),
            owner,
            select.qualifier.tpe
          )
        case _ => ()
      }
      tree match {
        case tpd.Ident(_) | tpd.Select(_, _) if !synthetic.top => callSites.invoke(tree, owner)
        case _                                                 => ()
      }
      symbols.refer(owner, tree.symbol)
      read(tree.symbol)
      recorder.mention(tree)
      derivations.observe(tree)
      structure.observe(tree, owner)
      reads.keyed(tree.tpe, owner)
      tree match {
        case select: tpd.Select => reads.rendered(select, owner)
        case _                  => ()
      }
      parent = parents.contains(tree)
      types.traverse(tree.tpe)
      parent = false
      children(tree)
  }

  /** The type clauses already applied in `function`, which is the index of the clause its application fills. */
  private def clause(function: tpd.Tree): Int = function match {
    case tpd.Apply(inner, _)     => clause(inner)
    case tpd.TypeApply(inner, _) => clause(inner) + 1
    case _                       => 0
  }

  // While bodies and the function arguments of collection traversals run once per iteration.
  private def children(tree: tpd.Tree)(using Context): Unit = tree match {
    case tpd.WhileDo(condition, body) =>
      pushing(loops, tree.span) {
        pushing(looping, value = true) {
          traverse(condition)
          traverse(body)
        }
      }
    case tpd.Apply(function, arguments)
        if Calls.selected(function).exists(select => library.traversal(select.qualifier.tpe)) =>
      traverse(function)
      arguments.foreach { argument =>
        val lambda = Calls.lambda(argument).isDefined
        pushing(loops, if (lambda) argument.span else loops.top) {
          pushing(looping, looping.top || lambda)(traverse(argument))
        }
      }
    case _ => traverseChildren(tree)
  }

  /** A receiver that is the same on every iteration of the innermost loop: this, or a value defined outside the loop
    * that nothing reassigns, or a stable path from one.
    */
  private def settled(receiver: tpd.Tree)(using Context): Boolean = Calls.peel(receiver) match {
    case tpd.This(_)          => true
    case reference: tpd.Ident =>
      val symbol = reference.symbol
      symbol.isTerm && !symbol.isOneOf(Flags.Mutable | Flags.Method) && symbol.span.exists &&
      !loops.top.contains(symbol.span)
    case select: tpd.Select => select.symbol.isStableMember && !select.symbol.is(Flags.Mutable) &&
      settled(select.qualifier)
    case _ => false
  }

  private def body(symbol: Symbol, tree: tpd.Tree, node: Option[String])(using Context): Unit =
    node.flatMap(symbols.described(_)).foreach(info =>
      if (!tree.isEmpty && !symbols.suppressions(symbol).exists(Set("*", Finding.Rule.DuplicateCode.id)))
        fragments.add(tree, info.name, info.test, symbol, family(symbol), specialising(symbol, tree))
    )

  /** The overloads, or methods named alike but for a width, that a method belongs to: those that take parameters of
    * different types, one per width, as numbered-family asks for.
    */
  private def family(symbol: Symbol)(using Context): String = {
    val owner = symbol.owner
    if (symbol.is(Flags.Method) && owner.isClass) {
      val name  = Finding.numberless(symbol.name.toString)
      val alike = methodsOf.getOrElseUpdate(
        owner,
        owner.info.decls.toList.filter(_.is(Flags.Method)).groupBy(member => Finding.numberless(member.name.toString))
      ).getOrElse(name, Nil)
      if (alike.map(_.info.firstParamTypes.map(_.show)).distinct.size > 1) s"${owner.fullName}.$name" else ""
    } else ""
  }

  // An inline method that expands inline methods on literals specialises them, as staged code does.
  private def specialising(symbol: Symbol, body: tpd.Tree)(using Context): Boolean =
    symbol.is(Flags.Inline) && body.existsSubTree {
      case application: tpd.Apply =>
        application.fun.symbol.is(Flags.Inline) && application.args.exists(argument =>
          Calls.peel(argument) match {
            case tpd.Literal(_) => true
            case _              => false
          }
        )
      case _ => false
    }

  private def importedSymbols(imported: tpd.Import): List[Symbol] =
    imported.expr.symbol ::
      (for {
        selector <- imported.selectors if !selector.isWildcard
        name     <- List(selector.name.toTermName, selector.name.toTypeName)
        member   <- imported.expr.tpe.member(name).alternatives
      } yield member.symbol)
}
