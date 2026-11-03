package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameKinds.DefaultGetterName
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.NamedType
import dotty.tools.dotc.core.Types.ThisType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeTraverser
import scala.collection.mutable

object Placements {

  /** Runs `body` with `value` on top of `stack`. */
  private[analysis] def pushing[A](stack: mutable.Stack[A], value: A)(body: => Unit): Unit = {
    stack.push(value)
    try body
    finally stack.pop(): Unit
  }

  // Overloads, and a method named like a type beside it, are told apart by their parameter types.
  def signed(symbol: Symbol, name: String)(using Context): String = {
    val decls  = symbol.owner.info.decls
    val shared = symbol.is(Flags.Method) &&
      (decls.lookupAll(symbol.name).exists(_ != symbol) || decls.lookup(symbol.name.toTypeName).exists)
    if (shared) name + symbol.info.paramInfoss.map(_.map(_.show).mkString("(", ", ", ")")).mkString else name
  }

  private case class Held(node: Node, lines: Int, inside: Boolean, movable: Boolean, owner: Boolean, bare: Boolean)
}

/** The declarations of a build and the references between them, for the nesting rule. A reference counts for the
  * declaration it names and for nothing around it: the imports, the prefix of a type, and a module named only to reach
  * what it declares are not references, so the graph is the same wherever its declarations stand.
  */
private class Placements(resolver: Symbols, lines: Lines)(using Context) extends tpd.TreeTraverser {
  import Placements.*

  private val units   = mutable.LinkedHashMap.empty[String, Held]
  private val members = mutable.LinkedHashMap.empty[String, Held]
  private val edges   = mutable.Map.empty[String, mutable.Set[String]]
  private val pinned  = mutable.Set.empty[String]

  // The declaration the code being read belongs to, and the classes around it with the unit each one is.
  private val atoms   = mutable.Stack[Option[String]](None)
  private val classes = mutable.Stack.empty[(Symbol, Option[String])]

  private val referred = TypeReferences()

  private def current: Option[String] = atoms.top

  def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
    case tpd.Import(_, _) | tpd.Export(_, _) => ()
    case tpd.PackageDef(_, statements)       => statements.foreach(traverse(_))
    case definition @ tpd.TypeDef(_, template: tpd.Template) if definition.symbol.isClass =>
      enter(definition, template)
    case definition: tpd.ValDef if definition.symbol.is(Flags.Module) => ()
    case definition: tpd.MemberDef                                    => declare(definition)
    case select: tpd.Select                                           => selection(select)
    case ident: tpd.Ident                                             => named(ident.symbol, ident.tpe)
    case self: tpd.This                                               => reach(self.symbol)
    case typed: tpd.TypeTree                                          => referred.traverse(typed.tpe)
    case _                                                            => traverseChildren(tree)
  }

  private def enter(definition: tpd.TypeDef, template: tpd.Template)(using Context): Unit = {
    val symbol = definition.symbol
    val atom   = unit(symbol)
    atom.foreach(holds(_, symbol, definition))
    pushing(classes, (symbol, atom)) {
      pushing(atoms, atom.orElse(current)) {
        template.parents.foreach(traverse(_))
        template.body.foreach(traverse(_))
      }
    }
  }

  private def declare(definition: tpd.MemberDef)(using Context): Unit = {
    val symbol = definition.symbol
    // the members the compiler writes for a class say nothing of what the author names
    if (!symbol.is(Flags.Synthetic) || !symbol.owner.isClass || symbol.name.is(DefaultGetterName))
      key(symbol) match {
        case Some(name) =>
          holds(name, symbol, definition)
          pushing(atoms, Some(name))(traverseChildren(definition))
        case None => traverseChildren(definition)
      }
  }

  private def selection(select: tpd.Select)(using Context): Unit = {
    named(select.symbol, select.tpe)
    if (!declares(select.qualifier, select.symbol)) traverse(select.qualifier)
  }

  private def declares(qualifier: tpd.Tree, selected: Symbol)(using Context): Boolean = {
    def path(tree: tpd.Tree): Boolean = tree match {
      case ident: tpd.Ident   => ident.symbol.isOneOf(Flags.Module | Flags.Package)
      case select: tpd.Select => select.symbol.isOneOf(Flags.Module | Flags.Package) && path(select.qualifier)
      case _                  => false
    }
    path(qualifier) && (qualifier.symbol.is(Flags.Package) || selected.owner == qualifier.symbol.moduleClass)
  }

  private def named(symbol: Symbol, tpe: Type)(using Context): Unit = {
    reference(symbol)
    tpe match {
      case prefixed: NamedType => prefixes(prefixed)
      case _                   => ()
    }
  }

  private def prefixes(tpe: NamedType)(using Context): Unit = tpe.prefix match {
    case ThisType(cls) => reach(cls.symbol)
    case _             => ()
  }

  // Marks the types inside `cls` as needing an instance of it.
  private def reach(cls: Symbol)(using Context): Unit =
    if (cls.isClass && !cls.is(Flags.ModuleClass) && classes.exists(_._1 == cls))
      classes.view.takeWhile(_._1 != cls).flatMap(_._2).foreach(pinned += _)

  private def reference(symbol: Symbol)(using Context): Unit = {
    if (symbol.is(Flags.TypeParam) && symbol.owner.isClass) reach(symbol.owner)
    for {
      from <- current
      to   <- atom(symbol) if from != to
    } edges.getOrElseUpdate(from, mutable.Set.empty) += to
  }

  private def atom(symbol: Symbol)(using Context): Option[String] =
    if (!symbol.exists || symbol.is(Flags.Package)) None
    else if (symbol.isClass) unit(symbol)
    else if (symbol.is(Flags.Module) && symbol.isTerm) unit(symbol.moduleClass)
    else if (symbol.owner.isClass) key(symbol).orElse(unit(symbol.owner))
    else None

  private def unit(cls: Symbol)(using Context): Option[String] =
    Option.when(
      cls.exists && cls.isClass && !cls.isAnonymousClass && !cls.isRefinementClass &&
        (cls.owner.isClass || cls.owner.is(Flags.Package)) && !cls.isTopLevelDefinitionsObject
    )(resolver.key(cls)).flatten.filter(described(_).exists(!_.test))

  private def described(name: String): Option[Node] = resolver.described(name)

  private def static(owner: Symbol)(using Context): Boolean =
    owner.is(Flags.Package) || owner.is(Flags.ModuleClass) && static(owner.owner)

  private def opaque(owner: Symbol)(using Context): Boolean =
    owner.isClass && owner.info.decls.toList.exists(declared => declared.isType && declared.is(Flags.Opaque))

  // A method, value or alias of an object that nothing about it ties down.
  private def key(symbol: Symbol)(using Context): Option[String] = {
    val owner = symbol.owner
    val tied  =
      if (symbol.isTerm)
        symbol.isOneOf(
          Flags.Synthetic | Flags.Given | Flags.Implicit | Flags.Inline | Flags.Mutable | Flags.Deferred |
            Flags.Module | Flags.Param | Flags.ParamAccessor | Flags.Exported | Flags.Macro | Flags.Enum
        ) || symbol.isConstructor || symbol.name.is(DefaultGetterName) || symbol.allOverriddenSymbols.hasNext ||
        symbol.paramSymss.flatten.exists(_.is(Flags.HasDefault)) || receivesOpaque(symbol) ||
        ctx.platform.isMainMethod(symbol) || symbol.hasAnnotation(defn.MainAnnot)
      else !symbol.isAliasType || symbol.isOneOf(Flags.Opaque | Flags.Synthetic | Flags.Deferred | Flags.Exported)
    if (!symbol.exists || symbol.isClass || tied || !owner.is(Flags.ModuleClass) || !static(owner.owner)) None
    else if (owner.isTopLevelDefinitionsObject)
      resolver.key(symbol).collect { case key if described(key).exists(!_.test) => Placements.signed(symbol, key) }
    else unit(owner).map(name => Placements.signed(symbol, s"$name.${symbol.name}"))
  }

  private def receivesOpaque(symbol: Symbol)(using Context): Boolean =
    symbol.is(Flags.ExtensionMethod) &&
      symbol.paramSymss.flatten.find(_.isTerm).exists(_.info.typeSymbol.is(Flags.Opaque))

  private def extent(definition: tpd.MemberDef): Int =
    if (definition.span.exists) lines.line(definition.source, definition.span.end) -
      lines.line(definition.source, definition.span.start) + 1
    else 0

  private def holds(name: String, symbol: Symbol, definition: tpd.MemberDef)(using Context): Unit = {
    current.foreach(from =>
      units.updateWith(from)(_.map(held => held.copy(lines = held.lines - extent(definition)))): Unit
    )
    val suppressed = resolver.suppressions(symbol)
    val inherited  = current.flatMap(from => units.get(from).orElse(members.get(from)).map(_.node.suppressed))
    val all        = suppressed ++ inherited.getOrElse(Set.empty)
    val quiet      = all.exists(id => id == "*" || id == Finding.Rule.Nesting.id)
    if (symbol.isClass) {
      described(name).foreach { node =>
        val sealedFamily = symbol.is(Flags.Sealed) && !symbol.is(Flags.Enum) ||
          symbol.asClass.parentSyms.exists(_.is(Flags.Sealed))
        val instance = symbol.is(Flags.Given) || symbol.is(Flags.ModuleClass) && symbol.sourceModule.is(Flags.Given)
        val tied     = symbol.is(Flags.Trait) || sealedFamily || instance || symbol.isAllOf(Flags.EnumCase) ||
          node.generated || quiet || opaque(symbol.owner)
        val fresh = Held(
          node.copy(suppressed = node.suppressed ++ all),
          extent(definition),
          symbol.owner.isClass && !symbol.owner.is(Flags.ModuleClass),
          !tied,
          !node.generated && !quiet && !opaque(symbol),
          nothingElse(definition)
        )
        units.updateWith(name)(known =>
          Some(known.fold(fresh)(was =>
            Held(
              was.node,
              was.lines + fresh.lines,
              was.inside || fresh.inside,
              was.movable && fresh.movable,
              was.owner && fresh.owner,
              was.bare && fresh.bare
            )
          ))
        ): Unit
      }
    } else {
      val owner = Option.unless(symbol.owner.isTopLevelDefinitionsObject)(unit(symbol.owner)).flatten
      described(owner.getOrElse(name)).foreach { node =>
        val line   = if (definition.span.exists) lines.line(definition.sourcePos) else node.line
        val public = node.exported && !symbol.isOneOf(Flags.Private | Flags.Protected) && !symbol.privateWithin.exists
        members(name) = Held(
          Node(name, node.module, node.scope, node.source, line, node.generated, exported = public, owner, all),
          extent(definition),
          inside = false,
          !quiet && !node.generated && !opaque(symbol.owner),
          owner = false,
          bare = false
        )
      }
    }
  }

  // A class or object whose body holds only declarations that can move.
  private def nothingElse(definition: tpd.MemberDef)(using Context): Boolean = definition match {
    case tpd.TypeDef(_, template: tpd.Template) =>
      template.body.forall {
        case nested @ tpd.TypeDef(_, _: tpd.Template)             => nested.symbol.isClass
        case module: tpd.ValDef if module.symbol.is(Flags.Module) => true
        case other: tpd.MemberDef                                 =>
          other.symbol.is(Flags.Synthetic) || other.symbol.isConstructor || key(other.symbol).isDefined
        case _ => false
      }
    case _ => false
  }

  /** The declarations found and the references between them; `entries` names the units code outside the build enters
    * at.
    */
  def model(entries: Set[String]): Nesting.Model = {
    val declarations = units.values.toVector.map(found =>
      Nesting.Declaration(
        found.node,
        unit = true,
        found.inside,
        found.lines.max(0),
        found.movable && !pinned(found.node.name),
        found.owner,
        found.bare,
        entries(found.node.name)
      )
    ) ++ members.values.toVector.map(found =>
      Nesting.Declaration(
        found.node,
        unit = false,
        inside = false,
        found.lines.max(0),
        found.movable,
        owner = false,
        bare = false,
        found.node.exported
      )
    )
    Nesting.Model(declarations, edges.view.mapValues(_.toSet).toMap)
  }

  private class TypeReferences(using Context) extends TypeTraverser {
    def traverse(tpe: Type): Unit = tpe match {
      case named: NamedType =>
        reference(named.symbol)
        prefixes(named)
      case _ => traverseChildren(tpe)
    }
  }
}
