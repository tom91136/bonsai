package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameKinds.DefaultGetterName
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.AppliedType
import dotty.tools.dotc.core.Types.ExprType
import dotty.tools.dotc.core.Types.MethodType
import dotty.tools.dotc.core.Types.PolyType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeRef
import net.kurobako.bonsai.lifting.Calls

/** Definitions and calls that opaque and refined types make mean something other than they read: extensions a member of
  * the underlying type replaces, methods that only call themselves, checked operations a conversion bypasses, equality
  * that boxes, and refinements inferred from mixed operands. `checked` names the refinement types, by their qualified
  * names, whose operations are checked.
  */
private class Refinements(checks: Checks, checked: Set[String], hot: => Boolean)(using Context) {

  /** An extension on a type whose members, as seen where the extension is called, include one of the same name the call
    * would take instead: members win over extensions, so the extension never runs.
    */
  def shadowed(method: tpd.DefDef): Unit = {
    val symbol = method.symbol
    method.termParamss match {
      case List(receiver) :: rest if symbol.is(Flags.ExtensionMethod) =>
        val own    = rest.headOption.getOrElse(Nil).map(parameter => visible(parameter.symbol.info))
        val target = visible(receiver.symbol.info)
        target.member(symbol.name).alternatives.find(member =>
          member.symbol != symbol && !member.symbol.is(Flags.ExtensionMethod) && applicable(member.info, own)
        ).foreach(member =>
          checks(
            Check.ShadowedExtension,
            s"the extension ${symbol.name} on ${receiver.symbol.info.show} never runs: ${target.show} has the member " +
              s"${member.symbol.name}${member.info.show}, which calls take before any extension; rename the extension, " +
              "or remove the bound that exposes the member",
            method.srcPos
          )
        )
      case _ => ()
    }
  }

  /** The type as code outside an opaque type's scope sees it: its upper bound, which is all an opaque type exposes. */
  @annotation.tailrec
  private def visible(tpe: Type): Type = tpe.widen.dealiasKeepOpaques match {
    case AppliedType(constructor: TypeRef, arguments) if constructor.symbol.isOpaqueAlias =>
      visible(constructor.symbol.info.hiBound.appliedTo(arguments))
    case reference: TypeRef if reference.symbol.isOpaqueAlias => visible(reference.symbol.info.hiBound)
    case other                                                => other
  }

  // A member without parameters takes a call without arguments; one with parameters, arguments that conform to them.
  private def applicable(member: Type, arguments: List[Type]): Boolean = (member, arguments) match {
    case (_: PolyType, _)        => false
    case (method: MethodType, _) =>
      method.paramInfos.size == arguments.size && arguments.zip(method.paramInfos).forall(_ <:< _)
    case (_: ExprType, Nil) => true
    case (_, Nil)           => true
    case _                  => false
  }

  /** A method whose whole body calls itself on the same receiver recurses until the stack overflows, whatever it
    * passes; an overload or conversion the arguments' types no longer select is the usual cause.
    */
  def selfCall(method: tpd.DefDef): Unit = {
    val symbol = method.symbol
    if (!symbol.isOneOf(Flags.Deferred | Flags.Synthetic) && !method.rhs.isEmpty)
      callee(Calls.peelInlined(method.rhs), symbol.owner).filter(_ == symbol).foreach(_ =>
        checks(
          Check.AccidentalSelfCall,
          s"${symbol.name} does nothing but call itself, so it recurses until the stack overflows; an overload or " +
            "conversion that its arguments' types no longer select is the usual cause",
          method.rhs.srcPos
        )
      )
  }

  /** The method a call on the instance `owner` defines, or on the object it is, invokes. */
  @annotation.tailrec
  private def callee(tree: tpd.Tree, owner: Symbol): Option[Symbol] = tree match {
    case tpd.Apply(function, _)     => callee(function, owner)
    case tpd.TypeApply(function, _) => callee(function, owner)
    case reference: tpd.Ident       => Some(reference.symbol).filter(_.is(Flags.Method))
    case tpd.Select(qualifier, _)   =>
      val same = qualifier match {
        case tpd.This(_) => true
        case reference   => reference.symbol.is(Flags.Module) && reference.symbol.moduleClass == owner
      }
      Option.when(same)(tree.symbol).filter(_.is(Flags.Method))
    case _ => None
  }

  /** A call that reaches an unchecked operation through a conversion of a refined value, where an extension or overload
    * taking the refined type exists that its arguments do not conform to.
    */
  def bypass(application: tpd.Apply): Unit = if (checked.nonEmpty) application.fun match {
    case select @ tpd.Select(converted, name) =>
      unrefined(converted).foreach { value =>
        if (extensions(value.tpe).exists(extension => extension.name == name && carries(extension, value.tpe)))
          checks(
            Check.CheckedOverloadBypass,
            s"$name on ${value.tpe.widen.show} converts it and runs ${select.symbol.owner.name}.$name, unchecked: the " +
              s"checked extension $name takes other refinements; refine the operands to keep the check, or convert " +
              "explicitly",
            application.srcPos
          )
      }
    case function =>
      val method       = function.symbol
      val alternatives =
        if (!method.exists || !method.owner.isClass) Nil
        else method.owner.info.member(method.name).alternatives.filterNot(_.symbol == method)
      application.args.zipWithIndex.foreach((argument, index) =>
        unrefined(argument).foreach(value =>
          if (alternatives.exists(_.info.paramInfoss.headOption.flatMap(_.lift(index)).exists(refined(_))))
            checks(
              Check.CheckedOverloadBypass,
              s"${function.symbol.name} takes the unchecked overload, since this argument's refinement " +
                s"${value.tpe.widen.show} does not conform to the overload taking a checked type; refine the argument to " +
                "keep the check, or convert it explicitly",
              argument.srcPos
            )
        )
      )
  }

  /** The refined value a tree converts implicitly to its carrier. */
  private def unrefined(tree: tpd.Tree): Option[tpd.Tree] = Calls.peelInlined(tree) match {
    case tpd.Apply(function, List(value)) if conversion(function) && refined(value.tpe) && !refined(tree.tpe) =>
      Some(value)
    case _ => None
  }

  private def conversion(function: tpd.Tree): Boolean =
    function.symbol.is(Flags.Implicit) ||
      function.symbol.name ==
      nme.apply && Calls.selected(function).exists(_.qualifier.tpe.derivesFrom(defn.ConversionClass))

  /** The extensions the scope of a refinement type declares, on itself or in its companion. */
  private def extensions(tpe: Type): List[Symbol] = refinement(tpe).toList.flatMap { symbol =>
    val scopes = List(symbol.owner, symbol.companionModule.moduleClass).filter(_.exists)
    scopes.flatMap(_.info.decls.toList.filter(_.is(Flags.ExtensionMethod)))
  }

  // An extension on a refinement of another carrier, as on Long where the value is an Int, is not what was bypassed.
  private def carries(extension: Symbol, tpe: Type): Boolean =
    extension.info.stripPoly.firstParamTypes.headOption.exists(receiver => underlying(receiver) =:= underlying(tpe))

  private def refined(tpe: Type): Boolean = refinement(tpe).isDefined

  private def head(tpe: Type): Symbol = tpe match {
    case AppliedType(constructor, _) => constructor.typeSymbol
    case other                       => other.typeSymbol
  }

  private def refinement(tpe: Type): Option[Symbol] = {
    val symbol = head(tpe.widen.dealias)
    Option.when(symbol.exists && checked(Calls.qualified(symbol)))(symbol)
  }

  /** Comparisons the compiler makes on boxed values when an opaque type stands for a primitive: `equals`, and `==` or
    * `!=` against a value of a wider type, which for Float and Double is not IEEE equality; reported everywhere for
    * those, and in hot code for the rest. `==` between two such values compares the primitives.
    */
  def equality(tree: tpd.Tree, operands: List[tpd.Tree]): Unit = {
    val types = operands.map(_.tpe.widen)
    types.find(boxing(_)).filter(_ => types.exists(other => !boxing(other) && !Trees.primitive(other)))
      .foreach(tpe => boxed(tree, tpe, "compares boxed values with equals"))
  }

  def equals(tree: tpd.Tree, operands: List[tpd.Tree]): Unit =
    operands.map(_.tpe.widen).find(boxing(_)).foreach(tpe => boxed(tree, tpe, "compares boxed values with equals"))

  /** A match on such a value with literal patterns compares each case with `Objects.equals` instead of a switch. */
  def matched(tree: tpd.Match): Unit =
    Some(tree.selector.tpe.widen).filter(boxing(_)).filter(_ => tree.cases.exists(clause => literal(clause.pat)))
      .foreach(tpe => boxed(tree, tpe, "matches each literal case with Objects.equals on boxed values, not a switch"))

  private def boxed(tree: tpd.Tree, tpe: Type, how: String): Unit = {
    val primitive = underlying(tpe)
    val floating  = primitive.isRef(defn.FloatClass) || primitive.isRef(defn.DoubleClass)
    if (floating || hot)
      checks(
        Check.BoxedOpaqueEquality,
        s"on ${tpe.show}, an opaque ${primitive.show}, this $how" +
          (if (floating) ", where -0.0 differs from 0.0 and NaN equals NaN" else "") +
          "; compare the underlying values",
        tree.srcPos
      )
  }

  private def literal(pattern: tpd.Tree): Boolean = pattern match {
    case _: tpd.Literal                => true
    case tpd.Alternative(alternatives) => alternatives.exists(literal(_))
    case tpd.Bind(_, body)             => literal(body)
    case _                             => false
  }

  private def boxing(tpe: Type): Boolean = opaque(tpe) && Trees.primitive(underlying(tpe))

  private def opaque(tpe: Type): Boolean = head(tpe.dealias).isOpaqueAlias

  /** The type an opaque type stands for, through any opaque types it is defined by. */
  @annotation.tailrec
  private def underlying(tpe: Type, depth: Int = 8): Type = tpe.widen.dealias match {
    case AppliedType(constructor: TypeRef, arguments) if depth > 0 && constructor.symbol.isOpaqueAlias =>
      underlying(constructor.symbol.opaqueAlias.appliedTo(arguments), depth - 1)
    case reference: TypeRef if depth > 0 && reference.symbol.isOpaqueAlias =>
      underlying(reference.symbol.opaqueAlias, depth - 1)
    case other => other
  }

  /** A definition without a type whose initialiser mixes refined and plain values, which leaves the refinement that
    * decides later overload and extension resolution to inference.
    */
  def inferred(definition: tpd.ValOrDefDef): Unit = {
    val symbol = definition.symbol
    if (
      checked.nonEmpty && symbol.exists && !symbol.isOneOf(Flags.Synthetic | Flags.Param) &&
      !symbol.name.is(DefaultGetterName) &&
      !symbol.owner.isAnonymousFunction && definition.tpt.span.isSynthetic && !definition.rhs.isEmpty &&
      refined(definition.tpt.tpe) && definition.rhs.existsSubTree(plain(_))
    )
      checks(
        Check.InferredRefinedType,
        s"${symbol.name} is inferred as ${definition.tpt.tpe.show} from refined and plain values, so later calls on it " +
          "resolve by that refinement; write the type",
        definition.srcPos
      )
  }

  // A literal or conversion that turns a plain value into a refined one.
  private def plain(tree: tpd.Tree): Boolean = tree match {
    case tpd.Apply(function, List(_)) => conversion(function) && refined(tree.tpe)
    case _                            => false
  }
}
