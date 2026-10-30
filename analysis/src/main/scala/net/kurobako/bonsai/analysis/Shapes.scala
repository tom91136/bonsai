package net.kurobako.bonsai.analysis

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.AppliedType
import dotty.tools.dotc.core.Types.TermRef
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeRef

/** Algebraic shapes of types: standard sums, products and functions are interpreted, every other type is an atom. */
object Shapes {
  private def of(tpe: Type)(using Context): Polynomial = {
    // Named parameters refine a function type with its `apply`, which hides the type arguments.
    val dealiased = tpe.widenDealias.dropDependentRefinement
    dealiased match {
      case _ if dealiased.isRef(defn.UnitClass)                                                => Polynomial.One
      case _ if dealiased.isRef(defn.NothingClass)                                             => Polynomial.Zero
      case _ if dealiased.isRef(defn.BooleanClass)                                             => Polynomial.constant(2)
      case AppliedType(constructor, List(value)) if constructor.typeSymbol == defn.OptionClass =>
        Polynomial.One + of(value)
      case AppliedType(constructor, List(left, right)) if constructor.typeSymbol == either() =>
        of(left) + of(right)
      case _ if defn.isTupleNType(dealiased) =>
        Polynomial.product(dealiased.tupleElementTypes.getOrElse(Nil).map(of(_)))
      case _ if defn.isFunctionNType(dealiased) && !defn.isContextFunctionType(dealiased) =>
        val arguments = dealiased.argInfos
        Polynomial.power(of(arguments.last), Polynomial.product(arguments.init.map(of(_))))
      case reference: TypeRef if reference.symbol.is(Flags.TypeParam) =>
        Polynomial.variable(reference.symbol.name.toString)
      case _ => Polynomial.atom(name(dealiased))
    }
  }

  private def kind(tpe: Type)(using Context): Algebra.Kind = {
    val dealiased     = tpe.widenDealias
    val symbol        = dealiased.typeSymbol
    lazy val children = if (symbol.is(Flags.Sealed)) symbol.children else Nil
    if (dealiased.isRef(defn.BooleanClass)) Algebra.Kind.Flag
    else if (symbol == defn.OptionClass) Algebra.Kind.Optional
    else if (numbers.exists(dealiased.isRef(_))) Algebra.Kind.Number
    else if (
      children.nonEmpty && children.lengthCompare(MaxTags) <= 0 &&
      children.forall(child => child.isTerm || child.is(Flags.ModuleClass))
    ) Algebra.Kind.Tag(children.map(_.effectiveName.toString).toVector)
    else Algebra.Kind.Other
  }

  def either()(using Context): Symbol = requiredClass("scala.util.Either")

  private val MaxTags = 16

  private def numbers(using Context): Vector[Symbol] =
    Vector(defn.ByteClass, defn.ShortClass, defn.IntClass, defn.LongClass, defn.FloatClass, defn.DoubleClass)

  /** The constructor parameters that make up a case class, with their declared types. */
  def fields(cls: Symbol)(using Context): Vector[Algebra.Field] = parameters(cls.primaryConstructor)

  /** The first term parameter list of a method. */
  def parameters(method: Symbol)(using Context): Vector[Algebra.Field] =
    clauses(method).headOption.getOrElse(Vector.empty)

  /** The term parameter lists of a method, in order. */
  def clauses(method: Symbol)(using Context): Vector[Vector[Algebra.Field]] =
    method.paramSymss.filter(_.headOption.exists(_.isTerm)).toVector.map(_.toVector.map { parameter =>
      Algebra.Field(
        parameter.name.toString,
        of(parameter.info),
        kind(parameter.info),
        primitive(parameter.info),
        parameter.isOneOf(Flags.Given | Flags.Implicit)
      )
    })

  private def primitive(tpe: Type)(using Context): Boolean = underlying(tpe, 8).typeSymbol.isPrimitiveValueClass

  /** The type an opaque type stands for, through any opaque types it is defined by. */
  @annotation.tailrec
  private def underlying(tpe: Type, depth: Int)(using Context): Type = tpe.widenDealias match {
    case AppliedType(constructor: TypeRef, arguments) if depth > 0 && constructor.symbol.isOpaqueAlias =>
      underlying(constructor.symbol.opaqueAlias.appliedTo(arguments), depth - 1)
    case reference: TypeRef if depth > 0 && reference.symbol.isOpaqueAlias =>
      underlying(reference.symbol.opaqueAlias, depth - 1)
    case other => other
  }

  // The full name of a term, with the objects it is nested in named as written rather than as their classes.
  private def path(symbol: Symbol)(using Context): String =
    if (symbol.is(Flags.ModuleClass)) path(symbol.sourceModule)
    else if (!symbol.owner.exists || symbol.owner.is(Flags.Package)) symbol.fullName.toString
    else s"${path(symbol.owner)}.${symbol.name}"

  private def name(tpe: Type)(using Context): String = tpe match {
    case AppliedType(constructor, arguments) => s"${name(constructor)}[${arguments.map(name(_)).mkString(", ")}]"
    // A type member a module inherits, as the Type of a newtype, is named by the module that holds it.
    case reference: TypeRef =>
      reference.prefix match {
        case prefix: TermRef if prefix.termSymbol.is(Flags.Module) =>
          s"${path(prefix.termSymbol)}.${reference.symbol.name}"
        case _ => reference.symbol.fullName.toString
      }
    case other => other.show
  }
}
