package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameKinds.DefaultGetterName
import dotty.tools.dotc.core.NameKinds.WildcardParamName
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.AndType
import dotty.tools.dotc.core.Types.AppliedType
import dotty.tools.dotc.core.Types.OrType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeRef
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.rewrites.Rewrites.ActionPatch
import dotty.tools.dotc.typer.Applications
import dotty.tools.dotc.util.NoSourcePosition
import dotty.tools.dotc.util.Spans.Span
import net.kurobako.bonsai.effects.Partials
import net.kurobako.bonsai.lifting.Calls
import net.kurobako.bonsai.lifting.Classes
import scala.collection.mutable

/** Lints that need the types of expressions, which SemanticDB does not record: array comparisons, conversions to the
  * type a value already has, `map` with a function producing `Unit`, nested futures, positional arguments that are easy
  * to misorder, type tests on open hierarchies, and function types that box their primitives.
  */
private class LintPhase(
    wildcards: Int,
    checks: Checks,
    partials: Map[String, Partials.Partial],
    throwing: Deferred[ThrowingTable],
    hot: String => Boolean,
    hotDeclared: Boolean,
    cold: String => Boolean,
    checked: Set[String]
) extends PluginPhase {
  val phaseName: String                = "bonsai-lint"
  override val runsAfter: Set[String]  = Set("posttyper")
  override val runsBefore: Set[String] = Set("pickler")
  private val Conversions              = Set(
    "toInt",
    "toLong",
    "toShort",
    "toByte",
    "toChar",
    "toDouble",
    "toFloat",
    "toString"
  )

  // Compiler-generated methods are not the author's code, but lambdas, which are synthetic methods too, are.
  private def authored(using Context): Boolean = !ctx.owner.is(Flags.Synthetic) || ctx.owner.isAnonymousFunction

  private val positionals     = checks.owning(LintPhase.Positionals)
  private val classifications = checks.owning(LintPhase.Classifications)
  private val identities      = checks.owning(LintPhase.Identities)

  private def refinements(using Context): Refinements =
    Refinements(
      checks,
      checked,
      Trees.enclosedBy(ctx.owner, owner => hot(Calls.qualified(owner)))
    )

  // A pass runs only when some check it reports is enabled, and reports no other.
  private def pass(declared: Set[Check])(run: Checks => Unit): Unit = {
    val owning = checks.owning(declared)
    if (owning.active) run(owning)
  }

  override def transformUnit(tree: tpd.Tree)(using Context): tpd.Tree = {
    pass(Loops.Checks)(Loops(_, hot, hotDeclared).run(tree))
    pass(Chains.Checks)(Chains(_).run(tree))
    pass(Repeats.Checks)(Repeats(_).run(tree))
    pass(Exceptions.Checks)(Exceptions(_, partials, throwing.value).run(tree))
    pass(Effects.Checks)(Effects(_).run(tree))
    pass(Implied.Checks)(Implied(_).run(tree))
    pass(Concurrency.Checks)(Concurrency(_).traverse(tree))
    pass(Fields.Checks)(Fields(_).run(tree))
    pass(Equality.Checks)(Equality(_).run(tree))
    pass(Nowarns.Checks)(Nowarns(_).run(tree))
    pass(Lambdas.Checks)(Lambdas(_, hot, cold).run(tree))
    pass(Aliases.Checks)(Aliases(_).run(tree))
    pass(Bundles.Checks)(Bundles(_).run(tree))
    pass(JdkInternals.Checks)(JdkInternals(_).traverse(tree))
    pass(Branches.Checks)(Branches(_).run(tree))
    pass(Bindings.Checks)(Bindings(_).run(tree))
    pass(Lazies.Checks)(Lazies(_).run(tree))
    pass(RoundTrips.Checks)(RoundTrips(_).run(tree))
    tree
  }

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree = {
    if (authored) {
      arguments(tree)
      if (tree.fun.symbol.is(Flags.Inline)) Inlines(checks).call(tree)
      refinements.bypass(tree)
      tree match {
        case tpd.Apply(function @ tpd.Select(left, _), List(right))
            if function.symbol == defn.Any_== || function.symbol == defn.Any_!= =>
          comparison(tree, left, right)
          refinements.equality(tree, List(left, right))
        case tpd.Apply(function @ tpd.Select(left, _), List(right))
            if function.symbol == defn.Any_equals || function.symbol.name == nme.equals_ =>
          refinements.equals(tree, List(left, right))
        case tpd.Apply(tpd.TypeApply(tpd.Select(receiver, nme.map), _), List(function)) =>
          unitInMap(tree, receiver, function)
        case tpd.Apply(tpd.Select(receiver, nme.map), List(function)) => unitInMap(tree, receiver, function)
        case tpd.Apply(select: tpd.Select, Nil) if select.symbol.name == nme.toString_ =>
          conversion(tree, select.qualifier)
        case tpd.Apply(function @ (_: tpd.Select | _: tpd.TypeApply), List(argument)) =>
          Calls.selected(function).filter(select => OrderedComparisons(select.name)).foreach(select =>
            unordered(tree, select.qualifier, argument)
          )
        case _ => ()
      }
    }
    tree
  }

  override def transformSelect(tree: tpd.Select)(using Context): tpd.Tree = {
    if (authored && Conversions(tree.name.toString) && tree.tpe.widen.paramInfoss.isEmpty) {
      conversion(tree, tree.qualifier)
      wrapped(tree)
    }
    tree
  }

  override def transformTypeApply(tree: tpd.TypeApply)(using Context): tpd.Tree = {
    if (authored && tree.fun.symbol == defn.Any_isInstanceOf)
      tree.args.foreach(argument =>
        typeTest(tree, argument.tpe, Calls.selected(tree.fun).fold(defn.AnyType)(_.qualifier.tpe), fixed = false)
      )
    tree
  }

  override def transformBlock(tree: tpd.Block)(using Context): tpd.Tree = {
    if (authored) closing(tree.stats :+ tree.expr)
    tree
  }

  override def transformTemplate(tree: tpd.Template)(using Context): tpd.Tree = {
    if (identities.active && authored) tokens(tree)
    tree
  }

  override def transformIf(tree: tpd.If)(using Context): tpd.Tree = {
    // Inline methods such as assert expand to conditionals whose source is the method's own.
    if (classifications.active && authored && tree.source == ctx.compilationUnit.source)
      classification(tree.cond, Set.empty)
    tree
  }

  override def transformCaseDef(tree: tpd.CaseDef)(using Context): tpd.Tree = {
    if (classifications.active && authored && !tree.guard.isEmpty) classification(tree.guard, messages(tree.pat))
    tree
  }

  override def transformMatch(tree: tpd.Match)(using Context): tpd.Tree = {
    if (authored) {
      tree.cases.foreach(clause =>
        patterns(clause.pat, tree.selector.tpe.widen, bound = false, fixed = fixedByLibrary(tree.selector))
      )
      refinements.matched(tree)
    }
    tree
  }

  override def transformTry(tree: tpd.Try)(using Context): tpd.Tree = {
    if (authored) tree.cases.foreach(clause => patterns(clause.pat, defn.ThrowableType, bound = false))
    tree
  }

  /** Checks a pattern knowing the static type of the value it matches, so a type test the value always passes, as in
    * the tuples older for comprehensions match their values in, is left alone.
    */
  private def patterns(pattern: tpd.Tree, scrutinee: Type, bound: Boolean, fixed: Boolean = false)(using
      Context
  ): Unit = pattern match {
    case tpd.Bind(_, body)                  => patterns(body, scrutinee, bound = true, fixed)
    case typed @ tpd.Typed(expression, tpt) =>
      if (!(scrutinee <:< tpt.tpe)) {
        typeTest(typed, tpt.tpe, scrutinee, fixed)
        unrelated(typed, tpt.tpe, scrutinee)
        if (!bound && wildcard(expression)) extractable(typed)
      }
      patterns(expression, tpt.tpe, bound)
    case unapply: tpd.UnApply =>
      positional(unapply)
      val components = selectorTypes(unapply)
      unapply.patterns.zipWithIndex.foreach((argument, index) =>
        patterns(argument, components.lift(index).getOrElse(defn.AnyType), bound = false)
      )
    case tpd.Alternative(alternatives) => alternatives.foreach(patterns(_, scrutinee, bound, fixed))
    case _                             => ()
  }

  /** The static types of the values an extractor's argument patterns match. */
  private def selectorTypes(unapply: tpd.UnApply)(using Context): List[Type] = {
    val result   = unapply.fun.tpe.widen.finalResultType
    val optional = result.derivesFrom(defn.OptionClass)
    val product  = if (optional) result.baseType(defn.OptionClass).argInfos.headOption.getOrElse(result) else result
    val selected = Applications.productSelectorTypes(product, NoSourcePosition)
    if (optional && selected.isEmpty) List(product) else selected
  }

  override def transformValDef(tree: tpd.ValDef)(using Context): tpd.Tree = {
    nested(tree, tree.tpt.tpe)
    if (authored) {
      unspecialised(tree)
      refinements.inferred(tree)
    }
    tree
  }

  override def transformDefDef(tree: tpd.DefDef)(using Context): tpd.Tree = {
    nested(tree, tree.tpt.tpe)
    if (authored) {
      Inlines(checks).definition(tree)
      refinements.shadowed(tree)
      refinements.selfCall(tree)
      refinements.inferred(tree)
    }
    tree
  }

  /** Collections of different kinds and unrelated types are compile errors under equality checking; iterators and
    * arrays are not, and comparing anything with null is an ordinary null check.
    */
  private def comparison(tree: tpd.Tree, left: tpd.Tree, right: tpd.Tree)(using Context): Unit = {
    val types = List(left.tpe.widen, right.tpe.widen)
    if (types.exists(_.isRef(defn.NullClass))) ()
    else if (types.exists(iterator(_)))
      checks(
        Check.IteratorEquality,
        "iterators compare by reference and are consumed by the comparison; compare materialised collections or use sameElements",
        tree.srcPos
      )
    else if (types.forall(_.isRef(defn.ArrayClass)))
      checks(Check.ArrayEquality, "arrays compare by reference; compare their elements with sameElements", tree.srcPos)
  }

  private def iterator(tpe: Type)(using Context): Boolean =
    tpe.derivesFrom(defn.IteratorClass) || Classes.derives(tpe, "java.util.Iterator")

  private val OrderedComparisons: Set[Name] = Set(termName("sameElements"), termName("corresponds"))

  /** `sameElements` and `corresponds` compare in iteration order, which an unordered Set or Map does not define. */
  private def unordered(tree: tpd.Tree, receiver: tpd.Tree, argument: tpd.Tree)(using Context): Unit =
    if (List(receiver.tpe.widen, argument.tpe.widen).exists(orderless(_)))
      checks(
        Check.UnorderedComparison,
        "sameElements and corresponds on an unordered collection are unpredictable; compare with ==, subsetOf, or after sorting",
        tree.srcPos
      )

  // Sorted and sequenced maps, and sets kept sorted or in insertion order, define their iteration order.
  private def orderless(tpe: Type)(using Context): Boolean =
    Classes.derives(tpe, "scala.collection.Set") &&
      !Classes.derives(
        tpe,
        "scala.collection.SortedSet",
        "scala.collection.immutable.ListSet",
        "scala.collection.mutable.LinkedHashSet"
      ) ||
      Classes.derives(tpe, "scala.collection.Map") &&
      !Classes.derives(tpe, "scala.collection.SortedMap", "scala.collection.SeqMap")

  private val MaximumPositional = 4

  /** Calls whose positional arguments are easy to misorder: long lists of arguments the type checker cannot tell apart,
    * adjacent numeric literals of one type, pairs of arguments each named after the other's parameter, and bare Boolean
    * literals. An argument named after its own parameter already reads as named. Standard library calls keep their
    * conventional positional style, and parameters named with one or two letters, or by position alone, carry no
    * meaning to add.
    */
  private def arguments(tree: tpd.Apply)(using Context): Unit = {
    val method = tree.fun.symbol
    if (positionals.active && tree.args.lengthCompare(1) > 0 && method.exists && !standard(method)) positional(tree)
  }

  private def positional(tree: tpd.Apply)(using Context): Unit = {
    val parameters = Trees.parameters(tree)
    val contextual = parameters.exists(_.isOneOf(Flags.Given | Flags.Implicit))
    val repeated   = parameters.exists(_.info.isRepeatedParam)
    val written    = tree.args.zip(parameters).takeWhile((argument, _) =>
      !argument.isInstanceOf[tpd.NamedArg] && argument.span.exists && !argument.span.isSynthetic &&
        !argument.symbol.name.is(DefaultGetterName) && !LintPhase.labelled(argument)
    )
    if (!contextual && !repeated && written.size > 1) {
      // A placeholder cannot be named: `f(x = _)` is a function of its own.
      val unlabelled = written.filter((argument, parameter) =>
        !named(argument, parameter) && !argument.symbol.name.is(WildcardParamName) && meaningful(parameter) &&
          parameters.exists(other => other != parameter && other.info =:= parameter.info)
      )
      // Each call gets its most specific warning: a long list, then swapped names, then ambiguous neighbours.
      val long = unlabelled.size > MaximumPositional && written.size == tree.args.size
      if (long)
        positionals(
          Check.LongArguments,
          s"${unlabelled.size} positional arguments; name them",
          tree.srcPos,
          naming(unlabelled)
        )
      written.sliding(2).foreach {
        case Seq((first, left), (second, right))
            if !long && literal(first) && literal(second) && left.info =:= right.info &&
              meaningful(left) && meaningful(right) =>
          positionals(
            Check.SameTypeArguments,
            s"${left.name} and ${right.name} share a type; name the arguments, or take them together as a named tuple " +
              "or a case class, so they cannot be swapped",
            second.srcPos,
            naming(List(first -> left, second -> right))
          )
        case _ => ()
      }
      val byParameter = written.groupBy((_, parameter) => parameter.name)
      written.foreach { (argument, parameter) =>
        called(argument).flatMap(byParameter.get).getOrElse(Nil).find((crossed, other) =>
          other != parameter && named(argument, other) && named(crossed, parameter) &&
            argument.tpe.widen <:< other.info.widen && !deliberate(argument, crossed)
        ).foreach((_, other) =>
          positionals(Check.SwappedArguments, s"${other.name} is passed to ${parameter.name}", argument.srcPos)
        )
        argument match {
          case tpd.Literal(constant)
              if constant.tag == Constants.BooleanTag && parameter.info.isRef(defn.BooleanClass) =>
            positionals(
              Check.BooleanArgument,
              s"name the Boolean argument ${parameter.name}",
              argument.srcPos,
              naming(List(argument -> parameter))
            )
          case _ => ()
        }
      }
    }
  }

  private val Ordinals = Set("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth")
  private val Indexed  = "[A-Za-z]+[0-9]+".r
  private val Standard = Vector("scala.", "java.", "javax.")

  // A name of a letter or two, or one that only says where the parameter is, gives an argument nothing to add.
  private def meaningful(parameter: Symbol)(using Context): Boolean = {
    val name = parameter.name.toString
    name.length > 2 && !Ordinals(name) && !Indexed.matches(name)
  }

  private def naming(arguments: List[(tpd.Tree, Symbol)])(using Context): List[ActionPatch] =
    arguments.map((argument, parameter) => ActionPatch(argument.sourcePos.startPos, s"${label(parameter.name)} = "))

  private def label(name: Name): String = if (nme.keywords(name.toTermName)) s"`$name`" else name.toString

  private def literal(argument: tpd.Tree): Boolean = argument match {
    case tpd.Literal(constant)            => constant.isNumeric && constant.tag != Constants.CharTag
    case tpd.Select(_: tpd.Literal, name) => name == nme.UNARY_-
    case _                                => false
  }

  // A caller passing its own parameters crosswise, as `min(a, b)` calling `less(b, a)`, reorders them on purpose.
  private def deliberate(argument: tpd.Tree, crossed: tpd.Tree)(using Context): Boolean =
    List(argument, crossed).forall(_.symbol.is(Flags.Param)) &&
      argument.symbol.owner == crossed.symbol.owner && argument.symbol.owner == ctx.owner.enclosingMethod

  private def standard(symbol: Symbol)(using Context): Boolean =
    Standard.exists(Calls.qualified(symbol).startsWith(_))

  // A parameter of an override of a library method takes the type the library fixes, which the build cannot change.
  private def fixedByLibrary(selector: tpd.Tree)(using Context): Boolean =
    selector.symbol.is(Flags.Param) && selector.symbol.owner.allOverriddenSymbols.exists(overridden =>
      standard(overridden.owner)
    )

  /** Only types nothing outside the compilation can extend are reported: local ones and those whose access is confined
    * to a class or object, which sealing makes checked. A scrutinee whose static type is an open trait of another
    * module stays unchecked however the tested type is sealed.
    */
  private def typeTest(tree: tpd.Tree, tpe: Type, scrutinee: Type, fixed: Boolean)(using Context): Unit = {
    val symbol  = tpe.widen.dealias.classSymbol
    val matched = scrutinee.widen.dealias.classSymbol
    val foreign = matched.is(Flags.Trait) && !matched.is(Flags.Sealed) &&
      (fixed || !standard(matched) && !matched.isDefinedInCurrentRun)
    if (
      symbol.isOneOf(Flags.Trait | Flags.Abstract) && !symbol.isOneOf(Flags.Sealed | Flags.Final) &&
      !symbol.is(Flags.JavaDefined) && confined(symbol) && !foreign
    )
      checks(
        Check.OpenTypeTest,
        s"type test on ${symbol.name}, which is open; seal it so matches are checked, or dispatch through a method",
        tree.srcPos
      )
  }

  @annotation.tailrec
  private def confined(symbol: Symbol)(using Context): Boolean =
    if (!symbol.exists || symbol.is(Flags.Package)) false
    else if (symbol.owner.isTerm) true
    else {
      val boundary = symbol.accessBoundary(defn.RootClass)
      if (boundary != defn.RootClass && !boundary.is(Flags.Package)) true else confined(symbol.owner)
    }

  /** A type test that no value of the scrutinee's static type passes: two classes neither of which extends the other,
    * which no class can extend both of, or a trait and a class that cannot be extended and does not implement it.
    */
  private def unrelated(tree: tpd.Tree, tpe: Type, scrutinee: Type)(using Context): Unit = {
    def cls(of: Type) = of.widen.dealias match {
      case _: OrType | _: AndType => None
      case other => Some(other.classSymbol).filter(symbol => symbol.isClass && !symbol.is(Flags.JavaDefined))
    }
    def closed(symbol: Symbol) =
      symbol.isOneOf(Flags.Final | Flags.Case | Flags.Module) || symbol.is(Flags.Sealed) && symbol.children.isEmpty
    for {
      tested <- cls(tpe)
      value  <- cls(scrutinee) if !tested.derivesFrom(value) && !value.derivesFrom(tested) && !standard(value)
      if !tested.is(Flags.Trait) && !value.is(Flags.Trait) || tested.is(Flags.Trait) && closed(value) ||
        value.is(Flags.Trait) && closed(tested)
    } checks(
      Check.UnrelatedTypeTest,
      s"no ${scrutinee.widen.show} is a ${tpe.show}, since no class can extend both; this case never matches",
      tree.srcPos
    )
  }

  /** Type tests on types with an extractor, which the extractor states with the shape of the value. */
  private def extractable(typed: tpd.Typed)(using Context): Unit = {
    val cls         = typed.tpt.tpe.widen.dealias.classSymbol
    val constructor = typed.tpt.tpe match {
      case AppliedType(constructor, _) => constructor
      case other                       => other
    }
    // An alias names an extractor only when a term of the same name stands beside it.
    val named = constructor match {
      case alias: TypeRef if alias.symbol.isAliasType => alias.prefix.member(alias.name.toTermName).exists
      case _                                          => true
    }
    val patterns =
      if (!cls.isClass || cls.is(Flags.Module) || !named) None
      else if (cls.is(Flags.Case))
        Some(cls.primaryConstructor.info.paramInfoss.headOption.getOrElse(Nil).map(field =>
          if (field.isRepeatedParam) "_*" else "_"
        ))
      else {
        // Only an extractor declared to always succeed matches exactly what the type test matches.
        val unapply = cls.companionModule.info.member(nme.unapply)
        Option.when(
          unapply.exists && !unapply.isOverloaded && !unapply.symbol.hasAnnotation(defn.DeprecatedAnnot)
        )(unapply.info).filter(
          _.firstParamTypes.headOption.exists(parameter => cls.derivesFrom(parameter.classSymbol))
        ).flatMap(_.finalResultType.baseType(defn.SomeClass).argInfos.headOption.map { value =>
          List.fill(if (defn.isTupleNType(value)) value.dealias.argInfos.size else 1)("_")
        })
      }
    // A tuple's extractor is written as its parentheses alone.
    val name = typed.tpt match {
      case _ if defn.isTupleClass(cls)     => ""
      case tpd.AppliedTypeTree(applied, _) => Trees.text(applied)
      case other                           => Trees.text(other)
    }
    // A case class always has its extractor, which states its arity, however many wildcards that takes.
    patterns.filter(fields => cls.is(Flags.Case) || fields.size <= wildcards).foreach(fields =>
      checks(
        Check.ExtractorTypeTest,
        s"write $name(${fields.mkString(", ")}) instead of the type test _: ${Trees.text(typed.tpt)}; " +
          "an intended type test needs @SuppressWarnings",
        typed.srcPos,
        List(ActionPatch(typed.sourcePos, s"$name(${fields.mkString(", ")})"))
      )
    )
  }

  /** Case class patterns with more positional wildcards than the limit, which name the fields they match instead. */
  private def positional(unapply: tpd.UnApply)(using Context): Unit = {
    val extractor = unapply.fun.symbol
    val cls       = extractor.owner.companionClass
    val fields    = Calls.termClauses(cls.primaryConstructor).headOption.getOrElse(Nil)
    val matched   = fields.zip(unapply.patterns).filterNot((_, pattern) => wildcard(pattern))
    val skipped   = unapply.patterns.count(wildcard(_))
    // A named pattern reaches the typed tree positionally, with the fields it omits filled by synthetic wildcards.
    val named = fields.zip(unapply.patterns).exists((field, pattern) =>
      pattern.span.isSynthetic || Trees.text(pattern).matches(s"(?s)${field.name}\\s*=[^=].*")
    )
    // Tuple fields are positions, named `_1` to `_N`, which a named pattern cannot use.
    if (
      cls.is(Flags.Case) && !defn.isTupleClass(cls) && extractor.name == nme.unapply && extractor.is(Flags.Synthetic) &&
      fields.size == unapply.patterns.size && !fields.exists(_.info.isRepeatedParam) && skipped > wildcards &&
      matched.nonEmpty && !named
    ) {
      val named    = matched.map((field, pattern) => s"${field.name} = ${Trees.text(pattern)}")
      val patterns = unapply.patterns.toVector
      // Each wildcard goes with the separator after it, or before it when no field follows, and each kept pattern
      // gains its field's name, so patterns nested inside keep their own fixes.
      val fix = fields.zip(patterns).zipWithIndex.map { case ((field, pattern), index) =>
        if (!wildcard(pattern)) ActionPatch(pattern.sourcePos.startPos, s"${label(field.name)} = ")
        else if (patterns.drop(index + 1).exists(!wildcard(_)))
          ActionPatch(pattern.sourcePos.withSpan(Span(pattern.span.start, patterns(index + 1).span.start)), "")
        else ActionPatch(pattern.sourcePos.withSpan(Span(patterns(index - 1).span.end, pattern.span.end)), "")
      }
      checks(
        Check.NamedPattern,
        s"write ${Trees.text(unapply).takeWhile(_ != '(')}(${named.mkString(", ")}) instead of $skipped positional " +
          "wildcards; named fields keep matching when fields are added or reordered",
        unapply.srcPos,
        fix
      )
    }
  }

  private def wildcard(pattern: tpd.Tree): Boolean = pattern match {
    case tpd.Ident(nme.WILDCARD) => true
    case _                       => false
  }

  private def named(argument: tpd.Tree, parameter: Symbol)(using Context): Boolean =
    called(argument).contains(parameter.name)

  private def called(argument: tpd.Tree): Option[Name] = argument match {
    case tpd.Ident(name)     => Some(name)
    case tpd.Select(_, name) => Some(name)
    case _                   => None
  }

  private def unitInMap(tree: tpd.Tree, receiver: tpd.Tree, function: tpd.Tree)(using Context): Unit = {
    val source  = receiver.tpe.widen
    val results = function.tpe.widen.argInfos
    if (
      defn.isFunctionType(function.tpe.widen) && results.lastOption.exists(_.isRef(defn.UnitClass)) &&
      (source.derivesFrom(defn.OptionClass) || Classes.derives(source, "scala.collection.IterableOnce"))
    )
      checks(Check.UnitMap, "map with a function returning Unit builds a container of Unit; use foreach", tree.srcPos)
  }

  // The selection starts at the first dot after the receiver, past any parentheses closing around it.
  private def removal(tree: tpd.Tree, receiver: tpd.Tree)(using Context): List[ActionPatch] = {
    val content = tree.source.content()
    val dot     = content.indexOf('.', receiver.span.end)
    Option.when(
      dot >= 0 && dot < tree.span.end &&
        content.slice(receiver.span.end, dot).forall(character => character.isWhitespace || character == ')')
    )(ActionPatch(tree.sourcePos.withSpan(Span(dot, tree.span.end)), "")).toList
  }

  private def conversion(tree: tpd.Tree, receiver: tpd.Tree)(using Context): Unit = {
    val source = receiver.tpe.widen.dealias
    val target = tree.tpe.widen.widenExpr.dealias
    if (!source.isRef(defn.NothingClass) && source =:= target)
      checks(
        Check.RedundantConversion,
        s"${tree.symbol.name} on a value that is already ${source.show} does nothing",
        tree.srcPos,
        removal(tree, receiver)
      )
  }

  private val Unwrapped = Map(
    "toByte"   -> "byteValue",
    "toShort"  -> "shortValue",
    "toInt"    -> "intValue",
    "toLong"   -> "longValue",
    "toFloat"  -> "floatValue",
    "toDouble" -> "doubleValue"
  )
  private val Wrappers = Map(
    "scala.math.BigDecimal" -> ("java.math.BigDecimal", "javaBigDecimal2bigDecimal"),
    "scala.math.BigInt"     -> ("java.math.BigInteger", "javaBigInteger2bigInt")
  )

  /** A Java big number wrapped in its Scala counterpart only to be converted, which its own conversion method does
    * without the wrapper.
    */
  private def wrapped(tree: tpd.Select)(using Context): Unit = Unwrapped.get(tree.name.toString).foreach { method =>
    Calls.peelInlined(tree.qualifier) match {
      case tpd.Apply(function, List(argument)) =>
        val symbol = function.symbol
        val owner  = symbol.owner.companionClass
        Wrappers.get(Calls.qualified(owner)).filter((java, converter) =>
          (symbol.name == nme.apply || symbol.name.toString == converter) &&
            Classes.derives(argument.tpe.widen, java)
        ).foreach { _ =>
          val simple = Trees.text(argument).matches("[A-Za-z_][A-Za-z0-9_.]*")
          checks(
            Check.WrappedConversion,
            s"${Trees.text(argument)}.$method converts the ${argument.tpe.widen.show} directly; wrapping it in " +
              s"${owner.name} only to call ${tree.name} allocates",
            tree.srcPos,
            if (simple) List(ActionPatch(tree.sourcePos, s"${Trees.text(argument)}.$method")) else Nil
          )
        }
      case _ => ()
    }
  }

  private def nested(tree: tpd.Tree, tpe: Type)(using Context): Unit =
    tpe.widen.dealias match {
      case AppliedType(outer, List(AppliedType(inner, _))) =>
        val future: Symbol = Classes.required("scala.concurrent.Future")
        if (outer.typeSymbol == future && inner.typeSymbol == future)
          checks(Check.NestedFuture, "a future of a future is usually a missing flatMap or flatten", tree.srcPos)
      case _ => ()
    }

  private def coldCode(using Context): Boolean =
    Trees.enclosedBy(ctx.owner, owner => cold(Calls.qualified(owner)))

  /** Parameters, fields and values of a function type that Scala does not specialise for its primitive type arguments.
    * Boxing a Boolean returns a shared instance, so predicates are left alone. A constructor reports its parameters
    * rather than the fields they become, an overriding method leaves them to the method it overrides, a lambda's
    * parameters are not declarations, and classes the build leaves out of its hot ones, such as test suites, may box.
    */
  private def unspecialised(tree: tpd.ValDef)(using Context): Unit = {
    val symbol   = tree.symbol
    val function = tree.tpt.tpe.dealias
    if (symbol.exists && defn.isPlainFunctionClass(function.typeSymbol)) unspecialised(tree, symbol, function)
  }

  private def unspecialised(tree: tpd.ValDef, symbol: Symbol, function: Type)(using Context): Unit = {
    val arguments = function.argInfos
    if (
      !symbol.isOneOf(Flags.Synthetic | Flags.Inline | Flags.ParamAccessor) && !coldCode &&
      !(symbol.is(Flags.Param) && (symbol.owner.isAnonymousFunction || symbol.owner.allOverriddenSymbols.hasNext)) &&
      !symbol.allOverriddenSymbols.hasNext &&
      arguments.exists(argument => Trees.primitive(argument) && !argument.isRef(defn.BooleanClass)) &&
      !defn.isSpecializableFunctionSAM(arguments.init, arguments.last)
    ) {
      val boxed = if (arguments.init.exists(Trees.primitive(_))) "arguments" else "result"
      checks(
        Check.UnspecialisedFunction,
        s"Function${arguments.size - 1}[${arguments.map(_.show).mkString(", ")}] is not specialised and boxes its " +
          s"primitive $boxed on every call; make it inline, a SAM trait with primitive parameters, or a class",
        tree.srcPos
      )
    }
  }

  private val ClassifyingMethods =
    Set("contains", "startsWith", "endsWith", "matches", "equals", "equalsIgnoreCase", "==", "!=")
  private val Messages = Set("getMessage", "getLocalizedMessage", "message")

  // An error's message is prose for people; deciding control flow by it breaks when the wording changes.
  private def classification(condition: tpd.Tree, bound: Set[Symbol])(using Context): Unit = {
    @annotation.tailrec
    def message(candidate: tpd.Tree): Boolean = Calls.peelInlined(candidate) match {
      case tpd.Apply(select: tpd.Select, Nil) => message(select)
      case tpd.Select(owner, name)            =>
        Messages(name.toString) && (name.toString == "message" || owner.tpe.widen.derivesFrom(defn.ThrowableClass))
      case reference: tpd.Ident => bound(reference.symbol)
      case _                    => false
    }
    def text(candidate: tpd.Tree): Boolean = Calls.peelInlined(candidate) match {
      case tpd.Literal(constant) => constant.tag == Constants.StringTag
      case _                     => false
    }
    condition.filterSubTrees {
      case tpd.Apply(tpd.Select(receiver, name), List(argument)) =>
        ClassifyingMethods(name.toString) &&
        (message(receiver) && text(argument) || message(argument) && text(receiver))
      case _ => false
    }.foreach(comparison =>
      classifications(
        Check.MessageClassification,
        "classifying an error by its message text breaks when the wording changes; give the error a distinct case or type",
        comparison.srcPos
      )
    )
  }

  /** Variables a pattern binds to the message field of a case class. */
  private def messages(pattern: tpd.Tree)(using Context): Set[Symbol] = pattern.filterSubTrees {
    case _: tpd.UnApply => true
    case _              => false
  }.collect { case unapply: tpd.UnApply =>
    val fields = Calls.termClauses(unapply.fun.symbol.owner.companionClass.primaryConstructor).headOption
    fields.getOrElse(Nil).zip(unapply.patterns).collect {
      case (field, binding: tpd.Bind) if Messages(field.name.toString) => binding.symbol
    }
  }.flatten.toSet

  /** A local closed by a statement of its own block after others that can fail first, leaking it when they do. A value
    * a `Using.Manager` registered closes on every path, as does one an earlier statement already closes.
    */
  private def closing(statements: List[tpd.Tree])(using Context): Unit = {
    val all          = statements.toVector
    lazy val closers = all.zipWithIndex.flatMap((statement, at) =>
      statement.filterSubTrees(closed(_).isDefined).flatMap(closed(_)).distinct.map(_ -> at)
    ).groupMap(_._1)(_._2)
    all.zipWithIndex.foreach {
      case (value: tpd.ValDef, index) if closeable(value.symbol.info) && !registered(value.rhs) =>
        closers.getOrElse(value.symbol, Vector.empty).find(_ > index).filter(_ > index + 1)
          .filter(at => closed(all(at)).contains(value.symbol)).foreach(at =>
            checks(
              Check.UnguardedClose,
              s"${value.name} is closed only when the statements before close succeed; use Using.resource or try/finally",
              all(at).srcPos
            )
          )
      case _ => ()
    }
  }

  private def closed(tree: tpd.Tree)(using Context): Option[Symbol] = Calls.peelInlined(tree) match {
    case tpd.Apply(tpd.Select(reference: tpd.Ident, name), Nil) if name.toString == "close" => Some(reference.symbol)
    case _                                                                                  => None
  }

  private def registered(value: tpd.Tree)(using Context): Boolean =
    Calls.qualified(Calls.peelInlined(value).symbol) == "scala.util.Using.Manager.apply"

  private def closeable(tpe: Type)(using Context): Boolean =
    tpe.member(termName("close")).alternatives.exists(_.info.paramInfoss.flatten.isEmpty)

  /** Private `Object()` fields used only by identity comparisons, or copied into values that are, which reimplement the
    * identity of their owner. Lock objects are used otherwise.
    */
  private def tokens(template: tpd.Template)(using Context): Unit = {
    val candidates = template.body.collect {
      case field: tpd.ValDef
          if field.symbol.is(Flags.Private) && !field.symbol.is(Flags.Mutable) &&
            (field.tpt.tpe =:= defn.ObjectType || field.tpt.tpe =:= defn.AnyType) && fresh(field.rhs) =>
        field.symbol
    }.toSet
    if (candidates.nonEmpty) {
      val uses = Tokens(candidates)
      template.body.foreach(uses.traverse(_))
      template.body.foreach {
        case field: tpd.ValDef
            if candidates(field.symbol) && uses.compared(field.symbol) && !uses.other(field.symbol) =>
          identities(
            Check.IdentityToken,
            s"${field.name} is an Object() compared only by identity; make the operation a method of its owner, or " +
              "compare the owners",
            field.srcPos
          )
        case _ => ()
      }
    }
  }

  private def fresh(tree: tpd.Tree)(using Context): Boolean = Calls.peelInlined(tree) match {
    case application @ tpd.Apply(_, Nil) =>
      application.fun.symbol.isConstructor && application.fun.symbol.owner == defn.ObjectClass
    case _ => false
  }

  private class Tokens(candidates: Set[Symbol]) extends tpd.TreeTraverser {
    val compared: mutable.Set[Symbol] = mutable.Set.empty
    val other: mutable.Set[Symbol]    = mutable.Set.empty

    private def token(tree: tpd.Tree)(using Context): Option[Symbol] =
      Some(Calls.peelInlined(tree).symbol).filter(candidates)

    override def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
      case field: tpd.ValDef if candidates(field.symbol) => ()
      case tpd.Apply(function @ tpd.Select(left, _), List(right))
          if function.symbol == defn.Object_eq || function.symbol == defn.Object_ne =>
        List(left, right).foreach(operand => token(operand).fold(traverse(operand))(compared += _))
      case application @ tpd.Apply(function, arguments) if Trees.constructs(application.fun.symbol) =>
        traverse(function)
        arguments.foreach(argument => if (token(argument).isEmpty) traverse(argument))
      case reference: (tpd.Ident | tpd.Select) if candidates(reference.symbol) => other += reference.symbol
      case _                                                                   => traverseChildren(tree)
    }
  }
}

private object LintPhase {
  val Positionals: Set[Check] =
    Set(Check.LongArguments, Check.SameTypeArguments, Check.SwappedArguments, Check.BooleanArgument)

  val Classifications: Set[Check] = Set(Check.MessageClassification)

  val Identities: Set[Check] = Set(Check.IdentityToken)

  private def space(char: Char): Boolean = char == ' ' || (char >= '\t' && char <= '\r')

  private def identifierStart(char: Char): Boolean =
    char >= 'a' && char <= 'z' || char >= 'A' && char <= 'Z' || char == '_' || char == '$'

  private def identifierPart(char: Char): Boolean = identifierStart(char) || char >= '0' && char <= '9'

  /** Whether the 256 characters of `content` before `end` finish with `(name = ` or `, name = `. */
  private def preceded(content: Array[Char], end: Int): Boolean = {
    val from = (end - 256).max(0)
    var at   = end.min(content.length) - 1
    while (at >= from && space(content(at))) at -= 1
    if (at < from || content(at) != '=') false
    else {
      at -= 1
      while (at >= from && space(content(at))) at -= 1
      val last = at
      while (at >= from && identifierPart(content(at))) at -= 1
      if (at == last || !identifierStart(content(at + 1))) false
      else {
        while (at >= from && space(content(at))) at -= 1
        at >= from && (content(at) == '(' || content(at) == ',')
      }
    }
  }

  /** Whether the text of `content` from `start` until `end` begins `name = value`. */
  private def begins(content: Array[Char], start: Int, end: Int): Boolean = {
    val limit = end.min(content.length)
    var at    = start
    if (at >= limit || !identifierStart(content(at))) false
    else {
      while (at < limit && identifierPart(content(at))) at += 1
      while (at < limit && space(content(at))) at += 1
      at + 1 < limit && content(at) == '=' && content(at + 1) != '=' && content(at + 1) != '>'
    }
  }

  /** Whether an argument was written as `name = value`, which typing can erase, as overload resolution does. Its span
    * may start at the value or at the name.
    */
  private def labelled(argument: tpd.Tree)(using Context): Boolean = argument.span.exists &&
    !argument.span.isSynthetic && {
      val position = argument.sourcePos
      preceded(argument.source.content(), argument.span.start) ||
      begins(position.source.content(), position.start, position.end)
    }
}
