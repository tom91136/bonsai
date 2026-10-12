package net.kurobako.baumkuchen.plugin

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
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.AppliedType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeRef
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.rewrites.Rewrites.ActionPatch
import dotty.tools.dotc.typer.Applications
import dotty.tools.dotc.util.NoSourcePosition
import dotty.tools.dotc.util.Spans.Span
import scala.collection.mutable

/** Lints that need the types of expressions, which SemanticDB does not record: array comparisons, conversions to the
  * type a value already has, `map` with a function producing `Unit`, nested futures, positional arguments that are easy
  * to misorder, and type tests on open hierarchies.
  */
private class LintPhase(
    wildcards: Int,
    checks: Checks,
    partials: Map[String, Exceptions.Partial],
    throwing: List[(scala.util.matching.Regex, List[String])]
) extends PluginPhase {
  val phaseName: String                = "baumkuchen-lint"
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
    "toString",
    "toList",
    "toVector",
    "toSeq",
    "toIndexedSeq",
    "toArray"
  )

  // Compiler-generated methods are not the author's code, but lambdas, which are synthetic methods too, are.
  private def authored(using Context): Boolean = !ctx.owner.is(Flags.Synthetic) || ctx.owner.isAnonymousFunction

  override def transformUnit(tree: tpd.Tree)(using Context): tpd.Tree = {
    Loops(checks).traverse(tree)
    Exceptions(checks, partials, throwing).run(tree)
    Effects(checks).run(tree)
    Concurrency(checks).traverse(tree)
    Fields(checks).run(tree)
    tree
  }

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree = {
    if (authored) {
      arguments(tree)
      tree match {
        case tpd.Apply(function @ tpd.Select(left, _), List(right))
            if function.symbol == defn.Any_== || function.symbol == defn.Any_!= =>
          comparison(tree, left, right)
        case tpd.Apply(tpd.TypeApply(tpd.Select(receiver, nme.map), _), List(function)) =>
          unitInMap(tree, receiver, function)
        case tpd.Apply(tpd.Select(receiver, nme.map), List(function)) => unitInMap(tree, receiver, function)
        case tpd.Apply(select: tpd.Select, Nil) if select.symbol.name == nme.toString_ =>
          conversion(tree, select.qualifier)
        case _ => ()
      }
    }
    tree
  }

  override def transformSelect(tree: tpd.Select)(using Context): tpd.Tree = {
    if (authored && Conversions(tree.name.toString) && tree.tpe.widen.paramInfoss.isEmpty)
      conversion(tree, tree.qualifier)
    tree
  }

  override def transformTypeApply(tree: tpd.TypeApply)(using Context): tpd.Tree = {
    if (authored && tree.fun.symbol == defn.Any_isInstanceOf)
      tree.args.foreach(argument => typeTest(tree, argument.tpe))
    tree
  }

  override def transformBlock(tree: tpd.Block)(using Context): tpd.Tree = {
    if (authored) closing(tree.stats :+ tree.expr)
    tree
  }

  override def transformTemplate(tree: tpd.Template)(using Context): tpd.Tree = {
    if (authored) tokens(tree)
    tree
  }

  override def transformIf(tree: tpd.If)(using Context): tpd.Tree = {
    // Inline methods such as assert expand to conditionals whose source is the method's own.
    if (authored && tree.source == ctx.compilationUnit.source) classification(tree.cond, Set.empty)
    tree
  }

  override def transformCaseDef(tree: tpd.CaseDef)(using Context): tpd.Tree = {
    if (authored && !tree.guard.isEmpty) classification(tree.guard, messages(tree.pat))
    tree
  }

  override def transformMatch(tree: tpd.Match)(using Context): tpd.Tree = {
    if (authored) tree.cases.foreach(clause => patterns(clause.pat, tree.selector.tpe.widen, bound = false))
    tree
  }

  override def transformTry(tree: tpd.Try)(using Context): tpd.Tree = {
    if (authored) tree.cases.foreach(clause => patterns(clause.pat, defn.ThrowableType, bound = false))
    tree
  }

  /** Checks a pattern knowing the static type of the value it matches, so a type test the value always passes, as in
    * the tuples older for comprehensions match their values in, is left alone.
    */
  private def patterns(pattern: tpd.Tree, scrutinee: Type, bound: Boolean)(using Context): Unit = pattern match {
    case tpd.Bind(_, body)                  => patterns(body, scrutinee, bound = true)
    case typed @ tpd.Typed(expression, tpt) =>
      if (!(scrutinee <:< tpt.tpe)) {
        typeTest(typed, tpt.tpe)
        if (!bound && wildcard(expression)) extractable(typed)
      }
      patterns(expression, tpt.tpe, bound)
    case unapply: tpd.UnApply =>
      positional(unapply)
      val components = selectorTypes(unapply)
      unapply.patterns.zipWithIndex.foreach((argument, index) =>
        patterns(argument, components.lift(index).getOrElse(defn.AnyType), bound = false)
      )
    case tpd.Alternative(alternatives) => alternatives.foreach(patterns(_, scrutinee, bound))
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
    tree
  }

  override def transformDefDef(tree: tpd.DefDef)(using Context): tpd.Tree = {
    nested(tree, tree.tpt.tpe)
    tree
  }

  // Unrelated types and collection kinds are already compile errors under multiversal equality; arrays are not, and
  // comparing one with null is an ordinary null check.
  private def comparison(tree: tpd.Tree, left: tpd.Tree, right: tpd.Tree)(using Context): Unit = {
    val types = List(left.tpe.widen, right.tpe.widen)
    if (types.exists(_.isRef(defn.ArrayClass)) && !types.exists(_.isRef(defn.NullClass)))
      checks(Check.ArrayEquality, "arrays compare by reference; compare their elements with sameElements", tree.srcPos)
  }

  private val MaximumPositional = 4

  /** Calls whose positional arguments are easy to misorder: long lists of arguments the type checker cannot tell apart,
    * adjacent numeric literals of one type, pairs of arguments each named after the other's parameter, and bare Boolean
    * literals. An argument named after its own parameter already reads as named. Standard library calls keep their
    * conventional positional style, and parameters named with one or two letters carry no meaning to add.
    */
  private def arguments(tree: tpd.Apply)(using Context): Unit = {
    val method     = tree.fun.symbol
    val parameters = Trees.parameters(tree)
    val contextual = parameters.exists(_.isOneOf(Flags.Given | Flags.Implicit))
    val repeated   = parameters.exists(_.info.isRepeatedParam)
    val written    = tree.args.zip(parameters).takeWhile((argument, _) =>
      !argument.isInstanceOf[tpd.NamedArg] && !Trees.named(argument) && !argument.symbol.name.is(DefaultGetterName) &&
        argument.span.exists &&
        !argument.span.isSynthetic
    )
    if (method.exists && !standard(method) && !contextual && !repeated && written.size > 1) {
      // A placeholder cannot be named: `f(x = _)` is a function of its own.
      val unlabelled = written.filter((argument, parameter) =>
        !named(argument, parameter) && !argument.symbol.name.is(WildcardParamName) &&
          parameters.exists(other => other != parameter && other.info =:= parameter.info)
      )
      // Each call gets its most specific warning: a long list, then swapped names, then ambiguous neighbours.
      val long = unlabelled.size > MaximumPositional && written.size == tree.args.size
      if (long)
        checks(
          Check.LongArguments,
          s"${unlabelled.size} positional arguments; name them",
          tree.srcPos,
          naming(unlabelled)
        )
      written.sliding(2).foreach {
        case Seq((first, left), (second, right))
            if !long && literal(first) && literal(second) && left.info =:= right.info &&
              left.name.toString.length > 2 && right.name.toString.length > 2 =>
          checks(
            Check.SameTypeArguments,
            s"${left.name} and ${right.name} share a type; name the arguments so they cannot be swapped",
            second.srcPos,
            naming(List(first -> left, second -> right))
          )
        case _ => ()
      }
      written.foreach { (argument, parameter) =>
        written.find((crossed, other) =>
          other != parameter && named(argument, other) && named(crossed, parameter) &&
            argument.tpe.widen <:< other.info.widen && !deliberate(argument, crossed)
        ).foreach((_, other) =>
          checks(Check.SwappedArguments, s"${other.name} is passed to ${parameter.name}", argument.srcPos)
        )
        argument match {
          case tpd.Literal(constant)
              if constant.tag == Constants.BooleanTag && parameter.info.isRef(defn.BooleanClass) =>
            checks(
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
    Vector("scala.", "java.", "javax.").exists(symbol.fullName.toString.startsWith(_))

  // Only hierarchies the build can seal are reported: those sharing the two leading package segments of the test.
  private def typeTest(tree: tpd.Tree, tpe: Type)(using Context): Unit = {
    val symbol                      = tpe.widen.dealias.classSymbol
    def organisation(owner: Symbol) = owner.enclosingPackageClass.fullName.toString.split('.').take(2).toList
    if (
      symbol.isOneOf(Flags.Trait | Flags.Abstract) && !symbol.isOneOf(Flags.Sealed | Flags.Final) &&
      !symbol.is(Flags.JavaDefined) && organisation(symbol) == organisation(ctx.owner)
    )
      checks(
        Check.OpenTypeTest,
        s"type test on ${symbol.name}, which is open; seal it so matches are checked, or dispatch through a method",
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
    patterns.filter(_.size <= wildcards).foreach(fields =>
      checks(
        Check.ExtractorTypeTest,
        s"write $name(${fields.mkString(", ")}) instead of the type test _: ${Trees.text(typed.tpt)}; " +
          "an intended type test needs @nowarn",
        typed.srcPos,
        List(ActionPatch(typed.sourcePos, s"$name(${fields.mkString(", ")})"))
      )
    )
  }

  /** Case class patterns with more positional wildcards than the limit, which name the fields they match instead. */
  private def positional(unapply: tpd.UnApply)(using Context): Unit = {
    val extractor = unapply.fun.symbol
    val cls       = extractor.owner.companionClass
    val fields    = cls.primaryConstructor.paramSymss.find(_.headOption.exists(_.isTerm)).getOrElse(Nil)
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

  private def named(argument: tpd.Tree, parameter: Symbol)(using Context): Boolean = argument match {
    case tpd.Ident(name)     => name == parameter.name
    case tpd.Select(_, name) => name == parameter.name
    case _                   => false
  }

  private def unitInMap(tree: tpd.Tree, receiver: tpd.Tree, function: tpd.Tree)(using Context): Unit = {
    val source = receiver.tpe.widen
    val mapped = source.derivesFrom(requiredClass("scala.collection.IterableOnce")) ||
      source.derivesFrom(defn.OptionClass)
    val results = function.tpe.widen.argInfos
    if (mapped && defn.isFunctionType(function.tpe.widen) && results.lastOption.exists(_.isRef(defn.UnitClass)))
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

  private def nested(tree: tpd.Tree, tpe: Type)(using Context): Unit = {
    val future: Symbol = requiredClass("scala.concurrent.Future")
    tpe.widen.dealias match {
      case AppliedType(outer, List(AppliedType(inner, _)))
          if outer.typeSymbol == future && inner.typeSymbol == future =>
        checks(Check.NestedFuture, "a future of a future is usually a missing flatMap or flatten", tree.srcPos)
      case _ => ()
    }
  }

  private val Classifications =
    Set("contains", "startsWith", "endsWith", "matches", "equals", "equalsIgnoreCase", "==", "!=")
  private val Messages = Set("getMessage", "getLocalizedMessage", "message")

  // An error's message is prose for people; deciding control flow by it breaks when the wording changes.
  private def classification(condition: tpd.Tree, bound: Set[Symbol])(using Context): Unit = {
    @annotation.tailrec
    def message(candidate: tpd.Tree): Boolean = Trees.unwrap(candidate) match {
      case tpd.Apply(select: tpd.Select, Nil) => message(select)
      case tpd.Select(owner, name)            =>
        Messages(name.toString) && (name.toString == "message" || owner.tpe.widen.derivesFrom(defn.ThrowableClass))
      case reference: tpd.Ident => bound(reference.symbol)
      case _                    => false
    }
    def text(candidate: tpd.Tree): Boolean = Trees.unwrap(candidate) match {
      case tpd.Literal(constant) => constant.tag == Constants.StringTag
      case _                     => false
    }
    condition.filterSubTrees {
      case tpd.Apply(tpd.Select(receiver, name), List(argument)) =>
        Classifications(name.toString) && (message(receiver) && text(argument) || message(argument) && text(receiver))
      case _ => false
    }.foreach(comparison =>
      checks(
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
    val fields =
      unapply.fun.symbol.owner.companionClass.primaryConstructor.paramSymss.find(_.headOption.exists(_.isTerm))
    fields.getOrElse(Nil).zip(unapply.patterns).collect {
      case (field, binding: tpd.Bind) if Messages(field.name.toString) => binding.symbol
    }
  }.flatten.toSet

  /** A local closed by a statement of its own block after others that can fail first, leaking it when they do. */
  private def closing(statements: List[tpd.Tree])(using Context): Unit = statements.zipWithIndex.foreach {
    case (value: tpd.ValDef, index) if closeable(value.symbol.info) =>
      statements.drop(index + 2).find(statement =>
        Trees.unwrap(statement) match {
          case tpd.Apply(tpd.Select(reference: tpd.Ident, name), Nil) =>
            name.toString == "close" && reference.symbol == value.symbol
          case _ => false
        }
      ).foreach(statement =>
        checks(
          Check.UnguardedClose,
          s"${value.name} is closed only when the statements before close succeed; use Using.resource or try/finally",
          statement.srcPos
        )
      )
    case _ => ()
  }

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
          checks(
            Check.IdentityToken,
            s"${field.name} is an Object() compared only by identity; make the operation a method of its owner, or " +
              "compare the owners",
            field.srcPos
          )
        case _ => ()
      }
    }
  }

  private def fresh(tree: tpd.Tree)(using Context): Boolean = Trees.unwrap(tree) match {
    case application @ tpd.Apply(_, Nil) =>
      application.fun.symbol.isConstructor && application.fun.symbol.owner == defn.ObjectClass
    case _ => false
  }

  private def constructs(symbol: Symbol)(using Context): Boolean =
    symbol.isConstructor || symbol.name == nme.apply && symbol.is(Flags.Synthetic)

  private class Tokens(candidates: Set[Symbol]) extends tpd.TreeTraverser {
    val compared: mutable.Set[Symbol] = mutable.Set.empty
    val other: mutable.Set[Symbol]    = mutable.Set.empty

    private def token(tree: tpd.Tree)(using Context): Option[Symbol] =
      Some(Trees.unwrap(tree).symbol).filter(candidates)

    override def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
      case field: tpd.ValDef if candidates(field.symbol) => ()
      case tpd.Apply(function @ tpd.Select(left, _), List(right))
          if function.symbol == defn.Object_eq || function.symbol == defn.Object_ne =>
        List(left, right).foreach(operand => token(operand).fold(traverse(operand))(compared += _))
      case application @ tpd.Apply(function, arguments) if constructs(application.fun.symbol) =>
        traverse(function)
        arguments.foreach(argument => if (token(argument).isEmpty) traverse(argument))
      case reference: (tpd.Ident | tpd.Select) if candidates(reference.symbol) => other += reference.symbol
      case _                                                                   => traverseChildren(tree)
    }
  }
}
