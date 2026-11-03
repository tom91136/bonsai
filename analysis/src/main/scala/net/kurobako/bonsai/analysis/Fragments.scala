package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.core.NameKinds.DefaultGetterName
import dotty.tools.dotc.core.NameOps.isOperatorName
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types
import net.kurobako.bonsai.collections.*
import net.kurobako.bonsai.java.Clones.*
import net.kurobako.bonsai.lifting.*
import scala.annotation.tailrec
import scala.collection.mutable

/** Finds duplicated code the way IntelliJ's duplicate locator does: subtrees and statement windows of method bodies are
  * hashed bottom-up with local definitions anonymised, and fragments sharing a hash are confirmed with a hash that
  * numbers locals by first occurrence, so `a + a` and `a + b` differ while consistent renamings match. Fragments that
  * also match once the values of literals are ignored are near duplicates, reported with the literals that differ as
  * holes.
  */
class Fragments(lines: Lines)(using Context) {

  /** Where a fragment comes from, as `Clones.Fragment` describes it. `helper` holds when calls of the member may
    * replace copies of its body, which `reach` limits to the code of a type unless all code may call it, and
    * `parameters` are what such a call passes.
    */
  private case class Origin(
      node: String,
      test: Boolean,
      family: String,
      specialised: Boolean,
      method: Boolean,
      member: String,
      constrained: Boolean,
      helper: Boolean,
      reach: Option[String],
      parameters: List[Symbol]
  )

  private class Piece(
      val trees: List[tpd.Tree],
      val cost: Int,
      val origin: Origin,
      val shape: Long,
      val loose: Long,
      val roles: Roles,
      val listing: Option[(String, Int, Int)]
  )

  /** What makes a piece a candidate for each of the ways copies are found. */
  private case class Roles(
      whole: Boolean,
      small: Boolean,
      tiny: Boolean,
      entire: Boolean,
      direct: Boolean,
      helper: Boolean
  )

  private case class Shaped(hash: Long, loose: Long, broad: Long, cost: Int, applications: Int)

  // A single dense expression is worth reporting from two lines.
  private val ExpressionLines = 2
  private val FamilyCost      = 12
  private val Seed            = 17L
  // Many fragments alike but for their literals are a table of rows, not copies a shared method would replace, and
  // grouping them grows with the square of the rows.
  private val MaximumNear = 32

  private val subtrees    = Children()
  private val pieces      = mutable.LongMap.empty[mutable.ArrayBuffer[Piece]]
  private val broadPieces = mutable.LongMap.empty[mutable.ArrayBuffer[Piece]]
  private val stable      = mutable.Map.empty[Symbol, Long]

  /** Hashes a member body owned by `node`; a method's body may form a family with others alike but for numbers, while a
    * value's initialiser, such as an array of some size, is no method a family would share.
    */
  def add(body: tpd.Tree, node: String, test: Boolean, member: Symbol, family: String, specialised: Boolean): Unit =
    if (!row(body)) shape(body, origin(node, test, member, family, specialised, body), entire = true): Unit

  def groups(): Vector[Clones.Group] = Clones.groups(candidates())

  private def origin(
      node: String,
      test: Boolean,
      member: Symbol,
      family: String,
      specialised: Boolean,
      body: tpd.Tree
  ): Origin = {
    val method      = member.is(Flags.Method)
    val constrained = member.is(Flags.Override) || member.allOverriddenSymbols.nonEmpty
    val parameters  = member.paramSymss.flatten.filter(_.isTerm)
    val callable    = !test && !constrained &&
      !member.isOneOf(Flags.Inline | Flags.Macro | Flags.Given | Flags.Synthetic) &&
      !member.name.is(NameKinds.UniqueName) &&
      !parameters.exists(_.isOneOf(Flags.GivenOrImplicit | Flags.Erased)) && faithful(member, body)
    val everywhere = member.ownersIterator.drop(1).takeWhile(!_.is(Flags.Package)).forall(owner =>
      owner.is(Flags.Module) && !owner.isOneOf(Flags.Private | Flags.Protected) && !owner.privateWithin.exists
    ) && !member.isOneOf(Flags.Private | Flags.Protected) && !member.privateWithin.exists
    Origin(
      node,
      test,
      family,
      specialised,
      method,
      member.name.toString,
      constrained,
      callable,
      Option.unless(everywhere)(node),
      parameters
    )
  }

  /** Whether a call of `member` has the type of its body, which a declared result type that is wider, or an opaque
    * alias the body does not have outside its scope, would change.
    */
  private def faithful(member: Symbol, body: tpd.Tree): Boolean = {
    val declared = member.info.widenExpr.finalResultType
    typeOf(body).forall(actual =>
      declared <:< actual.widen && !declared.existsPart {
        case reference: Types.NamedType =>
          reference.symbol.is(Flags.Opaque) && !actual.existsPart {
            case other: Types.NamedType => other.symbol == reference.symbol
            case _                      => false
          }
        case _ => false
      }
    )
  }

  private def typeOf(tree: tpd.Tree): Option[Types.Type] = Option(tree.tpe)

  /** The fragments that share a hash with another, confirmed with hashes that number locals by first occurrence. */
  private def candidates(): Vector[Clones.Candidate] = {
    val chosen = mutable.LinkedHashMap.empty[Piece, Boolean]
    pieces.values.filter(_.size > 1).foreach { alike =>
      val crowded = alike.count(_.roles.whole) > MaximumNear
      alike.foreach(piece => chosen(piece) = crowded)
    }
    broadPieces.values.filter(_.size > 1).foreach(_.foreach(piece => chosen.getOrElseUpdate(piece, false)))
    chosen.view.flatMap((piece, crowded) => candidate(piece, crowded)).toVector
  }

  private def candidate(piece: Piece, crowded: Boolean): Option[Clones.Candidate] =
    locate(piece).map { (fragment, span) =>
      val trees                 = piece.trees
      val (exact, loose, broad) = numbered(trees)
      Clones.Candidate(
        fragment,
        span,
        piece.cost,
        exact = exact,
        loose = loose,
        literals = Clones.Deferred(literals(trees).map(rendered(_))),
        minimumLines =
          if (trees.size > 1) Clones.WindowLines else if (piece.cost >= CloneCost.Expression) ExpressionLines else 1,
        solid = piece.cost >= CloneCost.Expression,
        crowded = crowded,
        application = !piece.origin.method && applications(trees),
        callee = Clones.Deferred(callee(trees)),
        listing = piece.listing,
        mapping = trees.exists(hasMap(_)),
        broad = if (trees.size == 1) broad else 0L,
        variables = Clones.Deferred(variables(trees, piece.origin.parameters)),
        small = piece.roles.small,
        tiny = piece.roles.tiny,
        entire = piece.roles.entire,
        direct = piece.roles.direct,
        helper = piece.roles.helper,
        reach = piece.origin.reach,
        whole = piece.roles.whole,
        method = piece.origin.method
      )
    }

  private def hasMap(tree: tpd.Tree): Boolean =
    typeOf(tree).exists(_.widen.baseClasses.exists(cls => cls.name.toString.endsWith("Map"))) ||
      subtrees.of(tree).exists(hasMap(_))

  private def spanOf(tree: tpd.Tree): Option[(String, Int, Int)] =
    Option.when(tree.span.exists && tree.source.exists)((tree.source.path, tree.span.start, tree.span.end))

  // Elements of one argument list are rows of a table, which a shared method would not replace.
  private def element(parent: tpd.Tree, listing: Option[(String, Int, Int)], child: tpd.Tree) = parent match {
    case tpd.Apply(_, args) if args.exists(_ eq child)              => spanOf(parent)
    case tpd.SeqLiteral(elems, _) if elems.exists(_ eq child)       => spanOf(parent)
    case block: tpd.Block if (block.expr eq child) && lifted(block) => listing
    case _                                                          => None
  }

  // Named arguments given out of order make the compiler hold them in values of a block around the call.
  private def lifted(block: tpd.Block): Boolean = block.stats.forall(_.symbol.is(Flags.Synthetic))

  private def shape(
      tree: tpd.Tree,
      origin: Origin,
      entire: Boolean,
      pattern: Boolean = false,
      listing: Option[(String, Int, Int)] = None
  ): Shaped = {
    val children = subtrees.of(tree)
    val shapes   = tree match {
      case tpd.CaseDef(pat, _, _) => children.map(child => shape(child, origin, entire = false, pattern = child eq pat))
      case _ => children.map(child => shape(child, origin, entire = false, pattern, element(tree, listing, child)))
    }
    val cost         = weight(tree) + shapes.map(_.cost).sum
    val applications = (tree match { case tpd.Apply(_, _) => 1; case _ => 0 }) + shapes.map(_.applications).sum
    val (ownHash, ownLoose, ownBroad) = own(tree, None)
    val hash                          = combine(ownHash, shapes.map(_.hash))
    val loose                         = combine(ownLoose, shapes.map(_.loose))
    val broad = if (reference(tree) || literal(tree)) Reference else combine(ownBroad, shapes.map(_.broad))
    if (!pattern && tree.isTerm) {
      val roles = rolesOf(tree, origin, entire, cost, applications)
      if (
        roles.whole || roles.small || roles.tiny || roles.helper ||
        applications > 0 && cost >= CloneCost.Helper && evaluated(tree)
      ) {
        val piece = Piece(List(tree), cost, origin, hash, loose, roles, listing)
        keep(loose, piece)
        broadPieces.getOrElseUpdate(broad, mutable.ArrayBuffer.empty) += piece: Unit
      }
    }
    tree match {
      case block @ tpd.Block(statements, expression) if !lifted(block) =>
        val items = (statements :+ expression).zip(shapes).toVector
        CloneCost.windows(items.map(_._2.cost)).foreach { (start, end) =>
          val run   = items.slice(start, end)
          val hash  = combine(Seed, run.map(_._2.hash))
          val loose = combine(Seed, run.map(_._2.loose))
          keep(loose, Piece(run.map(_._1).toList, run.map(_._2.cost).sum, origin, hash, loose, Windowed, None))
        }
      case _ => ()
    }
    Shaped(hash, loose, broad, cost, applications)
  }

  private val Windowed =
    Roles(whole = true, small = false, tiny = false, entire = false, direct = false, helper = false)

  private def rolesOf(tree: tpd.Tree, origin: Origin, entire: Boolean, cost: Int, applications: Int): Roles = {
    val calls = steps(tree)
    val sized = cost >= CloneCost.Small && cost < CloneCost.Expression && evaluated(tree)
    Roles(
      whole = cost >= CloneCost.Expression || entire && origin.method && cost >= FamilyCost,
      small = sized && (calls >= 2 || arguments(tree) >= 2 && !delegating(tree) && !unit(tree)) && !synthesised(tree),
      tiny = entire && !origin.constrained && cost >= CloneCost.Tiny && cost < CloneCost.Expression &&
        applications > 0 &&
        evaluated(tree) && stateless(tree) &&
        !(origin.method && delegating(tree)) && !converted(tree),
      entire = entire,
      direct = calls == 1,
      helper = entire && origin.helper && cost >= CloneCost.Helper && cost <= CloneCost.MaximumHelper &&
        applications > 0 &&
        evaluated(tree) && !synthesised(tree) &&
        (origin.method || stateless(tree))
    )
  }

  // A literal given another type by a conversion is a constant by its value, not a call to share.
  private def converted(tree: tpd.Tree): Boolean = Calls.peel(tree) match {
    case tpd.Apply(function, List(argument)) =>
      function.symbol.exists && function.symbol.owner.fullName.toString == "scala.Conversion" &&
      Calls.peel(argument).isInstanceOf[tpd.Literal]
    case _ => false
  }

  // A call of references and literals alone calls what exists, which a method wrapping it would only repeat.
  private def delegating(tree: tpd.Tree): Boolean = underlying(tree) match {
    case tpd.Apply(function, arguments) => arguments.forall(plain(_)) && target(function)
    case _                              => false
  }

  private def target(function: tpd.Tree): Boolean = function match {
    case tpd.TypeApply(inner, _)                  => target(inner)
    case tpd.Apply(inner, _) if contextual(inner) => target(inner)
    case tpd.Apply(inner, arguments)              => arguments.forall(plain(_)) && target(inner)
    case tpd.Select(qualifier, _)                 => path(qualifier)
    case tpd.Ident(_)                             => true
    case _                                        => false
  }

  private def plain(tree: tpd.Tree): Boolean = underlying(tree) match {
    case tpd.Literal(_) | tpd.This(_) => true
    case other                        => reference(other)
  }

  // A call made for its effect is no value to bind, and a method wrapping a single call shortens nothing.
  private def unit(tree: tpd.Tree): Boolean = typeOf(tree).exists(_.widen.isRef(defn.UnitClass))

  // A value that holds state is no constant to share, and a field that makes it is no method a call replaces.
  private def stateless(tree: tpd.Tree): Boolean = typeOf(tree).forall(tpe =>
    !tpe.widen.baseClasses.exists { cls =>
      val name = cls.fullName.toString
      name == "scala.Array" || name.startsWith("scala.collection.mutable.") || name.startsWith("java.util.")
    }
  )

  // A call that leaves arguments to their defaults, or passes named ones through values the compiler binds in order to
  // evaluate them, is no expression the author wrote.
  private def synthesised(tree: tpd.Tree): Boolean = {
    val symbol = tree.symbol
    symbol.exists &&
    (symbol.name.is(DefaultGetterName) ||
      tree.isInstanceOf[tpd.Ident] && symbol.owner.isTerm && symbol.is(Flags.Synthetic) &&
      !symbol.isOneOf(Flags.Param | Flags.Method | Flags.Module)) ||
    subtrees.of(tree).exists(synthesised(_))
  }

  // The callee of a call is no expression of its own.
  private def evaluated(tree: tpd.Tree): Boolean = typeOf(tree).forall(!_.widen.isInstanceOf[Types.MethodicType])

  private def keep(hash: Long, piece: Piece): Unit =
    pieces.getOrElseUpdate(hash, mutable.ArrayBuffer.empty) += piece: Unit

  // Conversions the compiler inserts, such as of a string to its operations, are no calls the author wrote.
  private def inserted(function: tpd.Tree): Boolean =
    function.symbol.exists && function.symbol.isOneOf(Flags.GivenOrImplicit)

  /** The steps of the chain a tree ends in, a call or a selection from a result, as `a.b(c).d` holds three. */
  private def steps(tree: tpd.Tree): Int = underlying(tree) match {
    case tpd.Apply(function, _)     => called(function)
    case tpd.TypeApply(function, _) => steps(function)
    case tpd.Select(qualifier, _)   =>
      val base = underlying(qualifier)
      if (path(base)) 0 else 1 + steps(base)
    case _ => 0
  }

  /** A tree without the conversions and the arguments of using clauses that the compiler adds to it. */
  private def underlying(tree: tpd.Tree): tpd.Tree = Calls.peel(tree) match {
    case tpd.Apply(function, _) if contextual(function)       => underlying(function)
    case tpd.Apply(function, arguments) if inserted(function) => arguments.headOption.fold(tree)(underlying(_))
    case other                                                => other
  }

  @tailrec
  private def called(function: tpd.Tree): Int = function match {
    case tpd.TypeApply(inner, _)  => called(inner)
    case tpd.Select(qualifier, _) => 1 + steps(qualifier)
    case _                        => 1
  }

  private def arguments(tree: tpd.Tree): Int = Calls.peel(tree) match {
    case tpd.Apply(function, arguments) if !inserted(function) && !contextual(function) => arguments.size
    case _                                                                              => 0
  }

  // The arguments the compiler passes for a using clause are no call the author wrote.
  private def contextual(function: tpd.Tree): Boolean = typeOf(function).exists(_.widen match {
    case method: Types.MethodType => method.isImplicitMethod
    case _                        => false
  })

  /** Whether a tree names a value, as a variable, a field, an object, or a selection from one, which a copy may refer
    * to another of, as the `apply` of an object may be that of another.
    */
  private def reference(tree: tpd.Tree): Boolean = tree match {
    case tpd.Ident(_)                     => value(tree.symbol)
    case tpd.Select(qualifier, nme.apply) => value(qualifier.symbol) && path(qualifier)
    case tpd.Select(qualifier, _)         => value(tree.symbol) && path(qualifier)
    case _                                => false
  }

  private def literal(tree: tpd.Tree): Boolean = tree match {
    case tpd.Literal(constant) => hole(constant)
    case _                     => false
  }

  private def value(symbol: Symbol): Boolean = symbol.exists && symbol.isTerm && !symbol.is(Flags.Method)

  @tailrec
  private def path(tree: tpd.Tree): Boolean = tree match {
    case tpd.Ident(_) | tpd.This(_) => true
    case tpd.Select(qualifier, _)   => path(qualifier)
    case _                          => false
  }

  private def definitions(tree: tpd.Tree): List[Symbol] = {
    val own = if (tree.isDef || tree.isInstanceOf[tpd.Bind]) List(tree.symbol) else Nil
    own ++ subtrees.of(tree).flatMap(definitions(_))
  }

  /** The hashes of a fragment, with and without its literal values and then without its references too, with its locals
    * numbered in order of first occurrence.
    */
  private def numbered(trees: List[tpd.Tree]): (Long, Long, Long) = {
    val locals = mutable.Map.empty[Symbol, Long]

    def go(tree: tpd.Tree, covered: Boolean): (Long, Long, Long) = {
      val (exact, loose, call) = own(tree, Some(locals))
      val named                = !covered && reference(tree)
      val children             = subtrees.of(tree).map(go(_, covered || named || enumerated(tree)))
      (
        combine(exact, children.map(_._1)),
        combine(loose, children.map(_._2)),
        if (named || literal(tree)) Reference else combine(call, children.map(_._3))
      )
    }
    val hashes = trees.map(go(_, covered = false))
    (combine(Seed, hashes.map(_._1)), combine(Seed, hashes.map(_._2)), combine(Seed, hashes.map(_._3)))
  }

  /** The literals and references that the last hash of `numbered` leaves out, with locals numbered in order of first
    * occurrence.
    */
  private def variables(trees: List[tpd.Tree], parameters: List[Symbol]): Vector[Clones.Literal] = {
    val defined = trees.flatMap(definitions(_)).toSet
    val inside  = mutable.Map.empty[Symbol, Int]
    val outside = mutable.Map.empty[Symbol, Int]
    val found   = Vector.newBuilder[Clones.Literal]

    def text(tree: tpd.Tree): (String, String) = tree match {
      case tpd.Select(qualifier, nme.apply) => text(qualifier)
      case tpd.Select(qualifier, name)      =>
        val (key, shown) = text(qualifier)
        (s"$key.$name", s"$shown.$name")
      case tpd.This(_) => ("this", "this")
      case other       =>
        val symbol = other.symbol
        if (!symbol.owner.isTerm) (symbol.fullName.toString, symbol.name.toString)
        else {
          val bound   = defined(symbol)
          val numbers = if (bound) inside else outside
          val name    = symbol.name.toString
          (
            (if (bound) Clones.Bound else Clones.Free) + numbers.getOrElseUpdate(symbol, numbers.size + 1),
            if (name.contains('$')) "_" else name
          )
        }
    }

    @tailrec
    def head(tree: tpd.Tree): tpd.Tree = tree match {
      case tpd.Select(qualifier, _) => head(qualifier)
      case other                    => other
    }

    def selections(tree: tpd.Tree): String = tree match {
      case tpd.Select(qualifier, nme.apply) => selections(qualifier)
      case tpd.Select(qualifier, name)      => s"${selections(qualifier)}.$name"
      case _                                => ""
    }

    def types(tree: tpd.Tree): Vector[Clones.Typed] = tree match {
      case tpd.Select(qualifier, nme.apply) => types(qualifier)
      case tpd.Select(qualifier, _)         => types(qualifier) :+ typed(tree.tpe)
      case other                            => Vector(typed(other.tpe))
    }

    def variable(tree: tpd.Tree): Clones.Literal = {
      val (key, shown) = text(tree)
      val chain        = types(tree)
      val parameter    = Option(head(tree).symbol).collect {
        case symbol if parameters.contains(symbol) =>
          val index = parameters.indexOf(symbol)
          Clones.Binding(index, selections(tree))(chain.size - 1, accepted(symbol))
      }
      Clones.Literal(key, reference = true, parameter = parameter)(shown, chain)
    }

    def go(tree: tpd.Tree, covered: Boolean): Unit = {
      val named = !covered && reference(tree)
      tree match {
        case tpd.Literal(constant) if !covered && hole(constant) => found += rendered(constant)
        case _ if named                                          => found += variable(tree)
        case _                                                   => ()
      }
      subtrees.of(tree).foreach(go(_, covered || named || enumerated(tree)))
    }
    trees.foreach(go(_, covered = false))
    found.result()
  }

  private def rendered(constant: Constant): Clones.Literal =
    if (constant.tag == Constants.StringTag)
      Clones.Literal(constant.stringValue, string = true)(constant.show, Vector(typed(Types.ConstantType(constant))))
    else Clones.Literal(constant.show)(types = Vector(typed(Types.ConstantType(constant))))

  private class Typing(val tpe: Types.Type) extends Clones.Typed {
    def conforms(to: Clones.Typed): Boolean = to match {
      case other: Typing => tpe <:< other.tpe
      case _             => true
    }
  }

  private def typed(tpe: Types.Type): Clones.Typed = Typing(Option(tpe).fold(defn.AnyType)(_.widenExpr.widen))

  // A type that mentions the parameters or type parameters of the method is not one a call elsewhere can be held to.
  private def accepted(parameter: Symbol): Option[Clones.Typed] = {
    val tpe = parameter.info.widenExpr
    Option.unless(tpe.existsPart {
      case reference: Types.NamedType => reference.symbol.exists && reference.symbol.owner == parameter.owner
      case _: Types.ParamRef          => true
      case _                          => false
    })(typed(tpe))
  }

  private def applications(trees: List[tpd.Tree]): Boolean = trees match {
    case List(tree) =>
      Calls.peel(tree) match {
        case tpd.Apply(_, _)             => true
        case tpd.SeqLiteral(elements, _) => elements.forall(Calls.peel(_).isInstanceOf[tpd.Apply])
        case _                           => false
      }
    case _ => false
  }

  private def callee(trees: List[tpd.Tree]): Option[Vector[String]] = trees match {
    case List(tree) =>
      Calls.peel(tree) match {
        case application: tpd.Apply =>
          val computed = application.args.filterNot(Calls.peel(_).isInstanceOf[tpd.Literal])
          Some(literals(application.fun :: computed).collect {
            case constant if constant.tag != Constants.StringTag => constant.show
          })
        case _ => None
      }
    case _ => None
  }

  /** The literals that may differ between near duplicates, in the order `numbered` visits them. */
  private def literals(trees: List[tpd.Tree]): Vector[Constant] = trees.toVector.flatMap {
    case tpd.Literal(constant) if hole(constant) => Vector(constant)
    // an enum case passes its ordinal and name to `$new`, which the compiler writes
    case tree if enumerated(tree) => Vector.empty
    case tree                     => literals(subtrees.of(tree))
  }

  private def enumerated(tree: tpd.Tree): Boolean = tree match {
    case tpd.Apply(fun, _) => fun.symbol.name.toString == "$new"
    case _                 => false
  }

  // A class literal names a type, which is as much part of the fragment as a reference is.
  private def hole(constant: Constant): Boolean = constant.tag != Constants.ClazzTag

  /** The hashes of `tree` itself, with and without the value of a literal; `locals` numbers the symbols local to a
    * fragment in order of first occurrence, which are otherwise all alike.
    */
  private def own(tree: tpd.Tree, locals: Option[mutable.Map[Symbol, Long]]): (Long, Long, Long) = {
    val symbol = tree match {
      case typeTree: tpd.TypeTree => typeTree.tpe.typeSymbol
      case other                  => other.symbol
    }
    val shared =
      if (!symbol.exists) 0L
      else if (symbol.owner.isTerm) Local
      else stable.getOrElseUpdate(symbol, identityOf(symbol))
    val reference = locals match {
      case Some(numbers) if symbol.exists && symbol.owner.isTerm => numbers.getOrElseUpdate(symbol, numbers.size + 1L)
      case _                                                     => shared
    }
    val (valued, shaped) = tree match {
      case tpd.Literal(constant) =>
        val value = constant.tag * 31L + String.valueOf(constant.value).hashCode
        (value, if (hole(constant)) constant.tag.toLong else value)
      case _ => (0L, 0L)
    }
    val kind = tree.getClass.getName.hashCode.toLong * 31
    val node = mix(kind + reference)
    // the callee of a call is hashed where it is named, so the call itself is the same whichever one it makes, and a
    // type is as the code that names it is
    val call = tree match {
      case tpd.Apply(_, _) | tpd.TypeApply(_, _) | tpd.TypeTree() => mix(kind)
      case _                                                      => mix(kind + shared)
    }
    (mix(node * 31 + valued), mix(node * 31 + shaped), call)
  }

  // Overloads share a full name, so methods also hash their signature.
  private def identityOf(symbol: Symbol): Long =
    nameHash(symbol.fullName.toString + (if (symbol.is(Flags.Method)) symbol.signature.toString else ""))

  // The cost counts what the author wrote, on the scale `CloneCost` describes: types, the scaffolding of lambdas and
  // constructions, the receiver a member is found on and the operator of an infix call are no nodes of their own.
  private def weight(tree: tpd.Tree): Int = tree match {
    case tpd.TypeTree() | tpd.Typed(_, _) | tpd.Inlined(_, _, _) | tpd.TypeApply(_, _) | tpd.New(_) |
        tpd.Closure(_, _, _) | tpd.Select(_, nme.CONSTRUCTOR) | tpd.Select(tpd.This(_), _) | tpd.Ident(nme.ANON_FUN) |
        tpd.Ident(nme.WILDCARD) =>
      0
    case _ if tree.isEmpty || tree.isType                                 => 0
    case tpd.Block(Nil, _)                                                => 0
    case tpd.Block(List(tpd.DefDef(_, _, _, _)), tpd.Closure(_, _, _))    => 0
    case tpd.Block(List(_), tpd.Literal(Constant(()))) if discarded(tree) => 0
    case tpd.Literal(Constant(())) if tree.span.isSynthetic               => 0
    case tpd.Apply(function, _) if operator(function)                     => 0
    case _                                                                => 1
  }

  private def discarded(tree: tpd.Tree): Boolean = tree match {
    case tpd.Block(_, expression) => expression.span.isSynthetic
    case _                        => false
  }

  private def operator(function: tpd.Tree): Boolean = function match {
    case tpd.Select(_, name) =>
      val symbol = function.symbol
      symbol.exists && name.isOperatorName &&
      (symbol.owner.isPrimitiveValueClass || symbol.owner == defn.StringClass)
    case _ => false
  }

  // An enum case without written members is a row of constructor arguments, which its compiler-written members repeat.
  private def row(tree: tpd.Tree): Boolean = tree match {
    case tpd.Block(List(definition @ tpd.TypeDef(_, template: tpd.Template)), _) =>
      definition.symbol.asClass.parentSyms.exists(_.is(Flags.Enum)) && template.body.forall(_.span.isSynthetic)
    case _ => false
  }

  // A for comprehension places a function's parameter at its generator, before the collection it ranges over, so a
  // function is found where its body is.
  private def located(tree: tpd.Tree): tpd.Tree = tree match {
    case tpd.Block(List(definition: tpd.DefDef), tpd.Closure(_, _, _)) => definition.rhs
    case definition: tpd.DefDef                                        => definition.rhs
    case other                                                         => other
  }

  private def locate(piece: Piece): Option[(Clones.Fragment, (Int, Int))] = {
    val (first, last) = (located(piece.trees.head), located(piece.trees.last))
    Option.when(first.span.exists && last.span.exists && first.source.exists) {
      val fragment = Clones.Fragment(
        piece.origin.node,
        first.source.path,
        lines.line(first.source, first.span.start),
        lines.line(last.source, last.span.end),
        piece.origin.test,
        piece.origin.family,
        piece.origin.specialised,
        piece.origin.member
      )
      fragment -> (first.span.start, last.span.end)
    }
  }

}
