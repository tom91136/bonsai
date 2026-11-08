package net.kurobako.bonsai.scalafix

import metaconfig.Conf
import metaconfig.Configured
import scala.meta.*
import scalafix.v1.*

/** Case patterns that bind a name already in scope, which reads like a match against the existing value but binds
  * anything. Fields, constructor, method and lambda parameters, earlier local values and enclosing case bindings are in
  * scope, as are the names a for comprehension binds before. Typed bindings cannot be mistaken for a match against a
  * value, and rebinding the scrutinee under its own name binds the same value.
  *
  * With `PatternShadow.locals = true`, local values, variables and methods that hide a field, method or constructor
  * parameter of an enclosing class or object are reported as well, when their types agree. The setting is off by
  * default, as naming a local after the member that computes it is common.
  */
class PatternShadow(locals: Boolean) extends SemanticRule("PatternShadow") {
  def this() = this(false)

  override def withConfiguration(config: Configuration): Configured[Rule] = Configured.Ok(
    PatternShadow(settings.section(config.conf, "PatternShadow").get("locals").contains(Conf.Bool(true)))
  )

  override def fix(using document: SemanticDocument): Patch = document.tree.collect {
    case local: Defn.Val if locals && inBlock(local) => declaring(local)
    case local: Defn.Var if locals && inBlock(local) => declaring(local)
    case local: Defn.Def if locals && inBlock(local) => hiding(local.name, local, methodKey(_))
    case clause: Case                                => rebound(clause)
  }.asPatch

  private def declaring(local: Defn.Val | Defn.Var)(using SemanticDocument): Patch =
    local.pats.flatMap(names(_)).map(hiding(_, local, valueKey(_))).asPatch

  private def rebound(clause: Case): Patch = {
    val visible   = scope(clause, clause.parent, Set.empty)
    val rebinding = (clause.pat, clause.parent.flatMap(_.parent)) match {
      case (Pat.Var(name), Some(matched: Term.Match)) => matched.expr match {
          case Term.Name(scrutinee) => name.value == scrutinee
          case _                    => false
        }
      case _ => false
    }
    clause.pat.collect {
      case variable: Pat.Var
          if visible(variable.name.value) && !rebinding && !variable.parent.exists(_.isInstanceOf[Pat.Typed]) =>
        val name = variable.name.value
        lint(
          "pattern-shadow",
          s"$name shadows the $name in scope; rename the binding, or write `$name` to match its value",
          variable.pos
        )
    }.asPatch
  }

  private def inBlock(definition: Defn): Boolean = definition.parent.exists(_.isInstanceOf[Term.Block])

  private def names(pattern: Pat): List[Name] = pattern.collect { case variable: Pat.Var => variable.name }

  private def hiding(name: Name, local: Defn, key: Symbol => Option[String])(using SemanticDocument): Patch = {
    val hidden = key(name.symbol).flatMap { own =>
      enclosing(name.value, local, local.parent).collectFirst {
        case (member, kind) if key(member).contains(own) => kind
      }
    }
    hidden.fold(Patch.empty) { kind =>
      lint("local-shadow", s"${name.value} hides the $kind ${name.value} that encloses it; rename the local", name.pos)
    }
  }

  @annotation.tailrec
  private def enclosing(name: String, child: Tree, tree: Option[Tree])(using SemanticDocument): List[(Symbol, String)] =
    tree match {
      case None         => Nil
      case Some(parent) =>
        val declared = parent match {
          case body: Template.Body => members(name, body.stats)
          case body: Pkg.Body      => members(name, body.stats)
          case source: Source      => members(name, source.stats)
          case owner: Defn.Class   => owner.ctor.paramClauses.toList.flatMap(_.values).collect {
              case parameter if parameter.name.value == name => parameter.name.symbol -> "parameter"
            }
          case _ => Nil
        }
        if (declared.nonEmpty) declared
        else if (intervening(name, child, parent)) Nil
        else enclosing(name, parent, parent.parent)
    }

  private def intervening(name: String, child: Tree, parent: Tree): Boolean = parent match {
    case method: Defn.Def  => method.name.value == name || definitions.parameters(method).exists(_.name.value == name)
    case block: Term.Block => block.stats.exists(stat => (stat ne child) && member(stat).exists(_._1.value == name))
    case function: Term.Function                   => function.paramClause.values.exists(_.name.value == name)
    case outer: Case                               => names(outer.pat).exists(_.value == name)
    case loop: Term.For if loop.body eq child      => bound(loop.enumsBlock.enums).contains(name)
    case loop: Term.ForYield if loop.body eq child => bound(loop.enumsBlock.enums).contains(name)
    case _                                         => false
  }

  private def members(name: String, stats: List[Stat])(using SemanticDocument): List[(Symbol, String)] =
    stats.flatMap(member(_)).collect { case (declared, kind) if declared.value == name => declared.symbol -> kind }

  private def member(stat: Stat): List[(Name, String)] = stat match {
    case method: Defn.Def   => List(method.name -> (if (method.paramClauses.isEmpty) "member" else "method"))
    case value: Defn.Val    => value.pats.flatMap(names(_)).map(_ -> "member")
    case variable: Defn.Var => variable.pats.flatMap(names(_)).map(_ -> "member")
    case _                  => Nil
  }

  private def valueKey(symbol: Symbol)(using SemanticDocument): Option[String] =
    definitions.info(symbol).map(_.signature).flatMap {
      case ValueSignature(ByNameType(tpe)) => typeKey(tpe)
      case ValueSignature(tpe)             => typeKey(tpe)
      case MethodSignature(_, Nil, tpe)    => typeKey(tpe)
      case _                               => None
    }

  private def methodKey(symbol: Symbol)(using SemanticDocument): Option[String] =
    definitions.info(symbol).map(_.signature).flatMap {
      case MethodSignature(typeParameters, parameterLists, tpe) if typeParameters.isEmpty =>
        val lists =
          parameterLists.map(parameters => joined(parameters.map(parameterKey(_)), ",").map(keys => s"($keys)"))
        joined(lists :+ typeKey(tpe), ":")
      case ValueSignature(ByNameType(tpe)) => typeKey(tpe)
      case _                               => None
    }

  private def parameterKey(parameter: SymbolInformation)(using SemanticDocument): Option[String] =
    parameter.signature match {
      case ValueSignature(tpe) => typeKey(tpe)
      case _                   => None
    }

  // the index has no symbol info for the Predef alias, so it is mapped by name
  private val aliases = Map("scala/Predef.String#" -> "java/lang/String#")

  private def typeKey(tpe: SemanticType)(using SemanticDocument): Option[String] = tpe match {
    case AnnotatedType(_, inner)          => typeKey(inner)
    case TypeRef(_, reference, arguments) =>
      definitions.info(reference).map(_.signature) match {
        case Some(TypeSignature(typeParameters, lower, upper)) if typeParameters.isEmpty && lower == upper =>
          typeKey(upper)
        case _ =>
          val name = aliases.getOrElse(reference.value, reference.value)
          joined(arguments.map(typeKey(_)), ",").map(keys => s"$name[$keys]")
      }
    case _ => None
  }

  private def joined(keys: List[Option[String]], separator: String): Option[String] =
    Option.when(keys.forall(_.isDefined))(keys.flatten.mkString(separator))

  private def bound(enumerators: List[Enumerator]): List[String] = enumerators.flatMap {
    case generator: Enumerator.Generator     => variables(generator.pat)
    case generator: Enumerator.CaseGenerator => variables(generator.pat)
    case definition: Enumerator.Val          => variables(definition.pat)
    case _                                   => Nil
  }

  @annotation.tailrec
  private def scope(child: Tree, tree: Option[Tree], names: Set[String]): Set[String] = tree match {
    case None         => names
    case Some(parent) =>
      val declared = parent match {
        case block: Term.Block                  => values(block.stats.takeWhile(_ ne child))
        case body: Template.Body                => values(body.stats)
        case body: Pkg.Body                     => values(body.stats)
        case source: Source                     => values(source.stats)
        case owner: Defn.Class                  => owner.ctor.paramClauses.flatMap(_.values.map(_.name.value))
        case method: Defn.Def                   => definitions.parameters(method).map(_.name.value)
        case function: Term.Function            => function.paramClause.values.map(_.name.value)
        case outer: Case if outer.body eq child => outer.pat.collect { case variable: Pat.Var => variable.name.value }
        case enumerators: Term.EnumeratorsBlock => bound(enumerators.enums.takeWhile(_ ne child))
        case loop: Term.ForYield if loop.body eq child => bound(loop.enumsBlock.enums)
        case loop: Term.For if loop.body eq child      => bound(loop.enumsBlock.enums)
        case _                                         => Nil
      }
      scope(parent, parent.parent, names ++ declared)
  }

  private def variables(pattern: Pat): List[String] = pattern.collect { case variable: Pat.Var => variable.name.value }

  private def values(stats: List[Stat]): List[String] = stats.flatMap {
    case value: Defn.Val    => value.pats.flatMap(variables(_))
    case variable: Defn.Var => variable.pats.flatMap(variables(_))
    case owner: Defn.Object => List(owner.name.value)
    case _                  => Nil
  }
}
