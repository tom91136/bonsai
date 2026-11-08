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
  * With `PatternShadow.locals = true`, local values, variables and methods that take the name of a member or method
  * that encloses them are reported as well, as they hide it for the rest of their block. Code that names locals after
  * the methods that compute them does this throughout, so the setting is off by default.
  */
class PatternShadow(locals: Boolean) extends SyntacticRule("PatternShadow") {
  def this() = this(false)

  override def withConfiguration(config: Configuration): Configured[Rule] = Configured.Ok(
    PatternShadow(settings.section(config.conf, "PatternShadow").get("locals").contains(Conf.Bool(true)))
  )

  override def fix(using document: SyntacticDocument): Patch = document.tree.collect {
    case local: Defn.Val if locals && inBlock(local) => local.pats.flatMap(names(_)).map(hiding(_, local)).asPatch
    case local: Defn.Var if locals && inBlock(local) => local.pats.flatMap(names(_)).map(hiding(_, local)).asPatch
    case local: Defn.Def if locals && inBlock(local) => hiding(local.name, local)
    case clause: Case                                => rebound(clause)
  }.asPatch

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

  private def hiding(name: Name, definition: Defn): Patch =
    members(definition, definition.parent, Map.empty).get(name.value).fold(Patch.empty) { kind =>
      lint("local-shadow", s"${name.value} hides the $kind ${name.value} that encloses it; rename the local", name.pos)
    }

  // The members and methods around a definition, which its own block can hide, by what each is.
  @annotation.tailrec
  private def members(local: Defn, tree: Option[Tree], found: Map[String, String]): Map[String, String] = tree match {
    case None         => found
    case Some(parent) =>
      val declared = parent match {
        case body: Template.Body => body.stats.flatMap(member(_))
        case body: Pkg.Body      => body.stats.flatMap(member(_))
        case source: Source      => source.stats.flatMap(member(_))
        case block: Term.Block   => block.stats.collect { case method: Defn.Def if method ne local => kind(method) }
        case owner: Defn.Class   => owner.ctor.paramClauses.flatMap(_.values.map(_.name.value -> "parameter"))
        case method: Defn.Def    => List(kind(method))
        case _                   => Nil
      }
      members(
        local,
        parent.parent,
        declared.foldLeft(found)((all, entry) => if (all.contains(entry._1)) all else all + entry)
      )
  }

  private def kind(method: Defn.Def): (String, String) =
    method.name.value -> (if (method.paramClauses.isEmpty) "member" else "method")

  private def member(stat: Stat): List[(String, String)] = stat match {
    case method: Defn.Def   => List(kind(method))
    case value: Defn.Val    => value.pats.flatMap(variables(_)).map(_ -> "member")
    case variable: Defn.Var => variable.pats.flatMap(variables(_)).map(_ -> "member")
    case owner: Defn.Object => List(owner.name.value -> "member")
    case _                  => Nil
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

  // Generators and value definitions of a for comprehension bind names for the enumerators after them and its body.
  private def bound(enumerators: List[Enumerator]): List[String] = enumerators.flatMap {
    case generator: Enumerator.Generator     => variables(generator.pat)
    case generator: Enumerator.CaseGenerator => variables(generator.pat)
    case definition: Enumerator.Val          => variables(definition.pat)
    case _                                   => Nil
  }

  private def variables(pattern: Pat): List[String] = pattern.collect { case variable: Pat.Var => variable.name.value }

  private def values(stats: List[Stat]): List[String] = stats.flatMap {
    case value: Defn.Val    => value.pats.flatMap(variables(_))
    case variable: Defn.Var => variable.pats.flatMap(variables(_))
    case owner: Defn.Object => List(owner.name.value)
    case _                  => Nil
  }
}
