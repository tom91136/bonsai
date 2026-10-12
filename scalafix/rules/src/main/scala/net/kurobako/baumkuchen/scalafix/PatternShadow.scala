package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Case patterns that bind a name already in scope, which reads like a match against the existing value but binds
  * anything. Fields, constructor, method and lambda parameters, earlier local values and enclosing case bindings are in
  * scope, as are the names a for comprehension binds before. Typed bindings cannot be mistaken for a match against a
  * value, and rebinding the scrutinee under its own name binds the same value.
  */
class PatternShadow extends SyntacticRule("PatternShadow") {
  override def fix(using document: SyntacticDocument): Patch = document.tree.collect { case clause: Case =>
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
  }.asPatch

  @annotation.tailrec
  private def scope(child: Tree, tree: Option[Tree], names: Set[String]): Set[String] = tree match {
    case None         => names
    case Some(parent) =>
      val declared = parent match {
        case block: Term.Block                  => values(block.stats.takeWhile(_ ne child))
        case body: Template.Body                => values(body.stats)
        case body: Pkg.Body                     => values(body.stats)
        case source: Source                     => values(source.stats)
        case owner: Defn.Class                  => owner.ctor.paramClauses.flatMap(_.values).map(_.name.value)
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
  private def bound(enumerators: List[Enumerator]): List[String] = enumerators.collect {
    case generator: Enumerator.Generator     => generator.pat
    case generator: Enumerator.CaseGenerator => generator.pat
    case definition: Enumerator.Val          => definition.pat
  }.flatMap(_.collect { case variable: Pat.Var => variable.name.value })

  private def values(stats: List[Stat]): List[String] = stats.flatMap {
    case value: Defn.Val    => value.pats.flatMap(_.collect { case variable: Pat.Var => variable.name.value })
    case variable: Defn.Var => variable.pats.flatMap(_.collect { case name: Pat.Var => name.name.value })
    case owner: Defn.Object => List(owner.name.value)
    case _                  => Nil
  }
}
