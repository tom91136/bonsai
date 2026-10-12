package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Objects that hold nothing but public methods passing their parameters straight to a method of another object, adding
  * a name and nothing else. Implicit definitions are excluded because their implicitness is the behaviour.
  */
class MiddleMan extends SyntacticRule("MiddleMan") {
  override def fix(using document: SyntacticDocument): Patch = document.tree.collect {
    case owner: Defn.Object =>
      val stats   = definitions.members(owner.templ).filterNot(_.isInstanceOf[Import])
      val methods = stats.collect {
        case method: Defn.Def if forwards(method) => method
      }
      Option.when(stats.nonEmpty && methods.size == stats.size && methods.exists(definitions.parameters(_).nonEmpty))(
        lint(
          "middle-man",
          s"every member of ${owner.name.value} only forwards to another object; call the target directly",
          owner.name.pos
        )
      ).asPatch
  }.asPatch

  private def forwards(method: Defn.Def): Boolean =
    definitions.public(method.mods) && !definitions.has[Mod.Implicit](method.mods) &&
      (method.body match {
        case application: Term.Apply => application.fun match {
            case Term.Select(_: Term.Ref, _) =>
              application.argClause.values.map(_.toString) == definitions.parameters(method).map(_.name.value)
            case _ => false
          }
        case _ => false
      })
}
