package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Enforces @tailrec on self-recursive methods whose recursive calls are all in tail positions. */
class RequireTailRec extends SemanticRule("RequireTailRec") {
  override def fix(using document: SemanticDocument): Patch = {
    val missing = document.tree.collect {
      case defn: Defn.Def
          if isEligible(defn) &&
            !defn.mods.exists(_.isInstanceOf[Mod.Inline]) &&
            !hasTailrec(defn) &&
            isTailRecursive(defn) =>
        val indent = " " * defn.pos.startColumn
        Patch.addLeft(defn, s"@tailrec\n$indent") +
          lint(
            "missing-tailrec",
            s"method `${defn.name.value}` is tail-recursive but missing @tailrec",
            defn.name.pos
          )
    }
    val needed =
      if (missing.isEmpty || references.imported("scala.annotation", "tailrec")) Nil
      else List(("scala.annotation", "tailrec"))
    (missing ++ references.placements(needed)).asPatch
  }

  private def hasTailrec(defn: Defn.Def): Boolean = definitions.annotated(defn.mods, "tailrec")

  private def isEligible(defn: Defn.Def): Boolean =
    defn.mods.exists(m => m.isInstanceOf[Mod.Private] || m.isInstanceOf[Mod.Final]) ||
      isLocalDef(defn) ||
      isInsideObject(defn)

  private def isLocalDef(defn: Defn.Def): Boolean =
    defn.parent.exists {
      case _: Term.Block | _: Defn.Def => true
      case _                           => false
    }

  @scala.annotation.tailrec
  private def isInsideObject(tree: Tree): Boolean = tree.parent match {
    case Some(_: Defn.Object)                               => true
    case Some(_: Defn.Class | _: Defn.Trait | _: Defn.Enum) => false
    case Some(parent)                                       => isInsideObject(parent)
    case None                                               => false
  }

  private def isTailRecursive(defn: Defn.Def)(using SemanticDocument): Boolean = {
    val methodSymbol  = defn.name.symbol
    val tailCallNames = if (methodSymbol.isNone) Nil else tailExpressions(defn.body).flatMap(callRef(_, methodSymbol))
    tailCallNames.nonEmpty && {
      val tailCallPositions = tailCallNames.map(_.pos).toSet
      defn.body.collect {
        case ref: Term.Ref if ref.symbol == methodSymbol && !ref.parent.exists {
              case _: Pat.Var => true; case _ => false
            } =>
          ref
      }.forall(ref => tailCallPositions.contains(ref.pos))
    }
  }

  private def tailExpressions(term: Term): List[Term] = term match {
    case Term.Block(stats) =>
      stats.lastOption match {
        case Some(last: Term) => tailExpressions(last)
        case _                => Nil
      }
    case conditional: Term.If => tailExpressions(conditional.thenp) ++ tailExpressions(conditional.elsep)
    case matched: Term.Match  => matched.casesBlock.cases.flatMap(c => tailExpressions(c.body))
    case other                => List(other)
  }

  @scala.annotation.tailrec
  private def callRef(term: Term, methodSymbol: Symbol)(using SemanticDocument): Option[Term.Ref] = term match {
    case reference: Term.Ref if reference.symbol == methodSymbol => Some(reference)
    case call: Term.Apply                                        => callRef(call.fun, methodSymbol)
    case application: Term.ApplyType                             => callRef(application.fun, methodSymbol)
    case _                                                       => None
  }
}
