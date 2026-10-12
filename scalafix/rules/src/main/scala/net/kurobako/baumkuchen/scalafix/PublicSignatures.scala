package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Public signatures that hide meaning in position or in the absence of a type: `Any` and `AnyRef`, tuples, and runs of
  * three or more parameters of the same primitive or `String` type. Local definitions, and members of private ones, are
  * not public.
  */
class PublicSignatures extends SemanticRule("PublicSignatures") {
  private val untyped    = Set("scala/Any#", "scala/AnyRef#", "java/lang/Object#")
  private val primitives = Set(
    "scala/Int#",
    "scala/Long#",
    "scala/Double#",
    "scala/Float#",
    "scala/Short#",
    "scala/Byte#",
    "scala/Boolean#",
    "scala/Char#",
    "scala/Predef.String#",
    "java/lang/String#"
  )

  override def fix(using document: SemanticDocument): Patch = document.tree.collect {
    case method: Defn.Def
        if definitions.public(method.mods) && definitions.reachable(method, method.mods) &&
          !definitions.has[Mod.Override](method.mods) =>
      val parameters = definitions.parameters(method)
      val types      = parameters.flatMap(_.decltpe) ++ method.decltpe
      val any        = types.collect {
        case tpe if untyped(definitions.symbol(tpe).value) =>
          lint("any-type", "Any in a public signature gives up type checking at every call", tpe.pos)
      }
      val tuples = types.collect {
        case tpe: Type.Tuple if !tpe.args.exists(_.isInstanceOf[Type.TypedParam]) =>
          lint("tuple-type", "a tuple in a public signature names its parts by position", tpe.pos)
      }
      val runs = parameters.map(parameter =>
        parameter.decltpe.filter(_ => parameter.default.isEmpty).map(definitions.symbol(_).value).filter(primitives)
      )
        .zip(parameters).sliding(3).collectFirst {
          case Seq((Some(first), _), (Some(second), _), (Some(third), last)) if first == second && second == third =>
            lint(
              "primitive-run",
              s"three or more ${first.split('/').last.stripSuffix("#")} parameters in a row are easy to swap",
              last.pos
            )
        }
      (any ++ tuples ++ runs).asPatch
  }.asPatch
}
