package net.kurobako.baumkuchen.scalafix

import scala.meta.*
import scalafix.v1.*

/** Case class fields whose types compare by reference or change after construction, so `equals` and `hashCode` stop
  * describing the value.
  */
class CaseClassEquality extends SemanticRule("CaseClassEquality") {
  override def fix(using document: SemanticDocument): Patch = {
    val implemented = document.tree.collect {
      case Defn.Object(_, _, template) => template.inits
      case instance: Defn.Given        => instance.templ.inits
    }.flatten.map(init => definitions.symbol(init.tpe)).toSet
    document.tree.collect {
      case definition: Defn.Class if definitions.has[Mod.Case](definition.mods) =>
        definition.ctor.paramClauses.headOption.toList.flatMap(_.values).flatMap { parameter =>
          parameter.decltpe.flatMap { tpe =>
            val symbol = definitions.symbol(tpe)
            val reason =
              if (symbol.value == "scala/Array#") Some("arrays compare by reference")
              else if (definitions.function(tpe)) Some("functions compare by reference")
              else if (lambdaShaped(symbol, implemented))
                Some("single-method traits that lambdas implement compare by reference")
              else Option.when(definitions.mutableType(symbol))("mutable fields change the value after construction")
            reason.map(why =>
              lint(
                "unstable-equality",
                s"${parameter.name.value}: $why, so ${definition.name.value} has unstable equality",
                parameter.pos
              )
            )
          }
        }.asPatch
    }.asPatch
  }

  // Orderings and the like are strategies, which their users share as objects and compare by identity on purpose.
  private val Strategies = Set(
    "scala/math/Ordering#",
    "scala/math/PartialOrdering#",
    "scala/math/Equiv#",
    "java/util/Comparator#",
    "java/lang/Comparable#"
  )

  // An open trait or Java interface whose one abstract member is a method, which a lambda implements, unless an object
  // of the file implements it, which makes it a strategy.
  private def lambdaShaped(symbol: Symbol, implemented: Set[Symbol])(using SemanticDocument): Boolean =
    !Strategies(symbol.value) && !implemented(symbol) && definitions.info(symbol).exists { info =>
      (info.isTrait || info.isInterface) && !info.isSealed &&
      (info.signature match {
        case ClassSignature(_, _, _, declarations) =>
          declarations.filter(_.isAbstract) match {
            case List(method) =>
              method.isMethod &&
              (method.signature match {
                case MethodSignature(_, parameters, _) => parameters.nonEmpty
                case _                                 => false
              })
            case _ => false
          }
        case _ => false
      })
    }
}
