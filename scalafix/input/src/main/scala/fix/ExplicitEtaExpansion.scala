/*
rule = ExplicitEtaExpansion
 */
package fix

import scala.language.dynamics

object ExplicitEtaExpansion {
  sealed abstract class Parent(value: Int)
  final case class Child(value: Int) extends Parent(value)
  case class Box(value: Int)

  private def increment(value: Int): Int                    = value + 1
  private val incrementFunction: Int => Int                 = value => value + 1
  private def acceptByName(value: => Int): Int              = value
  private def passFunction(function: Int => Int): List[Int] = List(1).map(function)
  private def passValue(value: Int): List[Int]              = List(1).map(_ => acceptByName(value))
  private def passByName(value: => Int): List[Int]          = List(1).map(_ => acceptByName(value))
  extension (value: Int) {
    def parity: Int            = value & 1
    def plus(amount: Int): Int = value + amount
  }

  def localMethod: List[Int] = {
    def localIncrement(value: Int): Int = value + 1
    // format: off
    List(1).map(localIncrement)// assert: ExplicitEtaExpansion.method-value
    // format: on
  }

  // format: off
  val rejected: List[Int]          = List(1).map(increment)// assert: ExplicitEtaExpansion.method-value
  val constructor: List[Box]       = List(1).map(Box.apply)// assert: ExplicitEtaExpansion.method-value
  val extensionMethod: List[Int]   = List(1).map(1.plus)// assert: ExplicitEtaExpansion.method-value
  val infix: List[Int]             = List(1) map increment// assert: ExplicitEtaExpansion.method-value
  val infixParenthesised: List[Int] = List(1) map (increment)// assert: ExplicitEtaExpansion.method-value
  // format: on
  val accepted: List[Int]          = List(1).map(increment(_))
  val functionValue: List[Int]     = List(1).map(incrementFunction)
  val identityValue: List[Int]     = List(1).map(identity)
  val identityQualified: List[Int] = List(1).map(Predef.identity)
  val extensionAccessor: List[Int] = List(1).filter(value => value.parity == 1)

  private given factor: Int                          = 3
  private def scaled(using factor: Int): Int         = factor * 2
  private def implied(implicit factor: Int): Int     = factor
  val contextual: List[Int]                          = List(scaled, implied)
  val contextualLibrary: List[List[Int]]             = List(List(2, 1).sorted)

  final class Lookup extends scala.Dynamic {
    def selectDynamic(name: String): Lookup = this
  }
  val lookup: Lookup                 = new Lookup
  def emits[O](items: Seq[O]): Int                       = items.size
  val nullaryOverload: Int                               = emits("12345".getBytes)
  val nullaryOverloadList: List[Array[Byte]]             = List("12345".getBytes)
  val overloadedArities: List[Int]                       = List(java.nio.file.Path.of("a")).map(java.nio.file.Files.readString).map(_.length)
  // format: off
  val javaMethod: List[Long]                             = List(java.nio.file.Path.of("a")).map(java.nio.file.Files.size)// assert: ExplicitEtaExpansion.method-value
  val javaOverloaded: List[Int]                          = List(1).map(java.lang.Math.abs)// assert: ExplicitEtaExpansion.method-value
  // format: on
  val dynamicSelection: List[Lookup] = List(lookup.process)
}
