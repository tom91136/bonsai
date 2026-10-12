/*
rule = CaseClassEquality
 */
package fix

import scala.collection.mutable

object CaseClassEquality {
  case class Samples(
      name: String,
      values: Array[Double],          // assert: CaseClassEquality.unstable-equality
      transform: Double => Double,    // assert: CaseClassEquality.unstable-equality
      cache: mutable.Map[String, Int] // assert: CaseClassEquality.unstable-equality
  )
  case class Active(
      client: String,
      var request: Vector[Byte] // assert: CaseClassEquality.unstable-equality
  )
  case class Target(endpoint: String, closeFn: () => Unit) // assert: CaseClassEquality.unstable-equality
  case class Plain(name: String, values: Vector[Double], cache: Map[String, Int])
  class Holder(values: Array[Double])
}
