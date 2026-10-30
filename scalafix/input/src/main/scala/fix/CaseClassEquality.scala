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
      var request: Vector[Byte]
  )
  case class Target(endpoint: String, closeFn: () => Unit) // assert: CaseClassEquality.unstable-equality
  case class Plain(name: String, values: Vector[Double], cache: Map[String, Int])
  trait Reduction { def apply(a: Int, b: Int): Int }
  trait Named { def name: String }
  sealed trait Shape { def scale(by: Double): Shape }
  case class Kernel(name: String, expected: Reduction) // assert: CaseClassEquality.unstable-equality
  case class Labelled(label: Named, shape: Shape)
  class Holder(values: Array[Double])
  trait Thunk { def apply(): Unit }
  case class Deferred(name: String, thunk: Thunk) // assert: CaseClassEquality.unstable-equality
  case class Job(name: String, task: Runnable)    // assert: CaseClassEquality.unstable-equality
  case class Mapper(name: String, transform: java.util.function.Function[Int, Int]) // assert: CaseClassEquality.unstable-equality
  trait Strategy { def choose(values: List[Int]): Int }
  object Smallest extends Strategy { def choose(values: List[Int]): Int = values.min }
  case class Planned(name: String, strategy: Strategy)
  case class Sorted(name: String, order: Ordering[Int], same: Equiv[Int], by: java.util.Comparator[Int])
  case class Ranked(name: String, rank: Comparable[Int])
}
