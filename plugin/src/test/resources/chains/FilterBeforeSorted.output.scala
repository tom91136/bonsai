// format: off
package fix

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

object FilterBeforeSorted {
  val values = List(3, 1, 2)

  val simple = values.sorted.filter(_ > 1)
  val explicitOrdering = values.sorted(using Ordering.Int).filter(_ > 1)
  val explicitType = values.sorted[Int].filter(_ > 1)

  type Vec[A] = ArraySeq[A]
  extension [A: ClassTag](input: IterableOnce[A]) def toVec: Vec[A] = ArraySeq.from(input)
  val encodedNames = List("z", "a", "m").toVec
  val vecPipeline = encodedNames.sorted.filter(_.nonEmpty)

  val alreadyFiltered = values.filter(_ > 1).sorted
  val directSorted = values.sorted

  final class ApplicationMethods {
    def sorted: ApplicationMethods = this
    def filter(predicate: Int => Boolean): ApplicationMethods = this
  }
  val custom = new ApplicationMethods
  val customPipeline = custom.sorted.filter(_ > 1)
}
