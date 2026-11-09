package scala.scalajs.js {
  class Array[A]
  object Array { def apply[A](items: A*): Array[A] = new Array[A] }
}

package fix {
  import scala.scalajs.js

  class Sack[A]
  object Sack {
    def apply[A](items: A*): Sack[A] = new Sack[A]
    def empty[A]: Sack[A]            = new Sack[A]
  }

  object EmptyCollectionMissing {
    val qualified = js.Array[Int]()
    val imported = scala.scalajs.js.Array[String]()
    val stdlib = Vector.empty[Int]
    val own = Sack.empty[Int]
  }
}
