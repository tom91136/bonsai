/*
rule = BannedSyntax
 */
package fix

import java.util.concurrent.locks.ReentrantLock

final class FinalClass                   // assert: BannedSyntax.final-class
final case class FinalRecord(value: Int) // assert: BannedSyntax.final-class

object ClassModifiers {
  private final class Nested // assert: BannedSyntax.final-class
  class Plain
  case class Record(value: Int)
  open class Extensible
  sealed class Closed
  final val MaximumBytes = 64
  final def value: Int   = MaximumBytes

  @SuppressWarnings(Array("scalafix:BannedSyntax.final-class"))
  final class Exception
}

object ExplicitNew {
  val rejected  = new StringBuilder() // assert: BannedSyntax.explicit-new
  val anonymous = new Runnable {      // assert: BannedSyntax.anonymous-allocation
    override def run(): Unit = ()
  }
  val accepted   = StringBuilder()
  val suppressed = new StringBuilder() // scalafix:ok BannedSyntax

  class Secret private (val value: String)
  object Secret {
    def apply(value: String): Secret = new Secret(value)
    def empty: Secret                = new Secret("")
  }

  class Bare(val value: Int)
  object Bare {
    def zero: Bare = new Bare(0) // assert: BannedSyntax.explicit-new
  }

  trait Creates[T] {
    def create(value: Int): T
    def apply(value: Int): T = create(value)
  }
  class Inherited(val value: Int)
  object Inherited extends Creates[Inherited] {
    def create(value: Int): Inherited = new Inherited(value)
  }

  class ApplyDiffers(val value: Double)
  object ApplyDiffers {
    def apply(): String = "now"
  }
  val differs = new ApplyDiffers(1.0)

  object Inferred {
    def apply[A](value: => A): Inferred[A] = new Inferred(value)
  }
  class Inferred[A](value: => A)

  class Given[A](val value: A)
  object Given {
    def apply[A](value: A): Given[A] = new Given[A](value)
  }

  class BareGeneric[A](val value: A)
  val bareGeneric = new BareGeneric(1) // assert: BannedSyntax.explicit-new

  case class Plain(value: Int)
  val plain = new Plain(1) // assert: BannedSyntax.explicit-new

  case class Guarded private (value: Int)
  object Guarded {
    def apply(value: Int): Guarded = new Guarded(value.max(0))
  }
}

object ShadowedNew {
  class Url(val raw: String)
  def Url(raw: String): Url = new Url(raw)

  class Link(val raw: String)
  def make(raw: String): Link = new Link(raw)
  def Link(raw: String): Link = make(raw)

  class Path(val raw: String)
  val makePath: String => Path = raw => new Path(raw)
  val Path: String => Path     = makePath
}

object UnshadowedNew {
  class Link(val raw: String)
  def make(raw: String): Link = new Link(raw) // assert: BannedSyntax.explicit-new
  def Other(raw: String): Link = make(raw)
}

object ArrayAllocations {
  val sized: Array[Long]                = new Array[Long](4) // assert: BannedSyntax.explicit-new-array
  val ofDim: Array[Long]                = Array.ofDim[Long](4)
  val literal: Array[Long]              = Array(1L, 2L)
  def sizedBy(length: Int): Array[Long] = new Array[Long](length) // assert: BannedSyntax.explicit-new-array
}

object IntrinsicLocks {
  private val monitor = Object()
  private val lock    = ReentrantLock()

  def rejected(): Unit            = monitor.synchronized(()) // assert: BannedSyntax.intrinsic-lock
  def rejectedUnqualified(): Unit = synchronized(())         // assert: BannedSyntax.intrinsic-lock
  def accepted(): Unit            = {
    lock.lock()
    try ()
    finally lock.unlock()
  }
  def suppressed(): Unit = monitor.synchronized(()) // scalafix:ok BannedSyntax
}

object Iterators {
  val scalaValues = List(1, 2, 3).iterator                // assert: BannedSyntax.no-iterator
  val javaValues  = java.util.List.of(1, 2, 3).iterator() // assert: BannedSyntax.no-iterator
  val mapKeys     = Map(1 -> 2).keysIterator              // assert: BannedSyntax.no-iterator
  val mapValues   = Map(1 -> 2).valuesIterator            // assert: BannedSyntax.no-iterator
  val lazyValues  = List(1, 2, 3).view
}

object StatefulIterators {
  val cursor       = List(1, 2, 3).iterator
  val first: Int   = cursor.next()
  val lead         = java.util.List.of(1, 2, 3).iterator()
  val leadPending  = lead.hasNext()
  val peekable     = List(1, 2, 3).iterator.buffered
  val chained      = List(1, 2, 3).iterator.map(_ + 1) // assert: BannedSyntax.no-iterator
}

object ProductTraversal {
  def inspect(value: Any): String = value match {
    case product: Product => product.toString // assert: BannedSyntax.product-traversal
    case number: Int      => number.toString
    case _                => "other"
  }

  def qualified(value: Any): Boolean = value match {
    case _: scala.Product => true // assert: BannedSyntax.product-traversal
    case _                => false
  }

  def rooted(value: Any): Boolean = value match {
    case _: _root_.scala.Product => true // assert: BannedSyntax.product-traversal
    case _                       => false
  }

  def fields(value: Product) = value.productIterator       // assert: BannedSyntax.product-traversal
  def field(value: Product)  = value.productElement(0)     // assert: BannedSyntax.product-traversal
  def names(value: Product)  = value.productElementNames   // assert: BannedSyntax.product-traversal
  def name(value: Product)   = value.productElementName(0) // assert: BannedSyntax.product-traversal
  def arity(value: Product)  = value.productArity          // assert: BannedSyntax.product-traversal

  case class Node(value: Int)
  def typed(value: Node): Int = value match {
    case Node(number) => number
  }

  @SuppressWarnings(Array("scalafix:BannedSyntax.product-traversal"))
  def derivedField(value: Product): Any = value.productElement(0)
}

object Conditionals {
  val accepted = if (1 > 0) "positive" else "non-positive"
  val rejected = if 1 > 0 then "positive" else "non-positive" // assert: BannedSyntax.no-then
}
