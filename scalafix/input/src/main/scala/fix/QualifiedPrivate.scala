/*
rule = QualifiedPrivate
 */
package fix

object QualifiedPrivate {
  class Needed private[QualifiedPrivate] ( // assert: QualifiedPrivate.companion-factory
      private[QualifiedPrivate] val token: Int
  )
  class Factory {
    def make: Needed     = new Needed(1)
    def read(needed: Needed): Int = needed.token
  }

  class Redundant private[QualifiedPrivate] ( // assert: QualifiedPrivate.redundant-qualifier
      private[QualifiedPrivate] val token: Int // assert: QualifiedPrivate.redundant-qualifier
  ) {
    def twice: Int = token * 2
  }
  object Redundant {
    def apply(): Redundant = new Redundant(1)
  }

  class Holder {
    private[Holder] def helper: Int = 1 // assert: QualifiedPrivate.redundant-qualifier
    def use: Int                    = helper
  }

  class Applied private[QualifiedPrivate] (val value: Int) // assert: QualifiedPrivate.companion-factory
  def applied: Applied = Applied(1)

  class Generic[A] private[QualifiedPrivate] (val value: A) // assert: QualifiedPrivate.companion-factory
  def generic: Generic[Int] = Generic(1)

  trait Base { private[QualifiedPrivate] def run: Int }
  class Impl extends Base { override private[QualifiedPrivate] def run: Int = 1 }
  def runs(base: Base): Int = base.run

  object Binary {
    @scala.annotation.publicInBinary private[Binary] def binary: Int = 1
    inline def inlined: Int                                         = binary
  }

  class Unused {
    private[QualifiedPrivate] def helper: Int = 1 // assert: QualifiedPrivate.redundant-qualifier
  }

  private[QualifiedPrivate] class Escaping(val value: Int)
  def escaping(value: Int): Escaping = new Escaping(value)

  private[QualifiedPrivate] class Scoped
  private[QualifiedPrivate] trait Marker
  private[QualifiedPrivate] object Nested

  class Shared {
    private[QualifiedPrivate] def peek: Int = 2
  }
  def read(shared: Shared): Int = shared.peek
}
