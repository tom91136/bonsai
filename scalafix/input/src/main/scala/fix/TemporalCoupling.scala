/*
rule = TemporalCoupling
 */
package fix

import scala.compiletime.uninitialized

object TemporalCoupling {
  class Session {
    private var connection: String         = uninitialized // assert: TemporalCoupling.uninitialised
    private var token: String              = null          // assert: TemporalCoupling.uninitialised
    private var retries: Int               = 0
    private var returns: List[Long] | Null = null
    private var empty: Int                 = _ // assert: TemporalCoupling.uninitialised
    def open(): Unit                       = { connection = "open"; token = "token" }
  }

  trait Pool {
    private var slot: String = _ // assert: TemporalCoupling.uninitialised
  }
}
