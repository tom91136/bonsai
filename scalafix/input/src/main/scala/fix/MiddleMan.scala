/*
rule = MiddleMan
 */
package fix

import scala.language.implicitConversions

object MiddleMan {
  object Engine {
    def start(speed: Int): Int            = speed * 2
    def stop(): Unit                      = ()
    def measure(bytes: Vector[Byte]): Int = bytes.size
  }

  object Facade { // assert: MiddleMan.middle-man
    def start(speed: Int): Int = Engine.start(speed)
    def stop(): Unit           = Engine.stop()
  }

  object Platform {
    def epochMillis: Long = Engine.start(0).toLong
    def monotonic(): Unit = Engine.stop()
  }

  object Keywords {
    private val values                  = Set("class", "def")
    def contains(name: String): Boolean = values(name)
  }

  object Conversions {
    implicit def widen(speed: Int): Long = Engine.start(speed).toLong
  }

  object Loader {
    case class Loaded(speed: Int)
    def load(speed: Int): Int = Engine.start(speed)
  }

  trait Decoder { def decode(bytes: Vector[Byte]): Int }

  object Disassembler extends Decoder { // assert: MiddleMan.middle-man
    override def decode(bytes: Vector[Byte]): Int = Engine.measure(bytes)
  }

  object Wrapper {
    def start(speed: Int): Int = Engine.start(speed + 1)
    def stop(): Unit           = Engine.stop()
  }

  object Bridged { // assert: MiddleMan.middle-man
    import Engine.*
    def start(speed: Int): Int = Engine.start(speed)
    def stop(): Unit           = Engine.stop()
  }
}
