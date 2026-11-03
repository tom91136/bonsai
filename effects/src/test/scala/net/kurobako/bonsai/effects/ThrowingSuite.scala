package net.kurobako.bonsai.effects

import net.kurobako.bonsai.effects.Throwing.*

class ThrowingSuite extends munit.FunSuite {
  test("exceptions by pattern, with the platform's own transport errors") {
    val common = Map(
      """java\.time\..*\.parse"""                          -> List("java.time.format.DateTimeParseException"),
      """java\.lang\.(Integer|Long|Short|Byte)\.parse.*""" -> List("java.lang.NumberFormatException"),
      """java\.lang\.(Double|Float)\.parse.*"""            -> List("java.lang.NumberFormatException"),
      """java\.util\.UUID\.fromString"""                   -> List("java.lang.IllegalArgumentException"),
      """java\.util\.Base64\.Decoder\.decode"""            -> List("java.lang.IllegalArgumentException"),
      """ujson(\.package)?\.read""" -> List("upickle.core.TraceVisitor.TraceException", "ujson.ParsingFailedException")
    )
    val send = """sttp\.client4\..*\.send"""
    assert(Throwing.varies(send))
    assert(!Throwing.varies("""ujson(\.package)?\.read"""))
    assertEquals(Throwing.throws(Platform.JVM), common + (send -> List("sttp.client4.SttpClientException")))
    assertEquals(
      Throwing.throws(Platform.JS),
      common + (send -> List("sttp.client4.SttpClientException", "scala.scalajs.js.JavaScriptException"))
    )
    assertEquals(
      Throwing.throws(Platform.Native),
      common + (send -> List("sttp.client4.SttpClientException", "java.lang.RuntimeException"))
    )
  }
}
