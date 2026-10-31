package net.kurobako.bonsai.effects

/** The exceptions calls to the standard library and to the libraries code commonly calls throw. */
object Throwing {

  enum Platform {
    case JVM, JS, Native
  }

  /** Calls matching each pattern throw the listed exceptions for a failure their callers are expected to handle. */
  private val Throws: Map[String, List[String]] = Map(
    """java\.time\..*\.parse"""                          -> List("java.time.format.DateTimeParseException"),
    """java\.lang\.(Integer|Long|Short|Byte)\.parse.*""" -> List("java.lang.NumberFormatException"),
    """java\.lang\.(Double|Float)\.parse.*"""            -> List("java.lang.NumberFormatException"),
    """java\.util\.UUID\.fromString"""                   -> List("java.lang.IllegalArgumentException"),
    """java\.util\.Base64\.Decoder\.decode"""            -> List("java.lang.IllegalArgumentException"),
    // Parse errors arrive wrapped in a TraceException unless read is called with trace = false.
    """ujson(\.package)?\.read""" -> List("upickle.core.TraceVisitor.TraceException", "ujson.ParsingFailedException")
  )

  /** Calls whose exceptions depend on the platform, such as HTTP clients over each platform's own transport. */
  private val Varying: Map[String, Map[Platform, List[String]]] = Map(
    """sttp\.client4\..*\.send""" -> Map(
      Platform.JVM    -> List("sttp.client4.SttpClientException"),
      Platform.JS     -> List("sttp.client4.SttpClientException", "scala.scalajs.js.JavaScriptException"),
      Platform.Native -> List("sttp.client4.SttpClientException", "java.lang.RuntimeException")
    )
  )

  /** Whether the exceptions of calls matching `pattern` depend on the platform. */
  def varies(pattern: String): Boolean = Varying.contains(pattern)

  def throws(platform: Platform): Map[String, List[String]] = Throws ++ Varying.view.mapValues(_(platform))
}
