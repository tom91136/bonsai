package fix

import fix.headers.`Content-Type`
import fix.headers.`X-Request-Id`

object FullyQualifiedReferenceBackticks {
  val header = `Content-Type`("text")
  val identifier = new `X-Request-Id`
}
