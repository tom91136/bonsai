/*
rule = FullyQualifiedReference
 */
package fix

object FullyQualifiedReferenceBackticks {
  val header = fix.headers.`Content-Type`("text")// assert: FullyQualifiedReference.fully-qualified-reference
  val identifier = new fix.headers.`X-Request-Id`// assert: FullyQualifiedReference.fully-qualified-reference
}
