/*
rule = FullyQualifiedReference
QualifiedCollectionNamespaces.namespaces = [
  { package = "java.util.concurrent", prefix = "java.util.concurrent", subpackages = false }
]
 */
package fix

object FullyQualifiedReferenceConfigured {
  val unit = java.util.concurrent.TimeUnit.SECONDS
  val pattern = java.util.regex.Pattern.compile("a")// assert: FullyQualifiedReference.fully-qualified-reference
}
