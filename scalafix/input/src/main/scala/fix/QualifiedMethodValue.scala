/*
rules = [FullyQualifiedReference, ExplicitEtaExpansion]
 */
package fix

import java.nio.file.Path

object QualifiedMethodValue {
  val paths: List[Path] = Nil
  val sizes = paths.map((// assert: ExplicitEtaExpansion.method-value
    java.nio.file.Files).size)// assert: FullyQualifiedReference.fully-qualified-reference
}
