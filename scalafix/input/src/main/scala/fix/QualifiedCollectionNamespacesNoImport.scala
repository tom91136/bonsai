/*
rule = QualifiedCollectionNamespaces
QualifiedCollectionNamespaces.namespaces = [
  { package = "org.w3c.dom", prefix = "dom", subpackages = false }
]
 */
package fix

object QualifiedCollectionNamespacesNoImport {
  val node: org.w3c.dom.Node = null// assert: QualifiedCollectionNamespaces.unqualified
}
