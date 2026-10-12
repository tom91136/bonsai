/*
rule = NamespaceImports
NamespaceImports.packages = "org.example"
 */
package org.example.singlefixture {
  class Item
  object Items { val value = 1 }
}

package fix {
  object NamespaceImportsSingle {
    import org.example.singlefixture.Item // assert: NamespaceImports.namespace-import
    val qualified = org.example.singlefixture.Items.value // assert: NamespaceImports.qualified-reference
  }
}
