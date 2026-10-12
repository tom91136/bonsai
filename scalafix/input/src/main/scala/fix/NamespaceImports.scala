/*
rule = NamespaceImports
NamespaceImports.packages = ["org.example"]
 */
package org.example.importfixture {
  class Item
  object Items {
    val value                          = 1
    given itemOrdering: Ordering[Item] = Ordering.by(_ => 0)
  }
}

package fix {
  object NamespaceImports {
    import org.example.importfixture.Item // assert: NamespaceImports.namespace-import
    val item: Item = new Item

    val typed: org.example.importfixture.Item = // assert: NamespaceImports.qualified-reference
      item
    val qualifiedValue =
      org.example.importfixture.Items.value // assert: NamespaceImports.qualified-reference
    val rooted =
      new _root_.org.example.importfixture.Item // assert: NamespaceImports.qualified-reference
    val escaped =
      `org`.example.importfixture.Items.value // assert: NamespaceImports.qualified-reference

    // format: off
    object Grouped {
      import org.example.importfixture.{Item, Items} // assert: NamespaceImports.namespace-import
      val pair = (new Item, Items.value)
    }
    // format: on

    object Allowed {
      import org.example.importfixture.*
      import org.example.importfixture.Items.{given, *}
      import org.example.importfixture.{Item as RenamedItem, *}
      import scala.collection.immutable.List
      val item     = new RenamedItem
      val ordering = summon[Ordering[Item]]
      val values   = List(value)
      val source   = "org.example.importfixture.Item"
      export org.example.importfixture.Items.value
    }

    // format: off
    object Relative {
      import org.example.importfixture.*
      import Items.value // assert: NamespaceImports.namespace-import
      val copy = value
    }
    // format: on

    object NamedGiven {
      import org.example.importfixture.*
      import org.example.importfixture.Items.itemOrdering
      val ordering = summon[Ordering[Item]]
    }

    object Unrelated {
      object org {
        object example {
          val value = 1
        }
      }
      val copy = org.example.value
    }
  }
}
