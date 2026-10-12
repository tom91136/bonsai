/*
rule = FilenameConventions
 */
package fix

package nested {
  object NestedPackage
  object spareObject // assert: FilenameConventions.top-level-filename
}
