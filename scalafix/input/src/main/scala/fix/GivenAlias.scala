/*
rule = FilenameConventions
 */
package fix

object GivenAlias {}

given namedAlias: Int = 1 // assert: FilenameConventions.top-level-filename
