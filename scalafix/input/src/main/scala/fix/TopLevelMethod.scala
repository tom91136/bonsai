/*
rule = FilenameConventions
 */
package fix

def TopLevelMethod: Int = 1
def spareMethod: Int    = 2 // assert: FilenameConventions.top-level-filename
