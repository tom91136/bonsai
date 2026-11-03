/*
rule = FilenameConventions
 */
package fix

object TopLevelStrays
object spareFirst // assert: FilenameConventions.top-level-filename
class spareSecond // assert: FilenameConventions.top-level-filename

class spareLeft(val right: spareRight) // assert: FilenameConventions.top-level-filename
class spareRight(val left: spareLeft) // assert: FilenameConventions.top-level-filename
