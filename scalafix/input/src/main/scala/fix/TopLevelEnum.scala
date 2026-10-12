/*
rule = FilenameConventions
 */
package fix

enum TopLevelEnum { case First }
enum spareEnum { case Second } // assert: FilenameConventions.top-level-filename
