/*
rule = FilenameConventions
 */
package fix

object WorkspaceDraft {}

val workspaceDraft = 1 /* assert: FilenameConventions.top-level-filename
    ^^^^^^^^^^^^^^
top-level declaration workspaceDraft in WorkspaceDraft.scala must be nested in object WorkspaceDraft
*/
