/*
rule = FilenameConventions
 */
package fix

object TopLevelCycle {
  def run(context: spareContext, note: spareNote): String = context.label + note.text
  def probe(directory: spareDirectory): TopLevelCycle.type = directory.ledger
}
class spareContext(val label: String) {
  def owner: TopLevelCycle.type = TopLevelCycle
}
trait spareDirectory { def ledger: spareLedger }
type spareLedger = TopLevelCycle.type
class spareNote(val text: String) // assert: FilenameConventions.top-level-filename
