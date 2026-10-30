/*
rule = ToolingComments
 */
package fix

object ToolingComments {
  val Limit = 4 /* assert: ToolingComments.tooling-comment*/ // a literal, so the linter stays quiet

  class Standalone /* assert: ToolingComments.tooling-comment*/ // in its own type so the linter is happy

  /* silencing the warning here would hide the cast */ // assert: ToolingComments.tooling-comment
  def cast(value: Any): Int = value.hashCode

  /* breaking the dependency cycle between the two packages */ // assert: ToolingComments.tooling-comment
  def cycle: Int = 1

  // scalafix:off ToolingComments
  // the linter wants this
  def disabled: Int = 2
  // scalafix:on ToolingComments

  def reported: Int = 3 /* assert: ToolingComments.tooling-comment*/ // would otherwise be flagged as unused

  def retried: Int = 11 // the loop would otherwise trigger another pass

  // the table is acyclic by construction
  def graph: Int = 4

  def ordered: Int = 9 /* assert: ToolingComments.tooling-comment*/ // sits above the ops module, which keeps the modules acyclic

  // the assembler breaks the cycle once the counter wraps
  def counter: Int = 12

  // the pipeline keeps the stages acyclic
  def stages: Int = 13

  // the division rounds its own quotient, which no fused form computes
  def division: Int = 5

  // the rule of three says to name a method after its second use
  def rules: Int = 6

  // bonsai:ignore forbidden-term
  def marked: Int = 7

  // scalafmt: { align.preset = none }
  def formatted: Int = 10

  // the unused parameter would otherwise be flagged
  @SuppressWarnings(Array("scalafix:ToolingComments"))
  def kept(unused: Int): Int = {
    // the linter wants this
    8
  }

  /* the default is the only one that makes sense, which a written default would only repeat */ // assert: ToolingComments.tooling-comment
  @SuppressWarnings(Array("bonsai:redundant-default"))
  def defaulted(value: Int = 1): Int = value

  /* a reader of the field would only repeat what the codec reads */ // assert: ToolingComments.tooling-comment
  @SuppressWarnings(Array("bonsai:unread-field"))
  def field: Int = 14

  // the default is the singleton constant, which widening to Int would lose
  @SuppressWarnings(Array("bonsai:redundant-default"))
  def narrow(value: Int = 1): Int = value

  // the second branch would only repeat the first
  @SuppressWarnings(Array("bonsai:redundant-default"))
  def branches: Int = 16
}
