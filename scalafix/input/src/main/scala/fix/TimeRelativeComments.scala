/*
rule = TimeRelativeComments
 */
package fix

object TimeRelativeComments {
  val Yet = 1 /* assert: TimeRelativeComments.time-relative-comment*/ // the cache is not wired up yet

  val Now = 2 /* assert: TimeRelativeComments.time-relative-comment*/ // a linear scan for now

  val Moment = 3 /* assert: TimeRelativeComments.time-relative-comment*/ // only one backend at the moment

  val Current = 4 /* assert: TimeRelativeComments.time-relative-comment*/ // this is currently unsupported

  val Longer = 5 // the token is no longer live once it completes

  val Anymore = 6 // a released slot is not reused anymore

  val Days = 7 /* assert: TimeRelativeComments.time-relative-comment*/ // blocking reads are rare these days

  /* not done yet */ /* assert: TimeRelativeComments.time-relative-comment
  ^^^^^^^^^^^^^^^^^^
the comment states a point in time that the code outlives; say what the code does and why
*/
  val Message = 8

  // the writer is not threaded through, which is why it is a global
  val Plain = 9

  // todo: drop this once it is no longer needed
  val Todo = 10

  // FIXME not finished yet
  val Fixme = 11

  // XXX for now
  val Marked = 12

  // names no longer than eight characters
  val Comparative = 13

  // the lock is released at the moment the callback returns
  val Instant = 14

  // returns the currently selected item
  val Selected = 15

  // yet another loop over the same table
  val Conjunction = 16

  // scalafix:off TimeRelativeComments
  // not wired up yet
  val Disabled = 17
  // scalafix:on TimeRelativeComments
}
