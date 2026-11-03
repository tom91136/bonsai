package net.kurobako.bonsai.effects

/** What a call is made on: this, a value the method allocated and no one else holds yet, or anything else, which is a
  * local when the receiver is a reference to one.
  */
enum Receiver[+T] {
  case This
  case Fresh
  case Other(target: Option[T])
}
