package net.kurobako.bonsai.effects

/** An effect; `onThis` when it acts on the receiver of the method, `target` the local it acts on, `via` the method it
  * comes through, `library` when a library body it reaches performs it, and `assumed` when nothing but the method's
  * name suggests it.
  */
final case class Effect[+T](
    kind: Kind,
    cause: String,
    onThis: Boolean = false,
    target: Option[T] = None,
    via: Option[String] = None,
    library: Boolean = false,
    assumed: Boolean = false
) {
  // hashed far more often than built
  override val hashCode: Int = (kind, cause, onThis, target, via, library, assumed).hashCode

  lazy val shown: String = via.fold(cause)(method => s"calls $method, which ${kind.shown}")
}

object Effect {

  /** An effect on whatever the callee is invoked on: the receiver of the method itself when it is `This`, a local when
    * it is `Other`, and none a caller can see when the receiver is `Fresh`.
    */
  def on[T](receiver: Receiver[T], kind: Kind, cause: String): Option[Effect[T]] = receiver match {
    case Receiver.This         => Some(Effect(kind, cause, onThis = true))
    case Receiver.Fresh        => None
    case Receiver.Other(local) => Some(Effect(kind, cause, target = local))
  }
}

/** What a call is made on: this, a value the method allocated and no one else holds yet, or anything else, which is a
  * local when the receiver is a reference to one.
  */
enum Receiver[+T] {
  case This
  case Fresh
  case Other(target: Option[T])
}

/** A method's effects and the indices of the function and by-name parameters it runs. */
final case class Summary[+T](effects: Iterable[Effect[T]], invokes: Set[Int])
