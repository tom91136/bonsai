package net.kurobako.bonsai.effects

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

/** An effect; `onThis` when it acts on the receiver of the method, `target` the local it acts on, `via` the method it
  * comes through, `library` when a library body it reaches performs it, `assumed` when nothing but the method's name
  * suggests it, and `partial` when only some of the bodies a call may run perform it.
  */
case class Effect[+T](
    kind: Kind,
    cause: String,
    onThis: Boolean = false,
    target: Option[T] = None,
    via: Option[String] = None,
    library: Boolean = false,
    assumed: Boolean = false,
    partial: Boolean = false
) {
  // hashed far more often than built
  override val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)

  lazy val shown: String = told(partial)

  def told(possibly: Boolean): String =
    via.fold(cause)(method => s"calls $method, which ${if (possibly) s"may ${kind.base}" else kind.shown}")
}
