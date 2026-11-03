package net.kurobako.bonsai.collections

/** The steps a law matched, which start at `from` in `chain`, and what they cost before and after the law. */
private[collections] case class Hit(chain: Chain, from: Int, steps: Vector[Step], before: Cost, after: Cost) {
  def until: Int                = from + steps.size
  def receiver: Kind            = steps.head.receiver
  def method(k: Int): String    = steps(k).method
  def args(k: Int): Vector[Arg] = steps(k).args
  def names: String             = steps.map(_.method).mkString(" then ")
  def gain: String              = Cost.gain(before, after)
}
