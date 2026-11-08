package demo

object Branches {
  def role(equals: Boolean, hash: Boolean, other: Boolean, name: String): Option[String] =
    if (equals) Some(name)
    else if (hash) Some(name)
    else if (other) Some(name.trim)
    else None
}
