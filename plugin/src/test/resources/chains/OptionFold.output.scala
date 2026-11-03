// format: off
// option: option-fold=map-get-or-else
package fix

import scala.language.implicitConversions

object OptionFold {
  case class User(name: String)

  val option: Option[Int] = Some(1)
  val user: Option[User] = None

  def twice(value: Int): Int = value * 2

  val counted = option.map(_ + 1).getOrElse(0)
  val named = user.map(_.name).getOrElse("")
  val positive = option.map(_ > 0).getOrElse(false)
  val referenced = option.map(twice).getOrElse(0)
  val labelled = user.map(found => s"user ${found.name}").getOrElse("anonymous")
  val widened = option.fold(0L)(_ + 1L)
  val wrapped = option.fold(None: Option[Int])(Some(_))
  val typed = option.fold[Int](0)(_ + 1)
  val adapted = Option(Some(1.toByte)).fold(0)(_.get)
  given Conversion[String, User] = User(_)
  val converted: User = user.fold(User("a"))(found => found.name)
  val identical = option.fold(0)(identity)
  val spelled = option.map(_ + 1).getOrElse(0)
  val list = List(1, 2).fold(0)(_ + _)
}
