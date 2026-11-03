// format: off
// option: option-fold=map-get-or-else
package fix

import scala.language.implicitConversions

object OptionFold {
  case class User(name: String)

  val option: Option[Int] = Some(1)
  val user: Option[User] = None

  def twice(value: Int): Int = value * 2

  val counted = option.fold(0)(_ + 1)// assert: fold-to-map-get-or-else
  val named = user.fold("")(_.name)// assert: fold-to-map-get-or-else
  val positive = option.fold(false)(_ > 0)// assert: fold-to-map-get-or-else
  val referenced = option.fold(0)(twice)// assert: fold-to-map-get-or-else
  val labelled = user.fold("anonymous")(found => s"user ${found.name}")// assert: fold-to-map-get-or-else
  val widened = option.fold(0L)(_ + 1L)// assert: fold-to-map-get-or-else
  val wrapped = option.fold(None: Option[Int])(Some(_))// assert: fold-to-map-get-or-else
  val typed = option.fold[Int](0)(_ + 1)// assert: fold-to-map-get-or-else
  val adapted = Option(Some(1.toByte)).fold(0)(_.get)// assert: fold-to-map-get-or-else
  given Conversion[String, User] = User(_)
  val converted: User = user.fold(User("a"))(found => found.name)// assert: fold-to-map-get-or-else
  val identical = option.fold(0)(identity)
  val spelled = option.map(_ + 1).getOrElse(0)
  val list = List(1, 2).fold(0)(_ + _)
}
