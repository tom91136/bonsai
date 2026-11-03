package net.kurobako.bonsai.collections

object Fn {
  case class Plain(text: String, pure: Boolean) extends Fn {
    def applied(value: String): String = text.replace("_", value)
  }

  case class Called(text: String, call: String => String, override val identity: Boolean = false) extends Fn {
    def pure: Boolean                  = true
    def applied(value: String): String = call(value)
  }

  case class Constant(value: Any) extends Fn {
    def text: String                   = value.toString
    def pure: Boolean                  = true
    override def literal: Option[Any]  = Some(value)
    def applied(value: String): String = text
  }

  case class Typed(text: String, override val valueType: Option[String], override val resultType: Option[String])
      extends Fn {
    def pure: Boolean                  = true
    def applied(value: String): String = text.replace("_", value)
  }

  case class Compares(text: String, equal: Boolean, constant: String, assignable: Boolean = true) extends Fn {
    def pure: Boolean                           = true
    override def comparison: Option[Comparison] = Some(Comparison(equal, constant, assignable))
    def applied(value: String): String          = text
  }

  case class Member(text: String, set: String) extends Fn {
    def pure: Boolean                       = true
    override def membership: Option[String] = Some(set)
    def applied(value: String): String      = s"$set.contains($value)"
  }

  case class Facts(
      text: String,
      override val inspects: Option[String] = None,
      override val destructures: Option[String] = None,
      override val selection: Boolean = false,
      override val diverges: Boolean = false
  ) extends Fn {
    def pure: Boolean                  = true
    def applied(value: String): String = text.replace("_", value)
  }

  case class Projected(index: Int) extends Fn {
    def text: String                     = s"_._$index"
    def pure: Boolean                    = true
    override def projection: Option[Int] = Some(index)
    def applied(value: String): String   = s"$value._$index"
  }
}

sealed trait Fn extends Arg
