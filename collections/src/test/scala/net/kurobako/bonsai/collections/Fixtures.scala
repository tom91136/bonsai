package net.kurobako.bonsai.collections

sealed trait Fn extends Arg

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

  case class Projected(index: Int) extends Fn {
    def text: String                     = s"_._$index"
    def pure: Boolean                    = true
    override def projection: Option[Int] = Some(index)
    def applied(value: String): String   = s"$value._$index"
  }
}

object Fixtures {
  val Strict: Kind = Kind(library = true)
  val Lazy: Kind   = Kind(library = true, strict = false)
  val User: Kind   = Kind(library = false)
  val Dict: Kind   = Kind(library = true, ordered = false, keyed = true)
  val Unique: Kind = Kind(library = true, ordered = false, distinct = true)
  val Opt: Kind    = Kind(library = true, single = true)

  def named(name: String, kind: Kind = Strict): Kind = kind.copy(name = name)

  val ListOf: Kind   = named("immutable.List")
  val VectorOf: Kind = named("immutable.Vector")
  val RangeOf: Kind  = named("immutable.Range")
  val SeqOf: Kind    = named("immutable.Seq")
  val ArrayOf: Kind  = named("Array")
  val StringOf: Kind = named("String")
  val SetOf: Kind    = named("immutable.Set", Unique)
  val MapOf: Kind    = named("immutable.Map", Dict)
  val OptionOf: Kind = named("Option", Opt)

  def equalTo(constant: String): Fn     = Fn.Compares(s"_ == $constant", equal = true, constant)
  def differsFrom(constant: String): Fn = Fn.Compares(s"_ != $constant", equal = false, constant)
  def member(set: String): Fn           = Fn.Member(s"$set.contains", set)

  def fn(text: String): Fn     = Fn.Plain(text, pure = true)
  def effect(text: String): Fn = Fn.Plain(text, pure = false)
  def literal(value: Any): Fn  = Fn.Constant(value)
  def projection(n: Int): Fn   = Fn.Projected(n)
  val same: Fn                 = Fn.Called("identity", value => value, identity = true)

  def step(method: String, args: Arg*): Step = Step(method, args.toVector, Strict, Strict)

  def on(kind: Kind, method: String, args: Arg*): Step = Step(method, args.toVector, kind, kind)

  def chain(source: String, steps: Step*): Chain = Chain(fn(source), steps.toVector)

  def rewrites(chain: Chain): Vector[Outcome.Rewrite] = Laws.outcomes(chain).collect { case r: Outcome.Rewrite => r }

  def reports(chain: Chain): Vector[Outcome.Report] = Laws.outcomes(chain).collect { case r: Outcome.Report => r }
}
